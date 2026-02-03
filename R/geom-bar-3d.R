StatBar3D <- ggproto("StatBar3D", Stat,
                     required_aes = c("x", "y"),
                     optional_aes = c("weight"),
                     default_aes = aes(z = after_stat(count),
                                       group = after_stat(group)),

                     compute_panel = function(data, scales, na.rm = FALSE,
                                              bins = 10, binwidth = NULL, drop = TRUE,
                                              width = 1.0, faces = "all",
                                              light = NULL, cull_backfaces = NULL) {

                           # Remove missing values if requested
                           if (na.rm) {
                                 data <- data[complete.cases(data[c("x", "y")]), ]
                           }

                           # Check we have enough data
                           if (nrow(data) < 1) {
                                 stop("stat_bar_3d requires at least 1 point")
                           }

                           # Handle weight aesthetic
                           if (!"weight" %in% names(data)) {
                                 data$weight <- 1
                           }

                           # Detect discrete vs continuous
                           x_discrete <- inherits(scales$x, "ScaleDiscrete")
                           y_discrete <- inherits(scales$y, "ScaleDiscrete")

                           # Normalize bins/binwidth to length-2 vectors
                           if (!is.null(binwidth)) {
                                 if (length(binwidth) == 1) binwidth <- c(binwidth, binwidth)
                           }
                           if (length(bins) == 1) bins <- c(bins, bins)

                           # Normalize width to length-2 vector
                           if (length(width) == 1) width <- c(width, width)

                           # Dispatch to appropriate aggregation method
                           if (x_discrete && y_discrete) {
                                 agg_data <- compute_count_2d(data, drop)
                           } else if (!x_discrete && !y_discrete) {
                                 agg_data <- compute_bin_2d(data, bins, binwidth, drop)
                           } else {
                                 agg_data <- compute_bin_mixed(data, bins, binwidth, drop, x_discrete)
                           }

                           # Check if aggregation produced any data
                           if (nrow(agg_data) == 0) {
                                 return(data.frame(
                                       x = numeric(0), y = numeric(0), z = numeric(0),
                                       group = character(0), col_id = integer(0), face_type = character(0),
                                       count = numeric(0), proportion = numeric(0), ncount = numeric(0),
                                       density = numeric(0), ndensity = numeric(0)
                                 ))
                           }

                           # Compute normalized stat variables
                           agg_data <- compute_bar_stats(agg_data)

                           # Calculate grid spacing
                           x_spacing <- resolution(agg_data$x, zero = FALSE)
                           y_spacing <- resolution(agg_data$y, zero = FALSE)

                           # Validate and process faces parameter
                           selected_faces <- select_faces(faces)
                           if (length(selected_faces) == 0) {
                                 return(data.frame(
                                       x = numeric(0), y = numeric(0), z = numeric(0),
                                       group = character(0), col_id = integer(0), face_type = character(0),
                                       count = numeric(0), proportion = numeric(0), ncount = numeric(0),
                                       density = numeric(0), ndensity = numeric(0)
                                 ))
                           }

                           # Create columns using shared infrastructure
                           # Note: z values will be overwritten by after_stat(count), so we use z0 metadata
                           # to mark which vertices should be at baseline (z=0)
                           col_faces <- create_bar_cols(agg_data, x_spacing, y_spacing, width, selected_faces)

                           if (nrow(col_faces) == 0) {
                                 return(data.frame(
                                       x = numeric(0), y = numeric(0), z = numeric(0),
                                       group = character(0), col_id = integer(0), face_type = character(0),
                                       count = numeric(0), proportion = numeric(0), ncount = numeric(0),
                                       density = numeric(0), ndensity = numeric(0)
                                 ))
                           }

                           col_faces %>%
                                 average_aesthetics() %>%
                                 mutate(cull_backfaces = cull_backfaces) %>%
                                 attach_light(light) %>%
                                 return()
                     }
)


#' Create column faces from aggregated bar data
#'
#' Similar to create_cols but accepts separate x/y widths and adds z0 metadata
#' to mark baseline vertices (since after_stat will overwrite z values)
#'
#' @param data Data frame with x, y, count columns
#' @param x_spacing Grid spacing in x direction
#' @param y_spacing Grid spacing in y direction
#' @param width Vector of length 2 giving width factors for x and y
#' @param selected_faces Character vector of face names to render
#' @return Data frame with column face vertices including z0 column
#' @keywords internal
create_bar_cols <- function(data, x_spacing, y_spacing, width, selected_faces) {

      # Calculate actual column dimensions (separate x and y widths)
      col_width_x <- x_spacing * width[1]
      col_width_y <- y_spacing * width[2]
      half_x <- col_width_x / 2
      half_y <- col_width_y / 2

      all_faces <- list()

      if (nrow(data) == 0) {
            warning("create_bar_cols: No data points provided")
            return(data.frame())
      }

      if (length(selected_faces) == 0) {
            warning("create_bar_cols: No faces selected")
            return(data.frame())
      }

      for (i in 1:nrow(data)) {
            point <- data[i, ]
            cx <- point$x
            cy <- point$y

            # Define bounds
            xmin_bound <- cx - half_x
            xmax_bound <- cx + half_x
            ymin_bound <- cy - half_y
            ymax_bound <- cy + half_y
            # Note: we store whether each corner is at z=0 (bottom) or z=height (top)
            # The actual z values will be set by after_stat(), then corrected in coord

            # Define the 8 corners of the column
            # Each corner: c(x, y, is_bottom)
            # is_bottom = TRUE means this vertex should be at z=0
            corners <- list(
                  list(x = xmin_bound, y = ymin_bound, z0 = TRUE),   # 1: left-front-bottom
                  list(x = xmax_bound, y = ymin_bound, z0 = TRUE),   # 2: right-front-bottom
                  list(x = xmax_bound, y = ymax_bound, z0 = TRUE),   # 3: right-back-bottom
                  list(x = xmin_bound, y = ymax_bound, z0 = TRUE),   # 4: left-back-bottom
                  list(x = xmin_bound, y = ymin_bound, z0 = FALSE),  # 5: left-front-top
                  list(x = xmax_bound, y = ymin_bound, z0 = FALSE),  # 6: right-front-top
                  list(x = xmax_bound, y = ymax_bound, z0 = FALSE),  # 7: right-back-top
                  list(x = xmin_bound, y = ymax_bound, z0 = FALSE)   # 8: left-back-top
            )

            # Define faces using corner indices (ccw when viewed from outside)
            face_definitions <- list(
                  zmin = c(1, 4, 3, 2),
                  zmax = c(5, 6, 7, 8),
                  xmin = c(1, 5, 8, 4),
                  xmax = c(2, 3, 7, 6),
                  ymin = c(1, 2, 6, 5),
                  ymax = c(4, 8, 7, 3)
            )

            for (face_name in selected_faces) {
                  if (face_name %in% names(face_definitions)) {
                        corner_indices <- face_definitions[[face_name]]
                        hierarchical_group <- paste0("col", i, "__", face_name)

                        face_vertices <- data.frame(
                              x = sapply(corner_indices, function(idx) corners[[idx]]$x),
                              y = sapply(corner_indices, function(idx) corners[[idx]]$y),
                              z0 = sapply(corner_indices, function(idx) corners[[idx]]$z0),
                              group = hierarchical_group,
                              col_id = i,
                              face_type = face_name
                        )

                        # Preserve all columns from aggregated data (count, density, etc.)
                        for (col_name in names(point)) {
                              if (!col_name %in% names(face_vertices)) {
                                    face_vertices[[col_name]] <- rep(point[[col_name]], 4)
                              }
                        }

                        all_faces[[length(all_faces) + 1]] <- face_vertices
                  }
            }
      }

      if (length(all_faces) == 0) {
            return(data.frame())
      }

      result <- do.call(rbind, all_faces)
      rownames(result) <- NULL
      return(result)
}


#' Count discrete x-y combinations
#'
#' @param data Data frame with x, y, weight columns
#' @param drop Whether to drop empty combinations
#' @return Data frame with x, y, count, bin_area columns
#' @keywords internal
compute_count_2d <- function(data, drop = TRUE) {

      # Aggregate counts
      agg <- data %>%
            group_by(x, y) %>%
            summarise(count = sum(weight), .groups = "drop")

      # Expand to all combinations if drop = FALSE
      if (!drop) {
            all_combos <- expand.grid(
                  x = unique(data$x),
                  y = unique(data$y),
                  stringsAsFactors = FALSE
            )
            agg <- merge(all_combos, agg, by = c("x", "y"), all.x = TRUE)
            agg$count[is.na(agg$count)] <- 0
      }

      # For discrete data, bin_area is 1 (for density calculation consistency)
      agg$bin_area <- 1

      # preserve aesthetics (e.g. fill, color) mapped to x or y
      agg <- data %>% select(-weight, -group) %>% distinct() %>%
            left_join(agg, ., by = join_by(x, y))

      return(agg)
}


#' Bin continuous x and y data into 2D grid
#'
#' @param data Data frame with x, y, weight columns
#' @param bins Vector of length 2 giving number of bins in x and y
#' @param binwidth Vector of length 2 giving bin widths (overrides bins if provided)
#' @param drop Whether to drop empty bins
#' @return Data frame with x, y, count, bin_area columns
#' @keywords internal
compute_bin_2d <- function(data, bins, binwidth, drop = TRUE) {

      # Compute bin edges
      x_range <- range(data$x, na.rm = TRUE)
      y_range <- range(data$y, na.rm = TRUE)

      if (!is.null(binwidth)) {
            x_breaks <- seq(x_range[1], x_range[2] + binwidth[1], by = binwidth[1])
            y_breaks <- seq(y_range[1], y_range[2] + binwidth[2], by = binwidth[2])
      } else {
            x_breaks <- seq(x_range[1], x_range[2], length.out = bins[1] + 1)
            y_breaks <- seq(y_range[1], y_range[2], length.out = bins[2] + 1)
      }

      # Compute bin widths
      x_width <- diff(x_breaks[1:2])
      y_width <- diff(y_breaks[1:2])
      bin_area <- x_width * y_width

      # Assign data to bins
      data$x_bin <- cut(data$x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE)
      data$y_bin <- cut(data$y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)

      # Compute bin centers
      x_centers <- (x_breaks[-length(x_breaks)] + x_breaks[-1]) / 2
      y_centers <- (y_breaks[-length(y_breaks)] + y_breaks[-1]) / 2

      # Aggregate counts
      agg <- data %>%
            filter(!is.na(x_bin), !is.na(y_bin)) %>%
            group_by(x_bin, y_bin) %>%
            summarise(count = sum(weight), .groups = "drop")

      # Expand to all bins if drop = FALSE
      if (!drop) {
            all_bins <- expand.grid(
                  x_bin = seq_along(x_centers),
                  y_bin = seq_along(y_centers)
            )
            agg <- merge(all_bins, agg, by = c("x_bin", "y_bin"), all.x = TRUE)
            agg$count[is.na(agg$count)] <- 0
      }

      # Convert bin indices to center coordinates
      agg$x <- x_centers[agg$x_bin]
      agg$y <- y_centers[agg$y_bin]
      agg$bin_area <- bin_area

      # Clean up
      agg <- agg %>% select(-x_bin, -y_bin)

      return(agg)
}


#' Bin mixed discrete/continuous data
#'
#' @param data Data frame with x, y, weight columns
#' @param bins Vector of length 2 giving number of bins
#' @param binwidth Vector of length 2 giving bin widths
#' @param drop Whether to drop empty bins
#' @param x_discrete Logical indicating if x is discrete (y is continuous) or vice versa
#' @return Data frame with x, y, count, bin_area columns
#' @keywords internal
compute_bin_mixed <- function(data, bins, binwidth, drop = TRUE, x_discrete) {

      if (x_discrete) {
            # x is discrete, bin y
            cont_var <- "y"
            disc_var <- "x"
            cont_bins <- bins[2]
            cont_binwidth <- if (!is.null(binwidth)) binwidth[2] else NULL
      } else {
            # y is discrete, bin x
            cont_var <- "x"
            disc_var <- "y"
            cont_bins <- bins[1]
            cont_binwidth <- if (!is.null(binwidth)) binwidth[1] else NULL
      }

      # Compute bin edges for continuous variable (globally across all discrete groups)
      cont_range <- range(data[[cont_var]], na.rm = TRUE)

      if (!is.null(cont_binwidth)) {
            cont_breaks <- seq(cont_range[1], cont_range[2] + cont_binwidth, by = cont_binwidth)
      } else {
            cont_breaks <- seq(cont_range[1], cont_range[2], length.out = cont_bins + 1)
      }

      cont_width <- diff(cont_breaks[1:2])
      cont_centers <- (cont_breaks[-length(cont_breaks)] + cont_breaks[-1]) / 2

      # Assign data to bins
      data$cont_bin <- cut(data[[cont_var]], breaks = cont_breaks, include.lowest = TRUE, labels = FALSE)

      # Aggregate counts
      agg <- data %>%
            filter(!is.na(cont_bin)) %>%
            group_by(across(all_of(disc_var)), cont_bin) %>%
            summarise(count = sum(weight), .groups = "drop")

      # Expand to all combinations if drop = FALSE
      if (!drop) {
            all_combos <- expand.grid(
                  disc_val = unique(data[[disc_var]]),
                  cont_bin = seq_along(cont_centers),
                  stringsAsFactors = FALSE
            )
            names(all_combos)[1] <- disc_var
            agg <- merge(all_combos, agg, by = c(disc_var, "cont_bin"), all.x = TRUE)
            agg$count[is.na(agg$count)] <- 0
      }

      # Convert bin indices to center coordinates
      agg[[cont_var]] <- cont_centers[agg$cont_bin]

      # bin_area is just the width of the continuous bin (discrete dimension has width 1)
      agg$bin_area <- cont_width

      # Clean up
      agg <- agg %>% select(-cont_bin)

      # preserve any aesthetics (e.g. fill, color) mapped to discrete x/y var
      if(cont_var == "x"){
            agg <- data %>% select(-x, -weight, -cont_bin) %>% distinct() %>%
                  left_join(agg, ., by = join_by(y))
      }else{
            agg <- data %>% select(-y, -weight, -cont_bin) %>% distinct() %>%
                  left_join(agg, ., by = join_by(x))
      }

      return(agg)
}


#' Compute normalized bar statistics
#'
#' @param agg_data Data frame with count and bin_area columns
#' @return Data frame with added proportion, ncount, density, ndensity columns
#' @keywords internal
compute_bar_stats <- function(agg_data) {

      n <- sum(agg_data$count)

      agg_data <- agg_data %>%
            mutate(
                  proportion = count / n,
                  ncount = count / max(count, na.rm = TRUE),
                  density = count / (n * bin_area),
                  ndensity = density / max(density, na.rm = TRUE)
            )

      # Handle edge cases where max is 0 or n is 0
      agg_data$ncount[is.nan(agg_data$ncount)] <- 0
      agg_data$ndensity[is.nan(agg_data$ndensity)] <- 0
      agg_data$proportion[is.nan(agg_data$proportion)] <- 0
      agg_data$density[is.nan(agg_data$density)] <- 0

      return(agg_data)
}


#' 3D bar chart with automatic counting or binning
#'
#' Creates 3D bar charts by automatically counting discrete data or binning continuous data.
#' This is the 3D analogue of [ggplot2::geom_bar()] and [ggplot2::geom_histogram()].
#'
#' The stat automatically detects whether x and y are discrete or continuous:
#' \itemize{
#'   \item **Both discrete**: Counts occurrences of each (x, y) combination
#'   \item **Both continuous**: Performs 2D binning (like a 3D histogram)
#'   \item **Mixed**: Bins the continuous axis while preserving discrete groups
#' }
#'
#' For pre-computed bar heights, use [geom_col_3d()] instead.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use. Defaults to `StatBar3D`.
#' @param geom The geometric object used to display the data. Defaults to `GeomPolygon3D`.
#'
#' @param bins Number of bins in each dimension. Either a single value (used for both x and y)
#'   or a vector of length 2 giving `c(bins_x, bins_y)`. Default is 10. Ignored for discrete
#'   variables.
#' @param binwidth Bin width in each dimension. Either a single value or a vector of length 2
#'   giving `c(binwidth_x, binwidth_y)`. If provided, overrides `bins`. Ignored for discrete
#'   variables.
#' @param drop If `TRUE` (the default), empty bins/combinations are not rendered. If `FALSE`,
#'   empty bins render as zero-height columns.
#' @param width Column width as a fraction of bin spacing. Either a single value (used for both
#'   x and y) or a vector of length 2 giving `c(width_x, width_y)`. Default is 1.0
#'   (columns touch). Use values less than 1 for gaps between columns.
#'
#' @inheritParams col_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#' @inheritParams position_param
#'
#' @section Aesthetics:
#' `stat_bar_3d()` requires the following aesthetics:
#' - **x**: X coordinate
#' - **y**: Y coordinate
#'
#' And optionally understands:
#' - **weight**: Observation weights for counting
#'
#' @section Computed variables:
#' These variables can be used with `after_stat()` to map to aesthetics:
#' - `count`: Number of observations in each bin (default for z)
#' - `proportion`: Count divided by total count (sums to 1)
#' - `ncount`: Count scaled to maximum of 1
#' - `density`: Count divided by (total count × bin area); integrates to 1 for continuous data
#' - `ndensity`: Density scaled to maximum of 1
#'
#' @examples
#' # Discrete x and y: count combinations
#' d_discrete <- data.frame(
#'   x = sample(letters[1:4], 200, replace = TRUE),
#'   y = sample(LETTERS[1:3], 200, replace = TRUE)
#' )
#' ggplot(d_discrete, aes(x, y)) +
#'   geom_bar_3d() +
#'   coord_3d()
#'
#' # Continuous x and y: 2D histogram
#' d_cont <- data.frame(x = rnorm(1000), y = rnorm(1000))
#' ggplot(d_cont, aes(x, y)) +
#'   geom_bar_3d() +
#'   coord_3d()
#'
#' # Mixed: one discrete, one continuous
#' d_mixed <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 100),
#'   value = c(rnorm(100, 2), rnorm(100, 1, 2), rnorm(100, 0))
#' )
#' ggplot(d_mixed, aes(x = group, y = value, fill = group)) +
#'   geom_bar_3d(bins = 20, width = c(.5, 1)) +
#'   coord_3d(scales = "fixed", ratio = c(1, 1, .1))
#'
#' # Use density instead of count for z
#' ggplot(d_mixed,
#'       aes(x = group, y = value, z = after_stat(density))) +
#'   geom_bar_3d(bins = 20, width = c(.5, 1)) +
#'   coord_3d()
#'
#' # Show empty bins with drop = FALSE
#' ggplot(d_cont, aes(x, y)) +
#'   geom_bar_3d(drop = FALSE) +
#'   coord_3d()
#'
#' @seealso [geom_col_3d()] for pre-computed heights, [coord_3d()] for 3D coordinate systems,
#'   [light()] for lighting specifications.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_bar_3d
#' @export
geom_bar_3d <- function(mapping = NULL, data = NULL,
                        stat = StatBar3D,
                        position = "identity",
                        ...,
                        bins = 10, binwidth = NULL, drop = TRUE,
                        width = 1.0, faces = "all",
                        light = NULL,
                        cull_backfaces = TRUE, sort_method = NULL,
                        scale_depth = TRUE, force_convex = FALSE,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = get_proto(stat, "stat"), geom = GeomPolygon3D,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, bins = bins, binwidth = binwidth, drop = drop,
                          width = width, faces = faces, light = light,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          ...)
      )
}


#' @rdname geom_bar_3d
#' @export
stat_bar_3d <- function(mapping = NULL, data = NULL,
                        geom = GeomPolygon3D,
                        position = "identity",
                        ...,
                        bins = 10, binwidth = NULL, drop = TRUE,
                        width = 1.0, faces = "all",
                        light = NULL,
                        cull_backfaces = TRUE, sort_method = NULL,
                        scale_depth = TRUE, force_convex = FALSE,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = StatBar3D, geom = get_proto(geom, "geom"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, bins = bins, binwidth = binwidth, drop = drop,
                          width = width, faces = faces, light = light,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          ...)
      )
}
