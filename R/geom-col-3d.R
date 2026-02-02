StatCol3D <- ggproto("StatCol3D", Stat,
                     required_aes = c("x", "y", "z"),
                     optional_aes = c("zmin"),
                     default_aes = aes(group = after_stat(group)),

                     compute_panel = function(data, scales, na.rm = FALSE,
                                              width = 1.0, faces = "all",
                                              light = NULL, zmin = NULL,
                                              cull_backfaces = NULL) {

                           # Remove missing values if requested
                           if (na.rm) {
                                 data <- data[complete.cases(data[c("x", "y", "z")]), ]
                           }

                           # Check we have enough data
                           if (nrow(data) < 1) {
                                 stop("stat_col_3d requires at least 1 point")
                           }

                           # Handle zmin with proper precedence:
                           # 1. Parameter (if provided and not NULL) - overrides aesthetic
                           # 2. Aesthetic (if mapped) - used if parameter is NULL
                           # 3. Default (0) - if neither provided

                           if (!is.null(zmin)) {
                                 # Explicit parameter provided - use for all columns (overrides aesthetic)
                                 data$zmin <- zmin
                           } else if (!"zmin" %in% names(data)) {
                                 # No aesthetic mapped and no parameter - use default of 0
                                 data$zmin <- 0
                           }
                           # If aesthetic exists and parameter is NULL, keep the aesthetic values

                           # Ensure z is never less than zmin (handle negative bars correctly)
                           zmin_vals <- pmin(data$zmin, data$z)
                           zmax_vals <- pmax(data$zmin, data$z)
                           data$zmin <- zmin_vals
                           data$z <- zmax_vals

                           # Generate numeric positions before calculating spacing
                           data <- convert_to_numeric(data)

                           # Calculate grid spacing using resolution (works for both regular and sparse grids)
                           x_spacing <- resolution(data$x, zero = FALSE)
                           y_spacing <- resolution(data$y, zero = FALSE)

                           # Validate and process faces parameter
                           selected_faces <- select_faces(faces)

                           if (length(selected_faces) == 0) {
                                 # Return empty data frame with required columns for consistency
                                 return(data.frame(
                                       x = numeric(0), y = numeric(0), z = numeric(0),
                                       group = character(0), col_id = integer(0), face_type = character(0),
                                       light = numeric(0),
                                       normal_x = numeric(0), normal_y = numeric(0), normal_z = numeric(0)
                                 ))
                           }

                           # Create columns
                           col_faces <- create_cols(data, x_spacing, y_spacing, width, selected_faces)

                           if (nrow(col_faces) == 0) {
                                 # Return empty data frame with required columns for consistency
                                 return(data.frame(
                                       x = numeric(0), y = numeric(0), z = numeric(0),
                                       group = character(0), col_id = integer(0), face_type = character(0),
                                       light = numeric(0),
                                       normal_x = numeric(0), normal_y = numeric(0), normal_z = numeric(0)
                                 ))
                           }

                           col_faces %>%
                                 average_aesthetics() %>%
                                 mutate(cull_backfaces = cull_backfaces) %>%
                                 attach_light(light) %>%
                                 return()
                     }
)

#' Create column faces from grid data
#'
#' @param data Data frame with x, y, z, zmin columns
#' @param x_spacing Grid spacing in x direction
#' @param y_spacing Grid spacing in y direction
#' @param width Width factor (1.0 = full grid spacing)
#' @param selected_faces Character vector of face names to render
#' @return Data frame with column face vertices
#' @keywords internal
create_cols <- function(data, x_spacing, y_spacing, width, selected_faces) {

      # Calculate actual column dimensions
      col_width_x <- x_spacing * width
      col_width_y <- y_spacing * width
      half_x <- col_width_x / 2
      half_y <- col_width_y / 2

      all_faces <- list()

      # Debug: check inputs
      if (nrow(data) == 0) {
            warning("create_cols: No data points provided")
            return(data.frame())
      }

      if (length(selected_faces) == 0) {
            warning("create_cols: No faces selected")
            return(data.frame())
      }

      for (i in 1:nrow(data)) {
            point <- data[i, ]
            cx <- point$x  # Center x
            cy <- point$y  # Center y

            # Define bounds
            # xmin_bound = left, xmax_bound = right
            # ymin_bound = front (smaller y), ymax_bound = back (larger y)
            # zmin_bound = bottom, zmax_bound = top
            xmin_bound <- cx - half_x
            xmax_bound <- cx + half_x
            ymin_bound <- cy - half_y
            ymax_bound <- cy + half_y
            zmin_bound <- point$zmin
            zmax_bound <- point$z

            # Define the 8 corners of the column
            # Using consistent naming: corners indexed by (x, y, z) position
            corners <- list(
                  c(xmin_bound, ymin_bound, zmin_bound),  # 1: left-front-bottom
                  c(xmax_bound, ymin_bound, zmin_bound),  # 2: right-front-bottom
                  c(xmax_bound, ymax_bound, zmin_bound),  # 3: right-back-bottom
                  c(xmin_bound, ymax_bound, zmin_bound),  # 4: left-back-bottom
                  c(xmin_bound, ymin_bound, zmax_bound),  # 5: left-front-top
                  c(xmax_bound, ymin_bound, zmax_bound),  # 6: right-front-top
                  c(xmax_bound, ymax_bound, zmax_bound),  # 7: right-back-top
                  c(xmin_bound, ymax_bound, zmax_bound)   # 8: left-back-top
            )

            # Define faces using corner indices (order: ccw when viewed from outside)
            # Each face uses corners that share the same value on that axis
            face_definitions <- list(
                  zmin = c(1, 4, 3, 2),  # Bottom face (all corners with z = zmin_bound), ccw from below
                  zmax = c(5, 6, 7, 8),  # Top face (all corners with z = zmax_bound), ccw from above
                  xmin = c(1, 5, 8, 4),  # Left face (all corners with x = xmin_bound), ccw from left
                  xmax = c(2, 3, 7, 6),  # Right face (all corners with x = xmax_bound), ccw from right
                  ymin = c(1, 2, 6, 5),  # Front face (all corners with y = ymin_bound), ccw from front
                  ymax = c(4, 8, 7, 3)   # Back face (all corners with y = ymax_bound), ccw from back
            )

            # Create requested faces
            for (face_name in selected_faces) {
                  if (face_name %in% names(face_definitions)) {
                        corner_indices <- face_definitions[[face_name]]

                        # Create 2-level hierarchical group ID: col_id__face_type
                        hierarchical_group <- paste0("col", i, "__", face_name)

                        # Create face vertices with hierarchical grouping
                        face_vertices <- data.frame(
                              x = sapply(corner_indices, function(idx) corners[[idx]][1]),
                              y = sapply(corner_indices, function(idx) corners[[idx]][2]),
                              z = sapply(corner_indices, function(idx) corners[[idx]][3]),
                              z_raw = point$z_raw,
                              group = hierarchical_group,
                              col_id = i,
                              face_type = face_name
                        )

                        # Preserve all non-coordinate columns
                        non_coord_cols <- setdiff(names(point), names(face_vertices))
                        for (col_name in non_coord_cols) {
                              face_vertices[[col_name]] <- rep(point[[col_name]], 4)
                        }

                        all_faces[[length(all_faces) + 1]] <- face_vertices
                  }
            }
      }

      if (length(all_faces) == 0) {
            return(data.frame())
      }

      # Combine all faces
      result <- do.call(rbind, all_faces)
      rownames(result) <- NULL

      return(result)
}


#' 3D columns from grid data
#'
#' Creates 3D columns (rectangular prisms) from grid data in which x and y fall on a
#' regular grid. Works with both complete and sparse grid data. Each data point becomes
#' a rectangular 3D column extending from a base level to the data value.
#'
#' This is analogous to [ggplot2::geom_col()] for 3D plots. For automatic counting or
#' binning, see [geom_bar_3d()].
#'
#' Note that column geometries often require pairwise depth sorting for correct rendering.
#' This is the default for smaller data sets, but not for larger data sets due to compute speed;
#' in those cases you may wish to manually specify `sort_method = "pairwise"` for good results.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to `StatCol3D`.
#' @param geom The geometric object used to display the data. Defaults to `GeomPolygon3D`.
#'
#' @param zmin Base level for all columns. When provided as a parameter, overrides any
#'   \code{zmin} aesthetic mapping. If \code{NULL} (the default), uses the \code{zmin}
#'   aesthetic if mapped, otherwise defaults to 0.
#'
#' @inheritParams col_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#' @inheritParams position_param
#'
#' @section Aesthetics:
#' `stat_col_3d()` requires the following aesthetics:
#' - **x**: X coordinate (grid position)
#' - **y**: Y coordinate (grid position)
#' - **z**: Z coordinate (column top height)
#'
#' And optionally understands:
#' - **zmin**: Base level for each column (can be overridden by the \code{zmin} parameter)
#'
#' @section Computed variables:
#' - `normal_x`, `normal_y`, `normal_z`: Face normal components
#' - `col_id`: Sequential column number
#' - `face_type`: Face name ("zmax", "xmin", etc.)
#'
#' @examples
#' # Basic 3D bar chart from regular grid
#' # (columns extend from z=0 by default)
#' d <- expand.grid(x = 1:5, y = 1:5)
#' d$z <- d$x + d$y + rnorm(25, 0, 0.5)
#' ggplot(d, aes(x, y, z)) +
#'   geom_col_3d() +
#'   coord_3d()
#'
#' # Set uniform base level using `zmin` parameter
#' ggplot(d, aes(x, y, z)) +
#'   geom_col_3d(aes(fill = z), color = "white",
#'               zmin = 5) +
#'   coord_3d()
#'
#' # Set variable base levels using `zmin` aesthetic
#' d$base_level <- runif(nrow(d), -5, 1)
#' ggplot(d, aes(x, y, z = z, zmin = base_level)) +
#'   geom_col_3d(color = "black") +
#'   coord_3d()
#'
#' # Show only a subset of column faces
#' ggplot(d, aes(x, y, z)) +
#'   geom_col_3d(faces = c("zmax", "ymin"),
#'     cull_backfaces = FALSE,
#'     fill = "steelblue", color = "black") +
#'   coord_3d()
#'
#' # With gaps between columns
#' ggplot(d, aes(x, y, z)) +
#'   geom_col_3d(color = "black", width = 0.6) +
#'   coord_3d()
#'
#' @seealso [geom_bar_3d()] for automatic counting/binning, [stat_surface_3d()] for smooth
#'   surface rendering, [coord_3d()] for 3D coordinate systems, [light()] for lighting
#'   specifications, [GeomPolygon3D] for the default geometry.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_col_3d
#' @export
geom_col_3d <- function(mapping = NULL, data = NULL,
                        stat = StatCol3D,
                        position = "identity",
                        ...,
                        width = 1.0, faces = "all", zmin = NULL,
                        light = NULL,
                        cull_backfaces = TRUE, sort_method = NULL,
                        scale_depth = TRUE, force_convex = FALSE,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = get_proto(stat), geom = GeomPolygon3D,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, width = width, faces = faces, light = light, zmin = zmin,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          ...)
      )
}


#' @rdname geom_col_3d
#' @export
stat_col_3d <- function(mapping = NULL, data = NULL,
                        geom = GeomPolygon3D,
                        position = "identity",
                        ...,
                        width = 1.0, faces = "all", zmin = NULL,
                        light = NULL,
                        cull_backfaces = TRUE, sort_method = NULL,
                        scale_depth = TRUE, force_convex = FALSE,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = StatCol3D, geom = get_proto(geom),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, width = width, faces = faces, light = light, zmin = zmin,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          ...)
      )
}
