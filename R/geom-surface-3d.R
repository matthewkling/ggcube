StatSurface3D <- ggproto("StatSurface3D", Stat,
                         required_aes = c("x", "y", "z"),

                         compute_group = function(data, scales, na.rm = FALSE,
                                                  cull_backfaces = NULL, light = NULL) {

                               # Remove missing values if requested
                               if (na.rm) {
                                     data <- data[complete.cases(data[c("x", "y", "z")]), ]
                               }

                               # Check we have enough data
                               if (nrow(data) < 4) {
                                     stop("stat_surface_3d requires at least 4 points")
                               }

                               # Detect grid structure
                               grid_info <- detect_grid_structure(data)
                               if (!grid_info$is_regular || !grid_info$is_complete) {
                                     stop("Data must be on a regular, complete grid. Each x,y combination should appear exactly once.")
                               }

                               # Add grouping variable if not present (required for face processing)
                               if (!"group" %in% names(data)) {
                                     data$group <- 1
                               }

                               # Create quadrilateral faces from grid cells
                               data <- data %>%
                                     convert_to_quads() %>%
                                     compute_surface_vars() %>%
                                     average_aesthetics() %>%
                                     mutate(cull_backfaces = cull_backfaces) %>%
                                     attach_light(light)

                               return(data)
                         }
)

# Helper function to detect if data is on regular grid
detect_grid_structure <- function(data) {

      # Get unique values
      x_unique <- sort(unique(data$x))
      y_unique <- sort(unique(data$y))

      # Check for regular spacing
      x_regular <- TRUE
      y_regular <- TRUE

      if (length(x_unique) > 1) {
            x_diffs <- diff(x_unique)
            x_regular <- all(abs(x_diffs - x_diffs[1]) < 1e-10)
      }

      if (length(y_unique) > 1) {
            y_diffs <- diff(y_unique)
            y_regular <- all(abs(y_diffs - y_diffs[1]) < 1e-10)
      }

      # Check if we have complete grid (every x,y combination)
      expected_points <- length(x_unique) * length(y_unique)
      has_complete_grid <- nrow(data) == expected_points

      # Check for duplicates
      has_duplicates <- any(duplicated(data[c("x", "y")]))

      list(
            is_regular = x_regular && y_regular,
            is_complete = has_complete_grid && !has_duplicates,
            x_vals = x_unique,
            y_vals = y_unique,
            n_x = length(x_unique),
            n_y = length(y_unique)
      )
}

#' Create quadrilateral faces from grid data
#'
#' @param data Regular grid data frame
#' @return Data frame with quad faces and computed gradients
#' @keywords internal
convert_to_quads <- function(data) {
      data <- data %>%
            ungroup() %>%
            mutate(quad_id = 1:nrow(.))

      dy <- data %>%
            group_by(x) %>%
            mutate(y = lag(y),
                   z = lag(z)) %>%
            ungroup()

      dx <- data %>%
            group_by(y) %>%
            mutate(x = lag(x),
                   z = lag(z)) %>%
            ungroup()

      dxy <- data.frame(x = dx$x,
                        y = dy$y) %>%
            left_join(data, by = join_by(x, y)) %>%
            mutate(quad_id = dx$quad_id)

      d <- bind_rows(data, dx, dxy, dy) %>%
            na.omit() %>%
            group_by(quad_id) %>%
            filter(n() == 4) %>%
            arrange(x, y) %>%
            mutate(order = c(4, 3, 1, 2)) %>% # ccw order
            ungroup() %>%
            arrange(quad_id, order) %>%
            mutate(group = paste0("surface__quad", quad_id, "::", group)) %>%
            as.data.frame()

      return(d)
}


#' 3D surface from regular grid data
#'
#' Creates 3D surfaces from regularly gridded data (like elevation maps).
#' The data must be on a regular, complete grid where every combination
#' of x and y values appears exactly once.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. This stat
#'   requires the `x`, `y`, and `z` aesthetics.
#' @param data The data to be displayed in this layer. Must contain x, y, z columns
#'   representing coordinates on a regular grid.
#' @param stat The statistical transformation to use on the data. Defaults to `StatSurface3D`.
#' @param geom The geometric object used to display the data. Defaults to `GeomPolygon3D`.
#'
#' @inheritParams position_param
#' @inheritParams light_param
#' @inheritParams polygon_params
#'
#' @section Aesthetics:
#' Requires the following aesthetics:
#' - **x**: X coordinate
#' - **y**: Y coordinate
#' - **z**: Z coordinate (elevation/height)
#'
#' @inheritSection surface_computed_vars Computed variables
#'
#' @examples
#' # data and base plot for basic surface
#' d <- dplyr::mutate(tidyr::expand_grid(x = -20:20, y = -20:20),
#'       z = sqrt(x^2 + y^2) / 1.5,
#'       z = cos(z) - z)
#' p <- ggplot(d, aes(x, y, z)) + coord_3d()
#'
#' # surface with 3d lighting
#' p + geom_surface_3d(fill = "steelblue", color = "steelblue", linewidth = .2,
#'       light = light(mode = "hsl", direction = c(1, 0, 0)))
#'
#' # mesh wireframe, without fill, with aes line color
#' p + geom_surface_3d(aes(color = z), fill = NA)
#'
#' # use after_stat to access computed surface-orientation variables
#' p + geom_surface_3d(aes(fill = after_stat(aspect))) +
#'       scale_fill_gradientn(colors = rainbow(20))
#'
#' # use `group` to plot data for multiple surfaces
#' d <- expand.grid(x = -5:5, y = -5:5)
#' d$z <- d$x^2 - d$y^2
#' d$g <- "a"
#' d2 <- d
#' d2$z <- d$z + 15
#' d2$g <- "b"
#' ggplot(rbind(d, d2), aes(x, y, z, group = g, fill = g)) +
#'   coord_3d() +
#'   geom_surface_3d(color = "black", alpha = .5, light = NULL)
#'
#' # terrain surface with topographic hillshade and elevational fill
#' ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
#'   geom_surface_3d(light = light(direction = c(1, 0, .5),
#'                            mode = "hsv", contrast = 1.5),
#'                linewidth = .2) +
#'   coord_3d(ratio = c(1, 1.5, .75)) +
#'   theme_light() +
#'   scale_fill_gradientn(colors = c("darkgreen", "rosybrown4", "gray60")) +
#'   scale_color_gradientn(colors = c("darkgreen", "rosybrown4", "gray60")) +
#'   guides(fill = guide_colorbar_3d())
#'
#' @seealso [stat_function_3d()] for surfaces representing mathematical functions;
#'   [stat_smooth_3d()] for surfaces based on fitted statistical models;
#'   [stat_pillar_3d()] for terraced column-like surfaces;
#'   [geom_polygon_3d()] for the default geom associated with this layer.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_surface_3d
#' @export
geom_surface_3d <- function(mapping = NULL, data = NULL, stat = StatSurface3D,
                            position = "identity",
                            ...,
                            light = NULL,
                            cull_backfaces = FALSE, sort_method = NULL,
                            force_convex = TRUE, scale_depth = TRUE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(mapping = mapping, data = data, stat = stat, geom = GeomPolygon3D,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          light = light, ...)
      )
}

#' @rdname geom_surface_3d
#' @export
stat_surface_3d <- function(mapping = NULL, data = NULL, geom = GeomPolygon3D,
                            position = "identity",
                            ...,
                            light = NULL,
                            cull_backfaces = FALSE, sort_method = NULL,
                            force_convex = TRUE, scale_depth = TRUE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(mapping = mapping, data = data, stat = StatSurface3D, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          light = light, ...)
      )
}

