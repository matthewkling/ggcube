StatPath3D <- ggproto("StatPath3D", Stat,
                      required_aes = c("x", "y", "z"),

                      compute_group = function(data, scales, na.rm = FALSE) {

                            # Remove missing values if requested
                            if (na.rm) {
                                  complete_cases <- complete.cases(data[c("x", "y", "z")])
                                  data <- data[complete_cases, ]
                            }

                            # Need at least 2 points to make a path
                            if (nrow(data) < 2) {
                                  return(data.frame())
                            }

                            # Handle discrete scale conversion (following StatPoint3D pattern)
                            if ("z" %in% names(data)) {
                                  data$z_raw <- data$z
                                  if (is.factor(data$z) || is.character(data$z)) {
                                        data$z <- as.numeric(as.factor(data$z))
                                  }
                            }

                            if ("x" %in% names(data)) {
                                  data$x_raw <- data$x
                                  if (is.factor(data$x) || is.character(data$x)) {
                                        data$x <- as.numeric(as.factor(data$x))
                                  }
                            }

                            if ("y" %in% names(data)) {
                                  data$y_raw <- data$y
                                  if (is.factor(data$y) || is.character(data$y)) {
                                        data$y <- as.numeric(as.factor(data$y))
                                  }
                            }

                            # Convert path to segments
                            n_points <- nrow(data)
                            n_segments <- n_points - 1

                            if (n_segments == 0) {
                                  return(data.frame())
                            }

                            # Create segments from consecutive points
                            segments <- data.frame(
                                  x = data$x[1:n_segments],
                                  y = data$y[1:n_segments],
                                  z = data$z[1:n_segments],
                                  xend = data$x[2:n_points],
                                  yend = data$y[2:n_points],
                                  zend = data$z[2:n_points]
                            )

                            # Create hierarchical group IDs
                            original_group <- data$group[1]
                            segments$group <- paste0(original_group, "__seg", 1:n_segments)

                            # Preserve other aesthetics from the first point of each segment
                            other_cols <- setdiff(names(data), c("x", "y", "z", "group"))
                            for (col_name in other_cols) {
                                  segments[[col_name]] <- data[[col_name]][1:n_segments]
                            }

                            return(segments)
                      }
)

StatSegment3D <- ggproto("StatSegment3D", Stat,
                         required_aes = c("x", "y", "z", "xend", "yend", "zend"),

                         compute_group = function(data, scales, na.rm = FALSE) {

                               # Remove missing values if requested
                               if (na.rm) {
                                     complete_cases <- complete.cases(data[c("x", "y", "z", "xend", "yend", "zend")])
                                     data <- data[complete_cases, ]
                               }

                               if (nrow(data) == 0) {
                                     return(data)
                               }

                               # Handle discrete scale conversion for start points
                               if ("z" %in% names(data)) {
                                     data$z_raw <- data$z
                                     if (is.factor(data$z) || is.character(data$z)) {
                                           data$z <- as.numeric(as.factor(data$z))
                                     }
                               }

                               if ("x" %in% names(data)) {
                                     data$x_raw <- data$x
                                     if (is.factor(data$x) || is.character(data$x)) {
                                           data$x <- as.numeric(as.factor(data$x))
                                     }
                               }

                               if ("y" %in% names(data)) {
                                     data$y_raw <- data$y
                                     if (is.factor(data$y) || is.character(data$y)) {
                                           data$y <- as.numeric(as.factor(data$y))
                                     }
                               }

                               # Handle discrete scale conversion for end points
                               if ("zend" %in% names(data)) {
                                     data$zend_raw <- data$zend
                                     if (is.factor(data$zend) || is.character(data$zend)) {
                                           data$zend <- as.numeric(as.factor(data$zend))
                                     }
                               }

                               if ("xend" %in% names(data)) {
                                     data$xend_raw <- data$xend
                                     if (is.factor(data$xend) || is.character(data$xend)) {
                                           data$xend <- as.numeric(as.factor(data$xend))
                                     }
                               }

                               if ("yend" %in% names(data)) {
                                     data$yend_raw <- data$yend
                                     if (is.factor(data$yend) || is.character(data$yend)) {
                                           data$yend <- as.numeric(as.factor(data$yend))
                                     }
                               }

                               # Create hierarchical group IDs if not already present
                               if (!"group" %in% names(data) || !any(grepl("__", data$group))) {
                                     original_group <- data$group[1] %||% "-1"
                                     data$group <- paste0(original_group, "__seg", 1:nrow(data))
                               }

                               return(data)
                         }
)

GeomSegment3D <- ggproto("GeomSegment3D", Geom,
                         required_aes = c("x", "y", "z", "xend", "yend", "zend"),
                         default_aes = aes(
                               colour = "black", linewidth = 0.5, linetype = 1, alpha = 1
                         ),

                         draw_panel = function(data, panel_params, coord, scale_depth = TRUE,
                                               arrow = NULL, lineend = "butt", na.rm = FALSE) {

                               validate_coord3d(coord)

                               if (nrow(data) == 0) {
                                     return(grid::nullGrob())
                               }

                               # Convert wide to long format
                               # Each segment becomes 2 rows with unique sub-group IDs
                               n_segments <- nrow(data)

                               # Create start points
                               start_data <- data.frame(
                                     x = data$x,
                                     y = data$y,
                                     z = data$z,
                                     group = paste0(data$group, "__start"),
                                     segment_id = 1:n_segments,
                                     point_type = "start"
                               )

                               # Create end points
                               end_data <- data.frame(
                                     x = data$xend,
                                     y = data$yend,
                                     z = data$zend,
                                     group = paste0(data$group, "__end"),
                                     segment_id = 1:n_segments,
                                     point_type = "end"
                               )

                               # Preserve other aesthetics for both points
                               other_cols <- setdiff(names(data), c("x", "y", "z", "xend", "yend", "zend", "group"))
                               for (col_name in other_cols) {
                                     start_data[[col_name]] <- data[[col_name]]
                                     end_data[[col_name]] <- data[[col_name]]
                               }

                               # Combine into long format
                               long_data <- rbind(start_data, end_data)

                               # Transform all points together (handles depth sorting)
                               coords <- coord$transform(long_data, panel_params)

                               # Apply depth scaling to linewidth
                               coords <- scale_depth(coords, scale_depth)

                               # Reconstruct segments from transformed data (vectorized)
                               start_coords <- coords[coords$point_type == "start", ]
                               end_coords <- coords[coords$point_type == "end", ]

                               # Order by segment_id to ensure proper pairing
                               start_coords <- start_coords[order(start_coords$segment_id), ]
                               end_coords <- end_coords[order(end_coords$segment_id), ]

                               # Vectorized segment creation
                               segments <- data.frame(
                                     x0 = start_coords$x,
                                     y0 = start_coords$y,
                                     x1 = end_coords$x,
                                     y1 = end_coords$y,
                                     depth = (start_coords$depth + end_coords$depth) / 2,
                                     colour = start_coords$colour,
                                     linewidth = start_coords$linewidth,
                                     linetype = start_coords$linetype,
                                     alpha = start_coords$alpha,
                                     segment_id = start_coords$segment_id
                               )

                               if (nrow(segments) == 0) {
                                     return(grid::nullGrob())
                               }

                               # Sort segments by depth (back to front)
                               segments <- segments[order(-segments$depth), ]

                               # Create segments grob
                               grid::segmentsGrob(
                                     x0 = segments$x0, y0 = segments$y0,
                                     x1 = segments$x1, y1 = segments$y1,
                                     default.units = "npc",
                                     arrow = arrow,
                                     gp = grid::gpar(
                                           col = segments$colour,
                                           lwd = segments$linewidth * .pt,
                                           lty = segments$linetype,
                                           lineend = lineend,
                                           alpha = segments$alpha
                                     )
                               )
                         },

                         draw_key = draw_key_path
)

#' 3D line segments with depth-based scaling and proper depth sorting
#'
#' `geom_segment_3d()` draws line segments in 3D space with automatic depth-based
#' linewidth scaling and proper depth sorting. Each segment is defined by start
#' coordinates (x, y, z) and end coordinates (xend, yend, zend).
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. Requires x, y, z
#'   for start coordinates and xend, yend, zend for end coordinates.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to
#'   [StatSegment3D] for proper discrete scale handling.
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed on to [layer()].
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param scale_depth Logical indicating whether to apply depth-based scaling
#'   to linewidth. When `TRUE` (default), segments closer to the viewer appear
#'   thicker, and segments farther away appear thinner.
#' @param arrow Specification for arrow heads, created by [arrow()].
#' @param lineend Line end style, one of "round", "butt", "square".
#'
#' @section Aesthetics:
#' `geom_segment_3d()` understands the following aesthetics:
#' - **x, y, z**: Start coordinates (required)
#' - **xend, yend, zend**: End coordinates (required)
#' - `colour`: Line color
#' - `linewidth`: Line width (gets depth-scaled when `scale_depth = TRUE`)
#' - `linetype`: Line type
#' - `alpha`: Transparency
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic 3D segments
#' segments_data <- data.frame(
#'   x = c(0, 1, 2), y = c(0, 1, 2), z = c(0, 1, 2),
#'   xend = c(1, 2, 3), yend = c(1, 2, 3), zend = c(1, 2, 3)
#' )
#'
#' ggplot(segments_data, aes(x, y, z, xend = xend, yend = yend, zend = zend)) +
#'   geom_segment_3d() +
#'   coord_3d()
#'
#' # With different colors and arrow heads
#' ggplot(segments_data, aes(x, y, z, xend = xend, yend = yend, zend = zend,
#'                          color = factor(1:3))) +
#'   geom_segment_3d(arrow = arrow(length = unit(0.2, "inches")),
#'                   linewidth = 2) +
#'   coord_3d()
#'
#' @seealso [geom_path_3d()] for connected paths, [geom_segment()] for 2D segments,
#'   [coord_3d()] for 3D coordinate systems.
#' @export
geom_segment_3d <- function(mapping = NULL, data = NULL,
                            stat = StatSegment3D, position = "identity",
                            ...,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                            scale_depth = TRUE, arrow = NULL, lineend = "butt") {

      layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomSegment3D,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  scale_depth = scale_depth,
                  arrow = arrow,
                  lineend = lineend,
                  ...
            )
      )
}

#' 3D connected paths with depth-based scaling and proper depth sorting
#'
#' `geom_path_3d()` connects observations in 3D space in the order they appear
#' in the data. It converts path data into individual segments for proper depth
#' sorting while maintaining the appearance of connected paths. Each path is
#' divided into segments that can be depth-sorted independently.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. Requires x, y, z
#'   coordinates. Grouping aesthetics determine separate paths.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to
#'   [StatPath3D] which converts paths to segments.
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed on to [layer()].
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param scale_depth Logical indicating whether to apply depth-based scaling
#'   to linewidth. When `TRUE` (default), path segments closer to the viewer
#'   appear thicker, and segments farther away appear thinner.
#' @param arrow Specification for arrow heads, created by [arrow()].
#' @param lineend Line end style, one of "round", "butt", "square".
#'
#' @section Aesthetics:
#' `geom_path_3d()` understands the following aesthetics:
#' - **x, y, z**: Coordinates (required)
#' - `group`: Grouping variable to create separate paths
#' - `colour`: Line color
#' - `linewidth`: Line width (gets depth-scaled when `scale_depth = TRUE`)
#' - `linetype`: Line type
#' - `alpha`: Transparency
#'
#' @section Grouping:
#' Multiple paths are created based on grouping aesthetics (group, colour, etc.).
#' Each group forms a separate path, and segments from different paths can be
#' interleaved during depth sorting for proper 3D rendering.
#'
#' @examples
#' library(ggplot2)
#'
#' x <- seq(0, 20*pi, pi/16)
#' spiral <- data.frame(
#'   x = x,
#'   y = sin(x),
#'   z = cos(x))
#'
#' # Basic path
#' ggplot(spiral, aes(x, y, z)) +
#'   geom_path_3d() +
#'   coord_3d()
#'
#' # With aesthetic coloring
#' ggplot(spiral, aes(x, y, z, color = y)) +
#'   geom_path_3d(linewidth = 1, lineend = "round") +
#'   coord_3d() +
#'   scale_color_gradientn(colors = c("red", "purple", "blue"))
#'
#' # With grouping
#' ggplot(spiral, aes(x, y, z, color = x > 30)) +
#'   geom_path_3d(linewidth = 1, lineend = "round") +
#'   coord_3d()
#'
#' @seealso [geom_segment_3d()] for individual segments, [geom_path()] for 2D paths,
#'   [coord_3d()] for 3D coordinate systems.
#' @export
geom_path_3d <- function(mapping = NULL, data = NULL,
                         stat = StatPath3D, position = "identity",
                         ...,
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         scale_depth = TRUE, arrow = NULL, lineend = "butt") {

      layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomSegment3D,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  scale_depth = scale_depth,
                  arrow = arrow,
                  lineend = lineend,
                  ...
            )
      )
}

#' 3D path statistical transformation
#'
#' `stat_path_3d()` converts path data (connected points) into segment data
#' for proper 3D depth sorting. Each consecutive pair of points in a path
#' becomes a separate segment that can be depth-sorted independently.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomSegment3D].
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed on to [layer()].
#'
#' @section Computed variables:
#' - `x`, `y`, `z`: Start coordinates of each segment
#' - `xend`, `yend`, `zend`: End coordinates of each segment
#' - `group`: Hierarchical group identifier preserving original grouping
#'
#' @export
stat_path_3d <- function(mapping = NULL, data = NULL,
                         geom = GeomSegment3D, position = "identity",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {

      layer(
            stat = StatPath3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
      )
}

#' 3D segment statistical transformation
#'
#' `stat_segment_3d()` provides 3D-aware processing for segment data, primarily
#' handling discrete scale conversion while preserving segment structure.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomSegment3D].
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed on to [layer()].
#'
#' @export
stat_segment_3d <- function(mapping = NULL, data = NULL,
                            geom = GeomSegment3D, position = "identity",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                            ...) {

      layer(
            stat = StatSegment3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
      )
}


#' Lorenz butterfly attractor generator
#'
#' @export
lorenz_attractor <- function(n_points = 5000, dt = 0.01,
                                      sigma = 10, rho = 28, beta = 8/3) {

      # Initialize vectors
      x <- numeric(n_points)
      y <- numeric(n_points)
      z <- numeric(n_points)

      # Initial conditions (slightly off the unstable equilibrium)
      x[1] <- 1
      y[1] <- 1
      z[1] <- 1

      # Integrate the Lorenz equations using Euler method
      for (i in 2:n_points) {
            # Lorenz equations:
            # dx/dt = σ(y - x)
            # dy/dt = x(ρ - z) - y
            # dz/dt = xy - βz

            dx_dt <- sigma * (y[i-1] - x[i-1])
            dy_dt <- x[i-1] * (rho - z[i-1]) - y[i-1]
            dz_dt <- x[i-1] * y[i-1] - beta * z[i-1]

            x[i] <- x[i-1] + dx_dt * dt
            y[i] <- y[i-1] + dy_dt * dt
            z[i] <- z[i-1] + dz_dt * dt
      }

      data.frame(x = x, y = y, z = z, time = (1:n_points) * dt)
}
