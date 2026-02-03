
#' 3D paths connecting observations
#'
#' Connects observations in 3D space in the order they appear
#' in the data. It converts path data into individual segments for proper depth
#' sorting while maintaining the appearance of connected paths. Each path is
#' divided into segments that can be depth-sorted independently.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. Requires x, y, z
#'   coordinates. Grouping aesthetics determine separate paths.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to [StatPath3D].
#' @param geom The geometric object used to display the data. Defaults to [GeomSegment3D].
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
#' @section Computed variables for StatSegment3D:
#' - `x`, `y`, `z`: Start coordinates of each segment
#' - `xend`, `yend`, `zend`: End coordinates of each segment
#' - `group`: Hierarchical group identifier preserving original grouping
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
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_path_3d
#' @export
geom_path_3d <- function(mapping = NULL, data = NULL,
                         stat = StatPath3D, position = "identity",
                         ...,
                         scale_depth = TRUE, arrow = NULL, lineend = "butt",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = get_proto(stat, "stat"), geom = GeomSegment3D,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  scale_depth = scale_depth,
                  arrow = arrow,
                  lineend = lineend,
                  ...
            )
      )
}

#' @rdname geom_path_3d
#' @export
stat_path_3d <- function(mapping = NULL, data = NULL,
                         geom = GeomSegment3D, position = "identity",
                         ...,
                         scale_depth = TRUE, arrow = NULL, lineend = "butt",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = StatPath3D, geom = get_proto(geom, "geom"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  scale_depth = scale_depth,
                  arrow = arrow,
                  lineend = lineend,
                  ...
            )
      )
}

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

                            segments <- average_aesthetics(segments)

                            return(segments)
                      }
)

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
