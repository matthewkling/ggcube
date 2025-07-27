StatFunction3D <- ggproto("StatFunction3D", Stat,
                          required_aes = character(0),
                          default_aes = aes(x = after_stat(x),
                                            y = after_stat(y),
                                            z = after_stat(z),
                                            fill = after_stat(z)),

                          setup_data = function(data, params) {
                                if (nrow(data) == 0) {
                                      # Create minimal empty data frame to trigger compute_panel
                                      data <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))
                                }
                                data
                          },

                          compute_panel = function(data, scales, fun = NULL, xlim = NULL, ylim = NULL,
                                                   n = 50, light = lighting(), na.rm = FALSE) {

                                # Validate function
                                if (is.null(fun)) {
                                      stop("stat_function_3d requires a fun parameter")
                                }

                                if (!is.function(fun)) {
                                      stop("`fun` must be a function")
                                }

                                # Get x and y ranges
                                if (is.null(xlim) || is.null(ylim)) {
                                      if (is.null(scales$x) || is.null(scales$x)) {
                                            stop("xlim and ylim must be specified when no alternative scale info is provided.\n",
                                                 "Try one of:\n",
                                                 "• stat_function_3d(..., xlim = c(min, max), ylim = c(min, max))\n",
                                                 "• ggplot() + stat_function_3d(...) + xlim(min, max) + ylim(min, max)\n",
                                                 "• ggplot(data_with_xy_columns, aes(x = x_column, y = y_column)) + stat_function_3d(...)")
                                      }
                                      xlim <- scales$x$dimension()
                                      ylim <- scales$y$dimension()
                                }

                                # Validate ranges
                                if (length(xlim) != 2 || !is.numeric(xlim) || xlim[1] >= xlim[2]) {
                                      stop("xlim must be a numeric vector of length 2 with xlim[1] < xlim[2]")
                                }

                                if (length(ylim) != 2 || !is.numeric(ylim) || ylim[1] >= ylim[2]) {
                                      stop("ylim must be a numeric vector of length 2 with ylim[1] < ylim[2]")
                                }

                                # Handle n parameter (grid resolution)
                                if (length(n) == 1) {
                                      nx <- ny <- n
                                } else if (length(n) == 2) {
                                      nx <- n[1]
                                      ny <- n[2]
                                } else {
                                      stop("n must be a single number or a vector of length 2")
                                }

                                if (!is.numeric(c(nx, ny)) || any(c(nx, ny) < 2)) {
                                      stop("Grid resolution (n) must be at least 2 in each dimension")
                                }

                                # Generate regular grid
                                x_seq <- seq(xlim[1], xlim[2], length.out = nx)
                                y_seq <- seq(ylim[1], ylim[2], length.out = ny)
                                grid_data <- expand.grid(x = x_seq, y = y_seq)

                                # Evaluate function at grid points
                                tryCatch({
                                      z_values <- fun(grid_data$x, grid_data$y)
                                }, error = function(e) {
                                      stop("Error evaluating function: ", e$message)
                                })

                                # Validate function output
                                if (length(z_values) != nrow(grid_data)) {
                                      stop("Function must return exactly one z value for each (x,y) pair")
                                }

                                if (!is.numeric(z_values)) {
                                      stop("Function must return numeric values")
                                }

                                grid_data$z <- z_values

                                # Remove missing values if requested
                                if (na.rm) {
                                      complete_cases <- complete.cases(grid_data)
                                      grid_data <- grid_data[complete_cases, ]

                                      # Check if we still have a complete grid after removing NAs
                                      remaining_x <- length(unique(grid_data$x))
                                      remaining_y <- length(unique(grid_data$y))

                                      if (remaining_x < 2 || remaining_y < 2) {
                                            stop("After removing missing values, insufficient grid points remain for surface creation")
                                      }

                                      if (nrow(grid_data) != remaining_x * remaining_y) {
                                            stop("After removing missing values, grid is no longer regular. Consider setting na.rm = FALSE or ensuring function returns finite values everywhere.")
                                      }
                                }

                                # Check we have enough data for surface creation
                                if (nrow(grid_data) < 4) {
                                      stop("Function evaluation resulted in insufficient data points for surface creation")
                                }

                                # Process surface using common pipeline
                                return(process_surface_grid(grid_data, light))
                          }
)

#' 3D function surface visualization
#'
#' Creates 3D surfaces by evaluating a function f(x,y) = z over a regular grid.
#' The function is evaluated at each grid point and the resulting surface is rendered
#' with proper 3D depth sorting and optional lighting effects.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. Since this stat
#'   generates its own data, typically only used for additional aesthetics like
#'   `fill` or `color` based on computed variables.
#' @param fun Function to evaluate. Must accept two arguments (vectors corresponding
#' to x and y axis values) and return a numeric vector of z values. Required parameter.
#' @param data The data to be displayed in this layer. Usually not needed since
#'   the stat generates its own data from the function.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomPolygon3D] for proper 3D depth sorting.
#' @param position Position adjustment, defaults to "identity". To collapse the result
#'   onto one 2D surface, use `position_on_face()`.
#' @param na.rm If `TRUE`, removes missing values from function evaluation results.
#'   If `FALSE`, missing values will cause an error. Default is `FALSE`.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param xlim,ylim Numeric vectors of length 2 giving the range for x and y values.
#'   If `NULL` (default), uses the scale ranges from the plot, which can be set via
#'   `xlim()` and `ylim()`, or trained by supplying data to the plot.
#' @param n Either a single integer specifying grid resolution in both dimensions,
#'   or a vector of length 2 specifying `c(nx, ny)` for different resolutions.
#'   Default is 50. Higher values create smoother surfaces but slower rendering.
#' @param light A lighting specification object created by [lighting()]
#' @param ... Other arguments passed on to [layer()].
#'
#' @section Aesthetics:
#' `stat_function_3d()` generates its own x, y, z coordinates, so typically no
#' positional aesthetics are needed in the mapping. However, you can use computed
#' variables with [after_stat()]:
#'
#' @section Computed variables:
#' - `x`, `y`, `z`: Grid coordinates and function values
#' - `light`: Computed lighting value (numeric for most methods, hex color for `normal_rgb`)
#' - `normal_x`, `normal_y`, `normal_z`: Surface normal components
#' - `slope`: Gradient magnitude from surface calculations
#' - `aspect`: Direction of steepest slope from surface calculations
#' - `dzdx`, `dzdy`: Partial derivatives from surface calculations
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic function surface
#' ggplot() +
#'   stat_function_3d(fun = function(x, y) x^2 + y^2,
#'                    xlim = c(-2, 2), ylim = c(-2, 2)) +
#'   coord_3d()
#'
#' # Wave function with lighting
#' wave_fun <- function(x, y) cos(x) + cos(y) + cos(x+y) + cos(sqrt(x^2 + y^2))
#' ggplot() +
#'   stat_function_3d(fun = wave_fun, fill = "steelblue",
#'                    xlim = c(-3*pi, 3*pi), ylim = c(-3*pi, 3*pi),
#'                    light = lighting(blend = "fill", blend_mode = "hsl")) +
#'   coord_3d(scales = "fixed") + theme_dark()
#'
#' # Higher resolution surface with color mapping
#' ggplot() +
#'   stat_function_3d(aes(fill = after_stat(light)),
#'                    fun = function(x, y) sin(x) * cos(y),
#'                    xlim = c(-pi, pi), ylim = c(-pi, pi),
#'                    n = 80) +
#'   scale_fill_viridis_c(option = "B") +
#'   coord_3d()
#'
#' # Complex mathematical surface
#' saddle <- function(x, y) x^2 - y^2
#' ggplot() +
#'   stat_function_3d(aes(fill = after_stat(x)),
#'                    fun = saddle,
#'                    xlim = c(-3, 3), ylim = c(-3, 3),
#'                    n = c(60, 40)) +  # Different resolution in x and y
#'   scale_fill_viridis_c() +
#'   coord_3d(scales = "fixed")
#'
#' # Function with lighting effects
#' gaussian <- function(a, b) exp(-(a^2 + b^2))
#' ggplot() +
#'   stat_function_3d(fun = gaussian, color = "white",
#'                    light = lighting(method = "direct",
#'                                   direction = c(1, 1, 0.25),
#'                                   blend = "both")) +
#'   scale_fill_viridis_c() + scale_color_viridis_c() +
#'   coord_3d(scales = "fixed", ratio = c(1, 2, 3), expand = FALSE) +
#'   xlim(-3, 3) + ylim(-2, 2) + theme_light()
#'
#' @seealso [stat_surface_3d()] for surfaces from existing grid data,
#'   [lighting()] for lighting specifications, [coord_3d()] for 3D coordinate systems.
#' @export
stat_function_3d <- function(mapping = NULL,
                             fun = NULL,
                             data = NULL,
                             geom = GeomPolygon3D,
                             position = "identity",
                             xlim = NULL,
                             ylim = NULL,
                             n = 50,
                             light = lighting(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             color = NULL,
                             colour = NULL,
                             ...) {

      # Ensure non-empty data like ggplot2::stat_function does
      data <- data %||% ensure_nonempty_data

      # Normalize to British spelling
      colour <- colour %||% color

      # Build params list, only including colour if not NULL
      params <- list(fun = fun, xlim = xlim, ylim = ylim, n = n,
                     light = light, na.rm = na.rm, ...)

      if (!is.null(colour)) {
            params$colour <- colour
      }

      layer(
            stat = StatFunction3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = params
      )
}

ensure_nonempty_data <- function(data) {
      if (ggplot2:::empty(data)) {
            ggplot2:::data_frame0(group = -1L, .size = 1)
      } else {
            data
      }
}
