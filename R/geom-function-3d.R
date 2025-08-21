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
                                                   n = NULL, grid = NULL, direction = NULL,
                                                   cull_backfaces = FALSE,
                                                   light = NULL, na.rm = FALSE) {

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

                                # Generate regular grid
                                grid_data <- make_tile_grid(grid, n, direction, xlim, ylim)
                                grid_data$group <- paste0("surface__tile", grid_data$group)

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

                                # Add computed variables and light info
                                grid_data <- grid_data %>%
                                      compute_surface_vars() %>%
                                      average_aesthetics() %>%
                                      mutate(cull_backfaces = cull_backfaces) %>%
                                      attach_light(light)

                                return(grid_data)
                          }
)

ensure_nonempty_data <- function(data) {
      if (ggplot2:::empty(data)) {
            ggplot2:::data_frame0(group = -1L, .size = 1)
      } else {
            data
      }
}

#' 3D function as a continuous surface
#'
#' A 3D version of `ggplot2::stat_function()`.
#' Creates surfaces by evaluating a function f(x,y) = z over a regular grid.
#' The function is evaluated at each grid point and the resulting surface is rendered
#' as a 3D surface.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. Since this stat
#'   generates its own data, typically only used for additional aesthetics like
#'   `fill` or `color` based on computed variables. Fill is mapped to `after_stat(z)` by default.
#' @param data The data to be displayed in this layer. Usually not needed since
#'   the stat generates its own data from the function.
#' @param stat The statistical transformation to use on the data. Defaults to `StatFunction3D`.
#' @param geom The geometric object used to display the data. Defaults to `GeomPolygon3D`.
#'
#' @param fun Function to evaluate. Must accept two arguments (vectors corresponding
#' to x and y axis values) and return a numeric vector of z values. Required parameter.
#' @param xlim,ylim Numeric vectors of length 2 giving the range for x and y values.
#'   If `NULL` (default), uses the scale ranges from the plot, which can be set via
#'   `xlim()` and `ylim()`, or trained by supplying data to the plot.
#'
#' @inheritParams grid_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#' @inheritParams position_param
#'
#'
#' @section Aesthetics:
#' `stat_function_3d()` generates its own x, y, z coordinates, so typically no
#' positional aesthetics are needed in the mapping. However, you can use computed
#' variables with [after_stat()].
#'
#' @inheritSection surface_computed_vars Computed variables
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic function surface
#' ggplot() +
#'   geom_function_3d(fun = function(a, b) exp(-(a^2 + b^2)),
#'                    xlim = c(-2, 2), ylim = c(-2, 2),
#'                    light = NULL, color = "white") +
#'   coord_3d() +
#'   scale_fill_viridis_c()
#'
#' # Wave function with lighting
#' wave_fun <- function(x, y) cos(x) + cos(y) + cos(x+y) + cos(sqrt(x^2 + y^2))
#' ggplot() +
#'   geom_function_3d(fun = wave_fun, fill = "steelblue",
#'                    xlim = c(-3*pi, 3*pi), ylim = c(-3*pi, 3*pi),
#'                    light = light(method = "direct", mode = "hsl",
#'                           contrast = .8, direction = c(1, -1, 1))) +
#'   coord_3d(scales = "fixed") + theme_dark()
#'
#' # Use after_stat to access computed surface-orientation variables
#' ggplot() +
#'   geom_function_3d(aes(fill = after_stat(dzdx),
#'                        color = after_stat(dzdx)),
#'                    fun = function(x, y) sin(x) * cos(y),
#'                    xlim = c(-pi, pi), ylim = c(-pi, pi),
#'                    light = NULL) +
#'   scale_fill_viridis_c(option = "B") +
#'   scale_color_viridis_c(option = "B") +
#'   coord_3d(scales = "fixed")
#'
#' # Use "filtering" functions to constrain output domain
#' ggplot() +
#'   geom_function_3d(fun = function(a, b) {
#'     ifelse(sqrt(a^2 + b^2) < 2, exp(-(a^2 + b^2)), NA)},
#'     xlim = c(-2, 2), ylim = c(-2, 2), color = "white") +
#'   coord_3d() +
#'   scale_fill_viridis_c() +
#'   guides(fill = guide_colorbar_3d())
#'
#' # Specify alternative grid geometry
#' ggplot() +
#'   geom_function_3d(fun = function(a, b) exp(-(a^2 + b^2)),
#'     xlim = c(-2, 2), ylim = c(-2, 2), color = "white",
#'     grid = "tri", n = 30, direction = "y") +
#'   coord_3d() +
#'   scale_fill_viridis_c() +
#'   guides(fill = guide_colorbar_3d())
#'
#' @seealso [stat_surface_3d()] for surfaces from existing grid data,
#'   [make_tile_grid()] for details about grid geometry options,
#'   [light()] for lighting specifications,
#'   [coord_3d()] for 3D coordinate systems.
#' @rdname geom_function_3d
#' @return A `Layer` object that can be added to a ggplot.
#' @export
geom_function_3d <- function(mapping = NULL,
                             data = NULL,
                             stat = StatFunction3D,
                             position = "identity",
                             ...,
                             fun = NULL,
                             xlim = NULL, ylim = NULL,
                             n = NULL, grid = NULL, direction = NULL,
                             light = NULL,
                             cull_backfaces = FALSE, sort_method = NULL,
                             force_convex = TRUE, scale_depth = TRUE,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {

      # Ensure non-empty data like ggplot2::stat_function does
      data <- data %||% ensure_nonempty_data

      layer(data = data, mapping = mapping, stat = stat, geom = GeomPolygon3D,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(fun = fun, xlim = xlim, ylim = ylim,
                          n = n, grid = grid, direction = direction,
                          light = light,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          na.rm = na.rm, ...)
      )
}


#' @rdname geom_function_3d
#' @export
stat_function_3d <- function(mapping = NULL,
                             data = NULL,
                             geom = GeomPolygon3D,
                             position = "identity",
                             ...,
                             fun = NULL,
                             xlim = NULL, ylim = NULL,
                             n = NULL, grid = NULL, direction = NULL,
                             cull_backfaces = FALSE, sort_method = NULL,
                             force_convex = TRUE, scale_depth = TRUE,
                             light = NULL,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {

      # Ensure non-empty data like ggplot2::stat_function does
      data <- data %||% ensure_nonempty_data

      layer(data = data, mapping = mapping, stat = StatFunction3D, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(fun = fun, xlim = xlim, ylim = ylim,
                          n = n, grid = grid, direction = direction,
                          light = light,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          na.rm = na.rm, ...)
      )
}
