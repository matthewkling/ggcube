# Stat that evaluates a function over a grid and outputs points with gradients

StatFunction3D <- ggproto("StatFunction3D", Stat,
                          required_aes = character(0),
                          default_aes = aes(x = after_stat(x),
                                            y = after_stat(y),
                                            z = after_stat(z),
                                            fill = after_stat(z)),

                          setup_data = function(data, params) {
                                if (nrow(data) == 0) {
                                      data <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))
                                }
                                data
                          },

                          compute_panel = function(data, scales, fun = NULL,
                                                   grid = "rectangle", n = 40, direction = "x", xlim = NULL, ylim = NULL, trim = TRUE,
                                                   cull_backfaces = FALSE, light = NULL, na.rm = FALSE) {

                                # Validate function
                                if (is.null(fun)) {
                                      stop("stat_function_3d requires a `fun` parameter")
                                }
                                if (!is.function(fun)) {
                                      stop("`fun` must be a function")
                                }

                                # Get x and y ranges
                                if (is.null(xlim) || is.null(ylim)) {
                                      if (is.null(scales$x) || is.null(scales$y)) {
                                            stop("xlim and ylim must be specified when no scale info is available.\n",
                                                 "Try: stat_function_3d(..., xlim = c(min, max), ylim = c(min, max))")
                                      }
                                      xlim <- xlim %||% scales$x$dimension()
                                      ylim <- ylim %||% scales$y$dimension()
                                }

                                # Validate ranges
                                if (length(xlim) != 2 || !is.numeric(xlim) || xlim[1] >= xlim[2]) {
                                      stop("xlim must be a numeric vector of length 2 with xlim[1] < xlim[2]")
                                }
                                if (length(ylim) != 2 || !is.numeric(ylim) || ylim[1] >= ylim[2]) {
                                      stop("ylim must be a numeric vector of length 2 with ylim[1] < ylim[2]")
                                }

                                # Generate point grid
                                grid_data <- make_point_grid(grid, n, direction, xlim, ylim, trim)

                                # Evaluate function
                                tryCatch({
                                      z_values <- fun(grid_data$x, grid_data$y)
                                }, error = function(e) {
                                      stop("Error evaluating function: ", e$message)
                                })

                                if (length(z_values) != nrow(grid_data)) {
                                      stop("Function must return one z value per (x,y) pair")
                                }
                                if (!is.numeric(z_values)) {
                                      stop("Function must return numeric values")
                                }

                                grid_data$z <- z_values

                                # Remove NAs if requested
                                if (na.rm) {
                                      grid_data <- grid_data[complete.cases(grid_data[c("x", "y", "z")]), ]
                                      if (nrow(grid_data) < 3) {
                                            stop("After removing NAs, insufficient points remain")
                                      }
                                }

                                # Compute point-level gradients
                                grid_data <- compute_point_gradients(grid_data)

                                # Add group if not present
                                if (!"group" %in% names(grid_data)) {
                                      grid_data$group <- 1
                                }

                                # Attach rendering parameters
                                grid_data$cull_backfaces <- cull_backfaces
                                grid_data <- attach_light(grid_data, light)

                                return(grid_data)
                          }
)


#' 3D surface from a function
#'
#' Evaluates a function f(x,y) = z over a regular grid and renders the result
#' as a 3D surface or ridgeline plot.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data Ignored; this stat generates its own data.
#' @param geom Geom to use for rendering. Defaults to [GeomSurface3D] for mesh
#'   surfaces. Use [GeomRidgeline3D] for ridgeline rendering.
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed to the layer.
#' @param fun Function to evaluate. Must accept (x, y) and return numeric z values.
#' @inheritParams grid_params
#' @inheritParams ridgeline_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#'
#' @section Computed variables:
#' \describe{
#'   \item{x, y, z}{Grid coordinates and function values}
#'   \item{dzdx, dzdy}{Partial derivatives at each point}
#'   \item{slope}{Gradient magnitude: sqrt(dzdx^2 + dzdy^2)}
#'   \item{aspect}{Direction of steepest slope: atan2(dzdy, dzdx)}
#' }
#'
#' @examples
#' # Basic function surface
#' ggplot() +
#'   geom_function_3d(fun = function(x, y) sin(x) * cos(y),
#'                    xlim = c(-pi, pi), ylim = c(-pi, pi)) +
#'   coord_3d()
#'
#' # Fill by slope
#' ggplot() +
#'   geom_function_3d(fun = function(x, y) x^2 + y^2,
#'                    xlim = c(-2, 2), ylim = c(-2, 2),
#'                    aes(fill = after_stat(slope)),
#'                    grid = "triangle") +
#'   scale_fill_viridis_c() +
#'   coord_3d()
#'
#' # As ridgelines
#' ggplot() +
#'   stat_function_3d(fun = function(x, y) dnorm(x) * dnorm(y) * 10,
#'                    xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), n = c(15, 30),
#'                    geom = "ridgeline_3d", base = 0, light = "none",
#'                    fill = "black", color = "white") +
#'   coord_3d()
#'
#' @seealso [geom_surface_3d()], [geom_ridgeline_3d()], [coord_3d()]
#' @export
stat_function_3d <- function(mapping = NULL, data = ensure_nonempty_data,
                             geom = "surface_3d", position = "identity",
                             ...,
                             fun = NULL,
                             xlim = NULL, ylim = NULL,
                             grid = "rectangle", n = 40, direction = "x", trim = TRUE,
                             cull_backfaces = FALSE,
                             light = NULL,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {

      layer(
            data = data,
            mapping = mapping,
            stat = StatFunction3D,
            geom = get_proto(geom, "geom"),
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  fun = fun,
                  xlim = xlim, ylim = ylim,
                  grid = grid, n = n, direction = direction, trim = trim,
                  cull_backfaces = cull_backfaces,
                  light = light,
                  na.rm = na.rm,
                  ...
            )
      )
}


#' @rdname stat_function_3d
#' @export
geom_function_3d <- function(mapping = NULL, data = ensure_nonempty_data,
                             stat = "function_3d", position = "identity",
                             ...,
                             fun = NULL,
                             xlim = NULL, ylim = NULL,
                             grid = "rectangle", n = 40, direction = "x", trim = TRUE,
                             cull_backfaces = FALSE,
                             light = NULL,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {

      layer(
            data = data,
            mapping = mapping,
            stat = get_proto(stat, "stat"),
            geom = GeomSurface3D,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  fun = fun,
                  xlim = xlim, ylim = ylim,
                  grid = grid, n = n, direction = direction, trim = trim,
                  cull_backfaces = cull_backfaces,
                  light = light,
                  na.rm = na.rm,
                  ...
            )
      )
}


#' Ensure non-empty data for stats that generate their own data
#' @keywords internal
ensure_nonempty_data <- function(data) {
      if (ggplot2:::empty(data)) {
            ggplot2:::data_frame0(group = -1L, .size = 1)
      } else {
            data
      }
}
