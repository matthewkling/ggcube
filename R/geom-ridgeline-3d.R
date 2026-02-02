# Geom that converts point grid to ridgeline polygons for Joy Division style rendering

GeomRidgeline3D <- ggproto("GeomRidgeline3D", GeomPolygon3D,

                           required_aes = c("x", "y", "z"),

                           default_aes = aes(
                                 colour = "black",
                                 fill = "white",
                                 linewidth = 0.1,
                                 linetype = 1,
                                 alpha = NA
                           ),

                           extra_params = c("na.rm", "direction", "base", "cull_backfaces",
                                            "sort_method", "scale_depth", "force_convex"),

                           setup_params = function(data, params) {
                                 if(is.null(params$direction) && ".direction" %in% names(data)) params$direction <- data$.direction[1]
                                 params$direction <- params$direction %||% "x"
                                 params$direction <- match.arg(params$direction, c("x", "y"))

                                 # make pairwise the default, as painter is so often problematic for ridges
                                 if(is.null(params$sort_method)) params$sort_method <- "pairwise"
                                 params
                           },

                           setup_data = function(data, params) {
                                 # Validate grid structure
                                 n_unique_x <- length(unique(round(data$x, 10)))
                                 n_unique_y <- length(unique(round(data$y, 10)))

                                 if (params$direction == "x" && n_unique_y < 2) {
                                       stop("geom_ridgeline_3d() with direction='x' requires at least 2 unique y values")
                                 }
                                 if (params$direction == "y" && n_unique_x < 2) {
                                       stop("geom_ridgeline_3d() with direction='y' requires at least 2 unique x values")
                                 }

                                 # Warn if many slices
                                 slice_var <- if (params$direction == "x") "x" else "y"
                                 n_slices <- length(unique(round(data[[slice_var]], 10)))
                                 if (n_slices > 100) {
                                       message("Creating ", n_slices, " ridgeline polygons. ",
                                               "Consider reducing grid resolution.")
                                 }

                                 # Cleanup
                                 if (".direction" %in% names(data)) data <- select(data, -.direction)

                                 # Convert to ridgeline polygons
                                 data <- points_to_ridgelines(
                                       data,
                                       direction = params$direction,
                                       base = params$base,
                                       group_prefix = "ridgeline__"
                                 )

                                 return(data)
                           }

                           # draw_panel inherited from GeomPolygon3D
)


#' 3D ridgeline plot from point grid
#'
#' Renders a point grid as ridgeline polygons (Joy Division style). Each slice
#' along one axis becomes a filled polygon, creating a cross-sectional view
#' of the surface.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data Point grid data with x, y, z coordinates.
#' @param stat Statistical transformation. Defaults to "identity".
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed to the layer.
#' @inheritParams ridgeline_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#'
#' @section Aesthetics:
#' \describe{
#'   \item{x, y, z}{Point coordinates (required)}
#'   \item{fill}{Polygon fill color (default: "grey70")}
#'   \item{colour}{Polygon border color (default: "black")}
#'   \item{alpha}{Transparency}
#'   \item{linewidth}{Border width}
#'   \item{linetype}{Border type}
#' }
#'
#' @examples
#' # From point grid
#' grid_data <- expand.grid(x = seq(-3, 3, 0.5), y = seq(-3, 3, 0.1))
#' grid_data$z <- with(grid_data, dnorm(y) * dnorm(x) * 10)
#'
#' ggplot(grid_data, aes(x, y, z = z)) +
#'   geom_ridgeline_3d() +
#'   coord_3d()
#'
#' # With fill
#' ggplot(grid_data, aes(x, y, z = z, fill = x)) +
#'   geom_ridgeline_3d(colour = "white", linewidth = 0.3) +
#'   scale_fill_viridis_c() +
#'   coord_3d()
#'
#' # Ridges in y direction
#' ggplot(grid_data, aes(x, y, z = z)) +
#'   geom_ridgeline_3d(direction = "y", fill = "steelblue") +
#'   coord_3d()
#'
#' # With stat_function_3d
#' ggplot() +
#'   stat_function_3d(fun = function(x, y) sin(x) * cos(y),
#'                    xlim = c(-pi, pi), ylim = c(-pi, pi),
#'                    n = 30,
#'                    geom = "ridgeline_3d") +
#'   coord_3d()
#'
#' @seealso [geom_surface_3d()], [stat_function_3d()], [coord_3d()]
#' @export
geom_ridgeline_3d <- function(mapping = NULL, data = NULL,
                              stat = "identity_3d", position = "identity",
                              ...,
                              direction = "x",
                              base = NULL,
                              cull_backfaces = FALSE,
                              sort_method = "pairwise",
                              scale_depth = TRUE,
                              force_convex = FALSE,
                              light = NULL,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {

      layer(
            data = data,
            mapping = mapping,
            stat = get_proto(stat),
            geom = GeomRidgeline3D,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  direction = direction,
                  base = base,
                  cull_backfaces = cull_backfaces,
                  sort_method = sort_method,
                  scale_depth = scale_depth,
                  force_convex = force_convex,
                  light = light,
                  na.rm = na.rm,
                  ...
            )
      )
}
