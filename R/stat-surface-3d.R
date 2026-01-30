# Stat that takes user-provided xyz data, validates it, and computes gradients

StatSurface3D <- ggproto("StatSurface3D", Stat,
                         required_aes = c("x", "y", "z"),
                         default_aes = aes(fill = after_stat(z)),

                         compute_group = function(data, scales, na.rm = FALSE,
                                                  cull_backfaces = FALSE, light = NULL) {

                               # Remove NAs if requested
                               if (na.rm) {
                                     data <- data[complete.cases(data[c("x", "y", "z")]), ]
                               }

                               if (nrow(data) < 3) {
                                     warning("stat_surface_3d requires at least 3 points; returning empty data")
                                     return(data.frame())
                               }

                               # Check if data forms a regular grid
                               x_vals <- sort(unique(data$x))
                               y_vals <- sort(unique(data$y))
                               is_regular_grid <- nrow(data) == length(x_vals) * length(y_vals)

                               if (is_regular_grid && length(x_vals) >= 2 && length(y_vals) >= 2) {
                                     # Add row/column indices for regular grid
                                     data <- data %>%
                                           mutate(column = match(x, !!x_vals),
                                                  row = match(y, !!y_vals))

                                     # Compute point-level gradients
                                     data <- compute_point_gradients(data)
                               } else {
                                     # Irregular data - gradients will be computed per-face after tessellation
                                     # Set gradient columns to NA for now
                                     data$dzdx <- NA_real_
                                     data$dzdy <- NA_real_
                                     data$slope <- NA_real_
                                     data$aspect <- NA_real_
                               }

                               # Attach rendering parameters
                               data$cull_backfaces <- cull_backfaces
                               data <- attach_light(data, light)

                               return(data)
                         }
)


#' 3D surface from point data
#'
#' Takes user-provided (x, y, z) point data and prepares it for surface
#' rendering. If data form a regular grid, can render either a GeomSurface3D
#' of rectangular or triangular tiles, or a GeomRidgeline3D of surface slices;
#' otherwise, renders triangular tiles via Delaunay trianuglation.
#'
#'
#'
#'  For regular grids, computes point-level gradients. Works with
#' both [geom_surface_3d()] for mesh rendering and [geom_ridgeline_3d()] for
#' ridgeline rendering.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. Must include
#'   `x`, `y`, and `z`.
#' @param data Data frame containing point coordinates.
#' @param geom Geom to use for rendering. Defaults to [GeomSurface3D].
#'   Use [GeomRidgeline3D] for ridgeline rendering.
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed to the layer.
#' @inheritParams ridgeline_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#'
#' @section Computed variables:
#' For regular grid data:
#' \describe{
#'   \item{dzdx, dzdy}{Partial derivatives at each point}
#'   \item{slope}{Gradient magnitude: sqrt(dzdx^2 + dzdy^2)}
#'   \item{aspect}{Direction of steepest slope: atan2(dzdy, dzdx)}
#' }
#'
#' For irregular data, gradient variables are NA (computed per-face by geom).
#'
#' @section Grid detection:
#' The stat automatically detects whether data forms a regular grid by checking
#' if `nrow(data) == length(unique(x)) * length(unique(y))`. Regular grids
#' get point-level gradient computation; irregular point clouds are passed
#' through for Delaunay tessellation by the geom.
#'
#' @examples

#' # Regular grid data ------------------------------------------
#'
#' # simulated data and base plot for basic surface
#' d <- dplyr::mutate(tidyr::expand_grid(x = -10:10, y = -10:10),
#'                    z = sqrt(x^2 + y^2) / 1.5,
#'                    z = cos(z) - z)
#' p <- ggplot(d, aes(x, y, z)) +
#'       coord_3d(light = light(mode = "hsl", direction = c(1, 0, 0)))
#'
#' # surface with 3d lighting
#' p + geom_surface_3d(fill = "steelblue", color = "steelblue", linewidth = .2)
#'
#' # mesh wireframe (`fill = NA`) with aes line color
#' p + geom_surface_3d(aes(color = z), fill = NA,
#'                     linewidth = .5, light = "none") +
#'       scale_color_gradientn(colors = c("black", "blue", "red"))
#'
#' # triangulated surface (can prevent lighting flaws)
#' p + geom_surface_3d(fill = "#9e2602", color = "black", grid = "tri2")
#'
#' # use after_stat to access computed surface-orientation variables
#' p + geom_surface_3d(aes(fill = after_stat(slope)), grid = "tri2") +
#'       scale_fill_viridis_c() +
#'       guides(fill = guide_colorbar_3d())
#'
#' # use `group` to plot data for multiple surfaces
#' d <- expand.grid(x = -5:5, y = -5:5)
#' d$z <- d$x^2 - d$y^2
#' d$g <- "a"
#' d2 <- d
#' d2$z <- d$z + 15
#' d2$g <- "b"
#' ggplot(rbind(d, d2), aes(x, y, z, group = g, fill = g)) +
#'       coord_3d(light = "none") +
#'       geom_surface_3d(color = "black", alpha = .5, light = NULL)
#'
#' # terrain surface with topographic hillshade and elevational fill
#' ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
#'       geom_surface_3d(light = light(direction = c(1, 0, .5),
#'                                     mode = "hsv", contrast = 1.5),
#'                       linewidth = .2) +
#'       coord_3d(ratio = c(1, 1.5, .75)) +
#'       theme_light() +
#'       scale_fill_gradientn(colors = c("darkgreen", "rosybrown4", "gray60")) +
#'       scale_color_gradientn(colors = c("darkgreen", "rosybrown4", "gray60")) +
#'       guides(fill = guide_colorbar_3d())
#'
#' # As ridgelines
#' ggplot(mountain, aes(x, y, z)) +
#'   stat_surface_3d(geom = "ridgeline_3d", sort_method = "pairwise",
#'                   fill = "black", color = "white", light = "none", linewidth = .1) +
#'       coord_3d(ratio = c(1, 1.5, .75), yaw = 45)
#'
#'
#' # Irregular point data ---------------------------------------
#'
#' set.seed(42)
#' pts <- data.frame(x = runif(200, -2, 2), y = runif(200, -2, 2))
#' pts$z <- with(pts, sin(x) * cos(y))
#'
#' ggplot(pts, aes(x, y, z = z, fill = z)) +
#'   stat_surface_3d(sort_method = "pairwise") +
#'   scale_fill_viridis_c() +
#'   coord_3d()
#'
#' @seealso [geom_surface_3d()], [geom_ridgeline_3d()], [stat_function_3d()],
#'   [coord_3d()]
#' @export
stat_surface_3d <- function(mapping = NULL, data = NULL,
                            geom = "surface_3d", position = "identity",
                            ...,
                            cull_backfaces = FALSE,
                            light = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

      layer(
            data = data,
            mapping = mapping,
            stat = StatSurface3D,
            geom = ggproto_lookup(geom, "geom"),
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  cull_backfaces = cull_backfaces,
                  light = light,
                  na.rm = na.rm,
                  ...
            )
      )
}


#' @rdname stat_surface_3d
#' @param method Tessellation method passed to [geom_surface_3d()]:
#'   "auto" (default), "grid", or "delaunay".
#' @param grid Tile geometry for regular grids: "rectangle" (default),
#'   "tri1", or "tri2".
#' @export
geom_surface_3d <- function(mapping = NULL, data = NULL,
                            stat = "surface_3d", position = "identity",
                            ...,
                            method = "auto",
                            grid = "rectangle",
                            cull_backfaces = FALSE,
                            sort_method = "auto",
                            scale_depth = TRUE,
                            force_convex = TRUE,
                            light = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

      layer(
            data = data,
            mapping = mapping,
            stat = ggproto_lookup(stat, "stat"),
            geom = GeomSurface3D,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  method = method,
                  grid = grid,
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
