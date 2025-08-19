

GeomPolygon3D <- ggproto("GeomPolygon3D", Geom,
                         required_aes = c("x", "y", "z", "group"),
                         default_aes = aes(
                               fill = "grey80", colour = NA, linewidth = 0.1, linetype = 1, alpha = 1
                         ),

                         draw_panel = function(data, panel_params, coord,
                                               sort_method = "auto",
                                               scale_depth = TRUE,
                                               force_convex = FALSE) {

                               # Parameter validation
                               validate_coord3d(coord)
                               sort_method <- match.arg(sort_method, c("auto", "pairwise", "painter"))

                               # Transform data
                               data$.sort_method <- sort_method
                               coords <- coord$transform(data, panel_params)

                               # Enforce convexity if requested
                               coords <- drop_nonconvex_vertices(coords, force_convex)

                               # Scale linewidths by depth
                               coords <- scale_depth(coords, scale_depth)

                               # Apply light blending to colors
                               coords <- blend_light(coords)

                               # Data is already hierarchically sorted by coord$transform()
                               # Just need to create polygon grobs using the group column

                               if (!"group" %in% names(coords)) {
                                     # Fallback for data without groups
                                     warning("No group column found in polygon data")
                                     return(grid::nullGrob())
                               }

                               # Create polygon grobs
                               polygon_grobs <- list()
                               polygon_ids <- unique(coords$group)

                               for(i in seq_along(polygon_ids)){
                                     poly_data <- coords[coords$group == polygon_ids[i], ]

                                     # Handle alpha values (default to 1 if NA)
                                     alpha_val <- poly_data$alpha[1]
                                     if (is.na(alpha_val)) alpha_val <- 1

                                     # Draw this polygon
                                     polygon_grobs[[i]] <- grid::polygonGrob(
                                           x = poly_data$x,
                                           y = poly_data$y,
                                           default.units = "npc",
                                           gp = grid::gpar(
                                                 col = poly_data$colour[1],
                                                 fill = poly_data$fill[1],
                                                 lwd = mean(poly_data$linewidth) * .pt,
                                                 lty = poly_data$linetype[1],
                                                 alpha = alpha_val
                                           ),
                                           name = paste0("polygon_", i)
                                     )
                               }

                               # Combine all polygon grobs
                               do.call(grid::grobTree, polygon_grobs)
                         },

                         draw_key = draw_key_polygon
)


#' 3D polygon geometry with depth sorting
#'
#' `geom_polygon_3d()` renders 3D polygons with proper depth sorting for realistic
#' 3D surface visualization. It's designed to work with surface data
#' from [stat_hull_3d()] and [stat_surface_3d()], as well as regular polygon data like maps.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to [StatIdentity3D].
#' @param sort_method Character indicating algorithm used to determine the order in which
#'   polygons are rendered.
#'   \itemize{
#'     \item \code{"painter"}: Polygons are sorted by the mean depth (distance from viewer after
#'     rotation) of their vertices. This is fast, but can give incorrect results in certain cases.
#'     \item \code{"pairwise"}: A more intensive sorting algorithm that compares every pair of
#'     polygons in 3D to determine which face should be rendered behind the other;
#'     slower but more accurate.
#'     \item \code{"auto"}: The default. Uses pairwise if polygon data has less
#'     than 500 rows and painter otherwise.
#'   }
#' @param scale_depth Logical indicating whether polygon linewidths should be scaled to make closer lines
#'   wider and farther lines narrower. Default is TRUE. Scaling is based on the mean depth of a polygon.
#' @param force_convex Logical indicating whether to remove polygon vertices that are not part of the
#'   convex hull. Default is FALSE. Specifying TRUE can be useful for preventing artifacts in surfaces
#'   that have polygon tiles that wrap over a visible horizon.
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed on to [layer()].
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#'
#' @section Aesthetics:
#' `geom_polygon_3d()` requires:
#' - **x**: X coordinate
#' - **y**: Y coordinate
#' - **z**: Z coordinate (for depth sorting)
#' - **group**: Polygon grouping variable
#'
#' And understands these additional aesthetics:
#' - `fill`: Polygon fill color
#' - `colour`: Border color
#' - `linewidth`: Border line width
#' - `linetype`: Border line type
#' - `alpha`: Transparency
#' - `order`: Vertex order within polygons (for proper polygon construction)
#'
#' @examples
#' # Typically used via stats like stat_surface_3d() or stat_hull_3d()
#' ggplot(sphere_points, aes(x, y, z)) +
#'   stat_hull_3d(method = "convex", fill = "dodgerblue",
#'             light = light(fill = TRUE, mode = "hsl")) +
#'   coord_3d()
#'
#' # Can be used directly with properly structured data
#' triangles <- data.frame(x = rep(c(1, 2, 3), 3),
#'                             y = rep(c(1, 3, 1), 3),
#'                             z = rep(1:3, each = 3),
#'                             shape = rep(letters[1:3], each = 3))
#' ggplot(triangles, aes(x, y, z, fill = shape)) +
#'   geom_polygon_3d(color = "black") +
#'   coord_3d()
#'
#' # Use `sort_method` to choose between depth sorting algorithms
#' d <- data.frame(group = rep(letters[1:3], each = 4),
#'                 x = c(1, 1, 2, 2,   1, 1, 3, 3,   2, 2, 3, 3),
#'                 y = rep(c(1, 2, 2, 1), 3),
#'                 z = rep(c(1, 1.5, 2), each = 4))
#' p <- ggplot(d, aes(x, y, z, group = group, fill = group)) +
#'       coord_3d(pitch = 50, roll = 20, yaw = 0, scales = "fixed") +
#'       theme_light()
#'
#' # fast, but rendering order is incorrect in this particular example
#' p + geom_polygon_3d(color = "black", linewidth = 1, alpha = .75,
#'       sort_method = "painter")
#'
#' # correct rendering order (but slower for large data sets)
#' p + geom_polygon_3d(color = "black", linewidth = 1, alpha = .75,
#'       sort_method = "pairwise")
#'
#' @export
geom_polygon_3d <- function(mapping = NULL, data = NULL, stat = StatIdentity3D,
                            position = "identity",
                            ...,
                            sort_method = "auto", scale_depth = TRUE, force_convex = FALSE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(
            geom = GeomPolygon3D, mapping = mapping, data = data, stat = stat,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(sort_method = sort_method,
                          scale_depth = scale_depth,
                          force_convex = force_convex,
                          na.rm = na.rm, ...)
      )
}

# force polygons to be convex, to prevent artifacts in surfaces that wrap beyond visible horizon
drop_nonconvex_vertices <- function(data, force_convex){
      if(!force_convex) return(data)
      data %>%
            group_by(group) %>%
            mutate(simple = n() <= 3,
                   keep = simple[1] | 1:n() %in% chull(x, y)) %>%
            ungroup() %>%
            filter(keep) %>%
            select(-keep, -simple) %>%
            return()
}

