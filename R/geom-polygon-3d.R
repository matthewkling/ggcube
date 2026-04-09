GeomPolygon3D <- ggproto("GeomPolygon3D", Geom,

                         required_aes = c("x", "y", "z", "group"),

                         optional_aes = c("subgroup"),

                         default_aes = aes(
                               fill = "grey50", colour = "grey50", linewidth = 0.1, linetype = 1, alpha = 1
                         ),

                         draw_panel = function(data, panel_params, coord,
                                               sort_method = "auto",
                                               scale_depth = TRUE,
                                               force_convex = FALSE,
                                               rule = "evenodd",
                                               point.colour = NULL, point.fill = NULL,
                                               point.size = NULL, point.shape = NULL,
                                               point.alpha = NULL, point.stroke = NULL,
                                               residual.colour = NULL, residual.linewidth = NULL,
                                               residual.linetype = NULL, residual.alpha = NULL) {

                               # Parameter validation
                               validate_coord3d(coord)

                               # Transform data
                               sort_method <- match.arg(sort_method, c("auto", "pairwise", "painter"))
                               data$.sort_method <- sort_method
                               if (!".prim" %in% names(data)) {
                                     data$.prim <- "polygon"
                               } else {
                                     data$.prim[is.na(data$.prim)] <- "polygon"
                               }
                               coords <- coord$transform(data, panel_params)

                               # Enforce convexity if requested (polygons only)
                               coords <- drop_nonconvex_vertices(coords, force_convex)

                               # Apply annotation point styling (post-transform, pre-depth-scale).
                               # Values come from geom_smooth_3d defaults/overrides via params.
                               if (".prim" %in% names(coords) && any(coords$.prim == "point", na.rm = TRUE)) {
                                     pt_mask <- coords$.prim == "point" & !is.na(coords$.prim)

                                     # Initialize point-specific columns if they don't exist
                                     for (col in c("size", "shape", "stroke")) {
                                           if (!col %in% names(coords)) coords[[col]] <- NA_real_
                                     }

                                     coords$colour[pt_mask] <- point.colour
                                     coords$fill[pt_mask] <- point.fill
                                     coords$size[pt_mask] <- point.size
                                     coords$shape[pt_mask] <- point.shape
                                     coords$alpha[pt_mask] <- point.alpha
                                     coords$stroke[pt_mask] <- point.stroke
                               }

                               # Apply annotation segment styling
                               if (".prim" %in% names(coords) && any(coords$.prim == "segment", na.rm = TRUE)) {
                                     seg_mask <- coords$.prim == "segment" & !is.na(coords$.prim)

                                     coords$colour[seg_mask] <- residual.colour
                                     coords$alpha[seg_mask] <- residual.alpha
                                     coords$linewidth[seg_mask] <- residual.linewidth
                                     coords$linetype[seg_mask] <- residual.linetype
                               }

                               # Scale linewidths/sizes by depth
                               coords <- scale_depth(coords, scale_depth)

                               # Apply light blending to colors
                               coords <- blend_light(coords)

                               if (!"group" %in% names(coords)) {
                                     warning("No group column found in polygon data")
                                     return(grid::nullGrob())
                               }

                               if (nrow(coords) == 0) {
                                     return(grid::nullGrob())
                               }

                               render_mixed_grobs(coords, rule = rule)
                         },

                         draw_key = draw_key_polygon
)


#' 3D polygon geometry with depth sorting
#'
#' `geom_polygon_3d()` renders 3D polygons with proper depth sorting for realistic
#' 3D surface visualization. It's designed to work with surface data
#' from [stat_hull_3d()] and [stat_surface_3d()], as well as regular polygon data like maps.
#'
#' From R 3.6 and onwards it is possible to draw polygons with holes by providing
#' a `subgroup` aesthetic that differentiates the outer ring points from those
#' describing holes in the polygon, just as in [ggplot2::geom_polygon()].
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer. Note that if you specify `light` or
#'   `cull_backfaces`, behavior will depend on the "winding order" of polygon vertices, with
#'   the counter-clockwise face considered the "front".
#' @param stat The statistical transformation to use on the data. Defaults to [StatIdentity3D].
#' @param rule Either `"evenodd"` or `"winding"`. If polygons with holes are
#'   being drawn (using the `subgroup` aesthetic) this argument defines how the
#'   hole coordinates are interpreted. See [ggplot2::geom_polygon()] for reference.
#' @inheritParams polygon_params
#' @inheritParams position_param
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
#' - `subgroup`: Secondary grouping for polygons with holes
#' - `order`: Vertex order within polygons (for proper polygon construction)
#'
#' @examples
#' # Typically used via stats such as stat_surface_3d() or stat_hull_3d()
#' ggplot(sphere_points, aes(x, y, z)) +
#'   stat_hull_3d(method = "convex", fill = "dodgerblue",
#'             light = light(fill = TRUE, mode = "hsl")) +
#'   coord_3d()
#'
#' # Can be used directly with properly structured data
#' triangles <- data.frame(x = rep(c(3, 2, 1), 3),
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
#'       coord_3d(pitch = 50, roll = 20, yaw = 0,
#'                scales = "fixed", light = "none") +
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
#' @return A `Layer` object that can be added to a ggplot.
#' @export
geom_polygon_3d <- function(mapping = NULL, data = NULL, stat = StatIdentity3D,
                            position = "identity",
                            ...,
                            rule = "evenodd",
                            sort_method = "auto", scale_depth = TRUE, force_convex = FALSE,
                            cull_backfaces = FALSE,
                            light = NULL,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(
            geom = GeomPolygon3D, mapping = mapping, data = data, stat = get_proto(stat, "stat"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(rule = rule,
                          sort_method = sort_method,
                          scale_depth = scale_depth,
                          force_convex = force_convex,
                          cull_backfaces = cull_backfaces,
                          light = light,
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
