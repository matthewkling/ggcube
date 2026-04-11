GeomPolygon3D <- ggproto("GeomPolygon3D", Geom,

                         required_aes = c("x", "y", "z", "group"),

                         optional_aes = c("subgroup"),

                         extra_params = c("na.rm", "annotate"),

                         default_aes = aes(
                               fill = "grey50", colour = "grey50", linewidth = 0.1, linetype = 1, alpha = 1
                         ),

                         setup_data = function(data, params) {
                               setup_annotations(data, params)
                         },

                         draw_panel = function(data, panel_params, coord,
                                               sort_method = "auto",
                                               scale_depth = TRUE,
                                               force_convex = FALSE,
                                               rule = "evenodd",
                                               annotate = NULL,
                                               point.colour = NULL, point.fill = NULL,
                                               point.size = NULL, point.shape = NULL,
                                               point.alpha = NULL, point.stroke = NULL,
                                               residual.colour = NULL, residual.linewidth = NULL,
                                               residual.linetype = NULL, residual.alpha = NULL) {

                               # Parameter validation
                               validate_coord3d(coord)

                               # Resolve annotation sentinels that need panel range info
                               # (no-op for v1 types; will expand plane extents when added)
                               data <- prepare_annotations(data, panel_params)

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

                               # Apply smooth overlay point styling (post-transform, pre-depth-scale).
                               # Values come from geom_smooth_3d point.* params.
                               # Only applied when params are non-NULL (i.e. when smooth overlay
                               # is active). Annotation points are styled separately via
                               # apply_annotation_styles().
                               if (".prim" %in% names(coords) && any(coords$.prim == "point", na.rm = TRUE)) {
                                     pt_mask <- coords$.prim == "point" & !is.na(coords$.prim)

                                     # Initialize point-specific columns if they don't exist
                                     for (col in c("size", "shape", "stroke")) {
                                           if (!col %in% names(coords)) coords[[col]] <- NA_real_
                                     }

                                     if (!is.null(point.colour)) coords$colour[pt_mask] <- point.colour
                                     if (!is.null(point.fill))   coords$fill[pt_mask]   <- point.fill
                                     if (!is.null(point.size))   coords$size[pt_mask]   <- point.size
                                     if (!is.null(point.shape))  coords$shape[pt_mask]  <- point.shape
                                     if (!is.null(point.alpha))  coords$alpha[pt_mask]  <- point.alpha
                                     if (!is.null(point.stroke)) coords$stroke[pt_mask] <- point.stroke
                               }

                               # Apply smooth overlay segment styling (residual lines)
                               if (".prim" %in% names(coords) && any(coords$.prim == "segment", na.rm = TRUE)) {
                                     seg_mask <- coords$.prim == "segment" & !is.na(coords$.prim)

                                     if (!is.null(residual.colour))    coords$colour[seg_mask]    <- residual.colour
                                     if (!is.null(residual.alpha))     coords$alpha[seg_mask]     <- residual.alpha
                                     if (!is.null(residual.linewidth)) coords$linewidth[seg_mask] <- residual.linewidth
                                     if (!is.null(residual.linetype))  coords$linetype[seg_mask]  <- residual.linetype
                               }

                               # Apply annotate_3d styles (overrides blanket smooth overlay styling above for annotate_3d rows only)
                               coords <- apply_annotation_styles(coords)

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
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer. Note that if you specify `light` or
#'   `cull_backfaces`, behavior will depend on the "winding order" of polygon vertices, with
#'   the counter-clockwise face considered the "front".
#' @param stat The statistical transformation to use on the data. Defaults to `"identity_3d"`.
#' @param rule Either `"evenodd"` or `"winding"`. If polygons with holes are
#'   being drawn (using the `subgroup` aesthetic) this argument defines how the
#'   hole coordinates are interpreted. See [ggplot2::geom_polygon()] for reference.
#' @inheritParams polygon_params
#' @inheritParams light_param
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
