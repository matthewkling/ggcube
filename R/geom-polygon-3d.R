GeomPolygon3D <- ggproto("GeomPolygon3D", Geom,

                         required_aes = c("x", "y", "z", "group"),

                         optional_aes = c("subgroup"),

                         setup_data = function(data, params) {
                               clip_to_limits(data)
                         },

                         handle_na = function(self, data, params) {
                               remove_partial_polygons(data, params$na.rm, snake_class_name(self))
                         },

                         default_aes = aes(
                               fill = "grey50", colour = "grey50", linewidth = 0.1, linetype = 1, alpha = 1
                         ),

                         # Whether the package's default lighting applies. Layers
                         # whose faces all point the same way, such as contours,
                         # set this to FALSE: lighting can only shift all their
                         # colors at once. Lighting set explicitly applies either way.
                         lit_by_default = TRUE,

                         # Whether, unless a color is mapped or set, opaque polygons
                         # are outlined in their fill color, which hides the seams
                         # that antialiasing leaves between adjacent polygons, and
                         # translucent ones aren't outlined, since outlines
                         # overlapping their neighbors would show as a mesh. An
                         # `after_scale(fill)` default can't do this, since ggplot2
                         # applies it before fixed parameters like `fill = "red"`.
                         outline_matches_fill = TRUE,

                         use_defaults = function(self, data, params = list(),
                                                 modifiers = aes(), default_aes = NULL,
                                                 theme = NULL, ...) {
                               own_color <- "colour" %in% c(names(data), names(params),
                                                            names(modifiers))
                               data <- ggproto_parent(Geom, self)$use_defaults(
                                     data, params, modifiers, default_aes, theme, ...)
                               if (self$outline_matches_fill && !own_color && nrow(data) > 0) {
                                     # Only polygons: points and lines of composite
                                     # layers keep their own colors
                                     polygon <- if (".prim" %in% names(data)) {
                                           is.na(data$.prim) | data$.prim == "polygon"
                                     } else {
                                           rep(TRUE, nrow(data))
                                     }
                                     opaque <- is.na(data$alpha) | data$alpha >= 1
                                     data$colour[polygon] <- ifelse(opaque[polygon], data$fill[polygon], NA)
                               }
                               data
                         },

                         draw_panel = function(self, data, panel_params, coord,
                                               scale_depth = TRUE,
                                               force_convex = FALSE,
                                               rule = "evenodd",
                                               on_top = FALSE,
                                               residual.colour = NULL, residual.linewidth = NULL,
                                               residual.linetype = NULL, residual.alpha = NULL) {

                               # Parameter validation
                               validate_coord3d(coord)

                               if (!self$lit_by_default && is_default_light(panel_params$light)) {
                                     panel_params$light <- NULL
                               }

                               # Transform data
                               if (!".prim" %in% names(data)) {
                                     data$.prim <- "polygon"
                               } else {
                                     data$.prim[is.na(data$.prim)] <- "polygon"
                               }
                               coords <- coord$transform(data, panel_params, sort = FALSE)

                               finish <- function(coords) {
                                     # Enforce convexity if requested (polygons only)
                                     coords <- drop_nonconvex_vertices(coords, force_convex)

                                     # Apply smooth overlay segment styling (residual lines)
                                     if (".prim" %in% names(coords) && any(coords$.prim == "segment", na.rm = TRUE)) {
                                           seg_mask <- coords$.prim == "segment" & !is.na(coords$.prim)

                                           if (!is.null(residual.colour))    coords$colour[seg_mask]    <- residual.colour
                                           if (!is.null(residual.alpha))     coords$alpha[seg_mask]     <- residual.alpha
                                           if (!is.null(residual.linewidth)) coords$linewidth[seg_mask] <- residual.linewidth
                                           if (!is.null(residual.linetype))  coords$linetype[seg_mask]  <- residual.linetype
                                     }

                                     # Scale linewidths/sizes by depth
                                     coords <- apply_depth_scaling(coords, scale_depth)

                                     # Apply light blending to colors
                                     blend_light(coords)
                               }

                               if (!"group" %in% names(coords)) {
                                     warning("No group column found in polygon data")
                                     return(grid::nullGrob())
                               }

                               layer_scene_grob(coords, panel_params, finish, list(rule = rule), on_top = on_top)
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
#'   the counter-clockwise face considered the "front". Each polygon is lit using the normal
#'   of its own plane; with the default `backface_scale = -1` in [light()], which lights a back
#'   face as the reverse side of its front face, this lights polygons the same way whatever
#'   their winding order.
#' @param stat The statistical transformation to use on the data. Defaults to `"identity_3d"`.
#' @param rule Either `"evenodd"` or `"winding"`. If polygons with holes are
#'   being drawn (using the `subgroup` aesthetic) this argument defines how the
#'   hole coordinates are interpreted. See [ggplot2::geom_polygon()] for reference.
#' @inheritParams polygon_params
#' @inheritParams light_param
#' @inheritParams position_param
#' @inheritParams on_top_param
#' @inheritParams annotate_params
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
#' # Overlapping polygons are drawn in the correct order, even where no
#' # ordering by average depth would be correct
#' d <- data.frame(group = rep(letters[1:3], each = 4),
#'                 x = c(1, 1, 2, 2,   1, 1, 3, 3,   2, 2, 3, 3),
#'                 y = rep(c(1, 2, 2, 1), 3),
#'                 z = rep(c(1, 1.5, 2), each = 4))
#' p <- ggplot(d, aes(x, y, z, group = group, fill = group)) +
#'       coord_3d(pitch = 50, roll = 20, yaw = 0,
#'                scales = "fixed", light = "none") +
#'       theme_light()
#' p + geom_polygon_3d(color = "black", linewidth = 1, alpha = .75)
#'
#' @return A `Layer` object that can be added to a ggplot.
#' @export
geom_polygon_3d <- function(mapping = NULL, data = NULL, stat = StatIdentity3D,
                            position = "identity",
                            ...,
                            annotate = deprecated(),
                            rule = "evenodd",
                            sort_method = deprecated(), scale_depth = TRUE, on_top = FALSE, force_convex = FALSE,
                            cull_backfaces = FALSE,
                            light = NULL,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
      check_sort_method(sort_method, "geom_polygon_3d")
      check_on_top(on_top)

      out <- layer(
            geom = GeomPolygon3D, mapping = mapping, data = data, stat = get_proto(stat, "stat"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(rule = rule,
                          scale_depth = scale_depth, on_top = on_top,
                          force_convex = force_convex,
                          cull_backfaces = cull_backfaces,
                          light = light,
                          na.rm = na.rm, ...)
      )
      with_annotations(out, annotate, "geom_polygon_3d")
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
