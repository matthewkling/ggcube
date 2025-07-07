
GeomPolygon3D <- ggproto("GeomPolygon3D", Geom,
                         required_aes = c("x", "y", "z", "group"),
                         default_aes = aes(
                               fill = "grey80", colour = NA, linewidth = 0.5, linetype = 1, alpha = 1
                         ),

                         draw_panel = function(data, panel_params, coord) {
                               # Transform ALL data at once
                               coords <- coord$transform(data, panel_params)

                               # Split by group and calculate depths
                               coords_split <- split(coords, coords$group)

                               # Check if we're rendering pillars (has pillar_id column)
                               has_pillar_ids <- "pillar_id" %in% names(coords)

                               if (has_pillar_ids) {
                                     # Two-level sorting: pillars first, then faces within pillars

                                     # Calculate pillar depths (using max z_proj of all faces in each pillar)
                                     pillar_ids <- unique(coords$pillar_id)
                                     pillar_depths <- sapply(pillar_ids, function(pid) {
                                           pillar_coords <- coords[coords$pillar_id == pid, ]
                                           max(pillar_coords$z_proj, na.rm = TRUE)
                                     })
                                     names(pillar_depths) <- pillar_ids

                                     # Calculate face depths within each group
                                     group_depths <- sapply(coords_split, function(poly) {
                                           max(poly$z_proj, na.rm = TRUE)  # Use closest vertex for faces
                                     })

                                     # Create sorting key: pillar_depth * 1e6 + face_depth
                                     # This ensures pillars sort first, then faces within pillars
                                     group_names <- names(group_depths)
                                     group_pillar_ids <- sapply(group_names, function(gname) {
                                           coords_split[[gname]]$pillar_id[1]  # Get pillar_id for this group
                                     })

                                     # Create combined sorting keys
                                     sorting_keys <- pillar_depths[as.character(group_pillar_ids)] * 1e6 + group_depths
                                     names(sorting_keys) <- group_names

                                     # Sort by combined key (back to front)
                                     ordered_groups <- names(sort(sorting_keys, decreasing = TRUE))

                               } else {
                                     # Original single-level sorting for surfaces and other geometries
                                     group_depths <- sapply(coords_split, function(tri) {
                                           mean(tri$z_proj, na.rm = TRUE)
                                     })

                                     # Sort groups by depth (back to front)
                                     ordered_groups <- names(sort(group_depths, decreasing = TRUE))
                               }

                               # Draw each polygon individually to preserve aesthetics
                               polygon_grobs <- list()
                               for(i in seq_along(ordered_groups)) {
                                     poly_data <- coords_split[[ordered_groups[i]]]

                                     # Sort by order if present to get correct vertex sequence
                                     if ("order" %in% names(poly_data)) {
                                           poly_data <- poly_data[order(poly_data$order), ]
                                     }

                                     # Draw this polygon
                                     polygon_grobs[[i]] <- grid::polygonGrob(
                                           x = poly_data$x,
                                           y = poly_data$y,
                                           default.units = "npc",
                                           gp = grid::gpar(
                                                 col = poly_data$colour[1],
                                                 fill = poly_data$fill[1],
                                                 lwd = poly_data$linewidth[1] * .pt,
                                                 lty = poly_data$linetype[1]
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
#' from [stat_hull()] and [stat_surface()], as well as other data.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to "identity".
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
#'
#' @examples
#' # Typically used via stat_surface() or stat_terrain()
#' ggplot(sphere_data, aes(x, y, z)) +
#'   stat_surface(method = "hull") +
#'   coord_3d()
#'
#' # Can be used directly with pre-triangulated data
#' ggplot(triangle_data, aes(x, y, z, group = triangle_id)) +
#'   geom_polygon_3d(fill = "lightblue") +
#'   coord_3d()
#'
#' @seealso [stat_hull()] and [stat_surface()].
#' @export
geom_polygon_3d <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ..., na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE) {
      layer(
            geom = GeomPolygon3D, mapping = mapping, data = data, stat = stat,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
      )
}
