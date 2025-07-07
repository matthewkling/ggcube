#' Blend lighting values with base colors using brightness-only HSV blending
blend_lighting_with_colors <- function(base_colors, light_values, lighting) {

      if (length(base_colors) != length(light_values)) {
            stop("base_colors and light_values must have the same length")
      }

      if (lighting$method == "normal_rgb") {
            warning("Color blending is not supported with normal_rgb lighting method")
            return(base_colors)
      }

      # Handle any invalid values
      valid_mask <- !is.na(base_colors) & !is.na(light_values) & is.finite(light_values)
      if (!any(valid_mask)) {
            return(base_colors)
      }

      result_colors <- base_colors

      # Process only valid entries
      valid_base <- base_colors[valid_mask]
      valid_light <- light_values[valid_mask]

      # Normalize lighting values to [0, 1] if needed
      if (lighting$method == "signed") {
            valid_light <- (valid_light + 1) / 2
      }
      valid_light <- pmax(0, pmin(1, valid_light))

      # Convert to HSV
      base_rgb <- col2rgb(valid_base)
      base_hsv <- rgb2hsv(base_rgb)

      # Modify only brightness (V component)
      new_hsv <- base_hsv

      for (i in seq_along(valid_light)) {
            h_val <- base_hsv[1, i]
            s_val <- base_hsv[2, i]
            v_val <- base_hsv[3, i]
            light_val <- valid_light[i]

            # Handle NaN hue (gray colors)
            if (is.nan(h_val)) h_val <- 0

            if (light_val > 0.5) {
                  # Brighten
                  blend_factor <- (light_val - 0.5) * 2 * lighting$blend_strength
                  new_v <- v_val + blend_factor * (1 - v_val)
            } else {
                  # Darken
                  blend_factor <- (0.5 - light_val) * 2 * lighting$blend_strength
                  new_v <- v_val * (1 - blend_factor)
            }

            new_hsv[1, i] <- h_val
            new_hsv[2, i] <- s_val
            new_hsv[3, i] <- pmax(0, pmin(1, new_v))
      }

      # Convert back to colors
      new_colors <- hsv(new_hsv[1, ], new_hsv[2, ], new_hsv[3, ])
      result_colors[valid_mask] <- new_colors

      return(result_colors)
}

GeomPolygon3D <- ggproto("GeomPolygon3D", Geom,
                         required_aes = c("x", "y", "z", "group"),
                         default_aes = aes(
                               fill = "grey80", colour = NA, linewidth = 0.5, linetype = 1, alpha = 1
                         ),

                         draw_panel = function(data, panel_params, coord) {
                               # Transform ALL data at once
                               coords <- coord$transform(data, panel_params)

                               # Extract lighting parameters from special columns
                               if ("blend_enabled" %in% names(coords) && any(coords$blend_enabled)) {
                                     # Reconstruct lighting object from columns
                                     light <- list(
                                           blend = coords$blend_enabled[1],
                                           blend_strength = coords$blend_strength[1],
                                           highlight_color = coords$light_highlight[1],
                                           shadow_color = coords$light_shadow[1],
                                           method = coords$lighting_method[1]
                                     )

                                     # Apply lighting blending if enabled
                                     if (light$blend && "light" %in% names(coords)) {
                                           coords$fill <- blend_lighting_with_colors(coords$fill, coords$light, light)
                                     }

                                     # Clean up lighting parameter columns
                                     coords <- coords[, !names(coords) %in% c("blend_enabled", "blend_strength", "light_highlight", "light_shadow", "lighting_method")]
                               }

                               # Set up hierarchical object IDs for pillars/voxels
                               if("pillar_id" %in% names(coords)){
                                     coords$object_id <- coords$pillar_id
                               } else if("voxel_id" %in% names(coords)){
                                     coords$object_id <- coords$voxel_id
                               } else {
                                     # For surfaces and other geoms, treat each face as its own object
                                     coords$object_id <- coords$group %||% 1:nrow(coords)
                               }

                               # Calculate depths and sort hierarchically using viewpoint distance
                               coords <- coords %>%
                                     group_by(object_id) %>%
                                     mutate(object_depth = max(depth)) %>%  # Farther objects first
                                     group_by(face_id) %>%
                                     mutate(face_depth = mean(depth)) %>%   # Face center depth
                                     ungroup()

                               # Sort by depths, and optionally by order if it exists
                               if ("order" %in% names(coords)) {
                                     coords <- coords %>%
                                           arrange(desc(object_depth), desc(face_depth), order)
                               } else {
                                     coords <- coords %>%
                                           arrange(desc(object_depth), desc(face_depth))
                               }

                               # Create polygon grobs
                               polygon_grobs <- list()
                               faces <- unique(coords$face_id)

                               for(i in seq_along(faces)){
                                     poly_data <- filter(coords, face_id == faces[i])

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
