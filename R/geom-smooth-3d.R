GeomSmooth3D <- ggproto("GeomSmooth3D", Geom,
                         required_aes = c("x", "y", "z", "group"),
                         default_aes = aes(
                               fill = "darkblue", colour = "white",
                               linewidth = 0.1, linetype = 1, alpha = 1
                         ),

                         draw_panel = function(data, panel_params, coord, scale_depth = TRUE) {

                               # Assign correct aesthetics to primary/CI elements
                               merge_aes <- function(a, b = NULL){
                                     if(is.null(b)) return(a)
                                     ifelse(!is.na(b), b, a)
                               }
                               data$fill <- merge_aes(data$fill, data$se.fill)
                               data$colour <- merge_aes(data$colour, data$se.colour)
                               data$alpha <- merge_aes(data$alpha, data$se.alpha)
                               data$linewidth <- merge_aes(data$linewidth, data$se.linewidth)

                               # Transform data
                               validate_coord3d(coord)
                               coords <- coord$transform(data, panel_params)

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
                                                 lwd = poly_data$linewidth[1] * .pt,
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


#' 3D smoothing geometry
#'
#' @export
geom_smooth_3d <- function(mapping = NULL, data = NULL, stat = StatSmooth3D,
                            position = "identity", ..., na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE) {
      layer(
            geom = GeomSmooth3D, mapping = mapping, data = data, stat = stat,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
      )
}
