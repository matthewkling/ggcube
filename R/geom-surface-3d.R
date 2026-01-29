# Geom that tessellates point grid into polygon tiles for 3D surface rendering

GeomSurface3D <- ggproto("GeomSurface3D", GeomPolygon3D,

                         required_aes = c("x", "y", "z"),

                         default_aes = aes(
                               colour = NA,
                               fill = "grey60",
                               linewidth = 0.1,
                               linetype = 1,
                               alpha = NA
                         ),

                         extra_params = c("na.rm", "method", "grid", "cull_backfaces",
                                          "sort_method", "scale_depth", "force_convex"),

                         setup_params = function(data, params) {
                               params$method <- params$method %||% "auto"
                               params$grid <- params$grid %||% "rectangle"
                               params
                         },

                         setup_data = function(data, params) {

                               # Convert point grid to polygon tiles
                               data <- points_to_tiles(
                                     data,
                                     method = params$method,
                                     grid_type = params$grid,
                                     group_prefix = "surface__tile"
                               )

                               # Average aesthetics per polygon
                               data <- average_aesthetics(data)

                               return(data)
                         }

                         # draw_panel inherited from GeomPolygon3D
)


# geom_surface_3d() function is defined in stat-surface-3d.R
