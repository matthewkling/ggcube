StatHull <- ggproto("StatHull", Stat,
                       required_aes = c("x", "y", "z"),

                       compute_group = function(data, scales, method = "alpha", alpha = 1.0,
                                                light = lighting("lambert")) {
                             coords <- as.matrix(data[, c("x", "y", "z")])

                             # Get triangle indices
                             tri <- switch(method,
                                           alpha = {
                                                 ashape <- alphashape3d::ashape3d(coords, alpha = alpha)
                                                 triangles <- ashape$triang

                                                 # Filter by rhoT <= alpha to get actual alpha shape
                                                 alpha_triangles <- triangles[triangles[,6] <= alpha, 1:3]

                                                 if (nrow(alpha_triangles) > 0) {
                                                       alpha_triangles
                                                 } else {
                                                       triangles[, 1:3]
                                                 }
                                           },
                                           hull = {
                                                 tryCatch({
                                                       hull_result <- geometry::convhulln(coords)
                                                       if (is.matrix(hull_result) && ncol(hull_result) == 3) {
                                                             hull_result
                                                       } else {
                                                             stop("Invalid hull result format")
                                                       }
                                                 }, error = function(e) {
                                                       stop("Convex hull computation failed: ", e$message)
                                                 })
                                           },
                                           stop("Unknown method: use 'alpha' or 'hull'")
                             )

                             # Check that we got valid triangulation
                             if (is.null(tri) || nrow(tri) == 0) {
                                   stop("No triangles generated - try adjusting alpha parameter or check data")
                             }

                             # Compute face normals using simple cross product
                             A <- coords[tri[,1], ]
                             B <- coords[tri[,2], ]
                             C <- coords[tri[,3], ]

                             # Edge vectors
                             edge1 <- B - A
                             edge2 <- C - A

                             # Cross product for each triangle
                             normals <- matrix(0, nrow = nrow(tri), ncol = 3)
                             for(i in 1:nrow(tri)) {
                                   # Cross product: edge1 × edge2
                                   cross <- c(
                                         edge1[i,2] * edge2[i,3] - edge1[i,3] * edge2[i,2],
                                         edge1[i,3] * edge2[i,1] - edge1[i,1] * edge2[i,3],
                                         edge1[i,1] * edge2[i,2] - edge1[i,2] * edge2[i,1]
                                   )

                                   # Normalize
                                   cross_length <- sqrt(sum(cross^2))
                                   if (cross_length > 0) {
                                         normals[i,] <- cross / cross_length
                                   } else {
                                         normals[i,] <- c(0, 0, 1)  # Default up
                                   }
                             }

                             # For convex hulls: ensure normals point outward from centroid
                             if (method == "hull") {
                                   data_center <- colMeans(coords)
                                   for(i in 1:nrow(tri)) {
                                         triangle_center <- (A[i,] + B[i,] + C[i,]) / 3
                                         outward_direction <- triangle_center - data_center

                                         # If normal points inward (negative dot product), flip it
                                         if (sum(normals[i,] * outward_direction) < 0) {
                                               normals[i,] <- -normals[i,]
                                         }
                                   }
                             }
                             # For alpha shapes: keep original normals (no orientation fix yet)

                             light_val <- compute_lighting(normals, light)

                             # Flatten triangle data with all computed variables
                             verts <- coords[as.vector(t(tri)), ]
                             face_id <- rep(paste0("tri_", 1:nrow(tri)), each = 3)
                             normal_x <- rep(normals[,1], each = 3)
                             normal_y <- rep(normals[,2], each = 3)
                             normal_z <- rep(normals[,3], each = 3)
                             light_val <- rep(light_val, each = 3)
                             triangle_index <- rep(1:nrow(tri), each = 3)

                             # Re-apply identity scaling for RGB colors after rep()
                             if (light$method == "normal_rgb") {
                                   light_val <- I(light_val)
                             }

                             data.frame(
                                   x = verts[,1],
                                   y = verts[,2],
                                   z = verts[,3],
                                   face_id = face_id,
                                   triangle_index = triangle_index,
                                   normal_x = normal_x,
                                   normal_y = normal_y,
                                   normal_z = normal_z,
                                   light = light_val
                             )
                       }
)


#' Create 3D convex and alpha hulls with lighting
#'
#' `stat_hull()` creates triangulated surfaces from 3D point clouds using either
#' convex hull or alpha shape algorithms. It computes surface normals and applies
#' various lighting models to create realistic 3D surface visualizations.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. The required
#'   aesthetics are `x`, `y`, and `z`. Additional aesthetics can use computed
#'   variables with [after_stat()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomPolygon3D] for proper 3D depth sorting.
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed on to [layer()].
#' @param method Triangulation method. Either:
#'   - `"hull"`: Convex hull triangulation (works well for convex shapes like spheres)
#'   - `"alpha"`: Alpha shape triangulation (can capture non-convex topologies like toruses)
#' @param alpha Alpha parameter for alpha shape triangulation. Smaller values create
#'   more detailed surfaces but may fragment. Larger values create smoother surfaces
#'   but may fill holes. Only used when `method = "alpha"`.
#' @param light A lighting specification object created by \code{lighting()}
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#'
#' @section Computed variables:
#' - `light`: Computed lighting value (numeric for most methods, hex color for `normal_rgb`)
#' - `normal_x`, `normal_y`, `normal_z`: Surface normal components
#' - `triangle_index`: Sequential triangle number (useful for debugging)
#' - `face_id`: Triangle group identifier
#'
#' @section Aesthetics:
#' `stat_hull()` requires the following aesthetics:
#' - **x**: X coordinate
#' - **y**: Y coordinate
#' - **z**: Z coordinate
#'
#' Computed variables can be accessed using [after_stat()]:
#' - `after_stat(light)`: Lighting values
#' - `after_stat(normal_x)`: X component of surface normal
#' - `after_stat(normal_y)`: Y component of surface normal
#' - `after_stat(normal_z)`: Z component of surface normal
#'
#' @examples
#' library(ggplot2)
#'
#' # Generate sphere points
#' set.seed(123)
#' theta <- runif(200, 0, 2*pi)
#' phi <- acos(runif(200, -1, 1))
#' sphere_df <- data.frame(
#'   x = sin(phi) * cos(theta),
#'   y = sin(phi) * sin(theta),
#'   z = cos(phi)
#' )
#'
#' # Basic surface with Lambert lighting
#' ggplot(sphere_df, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)), method = "hull") +
#'   scale_fill_gradient(low = "black", high = "white") +
#'   coord_3d()
#'
#' # Surface with normal-to-RGB coloring
#' ggplot(sphere_df, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)),
#'                method = "hull", light = "normal_rgb") +
#'   coord_3d()
#'
#' # Quantized lighting (cel shading effect)
#' ggplot(sphere_df, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)),
#'                method = "hull", lighting = "quantize", n_levels = 3) +
#'   scale_fill_gradient(low = "black", high = "white") +
#'   coord_3d()
#'
#' # Signed lighting with continuous gradient
#' ggplot(sphere_df, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)),
#'                method = "hull", light = "signed") +
#'   scale_fill_gradient2(low = "blue", mid = "gray", high = "white") +
#'   coord_3d()
#'
#' # Custom light direction
#' ggplot(sphere_df, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)),
#'                method = "hull", light_dir = c(1, 1, 1)) +
#'   scale_fill_gradient(low = "black", high = "white") +
#'   coord_3d()
#'
#' # Rotated RGB color scheme
#' ggplot(sphere_df, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)), light = "normal_rgb",
#'                method = "hull") +
#'   coord_3d()
#'
#' @seealso [coord_3d()] for 3D coordinate systems, [geom_polygon_3d] for the
#'   default geometry with depth sorting.
#'
#' @export
stat_hull <- function(mapping = NULL, data = NULL,
                         geom = GeomPolygon3D, # nonstandard syntax, but `"polygon_3d"` failed
                         position = "identity",
                         method = "alpha", alpha = 1.0,
                         light = lighting("lambert"),
                         inherit.aes = TRUE,
                         ...) {

      default_mapping <- aes(group = after_stat(face_id))

      # If mapping is provided, combine it with the default only if group isn't set
      if (!is.null(mapping)) {
            mapping_names <- names(mapping)
            if (!"group" %in% mapping_names) {
                  mapping <- modifyList(default_mapping, mapping)
            }
      } else {
            mapping <- default_mapping
      }

      layer(
            stat = StatHull, data = data, mapping = mapping, geom = geom,
            position = position, inherit.aes = inherit.aes,
            params = list(method = method, alpha = alpha, light = light, ...)
      )
}



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
                               group_depths <- sapply(coords_split, function(tri) {
                                     mean(tri$z_proj, na.rm = TRUE)
                               })

                               # Sort groups by depth (back to front)
                               ordered_groups <- names(sort(group_depths, decreasing = TRUE))

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




# test ---------------------------------

# # Random sphere points
# theta <- runif(1000, 0, 2*pi)
# phi <- acos(runif(1000, -1, 1))
# r <- 1
# df <- data.frame(
#       x = r * sin(phi) * cos(theta),
#       y = r * sin(phi) * sin(theta),
#       z = r * cos(phi)
# )
#
# ggplot(df, aes(x, y, z = z)) +
#       stat_hull(aes(fill = after_stat(light), color = after_stat(light)),
#                    method = "hull", light = lighting("signed")) +
#       scale_fill_gradient(low = "gray10", high = "white") +
#       scale_color_gradient(low = "gray10", high = "white") +
#       coord_3d(pitch = rnorm(1, 0, 360), roll = rnorm(1, 0, 360), yaw = rnorm(1, 0, 360), dist = 2)
#
# ggplot(df, aes(x, y, z = z)) +
#       stat_hull(aes(fill = after_stat(light), color = after_stat(light)),
#                    method = "hull", light = lighting("normal_rgb"),
#                    linewidth = .2) +
#       coord_3d() +
#       theme_void()

