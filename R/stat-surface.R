StatSurface <- ggproto("StatSurface", Stat,
                       required_aes = c("x", "y", "z"),

                       compute_group = function(data, scales, method = "alpha", alpha = 1.0,
                                                light_dir = c(0, 0, 1), lighting = "lambert",
                                                n_levels = 3, clamp_negative = TRUE) {
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

                             # Apply lighting model
                             light_dir <- light_dir / sqrt(sum(light_dir^2))
                             dot_products <- rowSums(normals * matrix(rep(light_dir, nrow(normals)),
                                                                      nrow = nrow(normals), byrow = TRUE))

                             light <- switch(lighting,
                                             lambert = pmax(0, dot_products),          # Standard diffuse lighting
                                             signed = dot_products,                    # Unclamped: full -1 to +1 range
                                             ambient = rep(0.5, length(dot_products)), # Uniform lighting
                                             quantize = {                              # Quantized lighting with parameters
                                                   if(clamp_negative) {
                                                         # Negative values → lowest level (0)
                                                         # Positive values → quantized across remaining levels
                                                         result <- numeric(length(dot_products))

                                                         # All negative values get level 0
                                                         negative_mask <- dot_products <= 0
                                                         result[negative_mask] <- 0

                                                         # Positive values get quantized across levels 1 to n_levels
                                                         positive_mask <- dot_products > 0
                                                         if(any(positive_mask)) {
                                                               pos_vals <- dot_products[positive_mask]
                                                               # Quantize positive values into (n_levels - 1) bins
                                                               breaks <- seq(0, 1, length.out = n_levels)  # n_levels-1 intervals
                                                               quantized <- cut(pos_vals, breaks = breaks, labels = FALSE, include.lowest = TRUE)
                                                               # Map to levels 1/(n_levels-1), 2/(n_levels-1), ..., 1
                                                               result[positive_mask] <- quantized / (n_levels - 1)
                                                         }
                                                         result
                                                   } else {
                                                         # Quantize full range [-1, 1]
                                                         breaks <- seq(-1, 1, length.out = n_levels + 1)
                                                         quantized <- cut(dot_products, breaks = breaks, labels = FALSE, include.lowest = TRUE)
                                                         (quantized - 1) / (n_levels - 1)
                                                   }
                                             },
                                             normal_rgb = {                            # Map normals to RGB hex colors
                                                   # Create rotation so that normals aligned with light_dir map to white
                                                   light_dir_norm <- light_dir / sqrt(sum(light_dir^2))

                                                   # Target direction for white color in RGB space
                                                   white_dir <- c(1, 1, 1) / sqrt(3)

                                                   # Create rotation matrix using Rodrigues' formula
                                                   # We want to rotate FROM white_dir TO light_dir_norm
                                                   # So we can apply the inverse rotation to normals

                                                   # Cross product for rotation axis (white_dir × light_dir_norm)
                                                   v <- c(
                                                         white_dir[2] * light_dir_norm[3] - white_dir[3] * light_dir_norm[2],
                                                         white_dir[3] * light_dir_norm[1] - white_dir[1] * light_dir_norm[3],
                                                         white_dir[1] * light_dir_norm[2] - white_dir[2] * light_dir_norm[1]
                                                   )

                                                   s <- sqrt(sum(v^2))  # sine of angle
                                                   c <- sum(white_dir * light_dir_norm)  # cosine of angle

                                                   if (s < 1e-10) {
                                                         # Vectors are already aligned or opposite
                                                         if (c > 0) {
                                                               rot_matrix <- diag(3)  # Identity matrix
                                                         } else {
                                                               # 180 degree rotation - use any perpendicular axis
                                                               rot_matrix <- -diag(3)
                                                         }
                                                   } else {
                                                         # Rodrigues' rotation formula
                                                         # R = I + [v]× + [v]×^2 * (1-c)/s^2
                                                         v_cross <- matrix(c(
                                                               0, -v[3], v[2],
                                                               v[3], 0, -v[1],
                                                               -v[2], v[1], 0
                                                         ), nrow = 3, byrow = TRUE)

                                                         rot_matrix <- diag(3) + v_cross + v_cross %*% v_cross * ((1 - c) / s^2)
                                                   }

                                                   # Apply rotation to normals
                                                   rotated_normals <- normals %*% rot_matrix

                                                   # Scale normals from [-1,1] to [0,1] for RGB
                                                   r <- (rotated_normals[,1] + 1) / 2
                                                   g <- (rotated_normals[,2] + 1) / 2
                                                   b <- (rotated_normals[,3] + 1) / 2

                                                   # Convert to hex colors
                                                   rgb(r, g, b)
                                             },
                                             normal_x = (normals[,1] + 1) / 2,         # X-normal as color (0-1 range)
                                             normal_y = (normals[,2] + 1) / 2,         # Y-normal as color (0-1 range)
                                             normal_z = (normals[,3] + 1) / 2,         # Z-normal as color (0-1 range)
                                             stop("Unknown lighting method")
                             )

                             # Flatten triangle data with all computed variables
                             verts <- coords[as.vector(t(tri)), ]
                             face_id <- rep(paste0("tri_", 1:nrow(tri)), each = 3)
                             normal_x <- rep(normals[,1], each = 3)
                             normal_y <- rep(normals[,2], each = 3)
                             normal_z <- rep(normals[,3], each = 3)
                             light_val <- rep(light, each = 3)
                             triangle_index <- rep(1:nrow(tri), each = 3)

                             # Auto-wrap RGB colors with I() for identity scaling
                             if (lighting == "normal_rgb") {
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


#' Create 3D surface triangulation with lighting
#'
#' `stat_surface()` creates triangulated surfaces from 3D point clouds using either
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
#' @param lighting Lighting model to apply (accessed via the computed variable `light`):
#'   - `"lambert"`: Standard diffuse lighting (surfaces facing away from light are dark)
#'   - `"signed"`: Continuous lighting gradient including negative values
#'   - `"ambient"`: Uniform lighting with no directional component
#'   - `"quantize"`: Quantized lighting with discrete levels
#'   - `"normal_rgb"`: Map surface normals to RGB colors (automatic identity scaling)
#'   - `"normal_x"`, `"normal_y"`, `"normal_z"`: Individual normal components
#' @param n_levels Number of discrete levels for `lighting = "quantize"`. Default is 3.
#' @param clamp_negative Logical indicating whether to clamp negative lighting values
#'   to the lowest level when using `lighting = "quantize"`. Default is `TRUE`.
#' @param light_dir Numeric vector of length 3 specifying the light direction in
#'   3D space. For most lighting methods, this defines the illumination direction.
#'   For `lighting = "normal_rgb"`, this defines the orientation of the RGB
#'   coordinate system (surfaces facing this direction will be brightest/whitest).
#'   Default is `c(0, 0, 1)` (light from above).
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#'
#' @section Computed variables:
#' - `light`: Computed lighting value (numeric for most methods, hex color for `normal_rgb`)
#' - `normal_x`, `normal_y`, `normal_z`: Surface normal components
#' - `triangle_index`: Sequential triangle number (useful for debugging)
#' - `face_id`: Triangle group identifier
#'
#' @section Aesthetics:
#' `stat_surface()` requires the following aesthetics:
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
#'                method = "hull", lighting = "normal_rgb") +
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
#'                method = "hull", lighting = "signed") +
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
#'   stat_surface(aes(fill = after_stat(light)), lighting = "normal_rgb",
#'                method = "hull") +
#'   coord_3d()
#'
#' @seealso [coord_3d()] for 3D coordinate systems, [geom_polygon_3d] for the
#'   default geometry with depth sorting.
#'
#' @export
stat_surface <- function(mapping = NULL, data = NULL,
                         geom = GeomPolygon3D, # nonstandard syntax, but `"polygon_3d"` failed
                         position = "identity", ...,
                         method = "alpha", alpha = 1.0,
                         light_dir = c(0, 0, 1), lighting = "lambert",
                         n_levels = 3, clamp_negative = TRUE,
                         inherit.aes = TRUE) {

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
            stat = StatSurface, data = data, mapping = mapping, geom = geom,
            position = position, inherit.aes = inherit.aes,
            params = list(method = method, alpha = alpha, light_dir = light_dir,
                          lighting = lighting, n_levels = n_levels,
                          clamp_negative = clamp_negative,
                          ...)
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
                               ordered_groups <- names(sort(group_depths, decreasing = FALSE))

                               # Draw each triangle individually to preserve aesthetics
                               triangle_grobs <- list()
                               for(i in seq_along(ordered_groups)) {
                                     tri_data <- coords_split[[ordered_groups[i]]]

                                     # Draw this triangle as individual polygon
                                     triangle_grobs[[i]] <- grid::polygonGrob(
                                           x = tri_data$x,
                                           y = tri_data$y,
                                           gp = grid::gpar(
                                                 col = tri_data$colour[1],      # Take first value for this triangle
                                                 fill = tri_data$fill[1],       # Take first value for this triangle
                                                 lwd = tri_data$linewidth[1] * .pt,
                                                 lty = tri_data$linetype[1]
                                           ),
                                           name = paste0("triangle_", i)
                                     )
                               }

                               # Combine all triangle grobs
                               do.call(grid::grobTree, triangle_grobs)
                         },

                         draw_key = draw_key_polygon
)


#' 3D polygon geometry with depth sorting
#'
#' `geom_polygon_3d()` renders 3D polygons with proper depth sorting for realistic
#' 3D surface visualization. It's designed to work with triangulated surface data
#' from [stat_surface()], as well as other .
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
#' # Typically used via stat_surface()
#' ggplot(sphere_data, aes(x, y, z)) +
#'   stat_surface(method = "hull") +
#'   coord_3d()
#'
#' # Can be used directly with pre-triangulated data
#' ggplot(triangle_data, aes(x, y, z, group = triangle_id)) +
#'   geom_polygon_3d(fill = "lightblue") +
#'   coord_3d()
#'
#' @seealso [stat_surface()] for surface triangulation
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
#       stat_surface(aes(fill = after_stat(light), color = after_stat(light)),
#                    method = "hull", light_dir = c(1, 1, 0), lighting = "lambert") +
#       scale_fill_gradient(low = "gray10", high = "white") +
#       scale_color_gradient(low = "gray10", high = "white") +
#       coord_3d(pitch = 0, roll = 0, dist = 2)
#
# ggplot(df, aes(x, y, z = z)) +
#       stat_surface(aes(fill = after_stat(light), color = after_stat(light)),
#                    method = "hull", light_dir = c(1, 1, 1),lighting = "normal_rgb",
#                    linewidth = .2) +
#       coord_3d() +
#       theme_void()

