StatHull3D <- ggproto("StatHull3D", Stat,
                      required_aes = c("x", "y", "z"),

                      compute_group = function(data, scales, method = "convex", radius = NA,
                                               light = lighting()) {
                            coords <- as.matrix(data[, c("x", "y", "z")])

                            # Get triangle indices
                            tri <- switch(method,
                                          alpha = {
                                                if(is.null(radius)){
                                                      radius <- signif(.5 * mean(apply(coords, 2, function(x) diff(range(na.omit(x))))), 3)
                                                      message("Alpha shape `radius` parameter is NULL; defaulting to ", radius, " based on data ranges.")
                                                }
                                                ashape <- alphashape3d::ashape3d(coords, alpha = radius^2)
                                                tri <- ashape$triang
                                                alpha_triangles <- tri[tri[,ncol(tri)] %in% c(2, 3), 1:3]

                                                if (nrow(alpha_triangles) > 0) {
                                                      alpha_triangles
                                                } else {
                                                      stop("No valid triangle set found")
                                                }
                                          },
                                          convex = {
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
                                          stop("Unknown method: use 'alpha' or 'convex'")
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

                            # Ensure normals point outward from centroid for both methods
                            # (imperfect for alpha, may want to fix)
                            data_center <- colMeans(coords)
                            for(i in 1:nrow(tri)) {
                                  triangle_center <- (A[i,] + B[i,] + C[i,]) / 3
                                  outward_direction <- triangle_center - data_center

                                  # If normal points inward (negative dot product), flip it
                                  if (sum(normals[i,] * outward_direction) < 0) {
                                        normals[i,] <- -normals[i,]
                                  }
                            }

                            # Calculate face centers for positional lighting
                            face_centers <- matrix(0, nrow = nrow(tri), ncol = 3)
                            for(i in 1:nrow(tri)) {
                                  face_centers[i,] <- (A[i,] + B[i,] + C[i,]) / 3
                            }

                            light_val <- compute_lighting(normals, light, face_centers)

                            # Flatten triangle data with all computed variables
                            verts <- coords[as.vector(t(tri)), ]

                            # Create unique face IDs that won't collide across groups
                            # Use a random suffix to ensure uniqueness across compute_group calls
                            group_suffix <- sample(10000:99999, 1)
                            face_id <- rep(paste0("tri_", group_suffix, "_", 1:nrow(tri)), each = 3)
                            normal_x <- rep(normals[,1], each = 3)
                            normal_y <- rep(normals[,2], each = 3)
                            normal_z <- rep(normals[,3], each = 3)
                            light_val_expanded <- rep(light_val, each = 3)
                            triangle_index <- rep(1:nrow(tri), each = 3)

                            # Re-apply identity scaling for RGB colors after rep()
                            if (light$method == "normal_rgb") {
                                  light_val_expanded <- I(light_val_expanded)
                            }

                            result <- data.frame(
                                  x = verts[,1],
                                  y = verts[,2],
                                  z = verts[,3],
                                  group = paste0("hull__", face_id),
                                  triangle_index = triangle_index,
                                  normal_x = normal_x,
                                  normal_y = normal_y,
                                  normal_z = normal_z,
                                  light = light_val_expanded,

                                  # Add lighting parameters for shade processing
                                  shade_enabled = light$shade,
                                  shade_strength = light$shade_strength,
                                  shade_mode = light$shade_mode,
                                  lighting_method = light$method,
                                  stringsAsFactors = FALSE
                            )

                            # Preserve all non-coordinate columns for each vertex
                            # Get original indices for each vertex
                            vertex_indices <- as.vector(t(tri))

                            # Preserve additional columns from original data
                            result <- bind_cols(result, data[vertex_indices, setdiff(names(data), names(result)), drop = FALSE])

                            return(result)
                      }
)


#' Create 3D convex and alpha hulls with lighting
#'
#' `stat_hull_3d()` turns 3D point clouds into surface hulls consisting of triangular polygons,
#' using either convex hull or alpha shape algorithms. It computes surface normals and applies
#' various lighting models to create realistic 3D surface visualizations.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. The required
#'   aesthetics are `x`, `y`, and `z`. Additional aesthetics can use computed
#'   variables with [after_stat()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomPolygon3D] for proper 3D depth sorting.
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed on to `layer()`, such as `sort_method` and `scale_depth`
#'    arguments to `geom_polygon_3d()`.
#' @param method Triangulation method. Either:
#'   - `"convex"`: Convex hull triangulation (default)
#'   - `"alpha"`: Alpha shape triangulation (can capture non-convex topologies)
#' @param radius Square root of "alpha" parameter when alpha method is used.
#'   A face is included in the resulting alpha shape if it can be "exposed" by a sphere of this radius.
#'   If NULL (the default), a simple heuristic based on the data scale is used to calculate a radius value.
#'   Note that alpha shapes are quite sensitive to the coordinate scales of your data. See Details section.
#' @param light A lighting specification object created by \code{lighting()}
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#'
#' @section Grouping:
#' `stat_hull_3d()` respects ggplot2 grouping aesthetics. To create separate hulls for different
#' subsets of your data, use `aes(group = category_variable)` or similar grouping aesthetics.
#' Each group will get its own independent hull calculation.
#'
#' @section Alpha scale sensitivity:
#' **Alpha shape method is highly sensitive to coordinate scales.** The `alpha` parameter
#' that works for data scaled 0-1 will likely fail for data scaled 0-1000.
#'
#' **Guidelines for choosing radius:**
#' - Start with `alpha = 1.0` and adjust based on results
#' - For data with mixed scales (e.g., x: 0-1, y: 0-1000), consider rescaling your data first
#' - Larger alpha values → smoother, more connected surfaces
#' - Smaller alpha values → more detailed surfaces, but may fragment
#' - If you get no triangles, try increasing alpha by 10x
#' - If surface fills unwanted holes, try decreasing alpha by 10x
#'
#' **Example scale effects:**
#' ```r
#' # These require very different alpha values:
#' data_small <- data.frame(x = runif(100, 0, 1), y = runif(100, 0, 1), z = runif(100, 0, 1))
#' data_large <- data.frame(x = runif(100, 0, 100), y = runif(100, 0, 100), z = runif(100, 0, 100))
#'
#' stat_hull_3d(data = data_small, alpha = 0.5)    # Might work well
#' stat_hull_3d(data = data_large, alpha = 50)     # Might need much larger alpha
#' ```
#'
#' @section Computed variables:
#' - `light`: Computed lighting value (numeric for most methods, hex color for `normal_rgb`)
#' - `normal_x`, `normal_y`, `normal_z`: Surface normal components
#' - `triangle_index`: Sequential triangle number (useful for debugging)
#' - `face_id`: Triangle group identifier
#'
#' @section Aesthetics:
#' `stat_hull_3d()` requires the following aesthetics:
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
#' # Convex hull
#' ggplot(sphere_points, aes(x, y, z)) +
#'   stat_hull_3d(method = "convex", fill = "gray40",
#'                light = lighting(shade = "fill")) +
#'   coord_3d()
#'
#' # Alpha shape (scale-sensitive - radius ~1 works for unit sphere)
#' ggplot(sphere_points, aes(x, y, z)) +
#'   stat_hull_3d(method = "alpha", radius = 1, fill = "gray40",
#'                light = lighting(shade = "fill")) +
#'   coord_3d()
#'
#' # Grouped hulls - separate hull for each species
#' spheres <- rbind(dplyr::mutate(sphere_points, group = "a"),
#'                  dplyr::mutate(sphere_points, group = "b", x = x + 3))
#' ggplot(spheres, aes(x, y, z, group = group)) +
#'   stat_hull_3d(aes(fill = group),
#'         light = lighting(shade = "fill", shade_mode = "hsl")) +
#'   coord_3d(scales = "fixed")
#'
#' # For larger coordinate scales, increase radius proportionally:
#' sphere_large <- sphere_points * 100  # Scale up by 100x
#' ggplot(sphere_large, aes(x, y, z)) +
#'    stat_hull_3d(method = "alpha", radius = 100,
#'              fill = "darkgreen", light = lighting(shade = "fill")) +
#'    coord_3d()
#'
#' @seealso [coord_3d()] for 3D coordinate systems, [geom_polygon_3d] for the
#'   default geometry with depth sorting, [lighting()] for lighting specifications.
#'
#' @export
stat_hull_3d <- function(mapping = NULL, data = NULL,
                         geom = GeomPolygon3D,
                         position = "identity",
                         method = "convex", radius = NULL,
                         light = lighting(),
                         inherit.aes = TRUE,
                         ...) {

      layer(
            stat = StatHull3D, data = data, mapping = mapping, geom = geom,
            position = position, inherit.aes = inherit.aes,
            params = list(method = method, radius = radius, light = light, ...)
      )
}
