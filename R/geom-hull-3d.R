StatHull3D <- ggproto("StatHull3D", Stat,
                      required_aes = c("x", "y", "z"),
                      compute_group = function(data, scales,
                                               method = "convex", radius = NA,
                                               cull_backfaces = NULL,
                                               light = NULL) {
                            coords <- as.matrix(data[, c("x", "y", "z")])

                            # Compute hull center in original data space
                            hull_center <- colMeans(coords)

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

                            # Standardize triangle winding order and compute normals
                            normals <- matrix(nrow = nrow(tri), ncol = 3)
                            for (i in 1:nrow(tri)) {
                                  # Get triangle vertices
                                  v1 <- coords[tri[i, 1], ]
                                  v2 <- coords[tri[i, 2], ]
                                  v3 <- coords[tri[i, 3], ]

                                  # Compute face normal using cross product (v2-v1) × (v3-v1)
                                  edge1 <- v2 - v1
                                  edge2 <- v3 - v1
                                  normal <- c(
                                        edge1[2] * edge2[3] - edge1[3] * edge2[2],  # x component
                                        edge1[3] * edge2[1] - edge1[1] * edge2[3],  # y component
                                        edge1[1] * edge2[2] - edge1[2] * edge2[1]   # z component
                                  )

                                  # Compute face center and vector to hull center
                                  face_center <- (v1 + v2 + v3) / 3
                                  to_hull_center <- hull_center - face_center

                                  # Check if normal points inward (toward hull center)
                                  if (sum(normal * to_hull_center) > 0) {
                                        # Normal points inward, swap v2 and v3 to flip it outward
                                        tri[i, c(2, 3)] <- tri[i, c(3, 2)]
                                        # Flip normal as well since we flipped the winding
                                        normal <- -normal
                                  }

                                  # Normalize and store the final outward-facing normal
                                  normal_length <- sqrt(sum(normal^2))
                                  if (normal_length > 0) {
                                        normals[i, ] <- normal / normal_length
                                  } else {
                                        normals[i, ] <- c(0, 0, 1)  # fallback
                                  }
                            }

                            # Assemble triangle vertices with corrected winding order
                            data <- data[as.vector(t(tri)), ]

                            # Create unique face IDs that won't collide across groups,
                            # using a random suffix to ensure uniqueness across compute_group calls
                            group_suffix <- sample(10000:99999, 1)
                            data$group <- rep(paste0("sh3d__hull", group_suffix, "_tri", 1:nrow(tri)), each = 3)

                            # Add computed normal variables (replicated for each vertex in triangle)
                            data$normal_x <- rep(normals[, 1], each = 3)
                            data$normal_y <- rep(normals[, 2], each = 3)
                            data$normal_z <- rep(normals[, 3], each = 3)

                            # add computed variables and lighting info
                            data <- data %>%
                                  mutate(cull_backfaces = cull_backfaces) %>%
                                  attach_light(light)
                            return(data)
                      }
)


#' 3D convex and alpha hulls
#'
#' Turns 3D point clouds into surface hulls consisting of triangular polygons,
#' using either convex hull or alpha shape algorithms.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. The required
#'   aesthetics are `x`, `y`, and `z`. Additional aesthetics can use computed
#'   variables with [after_stat()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to `StatHull3D`.
#' @param geom The geometric object used to display the data. Defaults to `GeomPolygon3D.`
#'
#' @param method Triangulation method. Either:
#'   - `"convex"`: Convex hull triangulation (default)
#'   - `"alpha"`: Alpha shape triangulation (can capture non-convex topologies)
#' @param radius Square root of "alpha" parameter when alpha method is used.
#'   A face is included in the resulting alpha shape if it can be "exposed" by a sphere of this radius.
#'   If NULL (the default), a simple heuristic based on the data scale is used to calculate a radius value.
#'   Note that alpha shapes are quite sensitive to the coordinate scales of your data. See Details section.
#'
#' @inheritParams light_param
#' @inheritParams polygon_params
#' @inheritParams position_param
#'
#' @section Grouping:
#' Hulls respect ggplot2 grouping aesthetics. To create separate hulls for different
#' subsets of your data, use `aes(group = category_variable)` or similar grouping aesthetics.
#' Each group will get its own independent hull.
#'
#' @section Alpha scale sensitivity:
#' Alpha shape method is highly sensitive to coordinate scales. The `alpha` parameter
#' that works for data scaled 0-1 will likely fail for data scaled 0-1000.
#' Guidelines for choosing radius:
#' - Start with `alpha = 1.0` and adjust based on results
#' - For data with mixed scales (e.g., x: 0-1, y: 0-1000), consider rescaling your data first
#' - Larger alpha values → smoother, more connected surfaces
#' - Smaller alpha values → more detailed surfaces, but may fragment
#' - If you get no triangles, try increasing alpha by 10x
#' - If surface fills unwanted holes, try decreasing alpha by 10x
#'
#' @section Aesthetics:
#' `stat_hull_3d()` requires the following aesthetics:
#' - **x**: X coordinate
#' - **y**: Y coordinate
#' - **z**: Z coordinate
#'
#' @section Computed variables:
#' - `normal_x`, `normal_y`, `normal_z`: Surface normal components
#'
#' @examples
#' # Convex hull
#' ggplot(sphere_points, aes(x, y, z)) +
#'   geom_hull_3d(method = "convex", fill = "gray40") +
#'   coord_3d()
#'
#' # Alpha shape (for sphere data, gives similar result to convex)
#' ggplot(sphere_points, aes(x, y, z)) +
#'   geom_hull_3d(method = "alpha", radius = 2, fill = "gray40") +
#'   coord_3d()
#'
#' # Use `cull_backfaces = FALSE` to render far side of hull
#' ggplot(sphere_points, aes(x, y, z)) +
#'   geom_hull_3d( # default culling for comparison
#'     method = "convex", light = NULL,
#'     fill = "steelblue", color = "darkred", linewidth = .5, alpha = .5) +
#'   geom_hull_3d( # culling disabled
#'     aes(x = x + 2.5), cull_backfaces = FALSE,
#'     method = "convex", light = NULL,
#'     fill = "steelblue", color = "darkred", linewidth = .5, alpha = .5) +
#'   coord_3d(scales = "fixed")
#'
#' # Use grouping to build separate hulls for data subsets
#' ggplot(iris, aes(Petal.Length, Sepal.Length, Sepal.Width,
#'                  color = Species, fill = Species)) +
#'       geom_hull_3d() +
#'       coord_3d(scales = "fixed")
#'
#' @seealso [coord_3d()] for 3D coordinate systems, [geom_polygon_3d] for the
#'   default geometry with depth sorting, [light()] for lighting specifications.
#' @rdname geom_hull_3d
#' @export
geom_hull_3d <- function(mapping = NULL, data = NULL,
                         stat = StatHull3D,
                         position = "identity",
                         ...,
                         method = "convex", radius = NULL, light = ggcube::light(),
                         cull_backfaces = TRUE, sort_method = NULL, scale_depth = TRUE,
                         inherit.aes = TRUE, show.legend = TRUE) {

      layer(data = data, mapping = mapping, stat = stat, geom = GeomPolygon3D,
            position = position, inherit.aes = inherit.aes, show.legend = show.legend,
            params = list(method = method, radius = radius, light = light,
                          cull_backfaces = cull_backfaces, sort_method = sort_method, scale_depth = scale_depth,
                          ...)
      )
}

#' @rdname geom_hull_3d
#' @export
stat_hull_3d <- function(mapping = NULL, data = NULL,
                         geom = GeomPolygon3D,
                         position = "identity",
                         ...,
                         method = "convex", radius = NULL, light = ggcube::light(),
                         cull_backfaces = TRUE, sort_method = NULL, scale_depth = TRUE,
                         inherit.aes = TRUE, show.legend = TRUE) {

      layer(data = data, mapping = mapping, stat = StatHull3D, geom = geom,
            position = position, inherit.aes = inherit.aes, show.legend = show.legend,
            params = list(method = method, radius = radius, light = light,
                          cull_backfaces = cull_backfaces, sort_method = sort_method, scale_depth = scale_depth,
                          ...)
      )
}
