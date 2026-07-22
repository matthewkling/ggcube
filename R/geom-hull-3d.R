#' Expand tetrahedra into their constituent faces
#'
#' Each tetrahedron contributes four triangular faces, one per omitted vertex.
#' The omitted vertex is retained as an interior reference point: it lies on the
#' interior side of that face by construction.
#'
#' @param tetra Integer matrix with 4 columns of vertex indices
#' @return List with `faces` (n*4 x 3 matrix of sorted vertex indices) and
#'   `opposite` (length n*4 vector of the omitted vertex index)
#' @keywords internal
#' @noRd
expand_tetra_faces <- function(tetra) {
      m <- nrow(tetra)
      faces <- matrix(nrow = m * 4L, ncol = 3L)
      opposite <- integer(m * 4L)

      for (k in 1:4) {
            rows <- ((k - 1L) * m + 1L):(k * m)
            faces[rows, ] <- tetra[, setdiff(1:4, k), drop = FALSE]
            opposite[rows] <- tetra[, k]
      }

      # Sort each face's vertex indices so faces can be matched regardless of winding
      faces <- sort_triples(faces)

      list(faces = faces, opposite = opposite)
}

#' Row-wise sort of a three-column integer matrix
#'
#' Equivalent to `t(apply(m, 1, sort))` but vectorized: for exactly three
#' values the sorted triple can be expressed with pairwise min and max,
#' avoiding a per-row R-level call.
#'
#' @param m Integer or numeric matrix with 3 columns
#' @return Matrix of the same dimensions with each row sorted ascending
#' @keywords internal
#' @noRd
sort_triples <- function(m) {
      a <- m[, 1]; b <- m[, 2]; cc <- m[, 3]
      lo <- pmin.int(a, b)
      hi <- pmax.int(a, b)
      cbind(pmin.int(lo, cc),
            pmin.int(hi, pmax.int(lo, cc)),
            pmax.int(hi, cc))
}

#' Encode sorted vertex triples as unique scalar keys
#'
#' Matching on numeric keys is substantially faster than on pasted strings.
#' Indices are packed in base `n`, which is exact in double precision while
#' `n^3` stays below 2^53; beyond that a string key is used instead.
#'
#' @param triples Integer matrix with 3 columns of sorted vertex indices
#' @param n Number of vertices (the base for packing)
#' @return Numeric or character vector of keys
#' @keywords internal
#' @noRd
triple_keys <- function(triples, n) {
      if (n <= 200000) {
            nn <- as.numeric(n)
            (triples[, 1] - 1) * nn * nn + (triples[, 2] - 1) * nn + triples[, 3]
      } else {
            paste(triples[, 1], triples[, 2], triples[, 3], sep = "_")
      }
}

#' Find an interior reference point for each boundary triangle
#'
#' For a triangle on the boundary of an alpha complex, exactly one adjacent
#' tetrahedron lies inside the complex. That tetrahedron's fourth vertex is a
#' point known to be on the interior side of the face, which determines the
#' outward normal direction. This is a local test, so unlike a global centroid
#' it remains correct for concave shapes and shapes with holes.
#'
#' @param tri Integer matrix with 3 columns of vertex indices (boundary triangles)
#' @param tetra Integer matrix with 4 columns of vertex indices (interior tetrahedra)
#' @return Integer vector of vertex indices, NA where no interior tetrahedron was found
#' @keywords internal
#' @noRd
find_interior_reference <- function(tri, tetra) {
      if (is.null(tetra) || nrow(tetra) == 0) {
            return(rep(NA_integer_, nrow(tri)))
      }

      expanded <- expand_tetra_faces(tetra)

      n <- max(max(tetra), max(tri))
      face_keys <- triple_keys(expanded$faces, n)
      tri_keys <- triple_keys(sort_triples(tri), n)

      # Match faces against triangles rather than the reverse: the expanded face
      # table is far larger than the boundary triangle set, and match() builds its
      # hash table from the second argument, so this hashes the smaller vector.
      hit <- match(face_keys, tri_keys)
      matched <- !is.na(hit)

      reference <- rep(NA_integer_, nrow(tri))
      if (any(matched)) {
            slot <- hit[matched]
            vert <- expanded$opposite[matched]
            # Keep the first matching tetrahedron per triangle
            keep <- !duplicated(slot)
            reference[slot[keep]] <- vert[keep]
      }

      reference
}

#' Compute outward-facing normals and consistent winding for a triangulation
#'
#' Computes face normals by cross product, then flips each normal (and the
#' corresponding triangle winding) so that it points away from the interior.
#' The interior side is determined per-face from `reference_points` when
#' available, falling back to the global centroid otherwise.
#'
#' @param coords Numeric matrix of vertex coordinates (n x 3)
#' @param tri Integer matrix with 3 columns of vertex indices
#' @param reference_points Integer vector of per-face interior vertex indices,
#'   or NULL to use the centroid for every face
#' @return List with `tri` (winding-corrected) and `normals` (unit outward normals)
#' @keywords internal
#' @noRd
orient_triangles <- function(coords, tri, reference_points = NULL) {
      v1 <- coords[tri[, 1], , drop = FALSE]
      v2 <- coords[tri[, 2], , drop = FALSE]
      v3 <- coords[tri[, 3], , drop = FALSE]

      edge1 <- v2 - v1
      edge2 <- v3 - v1

      normals <- cbind(
            edge1[, 2] * edge2[, 3] - edge1[, 3] * edge2[, 2],
            edge1[, 3] * edge2[, 1] - edge1[, 1] * edge2[, 3],
            edge1[, 1] * edge2[, 2] - edge1[, 2] * edge2[, 1]
      )

      face_centers <- (v1 + v2 + v3) / 3

      # Vector from the interior reference point toward the face. A normal
      # pointing outward has a positive dot product with this vector.
      if (is.null(reference_points)) {
            outward <- sweep(face_centers, 2, colMeans(coords), "-")
      } else {
            interior <- coords[reference_points, , drop = FALSE]
            outward <- face_centers - interior

            # Faces with no interior tetrahedron fall back to the centroid
            missing <- is.na(reference_points)
            if (any(missing)) {
                  centroid_dir <- sweep(face_centers[missing, , drop = FALSE], 2,
                                        colMeans(coords), "-")
                  outward[missing, ] <- centroid_dir
            }
      }

      # Flip inward-pointing normals and their triangle winding
      inward <- rowSums(normals * outward) < 0
      if (any(inward)) {
            normals[inward, ] <- -normals[inward, ]
            tri[inward, c(2, 3)] <- tri[inward, c(3, 2)]
      }

      lengths <- sqrt(rowSums(normals^2))
      degenerate <- lengths == 0 | !is.finite(lengths)
      normals <- normals / lengths
      if (any(degenerate)) {
            normals[degenerate, ] <- matrix(c(0, 0, 1), nrow = sum(degenerate),
                                            ncol = 3, byrow = TRUE)
      }

      list(tri = tri, normals = normals)
}

StatHull3D <- ggproto("StatHull3D", Stat,
                      required_aes = c("x", "y", "z"),
                      compute_group = function(data, scales,
                                               method = "convex", radius = NULL,
                                               singular = FALSE,
                                               cull_backfaces = NULL,
                                               light = NULL) {
                            coords <- as.matrix(data[, c("x", "y", "z")])

                            reference_points <- NULL
                            singular_faces <- NULL

                            # Get triangle indices
                            tri <- switch(method,
                                          alpha = {
                                                if (!requireNamespace("alphashape3d", quietly = TRUE)) {
                                                      stop('The alphashape3d package is required for method = "alpha". ',
                                                           'Install it with install.packages("alphashape3d").',
                                                           call. = FALSE)
                                                }
                                                if(is.null(radius)){
                                                      radius <- signif(.5 * mean(apply(coords, 2, function(x) diff(range(na.omit(x))))), 3)
                                                      message("Alpha shape `radius` parameter is NULL; defaulting to ", radius, " based on data ranges.")
                                                }
                                                ashape <- alphashape3d::ashape3d(coords, alpha = radius)

                                                # Triangle membership codes live in the column following the
                                                # standard 8, one per alpha value. Codes: 0 = exterior,
                                                # 1 = interior, 2 = regular (boundary), 3 = singular.
                                                tri_all <- ashape$triang
                                                tri_code <- tri_all[, 9]

                                                keep_codes <- if (isTRUE(singular)) c(2, 3) else 2
                                                selected <- tri_all[tri_code %in% keep_codes, 1:3, drop = FALSE]

                                                if (nrow(selected) == 0) {
                                                      stop("No valid triangle set found - try increasing `radius`")
                                                }

                                                # Singular faces bound no volume, so no outward direction exists
                                                singular_faces <- tri_code[tri_code %in% keep_codes] == 3

                                                # Tetrahedron membership codes follow the 4 vertex columns
                                                # and rhoT, one per alpha value: 1 = in the alpha complex.
                                                tetra_all <- ashape$tetra
                                                interior_tetra <- tetra_all[tetra_all[, 6] == 1, 1:4, drop = FALSE]

                                                reference_points <- find_interior_reference(selected, interior_tetra)

                                                unresolved <- sum(is.na(reference_points) & !singular_faces)
                                                if (unresolved > 0) {
                                                      warning(unresolved, " of ", nrow(selected),
                                                              " alpha shape faces had no adjacent interior tetrahedron; ",
                                                              "falling back to centroid orientation for these faces.",
                                                              call. = FALSE)
                                                }

                                                selected
                                          },
                                          convex = {
                                                if (!requireNamespace("geometry", quietly = TRUE)) {
                                                      stop('The geometry package is required for method = "convex". ',
                                                           'Install it with install.packages("geometry").',
                                                           call. = FALSE)
                                                }
                                                geometry::convhulln(coords)
                                          },
                                          stop("Unknown method: use 'alpha' or 'convex'")
                            )

                            # Check that we got valid triangulation
                            if (is.null(tri) || nrow(tri) == 0) {
                                  stop("No triangles generated - try adjusting alpha parameter or check data")
                            }

                            # Standardize triangle winding order and compute outward normals.
                            # A convex hull is star-shaped about its centroid, so the centroid
                            # test is exact there; alpha shapes need the per-face interior
                            # reference derived from the alpha complex.
                            oriented <- orient_triangles(coords, tri, reference_points)
                            tri <- oriented$tri
                            normals <- oriented$normals

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

                            # Singular faces have no consistent outward orientation, so culling
                            # them by winding would discard them arbitrarily
                            if (!is.null(singular_faces) && any(singular_faces)) {
                                  data$cull_backfaces <- rep(!singular_faces & isTRUE(cull_backfaces), each = 3)
                            } else {
                                  data$cull_backfaces <- cull_backfaces
                            }

                            # add computed variables and lighting info
                            data <- data %>%
                                  average_aesthetics() %>%
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
#'   variables with [ggplot2::after_stat()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to `StatHull3D`.
#' @param geom The geometric object used to display the data. Defaults to `GeomPolygon3D.`
#'
#' @param method Triangulation method. Either:
#'   - `"convex"`: Convex hull triangulation (default). Requires the \pkg{geometry} package.
#'   - `"alpha"`: Alpha shape triangulation (can capture non-convex topologies)
#' @param radius The "alpha" parameter when alpha method is used.
#'   A face is included in the resulting alpha shape if it can be "exposed" by a sphere of this radius.
#'   If NULL (the default), a simple heuristic based on the data scale is used to calculate a radius value.
#'   Note that alpha shapes are quite sensitive to the coordinate scales of your data. See Details section.
#' @param singular Whether to include "singular" faces when `method = "alpha"`.
#'   These are faces that do not bound any enclosed volume, appearing as isolated
#'   sheets dangling from the surface, and are usually a sign that `radius` is
#'   slightly too small. Because they enclose nothing, they have no well-defined
#'   outward direction: their lighting is arbitrary and they are never culled.
#'   Defaults to `FALSE`.
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
#' Alpha shape method is highly sensitive to coordinate scales. `radius` is expressed
#' in data units, so a value that works for data scaled 0-1 will likely fail for data
#' scaled 0-1000.
#' Guidelines for choosing radius:
#' - Start at a small fraction of your data's overall extent and adjust based on results
#' - For data with mixed scales (e.g., x: 0-1, y: 0-1000), consider rescaling your data first
#' - Larger radius values → smoother, more connected surfaces
#' - Smaller radius values → more detailed surfaces, but may fragment
#' - If you get no triangles, try increasing radius by 10x
#' - If surface fills unwanted holes, try decreasing radius by 10x
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
#' @examplesIf requireNamespace("geometry", quietly = TRUE)
#' # A solid torus: a shape whose hole a convex hull cannot represent
#' set.seed(1)
#' n <- 2000
#' theta <- runif(n, 0, 2 * pi)  # position around the ring
#' phi <- runif(n, 0, 2 * pi)    # position around the tube
#' rho <- sqrt(runif(n))         # distance from the tube center
#' torus <- data.frame(
#'   x = (3 + rho * cos(phi)) * cos(theta),
#'   y = (3 + rho * cos(phi)) * sin(theta),
#'   z = rho * sin(phi)
#' )
#'
#' # Convex hull: the hole is bridged over
#' ggplot(torus, aes(x, y, z)) +
#'   geom_hull_3d(method = "convex", fill = "gray40") +
#'   coord_3d(scales = "fixed")
#'
#' # Alpha shape: the hole is preserved
#' \donttest{
#' ggplot(torus, aes(x, y, z)) +
#'   geom_hull_3d(method = "alpha", radius = 0.6, fill = "gray40") +
#'   coord_3d(scales = "fixed")
#' }
#'
#' # Use `cull_backfaces = FALSE` to render far side of hull
#' \donttest{
#' ggplot(torus, aes(x, y, z)) +
#'   geom_hull_3d( # default culling for comparison
#'     method = "alpha", radius = 0.6, light = NULL,
#'     fill = "steelblue", color = "darkred", linewidth = .5, alpha = .5) +
#'   geom_hull_3d( # culling disabled
#'     aes(x = x + 9), cull_backfaces = FALSE,
#'     method = "alpha", radius = 0.6, light = NULL,
#'     fill = "steelblue", color = "darkred", linewidth = .5, alpha = .5) +
#'   coord_3d(scales = "fixed")
#' }
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
#' @return A `Layer` object that can be added to a ggplot.
#' @export
geom_hull_3d <- function(mapping = NULL, data = NULL,
                         stat = StatHull3D,
                         position = "identity",
                         ...,
                         method = "convex", radius = NULL, singular = FALSE, light = NULL,
                         cull_backfaces = TRUE, sort_method = NULL, scale_depth = TRUE,
                         inherit.aes = TRUE, show.legend = TRUE) {

      layer(data = data, mapping = mapping, stat = get_proto(stat, "stat"), geom = GeomPolygon3D,
            position = position, inherit.aes = inherit.aes, show.legend = show.legend,
            params = list(method = method, radius = radius, singular = singular, light = light,
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
                         method = "convex", radius = NULL, singular = FALSE, light = NULL,
                         cull_backfaces = TRUE, sort_method = NULL, scale_depth = TRUE,
                         inherit.aes = TRUE, show.legend = TRUE) {

      layer(data = data, mapping = mapping, stat = StatHull3D, geom = get_proto(geom, "geom"),
            position = position, inherit.aes = inherit.aes, show.legend = show.legend,
            params = list(method = method, radius = radius, singular = singular, light = light,
                          cull_backfaces = cull_backfaces, sort_method = sort_method, scale_depth = scale_depth,
                          ...)
      )
}
