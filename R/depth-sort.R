# Depth sorting for 3D rendering
#
# Implements back-to-front ordering of primitives for painter's algorithm
# rendering. Supports mixed primitive types (polygon, point, segment, text)
# identified by the `.prim` column.
#
# Two sorting modes:
#   - "painter": Fast approximate sort by mean depth per group, with
#     hierarchical support for `__`-separated group names.
#   - "pairwise": Accurate O(k^2) sort that compares every pair of groups
#     using type-specific geometric tests to determine occlusion order.
#     Segments that interpenetrate polygons in 3D are automatically split
#     at the crossing point before sorting.
#
# Pairwise comparison coverage:
#   polygon-polygon: SAT overlap test, depth interpolation at intersection centroid
#   polygon-point:   point-in-polygon test, depth interpolation at point xy
#   polygon-segment: polyclip intersection, depth interpolation at clipped midpoint
#   segment-segment: line intersection, depth interpolation at crossing point
#   point-segment:   bbox proximity test, depth at closest point on segment
#   point-point:     no constraint (painter fallback, zero-area overlap)
#
# When primitives are coplanar (depth delta ~ 0), a tiebreaker renders
# smaller primitives on top: point > segment > polygon.

interpolate_z <- function(train, new, beta = NULL) {
      if (!is.null(beta)) {
            return(beta[1] + beta[2] * new[1] + beta[3] * new[2])
      }
      train <- as.matrix(train)
      if(nrow(unique(train)) >= 3) {
            x <- cbind(1, as.matrix(train[,1:2]))
            beta <- tryCatch(
                  solve(crossprod(x), crossprod(x, train[,3])),
                  error = function(e) NULL
            )
            if (!is.null(beta)) {
                  return(beta[1] + beta[2] * new[1] + beta[3] * new[2])
            }
      }
      # Fallback: inverse-distance-weighted interpolation
      # for degenerate polygons (e.g. collinear vertices from face projection,
      # zero-height pillar faces)
      weights <- 1 / sqrt((train[,1] - new[1])^2 + (train[,2] - new[2])^2)
      z <- weighted.mean(train[,3], weights)
      return(z)
}

# Test if graph is acyclic using topological sort
is_acyclic <- function(adj_logical) {
      n <- nrow(adj_logical)
      in_degree <- colSums(adj_logical, na.rm = TRUE)
      queue <- which(in_degree == 0)
      processed <- 0

      while (length(queue) > 0) {
            current <- queue[1]
            queue <- queue[-1]
            processed <- processed + 1

            # Remove edges from current node
            neighbors <- which(adj_logical[current, ])
            for (neighbor in neighbors) {
                  in_degree[neighbor] <- in_degree[neighbor] - 1
                  if (in_degree[neighbor] == 0) {
                        queue <- c(queue, neighbor)
                  }
            }
      }

      return(processed == n)
}

# Break cycles by removing weakest constraints
#
# Only constraints that lie on a cycle (i.e. join two members of the same
# strongly connected component) are candidates for removal; the weakest
# candidate is removed and components are recomputed until none remain.
# Constraints flagged in `locked` (exact geometric results) are removed only
# when a cycle consists entirely of locked constraints.
break_cycles <- function(delta_matrix, locked = NULL) {
      n <- nrow(delta_matrix)
      if (n < 2) return(delta_matrix)
      if (is.null(locked)) locked <- matrix(FALSE, n, n)

      max_iterations <- n^2  # Safety limit
      iteration <- 0

      while (iteration < max_iterations) {
            iteration <- iteration + 1

            adj_logical <- (delta_matrix > 0) & !is.na(delta_matrix)
            comp <- strongly_connected_components(adj_logical)
            if (!anyDuplicated(comp)) break

            # Constraints between members of the same component lie on a cycle
            on_cycle <- adj_logical & outer(comp, comp, "==")
            if (!any(on_cycle)) break

            strength <- abs(delta_matrix)
            strength[!on_cycle] <- Inf
            if (any(locked & on_cycle)) {
                  strength[locked] <- strength[locked] + max(abs(delta_matrix[on_cycle])) + 1
            }

            weakest <- which(strength == min(strength), arr.ind = TRUE)[1, ]
            delta_matrix[weakest[1], weakest[2]] <- NA
            delta_matrix[weakest[2], weakest[1]] <- NA
      }

      if (iteration >= max_iterations &&
          !is_acyclic((delta_matrix > 0) & !is.na(delta_matrix))) {
            warning("Cycle breaking exceeded maximum iterations")
      }

      return(delta_matrix)
}


# Strongly connected components of a directed graph (Kosaraju's algorithm,
# iterative). Returns an integer component id per node.
strongly_connected_components <- function(adj_logical) {
      n <- nrow(adj_logical)
      out_nbrs <- lapply(seq_len(n), function(i) which(adj_logical[i, ]))
      in_nbrs <- lapply(seq_len(n), function(i) which(adj_logical[, i]))

      # Pass 1: finishing order on the forward graph
      visited <- logical(n)
      ptr <- integer(n)
      finish <- integer(0)
      for (s in seq_len(n)) {
            if (visited[s]) next
            stack <- s
            visited[s] <- TRUE
            while (length(stack) > 0) {
                  u <- stack[length(stack)]
                  nb <- out_nbrs[[u]]
                  ptr[u] <- ptr[u] + 1L
                  if (ptr[u] <= length(nb)) {
                        v <- nb[ptr[u]]
                        if (!visited[v]) {
                              visited[v] <- TRUE
                              stack <- c(stack, v)
                        }
                  } else {
                        finish <- c(finish, u)
                        stack <- stack[-length(stack)]
                  }
            }
      }

      # Pass 2: components on the reverse graph, in reverse finishing order
      comp <- integer(n)
      id <- 0L
      for (s in rev(finish)) {
            if (comp[s] != 0L) next
            id <- id + 1L
            stack <- s
            comp[s] <- id
            while (length(stack) > 0) {
                  u <- stack[length(stack)]
                  stack <- stack[-length(stack)]
                  for (v in in_nbrs[[u]]) {
                        if (comp[v] == 0L) {
                              comp[v] <- id
                              stack <- c(stack, v)
                        }
                  }
            }
      }
      comp
}


topological_sort <- function(adj_matrix, depths = NULL) {
      n <- nrow(adj_matrix)
      in_degree <- colSums(adj_matrix)
      queue <- which(in_degree == 0)
      result <- integer(0)

      # Sort initial queue by depth (farthest first) if depths provided
      if (!is.null(depths) && length(queue) > 1) {
            queue <- queue[order(-depths[queue])]
      }

      while (length(queue) > 0) {
            # Process node with no dependencies
            current <- queue[1]
            queue <- queue[-1]
            result <- c(result, current)

            # Remove edges from current node
            neighbors <- which(adj_matrix[current, ])
            new_free <- integer(0)
            for (neighbor in neighbors) {
                  in_degree[neighbor] <- in_degree[neighbor] - 1
                  if (in_degree[neighbor] == 0) {
                        new_free <- c(new_free, neighbor)
                  }
            }

            # Insert newly freed nodes into queue, sorted by depth
            if (length(new_free) > 0) {
                  if (!is.null(depths) && length(new_free) > 1) {
                        new_free <- new_free[order(-depths[new_free])]
                  }
                  # Merge into queue maintaining depth order
                  if (length(queue) == 0 || is.null(depths)) {
                        queue <- c(queue, new_free)
                  } else {
                        combined <- c(queue, new_free)
                        queue <- combined[order(-depths[combined])]
                  }
            }
      }

      # If we couldn't sort all nodes, there's a cycle (shouldn't happen after break_cycles)
      if (length(result) < n) {
            warning("Topological sort failed - remaining cycles detected")
            remaining <- setdiff(1:n, result)
            result <- c(result, remaining)
      }

      return(result)
}

# calculate edge normals
calculate_normals <- function(polygon_vertices) {
      # Input: polygon_vertices is a matrix where each row is a vertex (x, y)
      # Example: matrix(c(0,0, 1,0, 1,1, 0,1), ncol=2, byrow=TRUE) for a square

      num_vertices <- nrow(polygon_vertices)
      edge_vectors <- polygon_vertices[c(2:num_vertices, 1), ] - polygon_vertices
      normals <- cbind(-edge_vectors[, 2], edge_vectors[, 1])
      norm_values <- sqrt(rowSums(normals^2))
      normalized_normals <- normals / norm_values

      return(normalized_normals)
}

# Project vertices onto an axis
project_onto_axis <- function(vertices, axis) {
      # Input: vertices is a matrix of polygon vertices (x, y)
      #        axis is a 1x2 vector representing the separating axis

      # Project each vertex onto the axis using the dot product
      # Matrix multiplication is efficient for this
      projections <- vertices %*% axis

      # Find the minimum and maximum projection values
      min_projection <- min(projections)
      max_projection <- max(projections)

      return(list(min = min_projection, max = max_projection))
}

# separating axis theorem test for overlap
sat_overlap <- function(poly1, poly2, tol = 1e-9) {

      for(v in 1:2){
            if(v == 1){
                  v1 <- poly1
                  v2 <- poly2
            }else{
                  v1 <- poly2
                  v2 <- poly1
            }

            axes <- na.omit(calculate_normals(v1))
            for (i in 1:nrow(axes)) {
                  proj1 <- project_onto_axis(v1, axes[i,])
                  proj2 <- project_onto_axis(v2, axes[i,])
                  if (proj1$max <= proj2$min + tol || proj2$max <= proj1$min + tol) {
                        return(FALSE) # found separating axis, polygons don't touch
                  }
            }
      }

      return(TRUE) # no separating axis found, polygons overlap
}

get_centroid <- function(p) {
      x <- p$x
      y <- p$y

      # Close the loop if it's not already closed
      n <- length(x)
      if (x[1] != x[n] || y[1] != y[n]) {
            x <- c(x, x[1])
            y <- c(y, y[1])
            n <- n + 1
      }

      # Pre-calculate the cross products
      i <- 1:(n - 1)
      cross_prod <- x[i] * y[i+1] - x[i+1] * y[i]

      # Calculate Area
      area <- sum(cross_prod) / 2

      # Calculate Centroid Coordinates
      cx <- sum((x[i] + x[i+1]) * cross_prod) / (6 * area)
      cy <- sum((y[i] + y[i+1]) * cross_prod) / (6 * area)

      return(c(x = cx, y = cy))
}

# Get centroid of polygon intersection
intersection_centroid <- function(poly1, poly2) {
      clipped <- polyclip::polyclip(list(as.data.frame(poly1)),
                                    list(as.data.frame(poly2)))
      if(length(clipped) == 0) return(NULL) # non-intersecting polygons

      centroids <- do.call("rbind", lapply(clipped, get_centroid))

      # if multiple intersection polys, average their centroids
      return(colMeans(centroids))
}


# Point-in-polygon test using ray casting
point_in_polygon <- function(px, py, poly_x, poly_y) {
      n <- length(poly_x)
      inside <- FALSE
      j <- n
      for (i in seq_len(n)) {
            if (((poly_y[i] > py) != (poly_y[j] > py)) &&
                (px < (poly_x[j] - poly_x[i]) * (py - poly_y[i]) /
                 (poly_y[j] - poly_y[i]) + poly_x[i])) {
                  inside <- !inside
            }
            j <- i
      }
      inside
}


# Pre-compute plane coefficients for polygon depth interpolation ---------------

#' Compute least-squares plane betas for all polygon groups
#'
#' Returns a list parallel to grp_data where polygon entries contain a
#' length-3 numeric vector (intercept, x_coef, y_coef) and non-polygon
#' entries are NULL.
#'
#' @param grp_data List of per-group matrices (x, y, z, bbox...).
#' @param prim_lookup Named character vector of primitive types per group.
#' @return List of beta vectors (or NULL for non-polygons / degenerate cases).
#' @keywords internal
#' @noRd
compute_poly_betas <- function(grp_data, prim_lookup) {
      betas <- vector("list", length(grp_data))
      for (i in seq_along(grp_data)) {
            if (prim_lookup[i] != "polygon") next
            m <- grp_data[[i]]
            if (nrow(m) < 3) next
            xmat <- cbind(1, m[, 1], m[, 2])
            betas[[i]] <- tryCatch(
                  solve(crossprod(xmat), crossprod(xmat, m[, 3])),
                  error = function(e) NULL
            )
      }
      betas
}


# Pairwise comparison modules -------------------------------------------------
# Each returns a depth delta (positive = i behind j, i.e. i is drawn first)
# or NA (no constraint)

# Compare two polygon groups
pw_compare_poly_poly <- function(di, dj, bbi, bbj, beta_i = NULL, beta_j = NULL) {
      # Bounding box overlap check
      if(bbi[1] >= bbj[2]) return(NA)
      if(bbi[2] <= bbj[1]) return(NA)
      if(bbi[3] >= bbj[4]) return(NA)
      if(bbi[4] <= bbj[3]) return(NA)
      if(!sat_overlap(di[,1:2], dj[,1:2])) return(NA)

      # xy overlap but no z overlap: standard depth precedence
      if(bbi[5] >= bbj[6] | bbj[5] >= bbi[6]){
            return(mean(bbi[5:6]) - mean(bbj[5:6]))
      }

      # Overlapping in all dimensions: sample depth at intersection centroid
      int <- intersection_centroid(di[, 1:2], dj[, 1:2])
      if(is.null(int)) return(NA)
      zi <- interpolate_z(di[, 1:3], int, beta = beta_i)
      zj <- interpolate_z(dj[, 1:3], int, beta = beta_j)
      zi - zj
}

# Compare a polygon group with a point group
pw_compare_poly_point <- function(poly_data, point_data, poly_beta = NULL) {
      px <- point_data[1, 1]
      py <- point_data[1, 2]
      pz <- point_data[1, 3]

      # Check if point falls within polygon's bounding box
      poly_bb <- poly_data[1, 4:9]
      if (px < poly_bb[1] || px > poly_bb[2] ||
          py < poly_bb[3] || py > poly_bb[4]) {
            return(NA)
      }

      # Point-in-polygon test
      if (!point_in_polygon(px, py, poly_data[, 1], poly_data[, 2])) {
            return(NA)
      }

      # Point is inside polygon's xy projection: interpolate polygon depth at point location
      poly_z <- interpolate_z(poly_data[, 1:3], c(px, py), beta = poly_beta)
      poly_z - pz
}

# Compare a polygon group with a segment group
# Uses polyclip to find the segment's intersection with the polygon,
# then samples depth at the midpoint of the clipped portion.
pw_compare_poly_segment <- function(poly_data, seg_data, poly_beta = NULL) {
      seg_x <- seg_data[, 1]
      seg_y <- seg_data[, 2]

      poly_bb <- poly_data[1, 4:9]

      # Quick bounding box check
      seg_xmin <- min(seg_x); seg_xmax <- max(seg_x)
      seg_ymin <- min(seg_y); seg_ymax <- max(seg_y)

      if (seg_xmin >= poly_bb[2] || seg_xmax <= poly_bb[1] ||
          seg_ymin >= poly_bb[4] || seg_ymax <= poly_bb[3]) {
            return(NA)
      }

      # Clip segment against polygon to find overlap region
      seg_line <- list(list(x = seg_x, y = seg_y))
      poly_clip <- list(list(x = poly_data[, 1], y = poly_data[, 2]))
      clipped <- tryCatch(
            polyclip::polyclip(seg_line, poly_clip, op = "intersection", closed = FALSE),
            error = function(e) list()
      )

      if (length(clipped) == 0) return(NA)

      # Compute midpoint of the clipped portion(s)
      all_x <- unlist(lapply(clipped, `[[`, "x"))
      all_y <- unlist(lapply(clipped, `[[`, "y"))
      if (length(all_x) == 0) return(NA)

      sample_xy <- c(mean(all_x), mean(all_y))

      # Interpolate depths at sample point
      poly_z <- interpolate_z(poly_data[, 1:3], sample_xy, beta = poly_beta)
      seg_z <- interpolate_z_on_segment(seg_data[, 1:3], sample_xy)

      poly_z - seg_z
}

# Compare two segment groups
# Finds xy intersection point, interpolates depth of each segment there.
pw_compare_seg_seg <- function(seg_i, seg_j) {
      int <- segment_intersection(
            seg_i[1, 1], seg_i[1, 2], seg_i[2, 1], seg_i[2, 2],
            seg_j[1, 1], seg_j[1, 2], seg_j[2, 1], seg_j[2, 2]
      )
      if (is.null(int)) return(NA)

      zi <- interpolate_z_on_segment(seg_i[, 1:3], int)
      zj <- interpolate_z_on_segment(seg_j[, 1:3], int)
      zi - zj
}

# Compare a point group with a segment group
# Triggers when point is within segment's bounding box; compares depths
# at the point on the segment closest to the point center.
pw_compare_point_seg <- function(point_data, seg_data) {
      px <- point_data[1, 1]
      py <- point_data[1, 2]
      pz <- point_data[1, 3]

      # Bounding box check
      seg_xmin <- min(seg_data[, 1]); seg_xmax <- max(seg_data[, 1])
      seg_ymin <- min(seg_data[, 2]); seg_ymax <- max(seg_data[, 2])

      if (px < seg_xmin || px > seg_xmax ||
          py < seg_ymin || py > seg_ymax) {
            return(NA)
      }

      # Project point onto segment axis, clamp to endpoints
      closest <- closest_point_on_segment(
            px, py,
            seg_data[1, 1], seg_data[1, 2],
            seg_data[2, 1], seg_data[2, 2]
      )

      # Interpolate segment depth at the closest point
      seg_z <- interpolate_z_on_segment(seg_data[, 1:3], closest)
      seg_z - pz
}


# Geometry helpers for pairwise comparisons ------------------------------------

# Interpolate depth along a segment at a given xy point
# Uses linear interpolation based on projection parameter t
interpolate_z_on_segment <- function(seg_xyz, xy) {
      dx <- seg_xyz[2, 1] - seg_xyz[1, 1]
      dy <- seg_xyz[2, 2] - seg_xyz[1, 2]
      len_sq <- dx^2 + dy^2

      if (len_sq < 1e-20) {
            # Degenerate segment (zero length): return mean depth
            return(mean(seg_xyz[, 3]))
      }

      t <- ((xy[1] - seg_xyz[1, 1]) * dx + (xy[2] - seg_xyz[1, 2]) * dy) / len_sq
      t <- max(0, min(1, t))
      seg_xyz[1, 3] + t * (seg_xyz[2, 3] - seg_xyz[1, 3])
}

# Find intersection point of two line segments in 2D
# Returns c(x, y) or NULL if segments don't intersect
segment_intersection <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
      dx1 <- x2 - x1; dy1 <- y2 - y1
      dx2 <- x4 - x3; dy2 <- y4 - y3

      denom <- dx1 * dy2 - dy1 * dx2
      if (abs(denom) < 1e-12) return(NULL)  # Parallel or collinear

      t <- ((x3 - x1) * dy2 - (y3 - y1) * dx2) / denom
      u <- ((x3 - x1) * dy1 - (y3 - y1) * dx1) / denom

      # Check that intersection is within both segments
      if (t < 0 || t > 1 || u < 0 || u > 1) return(NULL)

      c(x1 + t * dx1, y1 + t * dy1)
}

# Find closest point on a line segment to a given point
# Returns c(x, y) of the closest point, clamped to segment endpoints
closest_point_on_segment <- function(px, py, x1, y1, x2, y2) {
      dx <- x2 - x1; dy <- y2 - y1
      len_sq <- dx^2 + dy^2

      if (len_sq < 1e-20) return(c(x1, y1))  # Degenerate segment

      t <- ((px - x1) * dx + (py - y1) * dy) / len_sq
      t <- max(0, min(1, t))
      c(x1 + t * dx, y1 + t * dy)
}


# Primitive type priority for coplanar tiebreaking (higher = renders on top)
.prim_priority <- c(polygon = 1, segment = 2, point = 3, text = 3)

# Coplanar tiebreaker tolerance
.coplanar_tol <- 1e-8

# Dispatch pairwise comparison based on primitive types.
# Returns depth delta (positive = i behind j) or NA (no constraint).
# When primitives are coplanar (delta ~ 0), smaller primitives render on top.
pw_compare_pair <- function(di, dj, type_i, type_j, beta_i = NULL, beta_j = NULL) {

      dlt <- pw_compare_pair_raw(di, dj, type_i, type_j, beta_i, beta_j)

      # Coplanar tiebreaker: smaller primitives on top
      if (!is.na(dlt) && abs(dlt) < .coplanar_tol) {
            pi <- .prim_priority[type_i] %||% 1
            pj <- .prim_priority[type_j] %||% 1
            if (pi != pj) return(pj - pi)  # higher priority j => positive => i behind j
      }

      dlt
}

# Raw pairwise comparison without tiebreaker
pw_compare_pair_raw <- function(di, dj, type_i, type_j, beta_i = NULL, beta_j = NULL) {

      # Polygon vs polygon
      if (type_i == "polygon" && type_j == "polygon") {
            bbi <- di[1, 4:9]
            bbj <- dj[1, 4:9]
            return(pw_compare_poly_poly(di, dj, bbi, bbj, beta_i = beta_i, beta_j = beta_j))
      }

      # Polygon vs point/text
      if (type_i == "polygon" && type_j %in% c("point", "text")) {
            return(pw_compare_poly_point(di, dj, poly_beta = beta_i))
      }
      if (type_j == "polygon" && type_i %in% c("point", "text")) {
            dlt <- pw_compare_poly_point(dj, di, poly_beta = beta_j)
            if (!is.na(dlt)) return(-dlt) else return(NA)
      }

      # Polygon vs segment
      if (type_i == "polygon" && type_j == "segment") {
            return(pw_compare_poly_segment(di, dj, poly_beta = beta_i))
      }
      if (type_j == "polygon" && type_i == "segment") {
            dlt <- pw_compare_poly_segment(dj, di, poly_beta = beta_j)
            if (!is.na(dlt)) return(-dlt) else return(NA)
      }

      # Segment vs segment
      if (type_i == "segment" && type_j == "segment") {
            return(pw_compare_seg_seg(di, dj))
      }

      # Point/text vs segment
      if (type_i %in% c("point", "text") && type_j == "segment") {
            return(pw_compare_point_seg(di, dj))
      }
      if (type_j %in% c("point", "text") && type_i == "segment") {
            dlt <- pw_compare_point_seg(dj, di)
            if (!is.na(dlt)) return(-dlt) else return(NA)
      }

      # Point vs point: no geometric constraint
      NA
}


# Pairwise render order with mixed-geometry support ----------------------------
# Uses base R for performance in the hot path.

pw_render_order <- function(data,
                            depth_var = "depth",
                            proj = NULL,
                            plot_bounds = NULL
){
      # Split polygons that pass through one another, then segments that pass
      # through polygons, so that every remaining pair has a consistent
      # occlusion order
      data <- split_intersecting_polygons(data, proj, plot_bounds)
      data <- split_interpenetrating_segments(data, proj, plot_bounds)

      has_prim <- ".prim" %in% names(data)

      grp <- data$group
      x <- data$x
      y <- data$y
      z <- data[[depth_var]]

      ugrp <- unique(grp)
      grp_idx <- split(seq_along(grp), grp)
      grp_idx <- grp_idx[ugrp]

      # Primitive type per group. Groups without one (e.g. in point layers,
      # whose types are assigned only when rendering) are typed by their
      # vertex count: 1 = point, 2 = segment, 3+ = polygon.
      prim_lookup <- if (has_prim) {
            vapply(grp_idx, function(idx) data$.prim[idx[1]], character(1))
      } else {
            setNames(rep(NA_character_, length(ugrp)), ugrp)
      }
      untyped <- is.na(prim_lookup)
      if (any(untyped)) {
            n_vert <- lengths(grp_idx)[untyped]
            prim_lookup[untyped] <- ifelse(n_vert == 1, "point",
                                           ifelse(n_vert == 2, "segment", "polygon"))
      }

      grp_data <- lapply(grp_idx, function(idx) {
            gx <- x[idx]; gy <- y[idx]; gz <- z[idx]
            bb <- c(min(gx), max(gx), min(gy), max(gy), min(gz), max(gz))
            m <- cbind(x = gx, y = gy, z = gz)
            cbind(m, matrix(bb, nrow = nrow(m), ncol = 6, byrow = TRUE))
      })

      poly_betas <- compute_poly_betas(grp_data, prim_lookup)

      k <- length(grp_data)
      delta <- matrix(NA_real_, k, k)
      locked <- matrix(FALSE, k, k)
      groups <- names(grp_data)

      # Pieces cut from the same polygon are coplanar and only share edges,
      # so they impose no ordering constraint on one another
      cut_parent <- ifelse(grepl("__cut[0-9]+$", groups),
                           sub("__cut[0-9]+$", "", groups), NA_character_)

      # Exact 3D geometry for each group, used to settle the sign of any
      # constraint involving a polygon (see plane_side_order())
      eye <- eye_in_cube_space(proj)
      geom_3d <- group_geometry_3d(data, groups, prim_lookup)

      for (i in 1:k) {
            for (j in 1:k) {
                  if (i >= j) next
                  if (!is.na(cut_parent[i]) && identical(cut_parent[i], cut_parent[j])) next

                  di <- grp_data[[i]]
                  dj <- grp_data[[j]]

                  ti <- prim_lookup[i]
                  tj <- prim_lookup[j]

                  dlt <- pw_compare_pair(di, dj, ti, tj,
                                         beta_i = poly_betas[[i]],
                                         beta_j = poly_betas[[j]])

                  if (is.na(dlt)) next

                  if (!is.null(eye) && !is.null(geom_3d[[i]]) && !is.null(geom_3d[[j]])) {
                        exact <- plane_side_order(geom_3d[[i]], geom_3d[[j]], eye)
                        if (is.na(exact)) {
                              exact <- coplanar_order(geom_3d[[i]], geom_3d[[j]], ti, tj)
                        }
                        if (is.na(exact)) {
                              exact <- ray_sample_order(geom_3d[[i]], geom_3d[[j]], di, dj,
                                                        ti, tj, proj, plot_bounds)
                        }
                        if (!is.na(exact)) {
                              dlt <- exact * max(abs(dlt), .coplanar_tol)
                              locked[i, j] <- locked[j, i] <- TRUE
                        }
                  }

                  delta[i, j] <- dlt
                  delta[j, i] <- -dlt
            }
      }

      delta_pruned <- break_cycles(delta, locked)
      adj_logical <- (delta_pruned > 0) & !is.na(delta_pruned)

      grp_depths <- vapply(grp_data, function(m) mean(m[, 3]), numeric(1))

      render_order <- groups[topological_sort(adj_logical, grp_depths)]
      list(data = data, render_order = match(data$group, render_order))
}


# Hierarchical depth sorting with mixed-geometry support
#
# Sorts data back-to-front for painter's algorithm rendering, supporting
# mixed primitive types (polygon, point, segment, text) via the `.prim`
# column. Preserves vertex order within multi-vertex groups (polygons).
#
# Sort hierarchy for groups with `__` separators:
#   level1 (e.g. "col1") sorted by max depth across all child groups
#   level2 (e.g. "col1__zmax") sorted by representative depth
#
# Pairwise sorting handles all primitive types via type-specific comparison
# modules. When primitives are coplanar, smaller types render on top.
#
# `proj` and `plot_bounds` are needed only to project the new vertices created
# when pairwise sorting splits interpenetrating polygons; without them, that
# splitting is skipped.
sort_by_depth <- function(data, proj = NULL, plot_bounds = NULL) {

      if(".sort_method" %in% names(data)){
            method <- data$.sort_method[1]
            if(method == "auto"){
                  method <- if(nrow(data) <= 500) "pairwise" else "painter"
            }
      }else{
            method <- "painter"
      }

      # Elements flagged `.on_top` (e.g. text annotations with `on_top = TRUE`)
      # aren't sorted against the rest: they're drawn after everything else,
      # in depth order among themselves
      top <- if (".on_top" %in% names(data)) data$.on_top %in% TRUE else FALSE
      if (any(top) && !all(top)) {
            front <- data[top, , drop = FALSE]
            front$.on_top <- FALSE
            front$.sort_method <- "painter"
            front <- sort_by_depth(front, proj, plot_bounds)
            front$.on_top <- TRUE
            rest <- sort_by_depth(data[!top, , drop = FALSE], proj, plot_bounds)
            return(dplyr::bind_rows(rest, front[intersect(names(front), names(rest))]))
      }

      # Vertex order within each group (preserves polygon winding)
      data <- data %>%
            group_by(group) %>%
            mutate(.vertex_order = row_number()) %>%
            ungroup()

      # Compute per-group representative depth
      data <- compute_prim_depth(data)

      if (method == "pairwise") {
            result <- pw_render_order(data, "depth", proj = proj,
                                      plot_bounds = plot_bounds)
            data <- result$data
            # Recompute .vertex_order and .prim_depth for any split rows
            data <- data %>%
                  group_by(group) %>%
                  mutate(.vertex_order = row_number(),
                         .prim_depth = mean(depth, na.rm = TRUE)) %>%
                  ungroup()
            data$.render_order <- result$render_order
            data <- data %>%
                  arrange(.render_order, .vertex_order) %>%
                  select(-.render_order)

      } else if(any(grepl("__", data$group))) {

            has_mixed_prims <- ".prim" %in% names(data) &&
                  length(unique(data$.prim[!is.na(data$.prim)])) > 1

            if (has_mixed_prims) {
                  # Mixed primitive types: flat sort by per-group depth to allow
                  # interleaving (hierarchical sort would block by level1)
                  data <- data %>%
                        arrange(desc(.prim_depth), group, .vertex_order)
            } else {
                  # Single primitive type: hierarchical sort preserves object grouping
                  data <- data %>%
                        tidyr::separate(group, c("level1", "level2"), sep = "__",
                                        remove = FALSE, extra = "merge") %>%
                        group_by(level1) %>% mutate(depth1 = max(depth)) %>%
                        ungroup() %>%
                        arrange(desc(depth1), desc(.prim_depth), group, .vertex_order) %>%
                        select(-level1, -level2, -depth1)
            }

      } else {
            # Simple painter sorting
            data <- data %>%
                  arrange(desc(.prim_depth), group, .vertex_order)
      }

      data <- data %>% select(-.vertex_order, -.prim_depth,
                              -any_of(c(".x3d", ".y3d", ".z3d")))
      return(data)
}


#' Compute per-group representative depth for sorting
#'
#' Uses mean depth of all vertices in each group, which works correctly
#' for all primitive types (polygon, point, segment, text).
#'
#' @param data Data frame with `group`, `depth`, and optionally `.prim`.
#' @return Data frame with `.prim_depth` column added.
#' @keywords internal
#' @noRd
compute_prim_depth <- function(data) {
      depth_col <- if("depth_3d" %in% names(data)) "depth_3d" else "depth"

      data <- data %>%
            group_by(group) %>%
            mutate(.prim_depth = mean(.data[[depth_col]], na.rm = TRUE)) %>%
            ungroup()

      return(data)
}


# Segment-polygon interpenetration splitting -----------------------------------

# Distance (cube units) below which a segment end counts as touching a polygon's
# plane rather than passing through it, e.g. residual lines that start on the
# surface they belong to
.split_tol <- 0.01

#' Split segments where they pass through polygons
#'
#' For each segment, finds every polygon it passes through in 3D and splits
#' the segment at those crossing points, so that each resulting piece lies on
#' one side of each polygon's plane and can be ordered against it exactly.
#'
#' Crossings are computed in standardized cube space (`.x3d`, `.y3d`, `.z3d`):
#' the segment's endpoints must lie on opposite sides of the polygon's plane,
#' each by more than `.split_tol` plus the polygon's warp (for non-planar
#' polygons, the distance of its vertices from their best-fit plane). The
#' crossing point must also fall inside the polygon's screen footprint, which
#' is tested by projecting it; for a point on the polygon's plane this is
#' equivalent to lying inside the polygon in 3D. New endpoints are projected
#' with the same functions used by `coord_3d()`.
#'
#' @param data Data frame with projected coordinates, `.x3d`/`.y3d`/`.z3d`,
#'   `group`, and `.prim`.
#' @param proj Projection parameters, as in `panel_params$proj`.
#' @param plot_bounds Plot bounds used for NPC scaling.
#' @return Data frame with interpenetrating segments replaced by sub-segments
#'   (groups suffixed `__split1`, `__split2`, ...), flagged with `.seg_first`
#'   and `.seg_last` to mark the pieces holding the original segment's start
#'   and end. Other rows are unchanged.
#' @keywords internal
#' @noRd
split_interpenetrating_segments <- function(data, proj, plot_bounds) {

      if (is.null(proj) || is.null(plot_bounds)) return(data)
      if (!all(c(".x3d", ".y3d", ".z3d", ".prim") %in% names(data))) return(data)

      grp <- as.character(data$group)
      ugrp <- unique(grp)
      grp_idx <- split(seq_along(grp), factor(grp, levels = ugrp))
      prim <- vapply(grp_idx, function(idx) data$.prim[idx[1]], character(1))

      seg_ids <- which(prim == "segment" & lengths(grp_idx) == 2)
      poly_ids <- which(prim == "polygon" & lengths(grp_idx) >= 3)
      if (length(seg_ids) == 0 || length(poly_ids) == 0) return(data)

      xyz <- cbind(data$.x3d, data$.y3d, data$.z3d)

      # Polygon planes, 3D bounding boxes, and screen footprints
      polys <- lapply(poly_ids, function(g) {
            idx <- grp_idx[[g]]
            p <- xyz[idx, , drop = FALSE]
            pl <- polygon_plane(p)
            if (is.null(pl)) return(NULL)
            list(plane = pl, lo = apply(p, 2, min), hi = apply(p, 2, max),
                 sx = data$x[idx], sy = data$y[idx])
      })
      polys <- polys[!vapply(polys, is.null, logical(1))]
      if (length(polys) == 0) return(data)
      lo <- do.call(rbind, lapply(polys, `[[`, "lo"))
      hi <- do.call(rbind, lapply(polys, `[[`, "hi"))

      # Candidate crossings: (segment, polygon, t, 3D crossing point)
      cand <- list()
      for (s in seg_ids) {
            idx <- seg_endpoint_rows(data, grp_idx[[s]])
            a <- xyz[idx[1], ]
            b <- xyz[idx[2], ]
            slo <- pmin(a, b); shi <- pmax(a, b)
            near <- which(colSums(t(lo) <= shi + .split_tol) == 3 &
                                colSums(t(hi) >= slo - .split_tol) == 3)
            for (pi in near) {
                  pl <- polys[[pi]]$plane
                  eps <- .split_tol + pl$warp
                  da <- sum(a * pl$normal) - pl$offset
                  db <- sum(b * pl$normal) - pl$offset
                  if (da * db >= 0 || min(abs(da), abs(db)) < eps) next
                  t <- da / (da - db)
                  cand <- c(cand, list(list(s = s, pi = pi, t = t, p = a + t * (b - a))))
            }
      }
      if (length(cand) == 0) return(data)

      # Keep crossings that fall inside the polygon's footprint
      pts <- do.call(rbind, lapply(cand, `[[`, "p"))
      scr <- scale_to_npc_coordinates(
            transform_3d_standard(data.frame(x = pts[, 1], y = pts[, 2], z = pts[, 3]), proj),
            plot_bounds = plot_bounds)
      inside <- vapply(seq_along(cand), function(k) {
            pg <- polys[[cand[[k]]$pi]]
            point_in_polygon(scr$x[k], scr$y[k], pg$sx, pg$sy)
      }, logical(1))
      cand <- cand[inside]
      if (length(cand) == 0) return(data)

      split_ts <- split(vapply(cand, `[[`, numeric(1), "t"),
                        vapply(cand, `[[`, numeric(1), "s"))

      # Build sub-segments
      data$group <- as.character(data$group)
      has_seg_id <- "segment_id" %in% names(data)
      rows_to_drop <- integer(0)
      new_rows <- list()

      for (s_chr in names(split_ts)) {
            s <- as.integer(s_chr)
            ts <- sort(unique(split_ts[[s_chr]]))
            ts <- ts[ts > 1e-3 & ts < 1 - 1e-3]
            if (length(ts) == 0) next

            idx <- seg_endpoint_rows(data, grp_idx[[s]])
            start_row <- data[idx[1], , drop = FALSE]
            end_row <- data[idx[2], , drop = FALSE]
            a <- xyz[idx[1], ]
            b <- xyz[idx[2], ]
            breaks <- c(0, ts, 1)

            for (k in seq_len(length(breaks) - 1)) {
                  piece <- rbind(start_row, end_row)
                  t01 <- breaks[k + 0:1]
                  p3 <- rbind(a + t01[1] * (b - a), a + t01[2] * (b - a))
                  piece$.x3d <- p3[, 1]
                  piece$.y3d <- p3[, 2]
                  piece$.z3d <- p3[, 3]
                  piece$.new_vertex <- c(t01[1] > 0, t01[2] < 1)
                  piece$group <- paste0(start_row$group, "__split", k)
                  # Which pieces hold the original segment's ends, so that
                  # arrowheads are drawn only there (see render_segments())
                  piece$.seg_first <- k == 1
                  piece$.seg_last <- k == length(breaks) - 1
                  if (has_seg_id && !is.na(start_row$segment_id)) {
                        piece$segment_id <- start_row$segment_id + (k - 1) * 0.01
                  }
                  new_rows <- c(new_rows, list(piece))
            }
            rows_to_drop <- c(rows_to_drop, idx)
      }

      if (length(new_rows) == 0) return(data)

      new_rows <- do.call(rbind, new_rows)
      new_rows <- project_new_vertices(new_rows, proj, plot_bounds)
      new_rows$.new_vertex <- NULL

      kept <- data[-rows_to_drop, , drop = FALSE]
      kept$.seg_first <- rep(NA, nrow(kept))
      kept$.seg_last <- rep(NA, nrow(kept))
      rbind(kept, new_rows)
}


#' Start and end row indices of a two-row segment group
#'
#' @param data Data frame, optionally with a `point_type` column.
#' @param idx Row indices of the segment group.
#' @return Integer vector `c(start, end)`.
#' @keywords internal
#' @noRd
seg_endpoint_rows <- function(data, idx) {
      if ("point_type" %in% names(data)) {
            pt <- data$point_type[idx]
            if (all(c("start", "end") %in% pt)) {
                  return(c(idx[pt == "start"][1], idx[pt == "end"][1]))
            }
      }
      idx[1:2]
}


# Polygon-polygon interpenetration splitting -----------------------------------

#' Split polygons that interpenetrate other polygons in 3D
#'
#' Painter's algorithm cannot order two polygons that pass through each other.
#' This pre-pass finds such pairs and cuts one polygon of each pair along the
#' other's plane, so that each resulting piece lies entirely on one side of
#' that plane and has a consistent occlusion order with it. Cutting one member
#' of a pair is sufficient. When both are eligible, the smaller polygon is
#' cut, since each cut runs along the cutter's full plane: cutting a large
#' polygon by many small ones would fragment it across its whole extent.
#'
#' All geometry is computed in standardized cube space (`.x3d`, `.y3d`, `.z3d`),
#' where planes are exactly planar under both orthographic and perspective
#' projection. New vertices are then projected with the same functions used by
#' `coord_3d()`.
#'
#' A pair is split only when each polygon has vertices strictly on both sides
#' of the other's plane and their crossing intervals along the planes' line of
#' intersection overlap. Polygons that merely touch, such as adjacent tiles of
#' a surface sharing an edge or vertex, are left intact.
#'
#' Only planar polygons can act as cutters. Non-planar polygons (e.g. warped
#' quads on a curved surface) can still be cut by a planar polygon, with their
#' best-fit plane and warp used in the straddle test. Polygons with holes
#' (multiple subgroups) are never cut. Pairs of two non-planar polygons are
#' left unsplit.
#'
#' Each vertex of a split piece carries a `.cut_edge` flag indicating whether
#' the edge from that vertex to the next lies along a cut, so that outlines can
#' be drawn along original edges only.
#'
#' @param data Data frame with projected coordinates, `.x3d`/`.y3d`/`.z3d`
#'   cube-space coordinates, `group`, and optionally `.prim`.
#' @param proj Projection parameters, as in `panel_params$proj`.
#' @param plot_bounds Plot bounds used for NPC scaling.
#' @param tol Absolute distance tolerance in standardized cube units.
#' @return Data frame with interpenetrating polygons replaced by pieces. When
#'   any split occurs, `group` becomes character and `.cut_edge` is added.
#' @keywords internal
#' @noRd
split_intersecting_polygons <- function(data, proj, plot_bounds, tol = 1e-6) {

      if (is.null(proj) || is.null(plot_bounds)) return(data)
      if (!all(c(".x3d", ".y3d", ".z3d") %in% names(data))) return(data)
      if (nrow(data) == 0) return(data)

      grp <- data$group
      ugrp <- unique(grp)
      grp_idx <- split(seq_along(grp), factor(grp, levels = ugrp))

      if (".prim" %in% names(data)) {
            is_poly <- vapply(grp_idx, function(idx) identical(data$.prim[idx[1]], "polygon"),
                              logical(1))
      } else {
            is_poly <- rep(TRUE, length(grp_idx))
      }
      is_poly <- is_poly & lengths(grp_idx) >= 3
      if (sum(is_poly) < 2) return(data)

      sg_col <- intersect(c(".subgroup", "subgroup"), names(data))
      xyz <- cbind(data$.x3d, data$.y3d, data$.z3d)

      # Per-polygon plane geometry
      poly_ids <- which(is_poly)
      info <- lapply(poly_ids, function(g) {
            idx <- grp_idx[[g]]
            p <- xyz[idx, , drop = FALSE]
            pl <- polygon_plane(p)
            if (is.null(pl)) return(NULL)
            has_holes <- length(sg_col) > 0 &&
                  length(unique(data[[sg_col[1]]][idx])) > 1
            c(pl, list(cuttable = !has_holes,
                       lo = apply(p, 2, min), hi = apply(p, 2, max)))
      })
      keep <- !vapply(info, is.null, logical(1))
      poly_ids <- poly_ids[keep]
      info <- info[keep]
      n_poly <- length(info)
      if (n_poly < 2) return(data)

      # Candidate pairs: overlapping 3D bounding boxes
      lo <- do.call(rbind, lapply(info, `[[`, "lo"))
      hi <- do.call(rbind, lapply(info, `[[`, "hi"))
      ov <- matrix(TRUE, n_poly, n_poly)
      for (a in 1:3) {
            ov <- ov & outer(lo[, a], hi[, a], "<") & outer(hi[, a], lo[, a], ">")
      }
      ov[lower.tri(ov, diag = TRUE)] <- FALSE
      pairs <- which(ov, arr.ind = TRUE)
      if (nrow(pairs) == 0) return(data)

      # Collect cutting planes for each polygon to be cut
      cuts <- vector("list", n_poly)

      for (r in seq_len(nrow(pairs))) {
            i <- pairs[r, 1]
            j <- pairs[r, 2]
            pi_ <- info[[i]]
            pj <- info[[j]]

            ok_cut_i <- pi_$cuttable && pj$planar
            ok_cut_j <- pj$cuttable && pi_$planar
            if (!ok_cut_i && !ok_cut_j) next

            pts_i <- xyz[grp_idx[[poly_ids[i]]], , drop = FALSE]
            pts_j <- xyz[grp_idx[[poly_ids[j]]], , drop = FALSE]

            if (!polygons_interpenetrate(pts_i, pi_, pts_j, pj, tol)) next

            cut_i <- if (ok_cut_i && ok_cut_j) pi_$area <= pj$area else ok_cut_i
            if (cut_i) {
                  cuts[[i]] <- c(cuts[[i]], list(pj[c("normal", "offset")]))
            } else {
                  cuts[[j]] <- c(cuts[[j]], list(pi_[c("normal", "offset")]))
            }
      }

      cut_which <- which(!vapply(cuts, is.null, logical(1)))
      if (length(cut_which) == 0) return(data)

      # Apply cuts
      data$group <- as.character(data$group)
      data$.cut_edge <- FALSE
      rows_to_drop <- integer(0)
      new_rows <- list()

      for (w in cut_which) {
            idx <- grp_idx[[poly_ids[w]]]
            pieces <- list(list(p = xyz[idx, , drop = FALSE],
                                src = idx,
                                is_new = rep(FALSE, length(idx)),
                                flag = rep(FALSE, length(idx))))
            for (cp in cuts[[w]]) {
                  pieces <- unlist(lapply(pieces, split_ring_by_plane,
                                          normal = cp$normal, offset = cp$offset,
                                          tol = tol),
                                   recursive = FALSE)
            }
            if (length(pieces) < 2) next

            orig_group <- data$group[idx[1]]
            for (k in seq_along(pieces)) {
                  pc <- pieces[[k]]
                  rows <- data[pc$src, , drop = FALSE]
                  rows$.x3d <- pc$p[, 1]
                  rows$.y3d <- pc$p[, 2]
                  rows$.z3d <- pc$p[, 3]
                  rows$group <- paste0(orig_group, "__cut", k)
                  rows$.cut_edge <- pc$flag
                  rows$.new_vertex <- pc$is_new
                  new_rows <- c(new_rows, list(rows))
            }
            rows_to_drop <- c(rows_to_drop, idx)
      }

      if (length(new_rows) == 0) {
            data$.cut_edge <- NULL
            return(data)
      }

      new_rows <- do.call(rbind, new_rows)
      new_rows <- project_new_vertices(new_rows, proj, plot_bounds)
      new_rows$.new_vertex <- NULL

      rbind(data[-rows_to_drop, , drop = FALSE], new_rows)
}


#' Fit a plane to a polygon's 3D vertices
#'
#' Uses Newell's method for the normal, which is robust for non-convex and
#' slightly non-planar polygons. Planarity is judged by the maximum vertex
#' distance from the plane through the vertex centroid.
#'
#' @param p Numeric matrix of vertices (n x 3).
#' @param tol Absolute planarity tolerance.
#' @return List with `normal` (unit vector), `offset` (plane is
#'   `normal . v = offset`), `warp` (max residual), `planar`, and `area`;
#'   or NULL for degenerate polygons.
#' @keywords internal
#' @noRd
polygon_plane <- function(p, tol = 1e-6) {
      nxt <- c(seq_len(nrow(p))[-1], 1L)
      x <- p[, 1]; y <- p[, 2]; z <- p[, 3]
      nrm <- c(sum((y - y[nxt]) * (z + z[nxt])),
               sum((z - z[nxt]) * (x + x[nxt])),
               sum((x - x[nxt]) * (y + y[nxt])))
      len <- sqrt(sum(nrm^2))
      if (len < 1e-12) return(NULL)
      nrm <- nrm / len
      offset <- sum(colMeans(p) * nrm)
      warp <- max(abs(p %*% nrm - offset))
      list(normal = nrm, offset = offset, warp = warp,
           planar = warp <= tol, area = len / 2)
}


#' Test whether two polygons pass through each other in 3D
#'
#' Requires each polygon to have vertices strictly on both sides of the
#' other's plane (beyond a tolerance widened by that plane's warp), and the
#' two polygons' crossing intervals along the planes' line of intersection to
#' overlap.
#'
#' @param pts_i,pts_j Vertex matrices (n x 3).
#' @param pl_i,pl_j Plane info lists from `polygon_plane()`.
#' @param tol Absolute distance tolerance.
#' @return Logical.
#' @keywords internal
#' @noRd
polygons_interpenetrate <- function(pts_i, pl_i, pts_j, pl_j, tol) {

      eps_i <- tol + pl_i$warp
      eps_j <- tol + pl_j$warp

      d_i <- as.vector(pts_i %*% pl_j$normal - pl_j$offset)
      if (!(max(d_i) > eps_j && min(d_i) < -eps_j)) return(FALSE)

      d_j <- as.vector(pts_j %*% pl_i$normal - pl_i$offset)
      if (!(max(d_j) > eps_i && min(d_j) < -eps_i)) return(FALSE)

      u <- cross3(pl_i$normal, pl_j$normal)
      u_len <- sqrt(sum(u^2))
      if (u_len < 1e-9) return(FALSE)
      u <- u / u_len

      int_i <- crossing_interval(pts_i, d_i, eps_j, u)
      int_j <- crossing_interval(pts_j, d_j, eps_i, u)

      min(int_i[2], int_j[2]) - max(int_i[1], int_j[1]) > tol
}


#' Range of a polygon's plane crossings, projected onto a direction
#'
#' @param pts Vertex matrix (n x 3).
#' @param d Signed distances of vertices to the crossing plane.
#' @param eps Tolerance below which a vertex is treated as on the plane.
#' @param u Unit direction of the planes' intersection line.
#' @return Length-2 numeric vector (min, max).
#' @keywords internal
#' @noRd
crossing_interval <- function(pts, d, eps, u) {
      d[abs(d) <= eps] <- 0
      nxt <- c(seq_along(d)[-1], 1L)
      cross <- (d > 0 & d[nxt] < 0) | (d < 0 & d[nxt] > 0)
      on <- d == 0
      proj <- c(
            if (any(cross)) {
                  t <- d[cross] / (d[cross] - d[nxt][cross])
                  pts_x <- pts[cross, , drop = FALSE] +
                        t * (pts[nxt[cross], , drop = FALSE] - pts[cross, , drop = FALSE])
                  as.vector(pts_x %*% u)
            },
            if (any(on)) as.vector(pts[on, , drop = FALSE] %*% u)
      )
      range(proj)
}


cross3 <- function(a, b) {
      c(a[2] * b[3] - a[3] * b[2],
        a[3] * b[1] - a[1] * b[3],
        a[1] * b[2] - a[2] * b[1])
}


#' Split a polygon ring by a plane
#'
#' Two-sided Sutherland-Hodgman clip of a single ring. Each vertex carries a
#' source row index (for copying aesthetics), a new-vertex flag, and a
#' cut-edge flag for the edge leaving that vertex. Edges running along the
#' cutting plane are flagged as cut edges; sub-edges of an original edge
#' inherit that edge's flag.
#'
#' @param ring List with `p` (n x 3), `src`, `is_new`, `flag`.
#' @param normal,offset Cutting plane (`normal . v = offset`).
#' @param tol Distance tolerance for snapping vertices onto the plane.
#' @return List of one ring (no split) or two rings.
#' @keywords internal
#' @noRd
split_ring_by_plane <- function(ring, normal, offset, tol) {
      d <- as.vector(ring$p %*% normal - offset)
      d[abs(d) <= tol] <- 0
      if (!(any(d > 0) && any(d < 0))) return(list(ring))

      sides <- lapply(c(1, -1), function(s) clip_ring_side(ring, s * d))
      sides <- sides[vapply(sides, function(r) !is.null(r) && nrow(r$p) >= 3,
                            logical(1))]
      if (length(sides) < 2) return(list(ring))
      sides
}


#' Keep the non-negative side of a ring
#'
#' @param ring Ring list (see `split_ring_by_plane()`).
#' @param dd Signed distances, oriented so the kept side is non-negative.
#' @return Clipped ring list, or NULL if empty.
#' @keywords internal
#' @noRd
clip_ring_side <- function(ring, dd) {
      n <- length(dd)
      p <- ring$p
      out_p <- list(); out_src <- integer(0)
      out_new <- logical(0); out_flag <- logical(0)

      for (i in seq_len(n)) {
            j <- if (i == n) 1L else i + 1L

            if (dd[i] >= 0) {
                  out_p <- c(out_p, list(p[i, ]))
                  out_src <- c(out_src, ring$src[i])
                  out_new <- c(out_new, ring$is_new[i])
                  out_flag <- c(out_flag, if (dd[i] == 0 && dd[j] < 0) TRUE else ring$flag[i])
            }

            if ((dd[i] > 0 && dd[j] < 0) || (dd[i] < 0 && dd[j] > 0)) {
                  t <- dd[i] / (dd[i] - dd[j])
                  out_p <- c(out_p, list(p[i, ] + t * (p[j, ] - p[i, ])))
                  out_src <- c(out_src, ring$src[i])
                  out_new <- c(out_new, TRUE)
                  out_flag <- c(out_flag, if (dd[i] > 0) TRUE else ring$flag[i])
            }
      }

      if (length(out_p) == 0) return(NULL)
      list(p = do.call(rbind, out_p), src = out_src,
           is_new = out_new, flag = out_flag)
}


#' Project newly created vertices into screen space
#'
#' Applies the same projection and NPC scaling as `coord_3d()` to rows flagged
#' by `.new_vertex`, using their `.x3d`/`.y3d`/`.z3d` coordinates.
#'
#' @param rows Data frame of piece rows.
#' @param proj Projection parameters.
#' @param plot_bounds Plot bounds for NPC scaling.
#' @return `rows` with projected columns updated for new vertices.
#' @keywords internal
#' @noRd
project_new_vertices <- function(rows, proj, plot_bounds) {
      nv <- which(rows$.new_vertex)
      if (length(nv) == 0) return(rows)

      pts <- data.frame(x = rows$.x3d[nv], y = rows$.y3d[nv], z = rows$.z3d[nv])
      tr <- transform_3d_standard(pts, proj)
      tr <- scale_to_npc_coordinates(tr, plot_bounds = plot_bounds)

      rows$x[nv] <- tr$x
      rows$y[nv] <- tr$y
      rows$z[nv] <- tr$z
      rows$depth[nv] <- tr$depth
      rows$depth_scale[nv] <- tr$depth_scale
      if ("depth_3d" %in% names(rows)) rows$depth_3d[nv] <- tr$depth
      rows
}


# Exact plane-side ordering ----------------------------------------------------

#' Viewer location in standardized cube space
#'
#' Inverts the z-flip and rotation of `transform_3d_standard()`. Under
#' perspective the viewer is a point at distance `dist` from the cube center;
#' under orthographic projection it is a direction toward the viewer.
#'
#' @param proj Projection parameters.
#' @return List with `persp` and `v` (eye point or unit direction), or NULL.
#' @keywords internal
#' @noRd
eye_in_cube_space <- function(proj) {
      if (is.null(proj)) return(NULL)
      rot <- rotate_3d(diag(3), proj$pitch, proj$roll, proj$yaw)
      r <- if (isTRUE(proj$persp)) c(0, 0, -proj$dist) else c(0, 0, -1)
      f <- as.vector(r %*% t(rot))
      list(persp = isTRUE(proj$persp), v = c(f[1], f[2], -f[3]))
}


#' Per-group 3D vertices, plane fits, and touch tolerances
#'
#' @param data Data frame with `.x3d`, `.y3d`, `.z3d`, and `group`.
#' @param groups Character vector of group names, in comparison order.
#' @param prim_lookup Primitive type per group, aligned with `groups`.
#' @return List aligned with `groups`, each a list with `pts` (vertex
#'   matrix), `plane` (from `polygon_plane()` for polygons with a valid fit,
#'   otherwise NULL), and `touch` (see `plane_side_order()`). All NULL if
#'   cube-space coordinates are unavailable.
#' @keywords internal
#' @noRd
group_geometry_3d <- function(data, groups, prim_lookup) {
      out <- vector("list", length(groups))
      if (!all(c(".x3d", ".y3d", ".z3d") %in% names(data))) return(out)

      idx <- split(seq_len(nrow(data)), factor(as.character(data$group), levels = groups))
      for (g in seq_along(groups)) {
            rows <- idx[[g]]
            if (length(rows) == 0) next
            pts <- cbind(data$.x3d[rows], data$.y3d[rows], data$.z3d[rows])
            if (anyNA(pts)) next
            is_poly <- prim_lookup[g] == "polygon"
            out[[g]] <- list(
                  pts = pts,
                  plane = if (is_poly && length(rows) >= 3) polygon_plane(pts),
                  touch = if (prim_lookup[g] == "segment") .split_tol else 0
            )
      }
      out
}


#' Exact occlusion order of two primitives from plane-side tests
#'
#' A polygon lies within a band of half-width `warp` around its best-fit plane
#' (zero for planar polygons). If another primitive lies entirely beyond that
#' band on one side, every line of sight meets the two in a fixed order:
#' whichever is on the viewer's side is in front wherever they overlap on
#' screen. This holds under both perspective and orthographic projection.
#'
#' The other primitive may reach back into the band by its `touch` tolerance,
#' so that a segment starting on a polygon it belongs to (e.g. a residual line
#' on its surface) is ordered as lying on its far side.
#'
#' @param gi,gj Group geometry from `group_geometry_3d()`.
#' @param eye Viewer from `eye_in_cube_space()`.
#' @param tol Absolute distance tolerance in cube units.
#' @return 1 if i is behind j, -1 if i is in front of j, or NA if neither
#'   test is decisive (e.g. coplanar, straddling, or no polygon involved).
#' @keywords internal
#' @noRd
plane_side_order <- function(gi, gj, eye, tol = 1e-6) {
      if (!is.null(gi$plane)) {
            s <- side_of_plane(gj$pts, gi$plane, tol, gj$touch %||% 0)
            e <- eye_side_of_plane(gi$plane, eye)
            if (!is.na(s) && e != 0) return(if (s == e) 1 else -1)
      }
      if (!is.null(gj$plane)) {
            s <- side_of_plane(gi$pts, gj$plane, tol, gi$touch %||% 0)
            e <- eye_side_of_plane(gj$plane, eye)
            if (!is.na(s) && e != 0) return(if (s == e) -1 else 1)
      }
      NA
}


#' Order of a polygon and a smaller primitive lying in its plane
#'
#' A point, segment, or text label lying exactly in a planar polygon's plane
#' (such as a line drawn along a plane) has no geometric depth order with it,
#' and comparing sampled depths gives an arbitrary result. Such primitives are
#' drawn on top of the polygon.
#'
#' @param gi,gj Group geometry from `group_geometry_3d()`.
#' @param ti,tj Primitive types.
#' @param tol Absolute distance tolerance in cube units.
#' @return 1 if i is the polygon (drawn behind j), -1 if j is, or NA if the
#'   pair isn't a planar polygon and a primitive lying in its plane.
#' @keywords internal
#' @noRd
coplanar_order <- function(gi, gj, ti, tj, tol = 1e-6) {
      in_plane <- function(poly, other, other_type) {
            !is.null(poly$plane) && isTRUE(poly$plane$planar) &&
                  other_type %in% c("point", "segment", "text") &&
                  all(abs(as.vector(other$pts %*% poly$plane$normal - poly$plane$offset)) <= tol)
      }
      if (ti == "polygon" && in_plane(gi, gj, tj)) return(1)
      if (tj == "polygon" && in_plane(gj, gi, ti)) return(-1)
      NA
}


#' Which side of a polygon's plane band a set of points lies on
#'
#' @param pts Vertex matrix (n x 3).
#' @param plane Plane info from `polygon_plane()`; the polygon lies within
#'   `plane$warp` of the plane.
#' @param tol Distance tolerance.
#' @param touch Additional distance the points may reach back into the band.
#' @return 1 or -1 if the points lie beyond the band on that side, with at
#'   least one strictly beyond it; otherwise NA.
#' @keywords internal
#' @noRd
side_of_plane <- function(pts, plane, tol, touch = 0) {
      d <- as.vector(pts %*% plane$normal - plane$offset)
      w <- plane$warp
      lo <- min(d); hi <- max(d)
      if (hi >= -lo) {
            if (lo >= w - tol - touch && hi > w + tol) return(1)
      } else {
            if (hi <= -w + tol + touch && lo < -w - tol) return(-1)
      }
      NA
}


#' Which side of a polygon's plane band the viewer is on
#'
#' @return 1, -1, or 0 when the viewer is within the band (perspective) or
#'   the plane is viewed edge-on (orthographic).
#' @keywords internal
#' @noRd
eye_side_of_plane <- function(plane, eye) {
      if (eye$persp) {
            s <- sum(plane$normal * eye$v) - plane$offset
            if (abs(s) <= plane$warp + 1e-9) 0 else sign(s)
      } else {
            s <- sum(plane$normal * eye$v)
            if (abs(s) < 1e-9) 0 else sign(s)
      }
}


# Exact ray sampling ------------------------------------------------------------

#' Exact occlusion order of two disjoint primitives from one line of sight
#'
#' For a planar polygon and a polygon or segment that straddle each other's
#' planes without intersecting (intersecting pairs are split beforehand), the
#' depth order is the same along every line of sight through their screen
#' overlap. This casts one exact ray through a point in that overlap and
#' compares where it meets each primitive in 3D.
#'
#' Pairs involving non-planar polygons, points, or text return NA.
#'
#' @param gi,gj Group geometry from `group_geometry_3d()`.
#' @param di,dj Screen-space group matrices, as compared in `pw_render_order()`.
#' @param ti,tj Primitive types.
#' @param proj Projection parameters.
#' @param plot_bounds Plot bounds used for NPC scaling.
#' @return 1 if i is behind j, -1 if i is in front of j, or NA.
#' @keywords internal
#' @noRd
ray_sample_order <- function(gi, gj, di, dj, ti, tj, proj, plot_bounds) {
      if (is.null(proj) || is.null(plot_bounds)) return(NA)
      exact_ok <- function(g, type) {
            (type == "polygon" && !is.null(g$plane) && g$plane$planar) ||
                  (type == "segment" && nrow(g$pts) == 2)
      }
      if (!(exact_ok(gi, ti) && exact_ok(gj, tj))) return(NA)
      if (ti != "polygon" && tj != "polygon") return(NA)

      xy <- if (ti == "polygon" && tj == "polygon") {
            intersection_centroid(di[, 1:2], dj[, 1:2])
      } else if (ti == "polygon") {
            segment_overlap_midpoint(di, dj)
      } else {
            segment_overlap_midpoint(dj, di)
      }
      if (is.null(xy)) return(NA)

      ray <- screen_ray(xy[1], xy[2], proj, plot_bounds)
      li <- ray_depth(ray, gi, ti)
      lj <- ray_depth(ray, gj, tj)
      if (!is.finite(li) || !is.finite(lj)) return(NA)
      if (abs(li - lj) <= 1e-9 * max(1, abs(li), abs(lj))) return(NA)
      if (li > lj) 1 else -1
}


#' Midpoint of the portion of a segment overlapping a polygon on screen
#'
#' @param poly_data,seg_data Screen-space group matrices.
#' @return Numeric `c(x, y)`, or NULL if they do not overlap.
#' @keywords internal
#' @noRd
segment_overlap_midpoint <- function(poly_data, seg_data) {
      clipped <- tryCatch(
            polyclip::polyclip(list(list(x = seg_data[, 1], y = seg_data[, 2])),
                               list(list(x = poly_data[, 1], y = poly_data[, 2])),
                               op = "intersection", closed = FALSE),
            error = function(e) list()
      )
      if (length(clipped) == 0) return(NULL)
      c(mean(unlist(lapply(clipped, `[[`, "x"))),
        mean(unlist(lapply(clipped, `[[`, "y"))))
}


#' Line of sight through a screen point, in standardized cube space
#'
#' Inverts the NPC scaling, perspective division, rotation, and z-flip of
#' `coord_3d()`'s projection.
#'
#' @param sx,sy Screen point in NPC units.
#' @param proj Projection parameters.
#' @param plot_bounds Plot bounds used for NPC scaling.
#' @return List with origin `o` and direction `d`; points along the ray are
#'   `o + lambda * d`, with larger `lambda` farther from the viewer.
#' @keywords internal
#' @noRd
screen_ray <- function(sx, sy, proj, plot_bounds) {
      xt <- plot_bounds[1] + sx * (plot_bounds[2] - plot_bounds[1])
      yt <- plot_bounds[3] + sy * (plot_bounds[4] - plot_bounds[3])
      rot <- rotate_3d(diag(3), proj$pitch, proj$roll, proj$yaw)
      to_cube <- function(v) {
            f <- as.vector(v %*% t(rot))
            c(f[1], f[2], -f[3])
      }
      p0 <- to_cube(c(xt, yt, 0))
      if (isTRUE(proj$persp)) {
            eye <- to_cube(c(0, 0, -proj$dist))
            list(o = eye, d = p0 - eye)
      } else {
            list(o = p0, d = to_cube(c(0, 0, 1)))
      }
}


#' Ray parameter where a ray meets a planar polygon or a segment
#'
#' @param ray Ray from `screen_ray()`.
#' @param g Group geometry from `group_geometry_3d()`.
#' @param type Primitive type, `"polygon"` or `"segment"`.
#' @return Numeric ray parameter (NA if parallel).
#' @keywords internal
#' @noRd
ray_depth <- function(ray, g, type) {
      if (type == "polygon") {
            den <- sum(g$plane$normal * ray$d)
            if (abs(den) < 1e-12) return(NA_real_)
            return((g$plane$offset - sum(g$plane$normal * ray$o)) / den)
      }
      # Segment: closest approach between the ray and the segment's line
      a <- g$pts[1, ]
      e <- g$pts[2, ] - a
      w <- a - ray$o
      dd <- sum(ray$d * ray$d); de <- sum(ray$d * e); ee <- sum(e * e)
      den <- dd * ee - de^2
      if (abs(den) < 1e-14) return(NA_real_)
      (sum(ray$d * w) * ee - de * sum(e * w)) / den
}
