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

# Quick check for definite cycles
definitely_has_cycles <- function(adj_logical) {
      in_degrees <- colSums(adj_logical, na.rm = TRUE)
      return(all(in_degrees > 0))  # Every node has incoming edges
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

# Break cycles by removing weakest constraints. Constraints flagged in
# `locked` (exact geometric results) are only removed once no unlocked
# constraint remains.
break_cycles <- function(delta_matrix, locked = NULL) {
      max_iterations <- nrow(delta_matrix)^2  # Safety limit
      iteration <- 0

      while (iteration < max_iterations) {
            iteration <- iteration + 1

            # Convert to logical adjacency matrix
            adj_logical <- (delta_matrix > 0) & !is.na(delta_matrix)

            # Quick cycle check first
            if (!definitely_has_cycles(adj_logical)) {
                  # Still might have cycles, need full check
                  if (is_acyclic(adj_logical)) break
            }

            # Find weakest constraint to remove
            has_constraint <- !is.na(delta_matrix)
            if (!any(has_constraint)) {
                  warning("No constraints left but graph still has cycles")
                  break
            }

            abs_deltas <- abs(delta_matrix)
            abs_deltas[!has_constraint] <- Inf  # Ignore NA entries
            if (!is.null(locked) && any(locked & has_constraint)) {
                  abs_deltas[locked] <- abs_deltas[locked] +
                        max(abs_deltas[has_constraint]) + 1
            }

            min_delta <- min(abs_deltas, na.rm = TRUE)
            if (!is.finite(min_delta)) break

            weakest <- which(abs_deltas == min_delta, arr.ind = TRUE)[1, ]

            # Remove this constraint (set back to NA)
            delta_matrix[weakest[1], weakest[2]] <- NA
            delta_matrix[weakest[2], weakest[1]] <- NA
      }

      if (iteration >= max_iterations &&
          !is_acyclic((delta_matrix > 0) & !is.na(delta_matrix))) {
            warning("Cycle breaking exceeded maximum iterations")
      }

      return(delta_matrix)
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
            polyclip::polyclip(seg_line, poly_clip, op = "intersection"),
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

# Find t-values where a segment crosses polygon edges in 2D
# Returns sorted numeric vector of t-values along the segment, or NULL
segment_polygon_edge_ts <- function(sx1, sy1, sx2, sy2, poly_x, poly_y) {
      n <- length(poly_x)
      ts <- numeric(0)
      dx_seg <- sx2 - sx1
      dy_seg <- sy2 - sy1
      seg_len_sq <- dx_seg^2 + dy_seg^2
      if (seg_len_sq < 1e-20) return(NULL)

      j <- n
      for (i in seq_len(n)) {
            # Polygon edge j -> i
            dx_edge <- poly_x[i] - poly_x[j]
            dy_edge <- poly_y[i] - poly_y[j]

            denom <- dx_seg * dy_edge - dy_seg * dx_edge
            if (abs(denom) > 1e-12) {
                  ox <- poly_x[j] - sx1
                  oy <- poly_y[j] - sy1
                  t_seg <- (ox * dy_edge - oy * dx_edge) / denom
                  u_edge <- (ox * dy_seg - oy * dx_seg) / denom

                  if (t_seg > 1e-9 && t_seg < (1 - 1e-9) &&
                      u_edge >= 0 && u_edge <= 1) {
                        ts <- c(ts, t_seg)
                  }
            }
            j <- i
      }
      if (length(ts) == 0) return(NULL)
      sort(ts)
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
      # Split polygons that pass through one another, so that every remaining
      # polygon pair has a consistent occlusion order
      data <- split_intersecting_polygons(data, proj, plot_bounds)

      has_prim <- ".prim" %in% names(data)

      grp <- data$group
      x <- data$x
      y <- data$y
      z <- data[[depth_var]]

      ugrp <- unique(grp)
      grp_idx <- split(seq_along(grp), grp)
      grp_idx <- grp_idx[ugrp]

      if (has_prim) {
            prim_vec <- data$.prim
            prim_lookup <- vapply(grp_idx, function(idx) prim_vec[idx[1]], character(1))
      } else {
            prim_lookup <- rep("polygon", length(ugrp))
            names(prim_lookup) <- ugrp
      }

      grp_data <- lapply(grp_idx, function(idx) {
            gx <- x[idx]; gy <- y[idx]; gz <- z[idx]
            bb <- c(min(gx), max(gx), min(gy), max(gy), min(gz), max(gz))
            m <- cbind(x = gx, y = gy, z = gz)
            cbind(m, matrix(bb, nrow = nrow(m), ncol = 6, byrow = TRUE))
      })

      poly_betas <- compute_poly_betas(grp_data, prim_lookup)

      # Cache betas by group name (for cheap reconstitution after splitting)
      names(poly_betas) <- names(grp_data)

      # Split segments that interpenetrate polygons
      data <- split_interpenetrating_segments(data, grp_data, grp_idx,
                                              prim_lookup, poly_betas)

      # Incrementally add new groups from splitting (only if data changed)
      new_grp <- data$group
      new_groups <- setdiff(unique(new_grp), ugrp)

      if (length(new_groups) > 0) {
            # Remove entries for original groups that were split
            split_originals <- unique(sub("__split[0-9]+$", "", new_groups))
            drop_idx <- which(names(grp_data) %in% split_originals)
            if (length(drop_idx) > 0) {
                  grp_data <- grp_data[-drop_idx]
                  prim_lookup <- prim_lookup[-drop_idx]
                  poly_betas <- poly_betas[-drop_idx]
            }

            # Build grp_data entries for new groups only
            new_x <- data$x; new_y <- data$y; new_z <- data[[depth_var]]
            new_mask <- new_grp %in% new_groups
            new_seq <- which(new_mask)
            new_grp_idx <- split(new_seq, new_grp[new_mask])

            for (ng in new_groups) {
                  idx <- new_grp_idx[[ng]]
                  gx <- new_x[idx]; gy <- new_y[idx]; gz <- new_z[idx]
                  bb <- c(min(gx), max(gx), min(gy), max(gy), min(gz), max(gz))
                  m <- cbind(x = gx, y = gy, z = gz)
                  grp_data[[ng]] <- cbind(m, matrix(bb, nrow = nrow(m), ncol = 6, byrow = TRUE))
                  prim_lookup[[ng]] <- "segment"
                  poly_betas[[ng]] <- NULL
            }
      }

      k <- length(grp_data)
      delta <- matrix(NA_real_, k, k)
      locked <- matrix(FALSE, k, k)
      groups <- names(grp_data)

      # Pieces cut from the same polygon are coplanar and only share edges,
      # so they impose no ordering constraint on one another
      cut_parent <- ifelse(grepl("__cut[0-9]+$", groups),
                           sub("__cut[0-9]+$", "", groups), NA_character_)

      # Exact 3D geometry for polygon groups, used to settle the sign of
      # polygon-polygon constraints (see plane_side_order())
      eye <- eye_in_cube_space(proj)
      poly_3d <- polygon_geometry_3d(data, groups, prim_lookup)

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

                  if (!is.null(eye) && !is.null(poly_3d[[i]]) && !is.null(poly_3d[[j]])) {
                        exact <- plane_side_order(poly_3d[[i]], poly_3d[[j]], eye)
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

#' Split segments at polygon interpenetration points
#'
#' For each segment, finds all polygons it passes through in 3D (i.e., the
#' segment is in front of the polygon at one end and behind at the other).
#' Computes the parameter t where segment depth equals polygon depth, and
#' splits the segment at all such crossing points.
#'
#' Handles three overlap cases:
#' \itemize{
#'   \item Both segment endpoints inside polygon footprint
#'   \item One endpoint inside, one outside
#'   \item Both endpoints outside but segment crosses through polygon
#' }
#'
#' @param data Data frame with columns x, y, depth (post-transform), group,
#'   .prim, and all aesthetic columns.
#' @param grp_data List of per-group matrices from pw_render_order.
#' @param grp_idx List of row index vectors per group, keyed by group name.
#' @param prim_lookup Named character vector of primitive types per group.
#' @param poly_betas List of pre-computed plane beta vectors.
#' @return Data frame with interpenetrating segments replaced by sub-segments.
#'   Non-segment rows are returned unchanged.
#' @keywords internal
#' @noRd
split_interpenetrating_segments <- function(data, grp_data, grp_idx,
                                            prim_lookup, poly_betas) {

      seg_which <- which(prim_lookup == "segment")
      poly_which <- which(prim_lookup == "polygon")

      if (length(seg_which) == 0 || length(poly_which) == 0) return(data)

      # Tolerance for near-surface detection: if either endpoint's depth
      # difference from the polygon is below this, the segment is touching
      # rather than crossing (e.g. residuals originating at their surface)
      .split_tol <- 0.01

      # Pre-extract polygon xy and bboxes for fast access
      n_poly <- length(poly_which)
      poly_px <- vector("list", n_poly)
      poly_py <- vector("list", n_poly)
      poly_bb <- matrix(NA_real_, nrow = n_poly, ncol = 4)

      for (pi in seq_along(poly_which)) {
            m <- grp_data[[poly_which[pi]]]
            poly_px[[pi]] <- m[, 1]
            poly_py[[pi]] <- m[, 2]
            poly_bb[pi, ] <- m[1, 4:7]  # xmin, xmax, ymin, ymax
      }

      # For each segment, collect all split t-values across all polygons
      splits_needed <- FALSE
      seg_split_ts <- vector("list", length(seg_which))

      for (si in seq_along(seg_which)) {
            m <- grp_data[[seg_which[si]]]
            if (nrow(m) != 2) next

            sx <- m[, 1]; sy <- m[, 2]; sz <- m[, 3]

            seg_xmin <- min(sx); seg_xmax <- max(sx)
            seg_ymin <- min(sy); seg_ymax <- max(sy)

            split_ts <- numeric(0)

            for (pi in seq_along(poly_which)) {
                  # Quick bbox rejection
                  if (seg_xmin > poly_bb[pi, 2] || seg_xmax < poly_bb[pi, 1] ||
                      seg_ymin > poly_bb[pi, 4] || seg_ymax < poly_bb[pi, 3]) {
                        next
                  }

                  ppx <- poly_px[[pi]]
                  ppy <- poly_py[[pi]]

                  p1_in <- point_in_polygon(sx[1], sy[1], ppx, ppy)
                  p2_in <- point_in_polygon(sx[2], sy[2], ppx, ppy)

                  if (!p1_in && !p2_in) {
                        # Neither endpoint inside: check if segment crosses polygon edges
                        edge_ts <- segment_polygon_edge_ts(sx[1], sy[1], sx[2], sy[2], ppx, ppy)
                        if (is.null(edge_ts) || length(edge_ts) < 2) next

                        # Segment enters and exits polygon; check depth at the entry/exit
                        t_enter <- edge_ts[1]
                        t_exit <- edge_ts[length(edge_ts)]

                        xy_enter <- c(sx[1] + t_enter * (sx[2] - sx[1]),
                                      sy[1] + t_enter * (sy[2] - sy[1]))
                        xy_exit <- c(sx[1] + t_exit * (sx[2] - sx[1]),
                                     sy[1] + t_exit * (sy[2] - sy[1]))

                        seg_z_enter <- sz[1] + t_enter * (sz[2] - sz[1])
                        seg_z_exit <- sz[1] + t_exit * (sz[2] - sz[1])

                        beta <- poly_betas[[poly_which[pi]]]
                        poly_z_enter <- interpolate_z(grp_data[[poly_which[pi]]][, 1:3],
                                                      xy_enter, beta = beta)
                        poly_z_exit <- interpolate_z(grp_data[[poly_which[pi]]][, 1:3],
                                                     xy_exit, beta = beta)

                        d_enter <- seg_z_enter - poly_z_enter
                        d_exit <- seg_z_exit - poly_z_exit

                        if (d_enter * d_exit >= 0 ||
                            min(abs(d_enter), abs(d_exit)) < .split_tol) next

                        # Crossing within [t_enter, t_exit] range
                        # Linear solve restricted to this sub-interval
                        denom <- (d_exit - d_enter)
                        if (abs(denom) < 1e-12) next
                        t_local <- -d_enter / denom  # fractional position within [t_enter, t_exit]
                        t_cross <- t_enter + t_local * (t_exit - t_enter)

                  } else {
                        # At least one endpoint inside polygon
                        beta <- poly_betas[[poly_which[pi]]]
                        pz1 <- interpolate_z(grp_data[[poly_which[pi]]][, 1:3],
                                             c(sx[1], sy[1]), beta = beta)
                        pz2 <- interpolate_z(grp_data[[poly_which[pi]]][, 1:3],
                                             c(sx[2], sy[2]), beta = beta)

                        d1 <- sz[1] - pz1
                        d2 <- sz[2] - pz2

                        if (d1 * d2 >= 0 ||
                            min(abs(d1), abs(d2)) < .split_tol) next

                        denom <- (sz[2] - sz[1]) - (pz2 - pz1)
                        if (abs(denom) < 1e-12) next

                        t_cross <- (pz1 - sz[1]) / denom
                  }

                  if (t_cross > 0.001 && t_cross < 0.999) {
                        split_ts <- c(split_ts, t_cross)
                  }
            }

            if (length(split_ts) > 0) {
                  seg_split_ts[[si]] <- sort(unique(split_ts))
                  splits_needed <- TRUE
            }
      }

      if (!splits_needed) return(data)

      # Perform the splits
      rows_to_drop <- integer(0)
      new_rows_list <- vector("list", length(seg_which))
      cols <- names(data)

      # Columns to interpolate at split points
      interp_cols <- c("x", "y", "depth")
      if ("depth_scale" %in% cols) interp_cols <- c(interp_cols, "depth_scale")
      if ("depth_3d" %in% cols) interp_cols <- c(interp_cols, "depth_3d")

      for (si in seq_along(seg_which)) {
            ts <- seg_split_ts[[si]]
            if (is.null(ts) || length(ts) == 0) next

            idx <- grp_idx[[seg_which[si]]]
            seg_rows <- data[idx, , drop = FALSE]

            if ("point_type" %in% cols) {
                  start_row <- seg_rows[seg_rows$point_type == "start", , drop = FALSE]
                  end_row <- seg_rows[seg_rows$point_type == "end", , drop = FALSE]
            } else {
                  start_row <- seg_rows[1, , drop = FALSE]
                  end_row <- seg_rows[2, , drop = FALSE]
            }

            orig_group <- start_row$group[1]
            orig_seg_id <- if ("segment_id" %in% cols) start_row$segment_id[1] else NA

            breaks <- c(0, ts, 1)
            n_sub <- length(breaks) - 1

            sub_rows <- vector("list", n_sub)

            for (k in seq_len(n_sub)) {
                  t0 <- breaks[k]
                  t1 <- breaks[k + 1]

                  new_start <- start_row
                  new_end <- end_row

                  for (col in interp_cols) {
                        v0 <- start_row[[col]]
                        v1 <- end_row[[col]]
                        if (is.numeric(v0) && is.numeric(v1)) {
                              new_start[[col]] <- v0 + t0 * (v1 - v0)
                              new_end[[col]] <- v0 + t1 * (v1 - v0)
                        }
                  }

                  suffix <- paste0("__split", k)
                  new_start$group <- paste0(orig_group, suffix)
                  new_end$group <- paste0(orig_group, suffix)

                  if ("segment_id" %in% cols && !is.na(orig_seg_id)) {
                        new_id <- orig_seg_id + (k - 1) * 0.01
                        new_start$segment_id <- new_id
                        new_end$segment_id <- new_id
                  }

                  sub_rows[[k]] <- rbind(new_start, new_end)
            }

            new_rows_list[[si]] <- do.call(rbind, sub_rows)
            rows_to_drop <- c(rows_to_drop, idx)
      }

      new_rows <- do.call(rbind, new_rows_list[!vapply(new_rows_list, is.null, logical(1))])

      if (is.null(new_rows) || nrow(new_rows) == 0) return(data)

      data_kept <- data[-rows_to_drop, , drop = FALSE]
      rbind(data_kept, new_rows)
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


#' Per-group 3D vertices and plane fits for polygon groups
#'
#' @param data Data frame with `.x3d`, `.y3d`, `.z3d`, and `group`.
#' @param groups Character vector of group names, in comparison order.
#' @param prim_lookup Primitive type per group, aligned with `groups`.
#' @return List aligned with `groups`: for polygons with a valid plane fit,
#'   a list with `pts` and `plane` (see `polygon_plane()`); otherwise NULL.
#' @keywords internal
#' @noRd
polygon_geometry_3d <- function(data, groups, prim_lookup) {
      out <- vector("list", length(groups))
      if (!all(c(".x3d", ".y3d", ".z3d") %in% names(data))) return(out)

      idx <- split(seq_len(nrow(data)), factor(as.character(data$group), levels = groups))
      for (g in which(prim_lookup == "polygon")) {
            rows <- idx[[g]]
            if (length(rows) < 3) next
            pts <- cbind(data$.x3d[rows], data$.y3d[rows], data$.z3d[rows])
            pl <- polygon_plane(pts)
            if (!is.null(pl)) out[[g]] <- list(pts = pts, plane = pl)
      }
      out
}


#' Exact occlusion order of two polygons from plane-side tests
#'
#' If one polygon is planar and the other lies entirely on one side of its
#' plane, whichever lies on the viewer's side of that plane is in front
#' wherever the two overlap on screen. This holds under both perspective and
#' orthographic projection.
#'
#' @param gi,gj Group geometry from `polygon_geometry_3d()`.
#' @param eye Viewer from `eye_in_cube_space()`.
#' @param tol Absolute distance tolerance in cube units.
#' @return 1 if i is behind j, -1 if i is in front of j, or NA if neither
#'   test is decisive (e.g. coplanar or mutually straddling polygons).
#' @keywords internal
#' @noRd
plane_side_order <- function(gi, gj, eye, tol = 1e-6) {
      if (gi$plane$planar) {
            s <- side_of_plane(gj$pts, gi$plane, tol)
            e <- eye_side_of_plane(gi$plane, eye)
            if (!is.na(s) && e != 0) return(if (s == e) 1 else -1)
      }
      if (gj$plane$planar) {
            s <- side_of_plane(gi$pts, gj$plane, tol)
            e <- eye_side_of_plane(gj$plane, eye)
            if (!is.na(s) && e != 0) return(if (s == e) -1 else 1)
      }
      NA
}


#' Which side of a plane a set of points lies on
#'
#' @return 1 or -1 if all points are on that side (touching allowed, at least
#'   one strictly off the plane), otherwise NA.
#' @keywords internal
#' @noRd
side_of_plane <- function(pts, plane, tol) {
      d <- as.vector(pts %*% plane$normal - plane$offset)
      if (all(d >= -tol) && any(d > tol)) return(1)
      if (all(d <= tol) && any(d < -tol)) return(-1)
      NA
}


#' Which side of a plane the viewer is on
#'
#' @return 1, -1, or 0 when the plane is viewed edge-on.
#' @keywords internal
#' @noRd
eye_side_of_plane <- function(plane, eye) {
      s <- if (eye$persp) {
            sum(plane$normal * eye$v) - plane$offset
      } else {
            sum(plane$normal * eye$v)
      }
      if (abs(s) < 1e-9) 0 else sign(s)
}
