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
#
# Known limitation: primitives that genuinely interpenetrate in 3D (e.g. a
# segment passing through a polygon) cannot be correctly ordered without
# splitting the geometry at the intersection. This is not currently implemented.

interpolate_z <- function(train, new) {
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

# Break cycles by removing weakest constraints
break_cycles <- function(delta_matrix) {
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

            min_delta <- min(abs_deltas, na.rm = TRUE)
            if (!is.finite(min_delta)) break

            weakest <- which(abs_deltas == min_delta, arr.ind = TRUE)[1, ]

            # Remove this constraint (set back to NA)
            delta_matrix[weakest[1], weakest[2]] <- NA
            delta_matrix[weakest[2], weakest[1]] <- NA
      }

      if (iteration >= max_iterations) {
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


# Pairwise comparison modules -------------------------------------------------
# Each returns a depth delta (positive = i in front of j) or NA (no constraint)

# Compare two polygon groups
pw_compare_poly_poly <- function(di, dj, bbi, bbj) {
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
      zi <- interpolate_z(di[, 1:3], int)
      zj <- interpolate_z(dj[, 1:3], int)
      zi - zj
}

# Compare a polygon group with a point group
pw_compare_poly_point <- function(poly_data, point_data) {
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
      poly_z <- interpolate_z(poly_data[, 1:3], c(px, py))
      poly_z - pz
}

# Compare a polygon group with a segment group
# Uses polyclip to find the segment's intersection with the polygon,
# then samples depth at the midpoint of the clipped portion.
pw_compare_poly_segment <- function(poly_data, seg_data) {
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
      poly_z <- interpolate_z(poly_data[, 1:3], sample_xy)
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
# Returns depth delta (positive = i in front of j) or NA (no constraint).
# When primitives are coplanar (delta ~ 0), smaller primitives render on top.
pw_compare_pair <- function(di, dj, type_i, type_j) {

      dlt <- pw_compare_pair_raw(di, dj, type_i, type_j)

      # Coplanar tiebreaker: smaller primitives on top
      if (!is.na(dlt) && abs(dlt) < .coplanar_tol) {
            pi <- .prim_priority[type_i] %||% 1
            pj <- .prim_priority[type_j] %||% 1
            if (pi != pj) return(pj - pi)  # higher priority j => positive => i behind j
      }

      dlt
}

# Raw pairwise comparison without tiebreaker
pw_compare_pair_raw <- function(di, dj, type_i, type_j) {

      # Polygon vs polygon
      if (type_i == "polygon" && type_j == "polygon") {
            bbi <- di[1, 4:9]
            bbj <- dj[1, 4:9]
            return(pw_compare_poly_poly(di, dj, bbi, bbj))
      }

      # Polygon vs point/text
      if (type_i == "polygon" && type_j %in% c("point", "text")) {
            return(pw_compare_poly_point(di, dj))
      }
      if (type_j == "polygon" && type_i %in% c("point", "text")) {
            dlt <- pw_compare_poly_point(dj, di)
            if (!is.na(dlt)) return(-dlt) else return(NA)
      }

      # Polygon vs segment
      if (type_i == "polygon" && type_j == "segment") {
            return(pw_compare_poly_segment(di, dj))
      }
      if (type_j == "polygon" && type_i == "segment") {
            dlt <- pw_compare_poly_segment(dj, di)
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
                            depth_var = "depth"
){
      has_prim <- ".prim" %in% names(data)

      grp <- data$group
      x <- data$x
      y <- data$y
      z <- data[[depth_var]]

      # Compute per-group bounding boxes using split + vapply
      ugrp <- unique(grp)
      grp_idx <- split(seq_along(grp), grp)
      grp_idx <- grp_idx[ugrp]  # preserve order of first appearance

      # Build per-group matrix: columns = x, y, z, xmin, xmax, ymin, ymax, zmin, zmax
      grp_data <- lapply(grp_idx, function(idx) {
            gx <- x[idx]; gy <- y[idx]; gz <- z[idx]
            bb <- c(min(gx), max(gx), min(gy), max(gy), min(gz), max(gz))
            m <- cbind(x = gx, y = gy, z = gz)
            # Replicate bounding box on each row
            cbind(m, matrix(bb, nrow = nrow(m), ncol = 6, byrow = TRUE))
      })

      # Build prim lookup
      if (has_prim) {
            prim_vec <- data$.prim
            prim_lookup <- vapply(grp_idx, function(idx) prim_vec[idx[1]], character(1))
      }

      k <- length(grp_data)
      delta <- matrix(NA_real_, k, k)
      groups <- names(grp_data)

      for (i in 1:k) {
            for (j in 1:k) {
                  if (i >= j) next

                  di <- grp_data[[i]]
                  dj <- grp_data[[j]]

                  if (has_prim) {
                        ti <- prim_lookup[i]
                        tj <- prim_lookup[j]
                  } else {
                        ti <- "polygon"
                        tj <- "polygon"
                  }

                  dlt <- pw_compare_pair(di, dj, ti, tj)

                  if (!is.na(dlt)) {
                        delta[i, j] <- dlt
                        delta[j, i] <- -dlt
                  }
            }
      }

      delta_pruned <- break_cycles(delta)
      adj_logical <- (delta_pruned > 0) & !is.na(delta_pruned)
      render_order <- groups[topological_sort(adj_logical)]
      match(data$group, render_order)
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
sort_by_depth <- function(data) {

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
            data$.render_order <- pw_render_order(data, "depth")
            data <- data %>%
                  arrange(.render_order, .vertex_order) %>%
                  select(-.render_order)

      } else if(any(grepl("__", data$group))) {
            # Hierarchical painter sorting
            data <- data %>%
                  tidyr::separate(group, c("level1", "level2"), sep = "__",
                                  remove = FALSE, extra = "merge") %>%
                  group_by(level1) %>% mutate(depth1 = max(depth)) %>%
                  ungroup() %>%
                  arrange(desc(depth1), desc(.prim_depth), group, .vertex_order) %>%
                  select(-level1, -level2, -depth1)

      } else {
            # Simple painter sorting
            data <- data %>%
                  arrange(desc(.prim_depth), group, .vertex_order)
      }

      data <- data %>% select(-.vertex_order, -.prim_depth)
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
compute_prim_depth <- function(data) {
      depth_col <- if("depth_3d" %in% names(data)) "depth_3d" else "depth"

      data <- data %>%
            group_by(group) %>%
            mutate(.prim_depth = mean(.data[[depth_col]], na.rm = TRUE)) %>%
            ungroup()

      return(data)
}
