
interpolate_z <- function(train, new) {
      train <- as.matrix(train)
      if(nrow(unique(train)) >= 3) { # interpolate with linear model (z ~ x + y)
            x <- cbind(1, as.matrix(train[,1:2]))
            beta <- solve(crossprod(x), crossprod(x, train[,3]))
            z <- beta[1] + beta[2] * new[1] + beta[3] * new[2]
      } else { # use inverse-distance-weighted interpolation for degenerate polygons (e.g. zero-height pillar faces)
            weights <- 1 / sqrt((train[,1] - new[1])^2 + (train[,2] - new[2])^2)
            z <- weighted.mean(train[,3], weights)
      }
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


topological_sort <- function(adj_matrix) {
      n <- nrow(adj_matrix)
      in_degree <- colSums(adj_matrix)
      queue <- which(in_degree == 0)
      result <- integer(0)

      while (length(queue) > 0) {
            # Process node with no dependencies
            current <- queue[1]
            queue <- queue[-1]
            result <- c(result, current)

            # Remove edges from current node
            neighbors <- which(adj_matrix[current, ])
            for (neighbor in neighbors) {
                  in_degree[neighbor] <- in_degree[neighbor] - 1
                  if (in_degree[neighbor] == 0) {
                        queue <- c(queue, neighbor)
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

# Check if polygon vertices are ordered counter-clockwise
is_counter_clockwise <- function(poly) {
      n <- nrow(poly)
      if (n < 3) return(TRUE)

      # Calculate signed area using shoelace formula
      area <- 0
      for (i in 1:n) {
            j <- if (i == n) 1 else i + 1
            area <- area + (poly[j, 1] - poly[i, 1]) * (poly[j, 2] + poly[i, 2])
      }
      return(area < 0)  # Negative area = counter-clockwise
}

# Ensure counter-clockwise vertex ordering
ensure_ccw <- function(poly) {
      if (!is_counter_clockwise(poly)) {
            return(poly[nrow(poly):1, ])  # Reverse order
      }
      return(poly)
}

# Sutherland-Hodgman polygon clipping algorithm
sutherland_hodgman_clip <- function(subject_poly, clip_poly) {
      # Handle degenerate cases
      if (nrow(subject_poly) < 3 || nrow(clip_poly) < 3) {
            return(matrix(numeric(0), ncol = 2))
      }

      # Ensure counter-clockwise ordering
      subject_poly <- ensure_ccw(subject_poly)
      clip_poly <- ensure_ccw(clip_poly)

      # Ensure polygons are closed (last vertex = first vertex)
      if (!all(subject_poly[1, ] == subject_poly[nrow(subject_poly), ])) {
            subject_poly <- rbind(subject_poly, subject_poly[1, ])
      }
      if (!all(clip_poly[1, ] == clip_poly[nrow(clip_poly), ])) {
            clip_poly <- rbind(clip_poly, clip_poly[1, ])
      }

      # Start with the subject polygon
      output_list <- subject_poly

      # Clip against each edge of the clipping polygon
      n_clip_edges <- nrow(clip_poly) - 1

      for (i in 1:n_clip_edges) {
            if (nrow(output_list) == 0) break  # Nothing left to clip

            # Current clipping edge
            edge_start <- clip_poly[i, ]
            edge_end <- clip_poly[i + 1, ]

            # Clip current polygon against this edge
            output_list <- clip_polygon_by_edge(output_list, edge_start, edge_end)
      }

      # Remove duplicate last vertex if it was added
      if (nrow(output_list) > 0 && all(output_list[1, ] == output_list[nrow(output_list), ])) {
            output_list <- output_list[-nrow(output_list), , drop = FALSE]
      }

      return(output_list)
}

# Clip a polygon against a single edge (half-plane)
clip_polygon_by_edge <- function(poly, edge_start, edge_end) {
      if (nrow(poly) < 3) return(matrix(numeric(0), ncol = 2))

      input_list <- poly
      output_list <- matrix(numeric(0), ncol = 2)

      if (nrow(input_list) == 0) return(output_list)

      # Start with the last vertex
      s <- input_list[nrow(input_list), ]

      for (i in 1:nrow(input_list)) {
            e <- input_list[i, ]

            if (is_inside(e, edge_start, edge_end)) {
                  # e is inside
                  if (!is_inside(s, edge_start, edge_end)) {
                        # s is outside, e is inside: add intersection
                        intersection <- line_intersect(s, e, edge_start, edge_end)
                        if (!is.null(intersection)) {
                              output_list <- rbind(output_list, intersection)
                        }
                  }
                  # Add e
                  output_list <- rbind(output_list, e)

            } else if (is_inside(s, edge_start, edge_end)) {
                  # s is inside, e is outside: add intersection only
                  intersection <- line_intersect(s, e, edge_start, edge_end)
                  if (!is.null(intersection)) {
                        output_list <- rbind(output_list, intersection)
                  }
            }

            # If both outside, add nothing

            s <- e  # Move to next vertex
      }

      return(output_list)
}

# Test if point is inside (on the left side of) the directed edge
is_inside <- function(point, edge_start, edge_end) {
      # Cross product test: (edge_end - edge_start) × (point - edge_start)
      # Positive cross product means point is on the left (inside)
      cross_product <- (edge_end[1] - edge_start[1]) * (point[2] - edge_start[2]) -
            (edge_end[2] - edge_start[2]) * (point[1] - edge_start[1])
      return(cross_product >= 0)  # >= 0 includes points on the edge
}

# Find intersection of two line segments
line_intersect <- function(p1, p2, p3, p4, tol = 1e-10) {
      # Line 1: p1 to p2
      # Line 2: p3 to p4

      x1 <- p1[1]; y1 <- p1[2]
      x2 <- p2[1]; y2 <- p2[2]
      x3 <- p3[1]; y3 <- p3[2]
      x4 <- p4[1]; y4 <- p4[2]

      denom <- (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

      if (abs(denom) < tol) {
            # Lines are parallel
            return(NULL)
      }

      t <- ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom

      # Calculate intersection point
      ix <- x1 + t * (x2 - x1)
      iy <- y1 + t * (y2 - y1)

      return(c(ix, iy))
}

# Helper function to check if two polygons actually intersect
polygons_intersect <- function(poly1, poly2) {
      clipped <- sutherland_hodgman_clip(poly1, poly2)
      return(nrow(clipped) >= 3)  # Valid intersection if result has 3+ vertices
}

# Helper function to get centroid of polygon intersection
intersection_centroid <- function(poly1, poly2) {
      clipped <- sutherland_hodgman_clip(poly1, poly2)
      return(c(x = mean(clipped[, 1]), y =  mean(clipped[, 2])))
}

pw_render_order <- function(data,
                            depth_var = "depth" # depth or depth_3d
){

      d <- data %>%
            mutate(z = data[[depth_var]]) %>%
            select(group, x, y, z) %>%
            mutate(xmin = min(x, na.rm = TRUE), xmax = max(x, na.rm = TRUE),
                   ymin = min(y, na.rm = TRUE), ymax = max(y, na.rm = TRUE),
                   zmin = min(z, na.rm = TRUE), zmax = max(z, na.rm = TRUE)) %>%
            ungroup()

      d <- d %>%
            select(-group) %>%
            as.matrix() %>%
            split.data.frame(d$group)

      k <- length(d)
      delta <- matrix(NA, k, k)
      groups <- names(d)

      for(i in 1:k){
            for(j in 1:k){
                  if(i >= j) next()

                  di <- d[[i]]
                  dj <- d[[j]]
                  bbi <- di[1, 4:9]
                  bbj <- dj[1, 4:9]

                  # if no xy overlap, no precedence
                  # check bounding box overlap before costlier SAT test
                  if(bbi[1] >= bbj[2]) next()
                  if(bbi[2] <= bbj[1]) next()
                  if(bbi[3] >= bbj[4]) next()
                  if(bbi[4] <= bbj[3]) next()
                  if(!sat_overlap(di[,1:2], dj[,1:2])) next()

                  # if xy overlap but no z overlap, standard depth precedence
                  if(bbi[5] >= bbj[6] | bbj[5] >= bbi[6]){
                        dlt <- mean(bbi[5:6]) - mean(bbj[5:6])
                        delta[i, j] <- dlt
                        delta[j, i] <- -dlt
                        next()
                  }

                  # otherwise, sample depth at center of polygon intersection
                  int <- intersection_centroid(di[, 1:2], dj[, 1:2])
                  zi <- interpolate_z(di[, 1:3], int)
                  zj <- interpolate_z(dj[, 1:3], int)
                  dlt <- zi - zj
                  delta[i, j] <- dlt
                  delta[j, i] <- -dlt
            }
      }

      delta_pruned <- break_cycles(delta)
      adj_logical <- (delta_pruned > 0) & !is.na(delta_pruned)
      render_order <- groups[topological_sort(adj_logical)]
      match(data$group, render_order)
}


# Hierarchical depth sorting
# split `group` into levels by `__`, and sort by group-level depth
# this prevents depth-sorting within lowest-level group, to preserve vertex order
sort_by_depth <- function(data) {

      if(".sort_method" %in% names(data)){
            method <- data$.sort_method[1]
            if(method == "auto"){
                  method <- if(nrow(data) <= 500) "pairwise" else "painter"
            }
      }else{
            method <- "painter"
      }

      # Vertex order within each group
      data <- data %>%
            group_by(group) %>%
            mutate(.vertex_order = row_number()) %>%
            ungroup()

      if (method == "pairwise") { # Pairwise sorting (group-wise)
            data$.render_order <- pw_render_order(data, "depth")
            data <- data %>%
                  arrange(.render_order, .vertex_order) %>%
                  select(-.render_order)

      } else if(any(grepl("__", data$group))) { # Hierarchical sorting
            data$depth2 <- if("depth_3d" %in% names(data)) data$depth_3d else data$depth

            data <- data %>%
                  tidyr::separate(group, c("level1", "level2"), sep = "__",
                                  remove = FALSE, extra = "merge") %>%
                  group_by(level1) %>% mutate(depth1 = max(depth)) %>%
                  group_by(level2) %>% mutate(depth2 = mean(depth2)) %>%
                  ungroup() %>%
                  arrange(desc(depth1), desc(depth2), group, .vertex_order) %>%
                  select(-level1, -level2, -depth1, -depth2)

      } else { # Simple sorting
            data <- data %>% arrange(desc(depth), group, .vertex_order)
      }

      data <- data %>% select(-.vertex_order)
      return(data)
}
