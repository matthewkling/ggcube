
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

      # alculate Centroid Coordinates
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
                  if(is.null(int)) next()
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
