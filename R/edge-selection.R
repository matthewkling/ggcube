enumerate_axis_edges <- function(axis) {
      # Return all 4 possible edges for the given axis
      # Each edge is defined by the 2 coordinates that are fixed (not varying along the axis)

      if (axis == "x") {
            # X-axis edges: x varies from -0.5 to 0.5, y and z are fixed
            edges <- list(
                  list(fixed_coords = c(y = -0.5, z = -0.5), touching_faces = c("ymin", "zmin")),
                  list(fixed_coords = c(y = -0.5, z =  0.5), touching_faces = c("ymin", "zmax")),
                  list(fixed_coords = c(y =  0.5, z = -0.5), touching_faces = c("ymax", "zmin")),
                  list(fixed_coords = c(y =  0.5, z =  0.5), touching_faces = c("ymax", "zmax"))
            )
      } else if (axis == "y") {
            # Y-axis edges: y varies from -0.5 to 0.5, x and z are fixed
            edges <- list(
                  list(fixed_coords = c(x = -0.5, z = -0.5), touching_faces = c("xmin", "zmin")),
                  list(fixed_coords = c(x = -0.5, z =  0.5), touching_faces = c("xmin", "zmax")),
                  list(fixed_coords = c(x =  0.5, z = -0.5), touching_faces = c("xmax", "zmin")),
                  list(fixed_coords = c(x =  0.5, z =  0.5), touching_faces = c("xmax", "zmax"))
            )
      } else { # axis == "z"
            # Z-axis edges: z varies from -0.5 to 0.5, x and y are fixed
            edges <- list(
                  list(fixed_coords = c(x = -0.5, y = -0.5), touching_faces = c("xmin", "ymin")),
                  list(fixed_coords = c(x = -0.5, y =  0.5), touching_faces = c("xmin", "ymax")),
                  list(fixed_coords = c(x =  0.5, y = -0.5), touching_faces = c("xmax", "ymin")),
                  list(fixed_coords = c(x =  0.5, y =  0.5), touching_faces = c("xmax", "ymax"))
            )
      }

      # Add edge endpoints to each edge
      for (i in seq_along(edges)) {
            if (axis == "x") {
                  edges[[i]]$p1 <- c(x = -0.5, edges[[i]]$fixed_coords)
                  edges[[i]]$p2 <- c(x =  0.5, edges[[i]]$fixed_coords)
            } else if (axis == "y") {
                  edges[[i]]$p1 <- c(edges[[i]]$fixed_coords[1], y = -0.5, edges[[i]]$fixed_coords[2])
                  edges[[i]]$p2 <- c(edges[[i]]$fixed_coords[1], y =  0.5, edges[[i]]$fixed_coords[2])
            } else { # axis == "z"
                  edges[[i]]$p1 <- c(edges[[i]]$fixed_coords, z = -0.5)
                  edges[[i]]$p2 <- c(edges[[i]]$fixed_coords, z =  0.5)
            }
            edges[[i]]$axis <- axis
            edges[[i]]$edge_id <- i
      }

      return(edges)
}

filter_edges_by_visible_faces <- function(edges, visible_faces) {
      # Keep only edges where at least one touching face is visible
      visible_edges <- list()

      for (edge in edges) {
            if (any(edge$touching_faces %in% visible_faces)) {
                  visible_edges[[length(visible_edges) + 1]] <- edge
            }
      }

      return(visible_edges)
}

# Calculate 2D projected area of a face
calculate_face_area_2d <- function(face_name, proj, effective_ratios) {
      corners_3d <- get_face_corners(face_name)

      # Scale by effective ratios to match coordinate system
      corners_3d$x <- corners_3d$x * effective_ratios[1]
      corners_3d$y <- corners_3d$y * effective_ratios[2]
      corners_3d$z <- corners_3d$z * effective_ratios[3]

      # Transform to 2D
      corners_2d <- transform_3d_standard(corners_3d, proj)

      # Calculate area using shoelace formula for quadrilateral
      x <- corners_2d$x
      y <- corners_2d$y

      # Shoelace formula: A = 0.5 * |sum(x_i * y_i+1 - x_i+1 * y_i)|
      area <- 0.5 * abs(sum(x * c(y[2:4], y[1]) - c(x[2:4], x[1]) * y))

      return(area)
}

# Helper function to get which axes have gridlines on a given face
get_gridline_axes_for_face <- function(face) {
      # Each face has gridlines for the 2 axes that aren't the face normal
      face_axis <- substr(face, 1, 1)  # "x", "y", or "z"
      all_axes <- c("x", "y", "z")
      gridline_axes <- setdiff(all_axes, face_axis)
      return(gridline_axes)
}

# Face-specific perpendicularity calculation
calculate_face_perpendicularity_score <- function(edge, face, axis, proj, effective_ratios) {
      # Get 2D directions for all three axes using cube corners
      axis_directions_2d <- list()

      # X-axis direction: from (-0.5, 0, 0) to (0.5, 0, 0)
      x_3d <- data.frame(
            x = c(-0.5, 0.5) * effective_ratios[1],
            y = c(0, 0),
            z = c(0, 0)
      )
      x_2d <- transform_3d_standard(x_3d, proj)
      axis_directions_2d$x <- c(x_2d$x[2] - x_2d$x[1], x_2d$y[2] - x_2d$y[1])

      # Y-axis direction: from (0, -0.5, 0) to (0, 0.5, 0)
      y_3d <- data.frame(
            x = c(0, 0),
            y = c(-0.5, 0.5) * effective_ratios[2],
            z = c(0, 0)
      )
      y_2d <- transform_3d_standard(y_3d, proj)
      axis_directions_2d$y <- c(y_2d$x[2] - y_2d$x[1], y_2d$y[2] - y_2d$y[1])

      # Z-axis direction: from (0, 0, -0.5) to (0, 0, 0.5)
      z_3d <- data.frame(
            x = c(0, 0),
            y = c(0, 0),
            z = c(-0.5, 0.5) * effective_ratios[3]
      )
      z_2d <- transform_3d_standard(z_3d, proj)
      axis_directions_2d$z <- c(z_2d$x[2] - z_2d$x[1], z_2d$y[2] - z_2d$y[1])

      # Get this edge's 2D direction
      edge_dir_2d <- c(edge$p2_2d$x - edge$p1_2d$x, edge$p2_2d$y - edge$p1_2d$y)
      edge_length_2d <- sqrt(sum(edge_dir_2d^2))

      if (edge_length_2d == 0) return(0)  # Degenerate edge

      edge_dir_2d <- edge_dir_2d / edge_length_2d  # Normalize

      # Get which axes have gridlines on this face
      gridline_axes <- get_gridline_axes_for_face(face)

      # Remove the axis we're labeling (since that's parallel to the edge)
      perpendicular_axes <- setdiff(gridline_axes, axis)

      if (length(perpendicular_axes) == 0) return(0)  # No perpendicular gridlines

      # Calculate perpendicularity to the gridline axes that should be perpendicular
      perp_scores <- numeric(length(perpendicular_axes))
      for (i in seq_along(perpendicular_axes)) {
            perp_axis <- perpendicular_axes[i]
            perp_axis_dir <- axis_directions_2d[[perp_axis]]
            perp_axis_length <- sqrt(sum(perp_axis_dir^2))

            if (perp_axis_length > 0) {
                  perp_axis_dir <- perp_axis_dir / perp_axis_length  # Normalize

                  # Calculate angle between edge and this perpendicular axis
                  dot_product <- sum(edge_dir_2d * perp_axis_dir)
                  angle_rad <- acos(pmax(-1, pmin(1, abs(dot_product))))  # Clamp for numerical stability
                  angle_deg <- angle_rad * 180 / pi

                  # Score based on deviation from 90 degrees (perfect perpendicularity)
                  deviation_from_90 <- abs(angle_deg - 90)
                  perp_scores[i] <- 90 - deviation_from_90  # Higher score = more perpendicular
            }
      }

      # Return average perpendicularity score (0-90, higher is better)
      return(mean(perp_scores))
}

# Score edge-face combinations
score_edge_face_combinations <- function(combinations, axis, proj, effective_ratios) {
      if (length(combinations) == 0) return(list())

      # Get convex hull of all 8 cube corners for reference
      all_corners <- expand.grid(
            x = c(-0.5, 0.5) * effective_ratios[1],
            y = c(-0.5, 0.5) * effective_ratios[2],
            z = c(-0.5, 0.5) * effective_ratios[3]
      )
      all_corners_2d <- transform_3d_standard(all_corners, proj)
      hull_indices <- chull(all_corners_2d$x, all_corners_2d$y)
      hull_vertices <- all_corners_2d[hull_indices, ]

      # Score each combination
      for (i in seq_along(combinations)) {
            edge <- combinations[[i]]$edge
            face <- combinations[[i]]$face

            # Apply effective ratios scaling to match hull corner scaling
            p1_scaled <- c(edge$p1[1] * effective_ratios[1],
                           edge$p1[2] * effective_ratios[2],
                           edge$p1[3] * effective_ratios[3])
            p2_scaled <- c(edge$p2[1] * effective_ratios[1],
                           edge$p2[2] * effective_ratios[2],
                           edge$p2[3] * effective_ratios[3])

            # Transform edge endpoints to 2D
            p1_df <- data.frame(x = p1_scaled[1], y = p1_scaled[2], z = p1_scaled[3])
            p2_df <- data.frame(x = p2_scaled[1], y = p2_scaled[2], z = p2_scaled[3])
            p1_2d <- transform_3d_standard(p1_df, proj)
            p2_2d <- transform_3d_standard(p2_df, proj)

            # Store 2D points in edge object
            combinations[[i]]$edge$p1_2d <- p1_2d
            combinations[[i]]$edge$p2_2d <- p2_2d

            # 1. Convex hull membership
            p1_is_hull_vertex <- any(abs(hull_vertices$x - p1_2d$x) < 1e-10 & abs(hull_vertices$y - p1_2d$y) < 1e-10)
            p2_is_hull_vertex <- any(abs(hull_vertices$x - p2_2d$x) < 1e-10 & abs(hull_vertices$y - p2_2d$y) < 1e-10)

            if (p1_is_hull_vertex && p2_is_hull_vertex) {
                  # Find which hull vertices these are
                  p1_hull_index <- which(abs(hull_vertices$x - p1_2d$x) < 1e-10 & abs(hull_vertices$y - p1_2d$y) < 1e-10)[1]
                  p2_hull_index <- which(abs(hull_vertices$x - p2_2d$x) < 1e-10 & abs(hull_vertices$y - p2_2d$y) < 1e-10)[1]

                  # Check if they are consecutive in the hull (considering wraparound)
                  n_hull <- nrow(hull_vertices)
                  consecutive <- (abs(p1_hull_index - p2_hull_index) == 1) ||
                        (abs(p1_hull_index - p2_hull_index) == n_hull - 1)

                  on_hull <- consecutive
            } else {
                  on_hull <- FALSE
            }

            # 2. Face-specific perpendicularity score
            perpendicularity_score <- calculate_face_perpendicularity_score(combinations[[i]]$edge, face, axis, proj, effective_ratios)

            # 3. Face area score (NEW)
            face_area <- calculate_face_area_2d(face, proj, effective_ratios)

            # 4. Depth score (average z of endpoints - lower is better/closer)
            avg_depth <- (p1_2d$z + p2_2d$z) / 2

            # 5. Corner score (average x+y of endpoints - lower is better/more bottom-left)
            avg_corner <- ((p1_2d$x + p1_2d$y) + (p2_2d$x + p2_2d$y)) / 2

            # 6. Edge length
            edge_length <- sqrt((p2_2d$x - p1_2d$x)^2 + (p2_2d$y - p1_2d$y)^2)

            # Store scores in the combination
            combinations[[i]]$on_hull <- on_hull
            combinations[[i]]$perpendicularity_score <- perpendicularity_score
            combinations[[i]]$face_area <- face_area
            combinations[[i]]$depth_score <- avg_depth
            combinations[[i]]$corner_score <- avg_corner
            combinations[[i]]$edge_length <- edge_length
      }

      return(combinations)
}

# Select best edge-face combination with weighted scoring
select_best_edge_face_combination <- function(scored_combinations, weights = c(0.5, 0.3, 0.2)) {
      if (length(scored_combinations) == 0) return(NULL)

      # Validate and normalize weights
      if (length(weights) != 3) {
            stop("weights must be a vector of length 3: c(perpendicularity, length, area)")
      }
      if (any(weights < 0)) {
            stop("All weights must be non-negative")
      }

      # Normalize weights to sum to 1
      weights <- weights / sum(weights)
      perp_weight <- weights[1]
      length_weight <- weights[2]
      area_weight <- weights[3]

      # STEP 1: Do any combinations have hull edges?
      hull_combinations <- scored_combinations[sapply(scored_combinations, function(c) c$on_hull)]
      combinations <- if(length(hull_combinations) > 0) hull_combinations else scored_combinations

      # STEP 2: Calculate weighted scores with three factors
      # Find max values for normalization
      max_length <- max(sapply(combinations, function(c) c$edge_length))
      max_area <- max(sapply(combinations, function(c) c$face_area))

      for(i in seq_along(combinations)) {
            c <- combinations[[i]]

            # Normalize to 0-1 range
            norm_perp <- c$perpendicularity_score / 90
            norm_length <- c$edge_length / max_length
            norm_area <- c$face_area / max_area

            # Multiplicative scoring - all factors must be decent
            c$combined_score <- (norm_perp^perp_weight) * (norm_length^length_weight) * (norm_area^area_weight)

            combinations[[i]] <- c
      }

      # STEP 3: Sort by combined score (highest first), then depth, then corner as tiebreakers
      combo_data <- data.frame(
            index = seq_along(combinations),
            combined_score = sapply(combinations, function(c) c$combined_score),
            depth = sapply(combinations, function(c) c$depth_score),
            corner = sapply(combinations, function(c) c$corner_score)
      )
      combo_data <- combo_data[order(-combo_data$combined_score, combo_data$depth, combo_data$corner), ]

      best_combination <- combinations[[combo_data$index[1]]]

      return(best_combination)
}

select_best_face_for_edge <- function(edge, visible_faces, proj) {
      candidate_faces <- intersect(edge$touching_faces, visible_faces)
      if (length(candidate_faces) <= 1) return(candidate_faces[1])

      face_scores <- list()

      for (face_name in candidate_faces) {
            # Get all 4 corners of this face
            face_corners <- get_face_corners(face_name)

            # Find the two corners that are NOT on the focal edge
            non_edge_corners <- face_corners[
                  !(abs(face_corners$x - edge$p1[1]) < 1e-10 &
                          abs(face_corners$y - edge$p1[2]) < 1e-10 &
                          abs(face_corners$z - edge$p1[3]) < 1e-10) &
                        !(abs(face_corners$x - edge$p2[1]) < 1e-10 &
                                abs(face_corners$y - edge$p2[2]) < 1e-10 &
                                abs(face_corners$z - edge$p2[3]) < 1e-10), ]

            # Transform corners to 2D for perpendicular distance
            corners_2d <- transform_3d_standard(non_edge_corners, proj)
            perpendicular_distance <- sqrt((corners_2d$x[1] - corners_2d$x[2])^2 +
                                                 (corners_2d$y[1] - corners_2d$y[2])^2)

            # Calculate existing tie-breaker scores
            face_corners_2d <- transform_3d_standard(face_corners, proj)
            avg_depth <- mean(face_corners_2d$z)
            avg_y <- mean(face_corners_2d$y)
            avg_x <- mean(face_corners_2d$x)

            face_scores[[face_name]] <- list(
                  name = face_name,
                  perpendicular_distance = perpendicular_distance,
                  depth = avg_depth,
                  y_pos = avg_y,
                  x_pos = avg_x
            )
      }

      # Convert to data frame and sort by: perpendicular distance (desc), depth (asc), y_pos (asc), x_pos (asc)
      score_df <- data.frame(
            name = names(face_scores),
            perpendicular_distance = sapply(face_scores, function(f) f$perpendicular_distance),
            depth = sapply(face_scores, function(f) f$depth),
            y_pos = sapply(face_scores, function(f) f$y_pos),
            x_pos = sapply(face_scores, function(f) f$x_pos)
      )

      score_df <- score_df[order(-score_df$perpendicular_distance, score_df$depth, score_df$y_pos, score_df$x_pos), ]

      return(as.character(score_df$name[1]))
}

select_axis_edge_and_face <- function(axis, visible_faces, proj, effective_ratios, weights = c(0.5, 0.3, 0.2)) {
      # Step 1: Enumerate all possible edges for this axis
      all_edges <- enumerate_axis_edges(axis)

      # Step 2: Create edge-face combinations
      edge_face_combinations <- list()
      for (edge in all_edges) {
            for (face in visible_faces) {
                  if (face %in% edge$touching_faces) {
                        edge_face_combinations[[length(edge_face_combinations) + 1]] <- list(
                              edge = edge,
                              face = face
                        )
                  }
            }
      }

      if (length(edge_face_combinations) == 0) {
            return(NULL)  # No valid combinations for this axis
      }

      # Step 3: Score each edge-face combination
      scored_combinations <- score_edge_face_combinations(edge_face_combinations, axis, proj, effective_ratios)

      # Step 4: Select best combination (with weighted scoring including area)
      best_combination <- select_best_edge_face_combination(scored_combinations, weights)

      if (is.null(best_combination)) {
            return(NULL)
      }

      return(list(
            axis = axis,
            edge = best_combination$edge,
            face = best_combination$face,
            edge_p1_2d = best_combination$edge$p1_2d,
            edge_p2_2d = best_combination$edge$p2_2d
      ))
}

get_axis_selection <- function(axis, self, panel_params, effective_ratios, weights = c(0.5, 0.3, 0.2)) {
      # Get the appropriate text parameter for this axis
      axis_override <- switch(axis,
                              "x" = self$xlabels,
                              "y" = self$ylabels,
                              "z" = self$zlabels)

      if (length(axis_override) == 1 && axis_override == "auto") {
            # Use automatic selection (needs effective_ratios for scoring)
            return(select_axis_edge_and_face(axis, panel_params$visible_faces, panel_params$proj, effective_ratios, weights))
      } else if (length(axis_override) == 2) {
            # Manual override (doesn't need effective_ratios)
            return(create_manual_axis_selection(axis, axis_override[1], axis_override[2],
                                                panel_params$proj, panel_params$visible_faces))
      } else {
            warning("Invalid ", axis, "text specification, using auto")
            return(select_axis_edge_and_face(axis, panel_params$visible_faces, panel_params$proj, effective_ratios, weights))
      }
}

create_manual_axis_selection <- function(axis, face1, face2, proj, visible_faces) {

      # Parse face names to get coordinate constraints
      parse_face <- function(face_name) {
            coord <- substr(face_name, 1, 1)  # "x", "y", or "z"
            value <- ifelse(substr(face_name, 2, 4) == "min", -0.5, 0.5)
            return(list(coord = coord, value = value))
      }

      f1 <- parse_face(face1)
      f2 <- parse_face(face2)

      # Verify faces are different and adjacent (share an edge)
      if (f1$coord == f2$coord) {
            stop("Faces ", face1, " and ", face2, " are parallel and don't share an edge")
      }

      # Find the shared edge: two coordinates are fixed, one varies
      # The varying coordinate should match the axis
      all_coords <- c("x", "y", "z")
      fixed_coords <- c(f1$coord, f2$coord)
      varying_coord <- setdiff(all_coords, fixed_coords)

      if (varying_coord != axis) {
            stop("Edge between ", face1, " and ", face2, " doesn't vary along ", axis, "-axis")
      }

      # Determine which face to use based on visibility
      face1_visible <- face1 %in% visible_faces
      face2_visible <- face2 %in% visible_faces

      if (face1_visible && face2_visible) {
            # Both visible - honor user's order (use face1)
            chosen_face <- face1
      } else if (face1_visible) {
            # Only face1 visible
            chosen_face <- face1
      } else if (face2_visible) {
            # Only face2 visible
            chosen_face <- face2
      } else {
            # Neither visible - warn and fall back to auto
            warning("Neither ", face1, " nor ", face2, " is visible, falling back to auto selection")
            return(select_axis_edge_and_face(axis, visible_faces, proj, c(1, 1, 1)))  # Use default ratios for fallback
      }

      # Create edge endpoints
      edge_coords <- list()
      edge_coords[[f1$coord]] <- f1$value
      edge_coords[[f2$coord]] <- f2$value

      # Edge varies along the axis coordinate
      p1 <- edge_coords
      p1[[axis]] <- -0.5
      p2 <- edge_coords
      p2[[axis]] <- 0.5

      # Create edge structure (similar to enumerate_axis_edges output)
      edge <- list(
            fixed_coords = setNames(c(edge_coords[[f1$coord]], edge_coords[[f2$coord]]),
                                    c(f1$coord, f2$coord)),
            touching_faces = c(face1, face2),
            p1 = c(p1$x, p1$y, p1$z),
            p2 = c(p2$x, p2$y, p2$z),
            axis = axis,
            edge_id = "manual"
      )

      # Transform to 2D for consistency with automatic selection
      edge_p1_3d <- data.frame(x = edge$p1[1], y = edge$p1[2], z = edge$p1[3])
      edge_p2_3d <- data.frame(x = edge$p2[1], y = edge$p2[2], z = edge$p2[3])
      edge_p1_2d <- transform_3d_standard(edge_p1_3d, proj)
      edge_p2_2d <- transform_3d_standard(edge_p2_3d, proj)

      # Return same structure as select_axis_edge_and_face
      return(list(
            axis = axis,
            edge = edge,
            face = chosen_face,
            edge_p1_2d = edge_p1_2d,
            edge_p2_2d = edge_p2_2d
      ))
}
