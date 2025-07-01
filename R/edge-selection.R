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

score_edges <- function(edges, proj) {
      if (length(edges) == 0) return(list())

      # Get convex hull of all 8 cube corners for reference
      all_corners <- expand.grid(x = c(-0.5, 0.5), y = c(-0.5, 0.5), z = c(-0.5, 0.5))
      all_corners_2d <- transform_3d_standard(all_corners, proj)
      hull_indices <- chull(all_corners_2d$x, all_corners_2d$y)
      hull_points <- all_corners_2d[hull_indices, ]

      # Score each edge
      for (i in seq_along(edges)) {
            edge <- edges[[i]]

            # Transform edge endpoints to 2D
            p1_df <- data.frame(x = edge$p1[1], y = edge$p1[2], z = edge$p1[3])
            p2_df <- data.frame(x = edge$p2[1], y = edge$p2[2], z = edge$p2[3])
            p1_2d <- transform_3d_standard(p1_df, proj)
            p2_2d <- transform_3d_standard(p2_df, proj)

            # 1. Convex hull membership
            # An edge is on the hull only if its endpoitns are consecutive vertices in the hull
            # Note: chull() returns indices in counter-clockwise order, so consecutive
            # indices represent adjacent vertices on the hull boundary
            hull_vertices <- all_corners_2d[hull_indices, ]
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

            # 2. Depth score (average z of endpoints - lower is better/closer)
            avg_depth <- (p1_2d$z + p2_2d$z) / 2

            # 3. Corner score (average x+y of endpoints - lower is better/more bottom-left)
            avg_corner <- ((p1_2d$x + p1_2d$y) + (p2_2d$x + p2_2d$y)) / 2

            # Store scores
            edges[[i]]$on_hull <- on_hull
            edges[[i]]$depth_score <- avg_depth
            edges[[i]]$corner_score <- avg_corner
            edges[[i]]$edge_length <- sqrt((p2_2d$x - p1_2d$x)^2 + (p2_2d$y - p1_2d$y)^2)
            edges[[i]]$p1_2d <- p1_2d
            edges[[i]]$p2_2d <- p2_2d
      }

      return(edges)
}

select_best_edge <- function(scored_edges) {
      if (length(scored_edges) == 0) return(NULL)

      # STEP 1: Do any edges lie on the convex hull?
      hull_edges <- scored_edges[sapply(scored_edges, function(e) e$on_hull)]
      edges <- if(length(hull_edges) > 0) hull_edges else scored_edges

      # STEP 2: Among edges, sort by length (desc), then depth (asc), then corner (asc)
      edge_data <- data.frame(
            index = seq_along(edges),
            length = sapply(edges, function(e) e$edge_length),
            depth = sapply(edges, function(e) e$depth_score),
            corner = sapply(edges, function(e) e$corner_score)
      )
      edge_data <- edge_data[order(-edge_data$length, edge_data$depth, edge_data$corner), ]
      best_edge <- edges[[edge_data$index[1]]]
      return(best_edge)
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


select_axis_edge_and_face <- function(axis, visible_faces, proj) {
      # Step 1: Enumerate all possible edges for this axis
      all_edges <- enumerate_axis_edges(axis)

      # Step 2: Filter by visible faces
      visible_edges <- filter_edges_by_visible_faces(all_edges, visible_faces)

      if (length(visible_edges) == 0) {
            return(NULL)  # No valid edges for this axis
      }

      # Step 3: Score edges
      scored_edges <- score_edges(visible_edges, proj)

      # Step 4: Select best edge
      best_edge <- select_best_edge(scored_edges)

      if (is.null(best_edge)) {
            return(NULL)
      }

      # Step 5: Select best face for the chosen edge
      best_face <- select_best_face_for_edge(best_edge, visible_faces, proj)

      if (is.null(best_face)) {
            return(NULL)
      }

      return(list(
            axis = axis,
            edge = best_edge,
            face = best_face,
            edge_p1_2d = best_edge$p1_2d,
            edge_p2_2d = best_edge$p2_2d
      ))
}


get_axis_selection <- function(axis, self, panel_params) {
      # Get the appropriate text parameter for this axis
      axis_override <- switch(axis,
                              "x" = self$xtext,
                              "y" = self$ytext,
                              "z" = self$ztext)

      if (length(axis_override) == 1 && axis_override == "auto") {
            # Use automatic selection
            return(select_axis_edge_and_face(axis, panel_params$visible_faces, panel_params$proj))
      } else if (length(axis_override) == 2) {
            # Manual override: c("face1", "face2") -> use edge shared by these faces
            return(create_manual_axis_selection(axis, axis_override[1], axis_override[2],
                                                panel_params$proj, panel_params$visible_faces))
      } else {
            warning("Invalid ", axis, "text specification, using auto")
            return(select_axis_edge_and_face(axis, panel_params$visible_faces, panel_params$proj))
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
            return(select_axis_edge_and_face(axis, visible_faces, proj))
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

