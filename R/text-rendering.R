extract_axis_theme_elements <- function(axis, theme) {
      axis_text_theme <- calc_element(paste0("axis.text.", axis), theme)
      axis_title_theme <- calc_element(paste0("axis.title.", axis), theme)
      parent_text_theme <- calc_element("axis.text", theme)
      parent_title_theme <- calc_element("axis.title", theme)
      default_theme <- theme_gray()
      default_axis_title <- calc_element(paste0("axis.title.", axis), default_theme)

      # Handle inheritance logic
      if (identical(axis_title_theme$margin, default_axis_title$margin)) {
            if (!is.null(parent_title_theme$margin)) {
                  axis_title_theme$margin <- parent_title_theme$margin
            }
      }

      return(list(
            axis_text = axis_text_theme,
            axis_title = axis_title_theme,
            parent_text = parent_text_theme
      ))
}

calculate_axis_offsets <- function(theme_elements, auto_text_orientation, pt_to_plot_factor) {
      parent_margin <- theme_elements$parent_text$margin %||% margin()
      axis_margin <- theme_elements$axis_text$margin %||% margin()

      parent_margin_numeric <- as.numeric(parent_margin)
      axis_margin_numeric <- as.numeric(axis_margin)

      has_custom_parent_margin <- any(parent_margin_numeric != 0)

      if (has_custom_parent_margin) {
            non_zero_values <- axis_margin_numeric[axis_margin_numeric != 0]
            looks_like_default <- length(non_zero_values) <= 1 && all(non_zero_values <= 2.2)

            if (looks_like_default) {
                  margin_t <- parent_margin_numeric[1]
                  margin_r <- parent_margin_numeric[2]
                  margin_b <- parent_margin_numeric[3]
                  margin_l <- parent_margin_numeric[4]
            } else {
                  margin_t <- axis_margin_numeric[1]
                  margin_r <- axis_margin_numeric[2]
                  margin_b <- axis_margin_numeric[3]
                  margin_l <- axis_margin_numeric[4]
            }
      } else {
            margin_t <- axis_margin_numeric[1]
            margin_r <- axis_margin_numeric[2]
            margin_b <- axis_margin_numeric[3]
            margin_l <- axis_margin_numeric[4]
      }

      # Calculate offsets
      if (auto_text_orientation) {
            base_offset <- max(margin_l, margin_r)
      } else {
            base_offset <- max(margin_t, margin_r, margin_b, margin_l)
      }
      MIN_TEXT_OFFSET_POINTS <- 5.5
      text_offset_distance <- max(base_offset, MIN_TEXT_OFFSET_POINTS) * pt_to_plot_factor

      title_margin <- theme_elements$axis_title$margin %||% margin()
      title_margin_numeric <- as.numeric(title_margin)

      title_margin_distance <- if (all(title_margin_numeric == 0)) {
            MIN_TEXT_OFFSET_POINTS * pt_to_plot_factor
      } else {
            max(title_margin_numeric[1], title_margin_numeric[3]) * pt_to_plot_factor
      }

      return(list(
            text_offset = text_offset_distance,
            title_margin = title_margin_distance
      ))
}

measure_axis_text_dimensions <- function(edge_gridlines, theme_elements, axis_labels = NULL, axis_breaks = NULL) {
      # Validate inputs
      if (is.null(edge_gridlines) || nrow(edge_gridlines) == 0) {
            return(list(fontsize = 8.5, max_width = 0.05, max_height = 0.02))
      }

      if (!"break_value" %in% names(edge_gridlines)) {
            return(list(fontsize = 8.5, max_width = 0.05, max_height = 0.02))
      }

      tryCatch({
            # Calculate text fontsize
            text_fontsize <- tryCatch({
                  size_val <- theme_elements$axis_text$size %||% 8.5
                  if (inherits(size_val, "unit")) as.numeric(size_val) else as.numeric(size_val)
            }, error = function(e) 8.5)

            # Ensure valid fontsize
            if (!is.finite(text_fontsize) || text_fontsize <= 0) {
                  text_fontsize <- 8.5
            }

            # Find the longest actual label text for this axis
            longest_label <- ""
            max_chars <- 0
            unique_groups <- unique(edge_gridlines$group)

            if (length(unique_groups) == 0) {
                  return(list(fontsize = text_fontsize, max_width = 0.05, max_height = 0.02))
            }

            for (group_id in unique_groups) {
                  gridline_data <- edge_gridlines[edge_gridlines$group == group_id, ]
                  if (nrow(gridline_data) >= 1) {
                        # NEW: Use custom labels if available
                        if (!is.null(axis_labels) && !is.null(axis_breaks)) {
                              # Find which break this gridline corresponds to
                              break_value <- gridline_data$break_value[1]
                              break_index <- which.min(abs(axis_breaks - break_value))

                              if (length(break_index) > 0 && break_index <= length(axis_labels)) {
                                    label_text <- as.character(axis_labels[break_index])
                              } else {
                                    # Fallback to break value
                                    label_text <- as.character(break_value)
                              }
                        } else {
                              # Fallback to break value
                              label_text <- as.character(gridline_data$break_value[1])
                        }

                        if (!is.na(label_text) && nchar(label_text) > max_chars) {
                              max_chars <- nchar(label_text)
                              longest_label <- label_text
                        }
                  }
            }

            # Measure the actual text size using grid functions
            max_text_width <- 0.05  # Default fallback
            max_text_height <- 0.02  # Default fallback

            if (max_chars > 0 && longest_label != "") {
                  sample_grob <- tryCatch({
                        grid::textGrob(
                              label = longest_label,
                              gp = grid::gpar(
                                    fontsize = text_fontsize,
                                    fontfamily = theme_elements$axis_text$family %||% "",
                                    fontface = theme_elements$axis_text$face %||% "plain"
                              )
                        )
                  }, error = function(e) {
                        return(NULL)
                  })

                  if (!is.null(sample_grob)) {
                        tryCatch({
                              # Measure actual dimensions in normalized coordinates (0-1)
                              max_text_width <- grid::convertWidth(grid::grobWidth(sample_grob), "npc", valueOnly = TRUE)
                              max_text_height <- grid::convertHeight(grid::grobHeight(sample_grob), "npc", valueOnly = TRUE)

                              # Validate measurements
                              if (!is.finite(max_text_width) || max_text_width <= 0) max_text_width <- 0.05
                              if (!is.finite(max_text_height) || max_text_height <= 0) max_text_height <- 0.02

                        }, error = function(e) {
                              # Keep defaults if measurement fails
                              max_text_width <- 0.05
                              max_text_height <- 0.02
                        })
                  }
            }

            return(list(
                  fontsize = text_fontsize,
                  max_width = max_text_width,
                  max_height = max_text_height
            ))

      }, error = function(e) {
            warning("Error measuring text dimensions: ", e$message)
            return(list(fontsize = 8.5, max_width = 0.05, max_height = 0.02))
      })
}

create_axis_title <- function(axis, edge, scale_info) {
      # Get axis name (title)
      axis_name <- scale_info[[axis]]$name

      # Handle waiver and missing names
      if (is.null(axis_name) || inherits(axis_name, "waiver")) {
            axis_name <- axis
      }

      if (is.null(axis_name) || axis_name == "") {
            return(NULL)
      }

      # Create title position at center of edge
      title_df <- data.frame(
            x = if (axis == "x") 0 else edge$coords[["x"]],  # Center of axis range
            y = if (axis == "y") 0 else edge$coords[["y"]],
            z = if (axis == "z") 0 else edge$coords[["z"]],
            label = as.character(axis_name),
            axis = axis,
            stringsAsFactors = FALSE
      )

      return(title_df)
}

create_axis_labels <- function(axis, edge_gridlines, theme_elements, offsets, text_dimensions,
                               panel_params, auto_text_orientation, plot_bounds, chosen_edge) {

      # Create text labels using the selected edge/face
      cube_center_3d <- data.frame(x = 0, y = 0, z = 0)
      cube_center_2d <- transform_3d_standard(cube_center_3d, panel_params$proj)

      # Transform chosen edge center to 2D
      edge_center_3d <- data.frame(
            x = (chosen_edge$p1[1] + chosen_edge$p2[1]) / 2,
            y = (chosen_edge$p1[2] + chosen_edge$p2[2]) / 2,
            z = (chosen_edge$p1[3] + chosen_edge$p2[3]) / 2
      )
      edge_center_2d <- transform_3d_standard(edge_center_3d, panel_params$proj)

      # For each gridline, pick the endpoint closer to the chosen edge
      use_start_endpoint <- 0
      use_end_endpoint <- 0

      for (group_id in unique(edge_gridlines$group)) {
            gridline_data <- edge_gridlines[edge_gridlines$group == group_id, ]
            if (nrow(gridline_data) >= 2) {
                  # Distance from first endpoint to chosen edge
                  dist_to_start <- sqrt((gridline_data$x[1] - edge_center_2d$x)^2 +
                                              (gridline_data$y[1] - edge_center_2d$y)^2)
                  # Distance from last endpoint to chosen edge
                  dist_to_end <- sqrt((gridline_data$x[nrow(gridline_data)] - edge_center_2d$x)^2 +
                                            (gridline_data$y[nrow(gridline_data)] - edge_center_2d$y)^2)

                  if (dist_to_start < dist_to_end) {
                        use_start_endpoint <- use_start_endpoint + 1
                  } else {
                        use_end_endpoint <- use_end_endpoint + 1
                  }
            }
      }
      axis_uses_start <- use_start_endpoint >= use_end_endpoint

      # Create labels for each gridline group
      all_labels <- list()
      for (group_id in unique(edge_gridlines$group)) {
            gridline_data <- edge_gridlines[edge_gridlines$group == group_id, ]

            if (nrow(gridline_data) >= 2) {
                  # Use consistent endpoint choice
                  if (axis_uses_start) {
                        label_x <- gridline_data$x[1]
                        label_y <- gridline_data$y[1]
                  } else {
                        label_x <- gridline_data$x[nrow(gridline_data)]
                        label_y <- gridline_data$y[nrow(gridline_data)]
                  }

                  # Apply offset away from gridline center
                  gridline_center_x <- mean(gridline_data$x)
                  gridline_center_y <- mean(gridline_data$y)

                  center_to_label_dx <- label_x - gridline_center_x
                  center_to_label_dy <- label_y - gridline_center_y
                  center_to_label_length <- sqrt(center_to_label_dx^2 + center_to_label_dy^2)

                  if (center_to_label_length > 0) {
                        label_x <- label_x + (center_to_label_dx / center_to_label_length) * offsets$text_offset
                        label_y <- label_y + (center_to_label_dy / center_to_label_length) * offsets$text_offset
                  }

                  # Calculate label rotation to be parallel to gridline
                  gridline_dx <- gridline_data$x[nrow(gridline_data)] - gridline_data$x[1]
                  gridline_dy <- gridline_data$y[nrow(gridline_data)] - gridline_data$y[1]
                  gridline_angle <- atan2(gridline_dy, gridline_dx) * 180 / pi

                  # Ensure readable orientation
                  if (abs(gridline_angle) > 90) {
                        gridline_angle <- gridline_angle + 180
                        if (gridline_angle > 180) gridline_angle <- gridline_angle - 360
                  }

                  # Calculate justification
                  condition1 <- label_x < gridline_center_x
                  condition2 <- abs(gridline_angle) <= 90
                  hjust <- if (condition1 == condition2) 1 else 0

                  # Get the custom labels if available
                  axis_labels <- panel_params$scale_info[[axis]]$labels %||% NULL
                  axis_breaks <- panel_params$scale_info[[axis]]$breaks %||% NULL

                  if (!is.null(axis_labels) && !is.null(axis_breaks)) {
                        # Find which break this gridline corresponds to
                        break_value <- gridline_data$break_value[1]
                        break_index <- which.min(abs(axis_breaks - break_value))

                        if (length(break_index) > 0 && break_index <= length(axis_labels)) {
                              label_text <- as.character(axis_labels[break_index])
                        } else {
                              # Fallback to break value
                              label_text <- as.character(break_value)
                        }
                  } else {
                        # Fallback to break value
                        label_text <- as.character(gridline_data$break_value[1])
                  }

                  x_scaled <- (label_x - plot_bounds[1]) / (plot_bounds[2] - plot_bounds[1])
                  y_scaled <- (label_y - plot_bounds[3]) / (plot_bounds[4] - plot_bounds[3])

                  if (!is.na(x_scaled) && !is.na(y_scaled)) {
                        if (auto_text_orientation) {
                              final_angle <- gridline_angle
                              final_hjust <- hjust
                              final_vjust <- 0.5
                        } else {
                              final_angle <- theme_elements$axis_text$angle %||% 0
                              final_hjust <- theme_elements$axis_text$hjust %||% 0.5
                              final_vjust <- theme_elements$axis_text$vjust %||% 0.5
                        }

                        label_grob <- tryCatch({
                              grid::textGrob(
                                    label = as.character(label_text),
                                    x = as.numeric(x_scaled),
                                    y = as.numeric(y_scaled),
                                    hjust = as.numeric(final_hjust),
                                    vjust = as.numeric(final_vjust),
                                    rot = as.numeric(final_angle),
                                    default.units = "npc",
                                    gp = grid::gpar(
                                          fontsize = text_dimensions$fontsize,
                                          col = theme_elements$axis_text$colour %||% "black",
                                          fontfamily = theme_elements$axis_text$family %||% "",
                                          fontface = theme_elements$axis_text$face %||% "plain"
                                    )
                              )
                        }, error = function(e) {
                              NULL
                        })

                        if (!is.null(label_grob)) {
                              all_labels[[length(all_labels) + 1]] <- label_grob
                        }
                  }
            }
      }

      return(list(labels = all_labels,
                  axis_uses_start = axis_uses_start,
                  cube_center_2d = cube_center_2d))
}

# all labels and titles
render_axis_text <- function(self, panel_params, theme) {
      tryCatch({
            all_labels <- list()
            all_titles <- list()

            # Calculate base conversion factors for points to plot units
            plot_width <- panel_params$plot_bounds[2] - panel_params$plot_bounds[1]
            plot_height <- panel_params$plot_bounds[4] - panel_params$plot_bounds[3]
            pt_to_plot_factor <- max(plot_width, plot_height) / 400

            for (axis in c("x", "y", "z")) {

                  # Get edge + face
                  axis_selection <- get_axis_selection(axis, self, panel_params)

                  # Skip this axis if no valid edge/face combination
                  if (is.null(axis_selection)) {
                        next
                  }

                  chosen_edge <- axis_selection$edge
                  chosen_face <- axis_selection$face

                  theme_elements <- extract_axis_theme_elements(axis, theme)

                  offsets <- calculate_axis_offsets(theme_elements, self$auto_text_orientation, pt_to_plot_factor)

                  # Get gridlines for the chosen face
                  edge_gridlines <- panel_params$grid_transformed[
                        panel_params$grid_transformed$face == chosen_face &
                              panel_params$grid_transformed$break_axis == axis, ]

                  # Skip if no gridlines found for this axis
                  if (nrow(edge_gridlines) == 0) next

                  # Get labels and breaks for the current axis (works for x, y, or z)
                  axis_labels <- panel_params$scale_info[[axis]]$labels %||% NULL
                  axis_breaks <- panel_params$scale_info[[axis]]$breaks %||% NULL

                  text_dimensions <- measure_axis_text_dimensions(edge_gridlines, theme_elements, axis_labels, axis_breaks)

                  # Calculate plot range for coordinate conversion
                  plot_range <- pt_to_plot_factor * 400

                  # Axis labels ---------
                  label_result <- create_axis_labels(axis, edge_gridlines, theme_elements, offsets, text_dimensions,
                                                     panel_params, self$auto_text_orientation, panel_params$plot_bounds, chosen_edge)

                  all_labels <- c(all_labels, label_result$labels)
                  axis_uses_start <- label_result$axis_uses_start
                  cube_center_2d <- label_result$cube_center_2d

                  # Use the SAME endpoint selection logic as text labels
                  title_edge_points <- list()

                  for (group_id in unique(edge_gridlines$group)) {
                        gridline_data <- edge_gridlines[edge_gridlines$group == group_id, ]
                        if (nrow(gridline_data) >= 2) {
                              if (axis_uses_start) {
                                    title_edge_points[[length(title_edge_points) + 1]] <- c(gridline_data$x[1], gridline_data$y[1])
                              } else {
                                    title_edge_points[[length(title_edge_points) + 1]] <- c(gridline_data$x[nrow(gridline_data)], gridline_data$y[nrow(gridline_data)])
                              }
                        }
                  }

                  # Axis title -------------
                  axis_title <- create_axis_title(axis, list(coords = chosen_edge$fixed_coords), panel_params$scale_info)

                  # Calculate complete title offset distance
                  title_offset_distance <- offsets$text_offset + ((text_dimensions$max_width + text_dimensions$max_height) * plot_range) + offsets$title_margin

                  if (!is.null(axis_title) && nrow(axis_title) > 0) {

                        # Calculate title position and rotation
                        if (length(title_edge_points) > 0) {
                              # Use actual label positions for both position and rotation (most reliable)
                              title_edge_matrix <- do.call(rbind, title_edge_points)

                              # Position: center of where labels are actually placed
                              edge_center_2d_x <- mean(title_edge_matrix[, 1])
                              edge_center_2d_y <- mean(title_edge_matrix[, 2])

                              # Rotation: direction between actual label positions
                              if (nrow(title_edge_matrix) >= 2) {
                                    # Sort points and use direction between first and last label positions
                                    if (axis == "x") {
                                          sorted_indices <- order(title_edge_matrix[, 1])
                                    } else if (axis == "y") {
                                          sorted_indices <- order(title_edge_matrix[, 2])
                                    } else { # axis == "z"
                                          # For z-axis, sort by whichever dimension has more variation
                                          x_var <- var(title_edge_matrix[, 1])
                                          y_var <- var(title_edge_matrix[, 2])
                                          if (x_var > y_var) {
                                                sorted_indices <- order(title_edge_matrix[, 1])
                                          } else {
                                                sorted_indices <- order(title_edge_matrix[, 2])
                                          }
                                    }

                                    first_point <- title_edge_matrix[sorted_indices[1], ]
                                    last_point <- title_edge_matrix[sorted_indices[length(sorted_indices)], ]

                                    edge_dx <- last_point[1] - first_point[1]
                                    edge_dy <- last_point[2] - first_point[2]
                              } else {
                                    # Single label - use geometric edge direction for rotation
                                    edge_p1_3d <- data.frame(x = chosen_edge$p1[1], y = chosen_edge$p1[2], z = chosen_edge$p1[3])
                                    edge_p2_3d <- data.frame(x = chosen_edge$p2[1], y = chosen_edge$p2[2], z = chosen_edge$p2[3])
                                    edge_p1_2d <- transform_3d_standard(edge_p1_3d, panel_params$proj)
                                    edge_p2_2d <- transform_3d_standard(edge_p2_3d, panel_params$proj)

                                    edge_dx <- edge_p2_2d$x - edge_p1_2d$x
                                    edge_dy <- edge_p2_2d$y - edge_p1_2d$y
                              }
                        } else {
                              # No labels - use geometric edge for both position and rotation
                              edge_p1_3d <- data.frame(x = chosen_edge$p1[1], y = chosen_edge$p1[2], z = chosen_edge$p1[3])
                              edge_p2_3d <- data.frame(x = chosen_edge$p2[1], y = chosen_edge$p2[2], z = chosen_edge$p2[3])
                              edge_p1_2d <- transform_3d_standard(edge_p1_3d, panel_params$proj)
                              edge_p2_2d <- transform_3d_standard(edge_p2_3d, panel_params$proj)

                              # Position: geometric center of chosen edge
                              edge_center_2d_x <- (edge_p1_2d$x + edge_p2_2d$x) / 2
                              edge_center_2d_y <- (edge_p1_2d$y + edge_p2_2d$y) / 2

                              # Rotation: geometric edge direction
                              edge_dx <- edge_p2_2d$x - edge_p1_2d$x
                              edge_dy <- edge_p2_2d$y - edge_p1_2d$y
                        }

                        title_angle <- atan2(edge_dy, edge_dx) * 180 / pi
                        if (abs(title_angle) > 90) {
                              title_angle <- title_angle + 180
                              if (title_angle > 180) title_angle <- title_angle - 360
                        }

                        # Calculate offset direction (perpendicular to edge, away from cube center)
                        cube_center_3d <- data.frame(x = 0, y = 0, z = 0)
                        cube_center_2d <- transform_3d_standard(cube_center_3d, panel_params$proj)

                        # Get perpendicular direction to the edge (rotate edge direction by 90 degrees)
                        edge_length <- sqrt(edge_dx^2 + edge_dy^2)
                        if (edge_length > 0) {
                              # Two possible perpendicular directions
                              perp1_dx <- -edge_dy / edge_length
                              perp1_dy <- edge_dx / edge_length
                              perp2_dx <- edge_dy / edge_length
                              perp2_dy <- -edge_dx / edge_length

                              # Choose the perpendicular direction that points away from cube center
                              to_center_dx <- cube_center_2d$x - edge_center_2d_x
                              to_center_dy <- cube_center_2d$y - edge_center_2d_y

                              # Pick the perpendicular direction that is most opposite to the "to center" direction
                              dot1 <- perp1_dx * to_center_dx + perp1_dy * to_center_dy
                              dot2 <- perp2_dx * to_center_dx + perp2_dy * to_center_dy

                              if (dot1 < dot2) {
                                    # perp1 points more away from center
                                    offset_dx <- perp1_dx
                                    offset_dy <- perp1_dy
                              } else {
                                    # perp2 points more away from center
                                    offset_dx <- perp2_dx
                                    offset_dy <- perp2_dy
                              }
                        } else {
                              # Fallback if edge has zero length
                              offset_dx <- 0
                              offset_dy <- -1  # Default to pointing up
                        }

                        # Convert current position to NPC coordinates
                        current_x_scaled <- (edge_center_2d_x - panel_params$plot_bounds[1]) /
                              (panel_params$plot_bounds[2] - panel_params$plot_bounds[1])
                        current_y_scaled <- (edge_center_2d_y - panel_params$plot_bounds[3]) /
                              (panel_params$plot_bounds[4] - panel_params$plot_bounds[3])

                        # Convert offset components to NPC coordinates
                        plot_range <- max(panel_params$plot_bounds[2] - panel_params$plot_bounds[1],
                                          panel_params$plot_bounds[4] - panel_params$plot_bounds[3])
                        text_offset_npc <- offsets$text_offset / plot_range
                        text_size_npc <- text_dimensions$max_width + text_dimensions$max_height
                        margin_npc <- offsets$title_margin / plot_range
                        title_height_npc <- text_dimensions$max_height

                        DEVICE_TO_DATA_RATIO <- 1.5
                        total_offset_npc <- text_offset_npc +
                              text_size_npc * DEVICE_TO_DATA_RATIO +
                              margin_npc +
                              title_height_npc * DEVICE_TO_DATA_RATIO

                        # Apply offset in the chosen direction
                        x_scaled <- current_x_scaled + offset_dx * total_offset_npc
                        y_scaled <- current_y_scaled + offset_dy * total_offset_npc

                        # Apply theme settings for angle and justification
                        if (self$auto_text_orientation) {
                              final_title_angle <- title_angle
                              final_title_hjust <- theme_elements$axis_title$hjust %||% 0.5
                              final_title_vjust <- theme_elements$axis_title$vjust %||% 0.5
                        } else {
                              final_title_angle <- theme_elements$axis_title$angle %||% 0
                              final_title_hjust <- theme_elements$axis_title$hjust %||% 0.5
                              final_title_vjust <- theme_elements$axis_title$vjust %||% 0.5
                        }

                        if (!is.na(x_scaled) && !is.na(y_scaled)) {
                              safe_title_fontsize <- tryCatch({
                                    size_val <- theme_elements$axis_title$size %||% 11
                                    if (inherits(size_val, "unit")) as.numeric(size_val) else as.numeric(size_val)
                              }, error = function(e) 11)

                              title_grob <- tryCatch({
                                    grid::textGrob(
                                          label = as.character(axis_title$label[1]),
                                          x = as.numeric(x_scaled),
                                          y = as.numeric(y_scaled),
                                          hjust = as.numeric(final_title_hjust),
                                          vjust = as.numeric(final_title_vjust),
                                          rot = as.numeric(final_title_angle),
                                          default.units = "npc",
                                          gp = grid::gpar(
                                                fontsize = safe_title_fontsize,
                                                col = theme_elements$axis_title$colour %||% "black",
                                                fontfamily = theme_elements$axis_title$family %||% "",
                                                fontface = theme_elements$axis_title$face %||% "plain"
                                          )
                                    )
                              }, error = function(e) {
                                    NULL
                              })

                              if (!is.null(title_grob)) {
                                    all_titles[[length(all_titles) + 1]] <- title_grob
                              }
                        }
                  }
            }

            return(list(labels = all_labels, titles = all_titles))
      }, error = function(e) {
            warning("Axis label/title rendering failed: ", e$message)
            return(list(labels = list(), titles = list()))
      })
}
