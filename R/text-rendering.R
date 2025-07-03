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

      # Define better defaults for 3D plots (in points)
      MIN_3D_TITLE_MARGIN_POINTS <- 10  # Larger than typical theme defaults (2-6 points)

      # Use the larger of: theme margin or our 3D minimum
      theme_margin_distance <- max(title_margin_numeric[1], title_margin_numeric[3])
      title_margin_distance <- max(theme_margin_distance, MIN_3D_TITLE_MARGIN_POINTS) * pt_to_plot_factor

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

# Helper function to calculate axis angle from gridlines
calculate_axis_angle <- function(axis_gridlines, axis_uses_start) {
      axis_gridlines_sorted <- axis_gridlines[order(axis_gridlines$break_value), ]
      first_gridline_group <- axis_gridlines[axis_gridlines$group == axis_gridlines_sorted$group[1], ]
      last_gridline_group <- axis_gridlines[axis_gridlines$group == axis_gridlines_sorted$group[nrow(axis_gridlines_sorted)], ]

      if (axis_uses_start) {
            axis_start_x <- first_gridline_group$x[1]
            axis_start_y <- first_gridline_group$y[1]
            axis_end_x <- last_gridline_group$x[1]
            axis_end_y <- last_gridline_group$y[1]
      } else {
            axis_start_x <- first_gridline_group$x[nrow(first_gridline_group)]
            axis_start_y <- first_gridline_group$y[nrow(first_gridline_group)]
            axis_end_x <- last_gridline_group$x[nrow(last_gridline_group)]
            axis_end_y <- last_gridline_group$y[nrow(last_gridline_group)]
      }

      axis_edge_dx <- axis_end_x - axis_start_x
      axis_edge_dy <- axis_end_y - axis_start_y
      axis_angle <- atan2(axis_edge_dy, axis_edge_dx)

      return(axis_angle)
}

# Helper function to calculate theta from axis angle and gridline data
calculate_theta <- function(axis_angle, gridline_data) {
      gridline_dx <- gridline_data$x[nrow(gridline_data)] - gridline_data$x[1]
      gridline_dy <- gridline_data$y[nrow(gridline_data)] - gridline_data$y[1]
      gridline_angle <- atan2(gridline_dy, gridline_dx)

      # Calculate corrected angle between axis edge and gridline in 2D projected space
      angle_diff <- axis_angle - gridline_angle
      while (angle_diff > pi) angle_diff <- angle_diff - 2*pi
      while (angle_diff < -pi) angle_diff <- angle_diff + 2*pi
      theta <- abs(angle_diff)
      if (theta > pi/2) theta <- pi - theta

      return(theta)
}

# Helper function to calculate trigonometric offset components
calculate_trigonometric_components <- function(theta, offsets, text_dimensions, theme_elements, plot_bounds) {
      # Calculate text and title dimensions
      plot_range <- max(plot_bounds[2] - plot_bounds[1], plot_bounds[4] - plot_bounds[3])

      text_height_plot <- text_dimensions$max_height * plot_range
      label_width_plot <- text_dimensions$max_width * plot_range

      safe_title_fontsize <- tryCatch({
            size_val <- theme_elements$axis_title$size %||% 11
            if (inherits(size_val, "unit")) as.numeric(size_val) else as.numeric(size_val)
      }, error = function(e) 11)

      title_height_plot <- safe_title_fontsize * plot_range / 400

      # Define systematic trigonometric components with safeguards
      min_sin_theta <- 0.1
      min_tan_theta <- 0.1

      a <- offsets$text_offset / max(sin(theta), min_sin_theta)  # theta-adjusted label clearance
      b <- text_height_plot / (2 * max(tan(theta), min_tan_theta))  # theta-adjusted text height component
      c <- label_width_plot  # label width
      d <- offsets$title_margin / max(sin(theta), min_sin_theta)  # theta-adjusted title margin
      e <- title_height_plot / (2 * max(sin(theta), min_sin_theta))  # theta-adjusted title height

      return(list(
            a = a, b = b, c = c, d = d, e = e,
            text_offset_total = a + b,
            title_offset_total = a + b + c + b + d + e
      ))
}

# Updated create_axis_labels function (working version)
create_axis_labels <- function(axis, edge_gridlines, theme_elements, offsets, text_dimensions,
                               panel_params, auto_text_orientation, plot_bounds, chosen_edge) {

      # Create text labels using the selected edge/face
      cube_center_3d <- data.frame(x = 0, y = 0, z = 0)
      cube_center_2d <- transform_3d_standard(cube_center_3d, panel_params$proj)

      # Use boundary information to determine which endpoints to use
      axis_uses_start <- determine_endpoint_preference_by_boundary(chosen_edge, edge_gridlines)

      # Calculate axis angle using helper function
      axis_angle <- calculate_axis_angle(edge_gridlines, axis_uses_start)

      # Create labels for each gridline group
      all_labels <- list()

      for (group_id in unique(edge_gridlines$group)) {
            gridline_data <- edge_gridlines[edge_gridlines$group == group_id, ]

            if (nrow(gridline_data) >= 2) {
                  # Use consistent endpoint choice based on boundary matching
                  if (axis_uses_start) {
                        label_x <- gridline_data$x[1]
                        label_y <- gridline_data$y[1]
                  } else {
                        label_x <- gridline_data$x[nrow(gridline_data)]
                        label_y <- gridline_data$y[nrow(gridline_data)]
                  }

                  # Calculate theta using helper function
                  theta <- calculate_theta(axis_angle, gridline_data)

                  # Calculate gridline angle for rotation (labels should be parallel to their gridline)
                  gridline_dx <- gridline_data$x[nrow(gridline_data)] - gridline_data$x[1]
                  gridline_dy <- gridline_data$y[nrow(gridline_data)] - gridline_data$y[1]
                  gridline_angle <- atan2(gridline_dy, gridline_dx)

                  # Calculate text offset using helper function
                  text_offset_total <- calculate_trigonometric_components(theta, offsets, text_dimensions, theme_elements, plot_bounds)$text_offset_total

                  # Apply offset away from gridline center
                  gridline_center_x <- mean(gridline_data$x)
                  gridline_center_y <- mean(gridline_data$y)

                  center_to_label_dx <- label_x - gridline_center_x
                  center_to_label_dy <- label_y - gridline_center_y
                  center_to_label_length <- sqrt(center_to_label_dx^2 + center_to_label_dy^2)

                  if (center_to_label_length > 0) {
                        label_x <- label_x + (center_to_label_dx / center_to_label_length) * text_offset_total
                        label_y <- label_y + (center_to_label_dy / center_to_label_length) * text_offset_total
                  }

                  # Calculate label rotation to be parallel to gridline
                  gridline_angle_degrees <- gridline_angle * 180 / pi

                  # Ensure readable orientation
                  if (abs(gridline_angle_degrees) > 90) {
                        gridline_angle_degrees <- gridline_angle_degrees + 180
                        if (gridline_angle_degrees > 180) gridline_angle_degrees <- gridline_angle_degrees - 360
                  }

                  # Calculate justification
                  condition1 <- label_x < gridline_center_x
                  condition2 <- abs(gridline_angle_degrees) <= 90
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
                              final_angle <- gridline_angle_degrees
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

# Updated create_axis_title function with all title logic
create_axis_title <- function(axis, edge_gridlines, theme_elements, offsets, text_dimensions,
                              panel_params, auto_text_orientation, plot_bounds, chosen_edge, axis_uses_start) {

      # Get axis name (title)
      axis_name <- panel_params$scale_info[[axis]]$name

      # Handle waiver and missing names
      if (is.null(axis_name) || inherits(axis_name, "waiver")) {
            axis_name <- axis
      }

      if (is.null(axis_name) || axis_name == "") {
            return(list())  # Return empty list if no title
      }

      # Find gridlines for this axis on the chosen face
      axis_gridlines <- edge_gridlines[edge_gridlines$break_axis == axis, ]

      if (nrow(axis_gridlines) == 0) {
            return(list())  # Return empty list if no gridlines
      }

      # Find the gridline closest to the center of the axis range
      axis_breaks <- panel_params$scale_info[[axis]]$breaks
      axis_center_value <- mean(range(axis_breaks))

      # Find which gridline is closest to the center value
      center_distances <- abs(axis_gridlines$break_value - axis_center_value)
      center_group <- axis_gridlines$group[which.min(center_distances)]
      center_gridline <- axis_gridlines[axis_gridlines$group == center_group, ]

      if (nrow(center_gridline) < 2) {
            return(list())  # Return empty list if insufficient gridline data
      }

      # Calculate axis angle using helper function (same as create_axis_labels)
      axis_angle <- calculate_axis_angle(axis_gridlines, axis_uses_start)

      # Calculate theta using helper function (same as create_axis_labels)
      theta <- calculate_theta(axis_angle, center_gridline)

      # Use the shared helper function to get components
      components <- calculate_trigonometric_components(theta, offsets, text_dimensions, theme_elements, plot_bounds)

      # For debugging, try just a + b first
      title_offset_total <- components$a + components$b  # Test with labels position
      # title_offset_total <- components$title_offset_total  # Full formula when ready

      # Get the label position for this center gridline (same logic as labels)
      if (axis_uses_start) {
            start_x <- center_gridline$x[1]
            start_y <- center_gridline$y[1]
      } else {
            start_x <- center_gridline$x[nrow(center_gridline)]
            start_y <- center_gridline$y[nrow(center_gridline)]
      }

      # Calculate direction from gridline center (same as labels)
      gridline_center_x <- mean(center_gridline$x)
      gridline_center_y <- mean(center_gridline$y)

      center_to_endpoint_dx <- start_x - gridline_center_x
      center_to_endpoint_dy <- start_y - gridline_center_y
      center_to_endpoint_length <- sqrt(center_to_endpoint_dx^2 + center_to_endpoint_dy^2)

      if (center_to_endpoint_length == 0) {
            return(list())  # Cannot calculate direction
      }

      # Normalize the direction (same as labels)
      unit_x <- center_to_endpoint_dx / center_to_endpoint_length
      unit_y <- center_to_endpoint_dy / center_to_endpoint_length

      # Calculate title position using shared components
      title_x_2d <- gridline_center_x + unit_x * title_offset_total
      title_y_2d <- gridline_center_y + unit_y * title_offset_total

      # Calculate title rotation using axis angle
      title_angle <- axis_angle * 180 / pi
      if (abs(title_angle) > 90) {
            title_angle <- title_angle + 180
            if (title_angle > 180) title_angle <- title_angle - 360
      }

      # Convert to NPC coordinates
      x_scaled <- (title_x_2d - plot_bounds[1]) / (plot_bounds[2] - plot_bounds[1])
      y_scaled <- (title_y_2d - plot_bounds[3]) / (plot_bounds[4] - plot_bounds[3])

      if (is.na(x_scaled) || is.na(y_scaled)) {
            return(list())  # Invalid coordinates
      }

      # Apply theme settings for angle and justification
      if (auto_text_orientation) {
            final_title_angle <- title_angle
            final_title_hjust <- theme_elements$axis_title$hjust %||% 0.5
            final_title_vjust <- theme_elements$axis_title$vjust %||% 0.5
      } else {
            final_title_angle <- theme_elements$axis_title$angle %||% 0
            final_title_hjust <- theme_elements$axis_title$hjust %||% 0.5
            final_title_vjust <- theme_elements$axis_title$vjust %||% 0.5
      }

      safe_title_fontsize <- tryCatch({
            size_val <- theme_elements$axis_title$size %||% 11
            if (inherits(size_val, "unit")) as.numeric(size_val) else as.numeric(size_val)
      }, error = function(e) 11)

      title_grob <- tryCatch({
            grid::textGrob(
                  label = as.character(axis_name),
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
            return(list(title_grob))
      } else {
            return(list())
      }
}

# Simplified render_axis_text function
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

                  # Get labels and breaks for the current axis
                  axis_labels <- panel_params$scale_info[[axis]]$labels %||% NULL
                  axis_breaks <- panel_params$scale_info[[axis]]$breaks %||% NULL

                  text_dimensions <- measure_axis_text_dimensions(edge_gridlines, theme_elements, axis_labels, axis_breaks)

                  # Create axis labels using parallel function
                  label_result <- create_axis_labels(axis, edge_gridlines, theme_elements, offsets, text_dimensions,
                                                     panel_params, self$auto_text_orientation, panel_params$plot_bounds, chosen_edge)

                  all_labels <- c(all_labels, label_result$labels)
                  axis_uses_start <- label_result$axis_uses_start

                  # Create axis titles using merged function
                  title_result <- create_axis_title(axis, edge_gridlines, theme_elements, offsets, text_dimensions,
                                                    panel_params, self$auto_text_orientation, panel_params$plot_bounds, chosen_edge, axis_uses_start)

                  all_titles <- c(all_titles, title_result)
            }

            return(list(labels = all_labels, titles = all_titles))
      }, error = function(e) {
            warning("Axis label/title rendering failed: ", e$message)
            return(list(labels = list(), titles = list()))
      })
}
