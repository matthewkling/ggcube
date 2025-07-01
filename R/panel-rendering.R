

#' Generate grid data using actual scale breaks
#'
#' @param visible_faces Character vector of visible face names
#' @param scale_info List containing limits and breaks for x, y, z
#' @return Data frame with grid lines in standard domain, including break values
make_scale_grid <- function(visible_faces, scale_info) {
      # Transform real-world breaks to standard domain [-0.5, 0.5]

      x_breaks_std <- scale_to_standard(scale_info$x$breaks, scale_info$x$limits)
      y_breaks_std <- scale_to_standard(scale_info$y$breaks, scale_info$y$limits)
      z_breaks_std <- scale_to_standard(scale_info$z$breaks, scale_info$z$limits)

      # Full extents in standard domain (always -0.5 to 0.5)
      x_extent_std <- c(-0.5, 0.5)
      y_extent_std <- c(-0.5, 0.5)
      z_extent_std <- c(-0.5, 0.5)

      grid_data <- list()
      group_id <- 1

      # Helper function to create a grid line with break information
      create_line <- function(x, y, z, group_id, face, break_value, break_axis) {
            data.frame(x = x, y = y, z = z, group = group_id, face = face,
                       break_value = break_value, break_axis = break_axis)
      }

      # Generate grid lines for each visible face using actual breaks
      if ("xmin" %in% visible_faces) {
            x_val <- -0.5
            # Lines marking Y breaks (extend across full Z range)
            for (i in seq_along(y_breaks_std)) {
                  y_val <- y_breaks_std[i]
                  y_break <- scale_info$y$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(rep(x_val, 2), rep(y_val, 2), z_extent_std, group_id, "xmin", y_break, "y")
                  group_id <- group_id + 1
            }
            # Lines marking Z breaks (extend across full Y range)
            for (i in seq_along(z_breaks_std)) {
                  z_val <- z_breaks_std[i]
                  z_break <- scale_info$z$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(rep(x_val, 2), y_extent_std, rep(z_val, 2), group_id, "xmin", z_break, "z")
                  group_id <- group_id + 1
            }
      }

      if ("xmax" %in% visible_faces) {
            x_val <- 0.5
            # Lines marking Y breaks (extend across full Z range)
            for (i in seq_along(y_breaks_std)) {
                  y_val <- y_breaks_std[i]
                  y_break <- scale_info$y$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(rep(x_val, 2), rep(y_val, 2), z_extent_std, group_id, "xmax", y_break, "y")
                  group_id <- group_id + 1
            }
            # Lines marking Z breaks (extend across full Y range)
            for (i in seq_along(z_breaks_std)) {
                  z_val <- z_breaks_std[i]
                  z_break <- scale_info$z$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(rep(x_val, 2), y_extent_std, rep(z_val, 2), group_id, "xmax", z_break, "z")
                  group_id <- group_id + 1
            }
      }

      if ("ymin" %in% visible_faces) {
            y_val <- -0.5
            # Lines marking X breaks (extend across full Z range)
            for (i in seq_along(x_breaks_std)) {
                  x_val <- x_breaks_std[i]
                  x_break <- scale_info$x$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(rep(x_val, 2), rep(y_val, 2), z_extent_std, group_id, "ymin", x_break, "x")
                  group_id <- group_id + 1
            }
            # Lines marking Z breaks (extend across full X range)
            for (i in seq_along(z_breaks_std)) {
                  z_val <- z_breaks_std[i]
                  z_break <- scale_info$z$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(x_extent_std, rep(y_val, 2), rep(z_val, 2), group_id, "ymin", z_break, "z")
                  group_id <- group_id + 1
            }
      }

      if ("ymax" %in% visible_faces) {
            y_val <- 0.5
            # Lines marking X breaks (extend across full Z range)
            for (i in seq_along(x_breaks_std)) {
                  x_val <- x_breaks_std[i]
                  x_break <- scale_info$x$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(rep(x_val, 2), rep(y_val, 2), z_extent_std, group_id, "ymax", x_break, "x")
                  group_id <- group_id + 1
            }
            # Lines marking Z breaks (extend across full X range)
            for (i in seq_along(z_breaks_std)) {
                  z_val <- z_breaks_std[i]
                  z_break <- scale_info$z$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(x_extent_std, rep(y_val, 2), rep(z_val, 2), group_id, "ymax", z_break, "z")
                  group_id <- group_id + 1
            }
      }

      if ("zmin" %in% visible_faces) {
            z_val <- -0.5
            # Lines marking X breaks (extend across full Y range)
            for (i in seq_along(x_breaks_std)) {
                  x_val <- x_breaks_std[i]
                  x_break <- scale_info$x$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(rep(x_val, 2), y_extent_std, rep(z_val, 2), group_id, "zmin", x_break, "x")
                  group_id <- group_id + 1
            }
            # Lines marking Y breaks (extend across full X range)
            for (i in seq_along(y_breaks_std)) {
                  y_val <- y_breaks_std[i]
                  y_break <- scale_info$y$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(x_extent_std, rep(y_val, 2), rep(z_val, 2), group_id, "zmin", y_break, "y")
                  group_id <- group_id + 1
            }
      }

      if ("zmax" %in% visible_faces) {
            z_val <- 0.5
            # Lines marking X breaks (extend across full Y range)
            for (i in seq_along(x_breaks_std)) {
                  x_val <- x_breaks_std[i]
                  x_break <- scale_info$x$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(rep(x_val, 2), y_extent_std, rep(z_val, 2), group_id, "zmax", x_break, "x")
                  group_id <- group_id + 1
            }
            # Lines marking Y breaks (extend across full X range)
            for (i in seq_along(y_breaks_std)) {
                  y_val <- y_breaks_std[i]
                  y_break <- scale_info$y$breaks[i]
                  grid_data[[length(grid_data) + 1]] <-
                        create_line(x_extent_std, rep(y_val, 2), rep(z_val, 2), group_id, "zmax", y_break, "y")
                  group_id <- group_id + 1
            }
      }

      # Combine all grid lines
      if (length(grid_data) == 0) {
            return(NULL)
      }

      do.call(rbind, grid_data)
}


make_face_panels <- function(visible_faces){
      if (length(visible_faces) == 0) return(NULL)
      panels <- list()
      corners <- expand.grid(x = c(-0.5, 0.5), y = c(-0.5, 0.5), z = c(-0.5, 0.5))

      for(face in visible_faces){
            crnrs <- corners[corners[substr(face, 1, 1)] == switch(substr(face, 2, 4), "min" = -.5, "max" = .5), ]
            crnrs$face <- face
            crnrs <- crnrs[c(1, 3, 4, 2)]
            panels[[length(panels) + 1]] <- crnrs
      }

      if(length(panels) == 0) return(NULL)
      do.call(rbind, panels)[,c(1,2,4,3)]
}


#' Create grid segments for rendering
#'
#' @param grid_data Transformed grid data
#' @param plot_bounds Bounds for the final plot [xmin, xmax, ymin, ymax]
#' @return Data frame with segment coordinates for grid grob
create_grid_segments <- function(grid_data, plot_bounds) {
      if (is.null(grid_data) || nrow(grid_data) == 0) {
            return(NULL)
      }

      # Scale to [0, 1] based on plot bounds
      x_scaled <- (grid_data$x - plot_bounds[1]) / (plot_bounds[2] - plot_bounds[1])
      y_scaled <- (grid_data$y - plot_bounds[3]) / (plot_bounds[4] - plot_bounds[3])

      # Create segments by connecting consecutive points in each group
      segments <- data.frame()
      for (g in unique(grid_data$group)) {
            group_data <- grid_data[grid_data$group == g, ]
            group_x <- x_scaled[grid_data$group == g]
            group_y <- y_scaled[grid_data$group == g]

            if (length(group_x) >= 2) {
                  for (i in 1:(length(group_x) - 1)) {
                        segments <- rbind(segments, data.frame(
                              x0 = group_x[i],
                              y0 = group_y[i],
                              x1 = group_x[i + 1],
                              y1 = group_y[i + 1],
                              depth = group_data$z_proj[i],
                              face = group_data$face[i]
                        ))
                  }
            }
      }

      # Sort by depth (back to front)
      if (nrow(segments) > 0) {
            segments <- segments[order(-segments$depth), ]
      }

      return(segments)
}



create_panel_polygons = function(face_corners_transformed, panel_params, theme, layer = "background") {
      if (is.null(face_corners_transformed) || nrow(face_corners_transformed) == 0) {
            return(NULL)
      }

      # Get theme elements for panel styling
      panel_bg <- calc_element(switch(layer, background = "panel.background", foreground = "panel.foreground"), theme)
      panel_border <- calc_element("panel.border", theme)

      # Extract styling from theme with proper fallbacks
      # Priority: panel.border properties > panel.background properties > defaults

      # Fill color: prefer panel.background fill
      fill_color <- panel_bg$fill %||% "white"

      # Border: prefer panel.border if it exists, fallback to panel.background
      if (!is.null(panel_border) && !inherits(panel_border, "element_blank")) {
            # Use panel.border properties (themes like theme_bw, theme_light)
            border_color <- panel_border$colour %||% "black"
            border_width <- (panel_border$linewidth %||% 0.5) * .pt
            border_type <- panel_border$linetype %||% 1
      } else if (!is.null(panel_bg) && !inherits(panel_bg, "element_blank")) {
            # Use panel.background border properties (themes like theme_minimal)
            border_color <- panel_bg$colour %||% NA
            border_width <- (panel_bg$linewidth %||% 0) * .pt
            border_type <- panel_bg$linetype %||% 1
      } else {
            # Fallback defaults
            border_color <- "grey85"
            border_width <- 0.5 * .pt
            border_type <- 1
      }

      alpha_val <- panel_bg$alpha %||% 1

      # Scale to [0, 1] based on plot bounds
      x_scaled <- (face_corners_transformed$x - panel_params$plot_bounds[1]) /
            (panel_params$plot_bounds[2] - panel_params$plot_bounds[1])
      y_scaled <- (face_corners_transformed$y - panel_params$plot_bounds[3]) /
            (panel_params$plot_bounds[4] - panel_params$plot_bounds[3])

      # Get unique faces and their z-depths for sorting
      unique_faces <- unique(face_corners_transformed$face)
      face_depths <- sapply(unique_faces, function(f) {
            face_data <- face_corners_transformed[face_corners_transformed$face == f, ]
            mean(face_data$z, na.rm = TRUE)
      })

      # Sort faces by depth (back to front)
      sorted_faces <- unique_faces[order(-face_depths)]

      # Prepare data for single polygonGrob call with id parameter
      all_x <- numeric(0)
      all_y <- numeric(0)
      polygon_ids <- integer(0)

      polygon_id <- 1
      for (face in sorted_faces) {
            # Get data for this face
            face_indices <- which(face_corners_transformed$face == face)

            # Reorder vertices to form proper rectangles
            face_x <- x_scaled[face_indices][c(1, 3, 4, 2)]
            face_y <- y_scaled[face_indices][c(1, 3, 4, 2)]

            # Append to master lists
            all_x <- c(all_x, face_x)
            all_y <- c(all_y, face_y)
            polygon_ids <- c(polygon_ids, rep(polygon_id, length(face_x)))

            polygon_id <- polygon_id + 1
      }

      grid::polygonGrob(
            x = all_x,
            y = all_y,
            id = polygon_ids,
            default.units = "npc",
            gp = grid::gpar(
                  fill = alpha(fill_color, alpha_val),
                  col = border_color,
                  lwd = border_width,
                  lty = border_type
            ),
            name = "panel.faces.3d"
      )
}


render_cube <- function(self, panel_params, theme, layer = "background"){

      # Transparent base
      bg <- grid::rectGrob(
            x = 0.5, y = 0.5, width = 1, height = 1,
            gp = grid::gpar(fill = "transparent", col = NA),
            name = "panel.background.transparent"
      )

      # Get theme elements
      panel_bg <- calc_element(switch(layer, background = "panel.background", foreground = "panel.foreground"), theme)
      panel_grid <- calc_element(switch(layer, background = "panel.grid", foreground = "panel.grid.foreground"), theme)
      panel_grid_major <- calc_element(switch(layer, background = "panel.grid.major", foreground = "panel.grid.major.foreground"), theme)
      panel_border <- calc_element("panel.border", theme)

      # Determine if 3D panels should be rendered
      show_panels <- !inherits(panel_bg, "element_blank")
      show_border <- !inherits(panel_border, "element_blank")
      grid_element <- panel_grid_major %||% panel_grid
      show_grid <- !inherits(grid_element, "element_blank")
      visible_faces <- switch(layer,
                              background = panel_params$visible_faces_bg,
                              foreground = panel_params$visible_faces_fg)

      # 1. Render panels
      if (show_panels && !is.null(visible_faces)) {
            tryCatch({
                  face_corners <- make_face_panels(visible_faces)
                  if (!is.null(face_corners) && nrow(face_corners) > 0) {
                        face_corners_transformed <- transform_3d_standard(face_corners, panel_params$proj)
                        face_corners_transformed$face <- face_corners$face

                        # Use background fill only
                        panel_grobs <- create_panel_polygons(face_corners_transformed, panel_params, theme, layer)
                        if (!is.null(panel_grobs)) {
                              bg <- grid::grobTree(bg, panel_grobs, name = "panel.background.with.3d.panels")
                        }
                  }
            }, error = function(e) {
                  warning("Panel rendering failed: ", e$message)
            })
      }

      # 2. Render gridlines
      if (show_grid && !is.null(panel_params$grid_transformed)) {
            tryCatch({
                  visible_grid <- panel_params$grid_transformed[panel_params$grid_transformed$face %in% visible_faces,]
                  segments <- create_grid_segments(visible_grid, panel_params$plot_bounds)
                  if (!is.null(segments) && nrow(segments) > 0) {
                        grid_grob <- grid::segmentsGrob(
                              x0 = segments$x0, y0 = segments$y0,
                              x1 = segments$x1, y1 = segments$y1,
                              default.units = "npc",
                              gp = grid::gpar(
                                    col = grid_element$colour %||% "grey90",
                                    lwd = (grid_element$linewidth %||% 0.5) * .pt,
                                    lty = grid_element$linetype %||% 1
                              ),
                              name = "grid.3d"
                        )
                        bg <- grid::grobTree(bg, grid_grob, name = "panel.background.with.grid")
                  }
            }, error = function(e) {
                  warning("Grid rendering failed: ", e$message)
            })
      }

      # 3. Render panel.border
      if (show_border && !is.null(visible_faces)) {
            tryCatch({
                  face_corners <- make_face_panels(visible_faces)
                  if (!is.null(face_corners) && nrow(face_corners) > 0) {
                        face_corners_transformed <- transform_3d_standard(face_corners, panel_params$proj)
                        face_corners_transformed$face <- face_corners$face

                        # Render only polygon *outlines*
                        x_scaled <- (face_corners_transformed$x - panel_params$plot_bounds[1]) /
                              (panel_params$plot_bounds[2] - panel_params$plot_bounds[1])
                        y_scaled <- (face_corners_transformed$y - panel_params$plot_bounds[3]) /
                              (panel_params$plot_bounds[4] - panel_params$plot_bounds[3])

                        # Prepare outlines
                        unique_faces <- unique(face_corners_transformed$face)
                        face_depths <- sapply(unique_faces, function(f) {
                              mean(face_corners_transformed$z[face_corners_transformed$face == f], na.rm = TRUE)
                        })
                        sorted_faces <- unique_faces[order(-face_depths)]

                        all_x <- numeric()
                        all_y <- numeric()
                        polygon_ids <- numeric()

                        pid <- 1
                        for (face in sorted_faces) {
                              idx <- which(face_corners_transformed$face == face)
                              all_x <- c(all_x, x_scaled[idx][c(1, 3, 4, 2)])
                              all_y <- c(all_y, y_scaled[idx][c(1, 3, 4, 2)])
                              polygon_ids <- c(polygon_ids, rep(pid, 4))
                              pid <- pid + 1
                        }

                        border_grob <- grid::polygonGrob(
                              x = all_x,
                              y = all_y,
                              id = polygon_ids,
                              default.units = "npc",
                              gp = grid::gpar(
                                    fill = NA,
                                    col = panel_border$colour %||% "black",
                                    lwd = (panel_border$linewidth %||% 0.5) * .pt,
                                    lty = panel_border$linetype %||% 1
                              ),
                              name = "panel.border.3d"
                        )
                        bg <- grid::grobTree(bg, border_grob, name = "panel.background.with.border")
                  }
            }, error = function(e) {
                  warning("Border rendering failed: ", e$message)
            })
      }


      # 4. Render axis text and titles
      if(layer == "background"){
            axis_elements <- render_axis_text(self, panel_params, theme)

            # Add all labels to the plot
            if (length(axis_elements$labels) > 0) {
                  axis_elements$labels <- axis_elements$labels[!sapply(axis_elements$labels, is.null)]
                  if (length(axis_elements$labels) > 0) {
                        labels_grob <- tryCatch({
                              do.call(grid::grobTree, c(list(name = "axis.labels.3d"), axis_elements$labels))
                        }, error = function(e) {
                              warning("Failed to create labels grob: ", e$message)
                              NULL
                        })

                        if (!is.null(labels_grob)) {
                              bg <- grid::grobTree(bg, labels_grob, name = "panel.background.with.labels")
                        }
                  }
            }

            # Add all titles to the plot
            if (length(axis_elements$titles) > 0) {
                  axis_elements$titles <- axis_elements$titles[!sapply(axis_elements$titles, is.null)]
                  if (length(axis_elements$titles) > 0) {
                        titles_grob <- tryCatch({
                              do.call(grid::grobTree, c(list(name = "axis.titles.3d"), axis_elements$titles))
                        }, error = function(e) {
                              warning("Failed to create titles grob: ", e$message)
                              NULL
                        })

                        if (!is.null(titles_grob)) {
                              bg <- grid::grobTree(bg, titles_grob, name = "panel.background.with.titles")
                        }
                  }
            }
      }

      return(bg)
}
