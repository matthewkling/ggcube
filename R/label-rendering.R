# Tick length for one axis, in points. ggplot2 separates the panel edge from
# axis text by the tick length plus the text margin, so the tick length is
# part of label clearance whether or not ticks are drawn.
#
# `axis.ticks.length.z` is not a registered element, so the axis-specific
# lookup is allowed to fail and fall back to the shared element. The final
# 2.75pt fallback is theme_grey()'s documented value (half_line / 2).
resolve_tick_length_points <- function(axis, theme) {
      element <- tryCatch(calc_element(paste0("axis.ticks.length.", axis), theme),
                          error = function(e) NULL)

      if (is.null(element)) {
            element <- tryCatch(calc_element("axis.ticks.length", theme),
                                error = function(e) NULL)
      }

      if (is.null(element) || !grid::is.unit(element)) return(2.75)

      value <- tryCatch(grid::convertUnit(element, "pt", valueOnly = TRUE),
                        error = function(e) NA_real_)

      if (length(value) != 1 || !is.finite(value)) return(2.75)

      value
}

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

      axis_ticks_theme <- tryCatch(calc_element(paste0("axis.ticks.", axis), theme),
                                   error = function(e) NULL)
      if (is.null(axis_ticks_theme)) {
            axis_ticks_theme <- tryCatch(calc_element("axis.ticks", theme),
                                         error = function(e) NULL)
      }

      return(list(
            axis_text = axis_text_theme,
            axis_title = axis_title_theme,
            parent_text = parent_text_theme,
            axis_ticks = axis_ticks_theme,
            tick_length = resolve_tick_length_points(axis, theme)
      ))
}

# Resolve axis text and title margins, in points. Placement converts these to
# device space at draw time, so no plot-unit conversion happens here.
calculate_axis_offsets <- function(theme_elements, rotate_labels) {
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

      if (rotate_labels) {
            text_margin_points <- max(margin_l, margin_r)
      } else {
            text_margin_points <- max(margin_t, margin_r, margin_b, margin_l)
      }

      # Tick clearance is not added here. A tick is 3D geometry, so how far it
      # actually reaches from the edge is only known once projected; the drawn
      # extent is supplied by create_axis_ticks() at render time.
      #
      # ggplot2's 2.75pt margin assumes a vertical tick beside horizontal text.
      # Here labels hug the tick tip along the same line, so the same value
      # reads much tighter and needs a floor. Scaling with the text size keeps
      # spacing proportionate as base_size changes.
      text_size <- resolve_fontsize(theme_elements$axis_text$size, 8.5)
      text_offset_points <- max(text_margin_points, 0.6 * text_size)

      title_margin <- theme_elements$axis_title$margin %||% margin()
      title_margin_numeric <- as.numeric(title_margin)

      # Larger than typical theme defaults (2-6 points), which sit too close to
      # the cube once labels are cleared.
      MIN_3D_TITLE_MARGIN_POINTS <- 10

      theme_margin_distance <- max(title_margin_numeric[1], title_margin_numeric[3])
      title_margin_points <- max(theme_margin_distance, MIN_3D_TITLE_MARGIN_POINTS)

      return(list(
            text_offset = text_offset_points,
            title_margin = title_margin_points
      ))
}

# Device-space conversion --------------------------------------------------

# Placement geometry runs in points measured from the panel's lower-left
# corner. That space is isotropic, unlike npc, whose x and y units differ
# whenever the panel is not square.
make_device_context <- function(plot_bounds, panel_width_pt, panel_height_pt) {
      plot_width <- plot_bounds[2] - plot_bounds[1]
      plot_height <- plot_bounds[4] - plot_bounds[3]

      if (!is.finite(plot_width) || plot_width <= 0) plot_width <- 1
      if (!is.finite(plot_height) || plot_height <= 0) plot_height <- 1

      list(
            plot_bounds = plot_bounds,
            sx = panel_width_pt / plot_width,
            sy = panel_height_pt / plot_height,
            panel_width_pt = panel_width_pt,
            panel_height_pt = panel_height_pt
      )
}

plot_to_pt <- function(x, y, ctx) {
      list(x = (x - ctx$plot_bounds[1]) * ctx$sx,
           y = (y - ctx$plot_bounds[3]) * ctx$sy)
}

pt_to_npc <- function(x_pt, y_pt, ctx) {
      if (!is.finite(x_pt) || !is.finite(y_pt)) return(NULL)
      list(x = x_pt / ctx$panel_width_pt,
           y = y_pt / ctx$panel_height_pt)
}

# Convert a gridline data frame's projected coordinates into device points,
# leaving every other column untouched so downstream helpers work unchanged.
gridlines_to_pt <- function(gridlines, ctx) {
      converted <- plot_to_pt(gridlines$x, gridlines$y, ctx)
      gridlines$x <- converted$x
      gridlines$y <- converted$y
      gridlines
}

# Text measurement and placement -------------------------------------------

# Measure an unrotated label. Returns width and height in points. Must be
# called with an active device, i.e. from makeContent().
measure_text_box <- function(label, fontsize, family = "", face = "plain") {
      fallback <- list(
            width = 0.6 * fontsize * max(nchar(paste(deparse(label), collapse = "")), 1),
            height = fontsize
      )

      grob <- tryCatch({
            grid::textGrob(
                  label = as_grob_label(label),
                  gp = grid::gpar(fontsize = fontsize, fontfamily = family, fontface = face)
            )
      }, error = function(e) NULL)

      if (is.null(grob)) return(fallback)

      tryCatch({
            width <- grid::convertWidth(grid::grobWidth(grob), "pt", valueOnly = TRUE)
            height <- grid::convertHeight(grid::grobHeight(grob), "pt", valueOnly = TRUE)
            if (!is.finite(width) || width <= 0) width <- fallback$width
            if (!is.finite(height) || height <= 0) height <- fallback$height
            list(width = width, height = height)
      }, error = function(e) fallback)
}

# Corner offsets of a rotated text box relative to its justification point.
# grid rotates the box about that point, so corners are taken in the text's
# own frame and then rotated. Returns a 4x2 matrix of point offsets.
text_box_corner_offsets <- function(width, height, angle_degrees, hjust, vjust) {
      hjust <- if (is.null(hjust) || !is.finite(hjust)) 0.5 else hjust
      vjust <- if (is.null(vjust) || !is.finite(vjust)) 0.5 else vjust
      angle_degrees <- if (is.null(angle_degrees) || !is.finite(angle_degrees)) 0 else angle_degrees

      dx <- c(-hjust * width, (1 - hjust) * width)
      dy <- c(-vjust * height, (1 - vjust) * height)
      local <- as.matrix(expand.grid(dx = dx, dy = dy))

      angle <- angle_degrees * pi / 180
      cbind(
            local[, "dx"] * cos(angle) - local[, "dy"] * sin(angle),
            local[, "dx"] * sin(angle) + local[, "dy"] * cos(angle)
      )
}

# Unit normal to the axis edge, oriented to agree with the offset direction.
edge_normal <- function(axis_angle, direction) {
      u <- c(cos(axis_angle), sin(axis_angle))
      n <- c(-u[2], u[1])
      if (sum(n * direction) < 0) n <- -n
      n
}

# Distance to travel along `direction` so that every corner of the text box
# clears the axis edge by `margin`.
#
# The anchor lies on the edge, so a point P clears by (P - anchor) . normal.
# Solving min over corners of t * (direction . normal) + corner . normal >= margin
# gives the offset below. `min_cosine` caps the offset when the gridline runs
# nearly parallel to the edge and pushing outward gains almost no clearance.
#
# Pure function: no device or grid state, so it is testable with synthetic
# corner offsets.
place_axis_text <- function(direction, normal, margin, corner_offsets,
                            min_cosine = 0.15) {
      projections <- corner_offsets[, 1] * normal[1] + corner_offsets[, 2] * normal[2]
      inward_extent <- max(0, max(-projections))

      cosine <- sum(direction * normal)
      cosine <- max(cosine, min_cosine)

      (margin + inward_extent) / cosine
}

# How far a placed text box reaches from the edge, measured along the normal.
# Used to space titles beyond the labels they must clear.
text_box_reach <- function(offset, direction, normal, corner_offsets) {
      projections <- corner_offsets[, 1] * normal[1] + corner_offsets[, 2] * normal[2]
      offset * sum(direction * normal) + max(projections)
}

resolve_fontsize <- function(size, default) {
      if (is.null(size)) return(default)
      if (inherits(size, "unit")) size <- as.numeric(size)
      size <- suppressWarnings(as.numeric(size))
      if (length(size) != 1 || !is.finite(size) || size <= 0) return(default)
      size
}

# Geometry helpers ---------------------------------------------------------

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

# Helper function to calculate gridline position. Carries the endpoint's
# depth scaling factor so text can be sized by viewing distance.
calculate_gridline_position <- function(gridline_data, axis_uses_start) {
      index <- if (axis_uses_start) 1 else nrow(gridline_data)

      depth_scale <- 1
      if ("depth_scale" %in% names(gridline_data)) {
            candidate <- gridline_data$depth_scale[index]
            if (length(candidate) == 1 && is.finite(candidate) && candidate > 0) {
                  depth_scale <- candidate
            }
      }

      return(list(x = gridline_data$x[index],
                  y = gridline_data$y[index],
                  depth_scale = depth_scale))
}

# Helper function to calculate offset direction
calculate_offset_direction <- function(gridline_data, target_x, target_y) {
      gridline_center_x <- mean(gridline_data$x)
      gridline_center_y <- mean(gridline_data$y)

      center_to_target_dx <- target_x - gridline_center_x
      center_to_target_dy <- target_y - gridline_center_y
      center_to_target_length <- sqrt(center_to_target_dx^2 + center_to_target_dy^2)

      if (center_to_target_length == 0) {
            return(NULL)
      }

      return(list(
            dx = center_to_target_dx / center_to_target_length,
            dy = center_to_target_dy / center_to_target_length,
            center_x = gridline_center_x,
            center_y = gridline_center_y,
            length = center_to_target_length
      ))
}

# Helper function to calculate text rotation and justification
calculate_text_rotation_and_justification <- function(gridline_data, rotate_labels, theme_elements, is_title = FALSE, axis_angle = NULL) {
      if (is_title && !is.null(axis_angle)) {
            # For titles, use axis angle (parallel to axis edge)
            angle_radians <- axis_angle
      } else {
            # For labels, use gridline angle (parallel to gridline)
            gridline_dx <- gridline_data$x[nrow(gridline_data)] - gridline_data$x[1]
            gridline_dy <- gridline_data$y[nrow(gridline_data)] - gridline_data$y[1]
            angle_radians <- atan2(gridline_dy, gridline_dx)
      }

      angle_degrees <- angle_radians * 180 / pi

      # Ensure readable orientation
      if (abs(angle_degrees) > 90) {
            angle_degrees <- angle_degrees + 180
            if (angle_degrees > 180) angle_degrees <- angle_degrees - 360
      }

      if (rotate_labels) {
            result <- list(
                  angle = angle_degrees,
                  vjust = 0.5
            )
            # Add hjust calculation for labels (not titles)
            if (!is_title) {
                  result$hjust <- NULL  # Will be calculated in caller based on position
                  result$gridline_center_x <- mean(gridline_data$x)
                  result$gridline_center_y <- mean(gridline_data$y)
            } else {
                  element_type <- "axis_title"
                  result$hjust <- theme_elements[[element_type]]$hjust %||% 0.5
            }
            return(result)
      } else {
            element_type <- if (is_title) "axis_title" else "axis_text"
            return(list(
                  angle = theme_elements[[element_type]]$angle %||% 0,
                  hjust = theme_elements[[element_type]]$hjust %||% 0.5,
                  vjust = theme_elements[[element_type]]$vjust %||% 0.5
            ))
      }
}

# Anchor rotated text at its near end so it extends away from the cube.
resolve_end_anchor_hjust <- function(rotation_info, position_x, position_y) {
      offset_dx <- position_x - rotation_info$gridline_center_x
      offset_dy <- position_y - rotation_info$gridline_center_y
      angle_rad <- rotation_info$angle * pi / 180
      text_dir <- c(cos(angle_rad), sin(angle_rad))
      dot <- offset_dx * text_dir[1] + offset_dy * text_dir[2]
      if (dot > 0) 0 else 1
}

# Helper to normalise a single label into a value grid::textGrob accepts.
# Character and numeric labels pass through as strings; language objects
# (calls, names) and expression elements are preserved so plotmath renders.
# An expression vector of length >= 1 is reduced to its first element.
as_grob_label <- function(x) {
      if (is.expression(x)) {
            if (length(x) == 0) return("")
            return(x[[1]])
      }
      if (is.language(x)) {
            return(x)
      }
      as.character(x)
}

# Helper function to resolve label text. May return a character string or a
# language object (for plotmath labels); callers must not coerce the result.
resolve_label_text <- function(break_value, axis_labels, axis_breaks) {
      if (!is.null(axis_labels) && !is.null(axis_breaks)) {
            break_index <- match(break_value, axis_breaks)
            if (length(break_index) > 0 && !is.na(break_index) &&
                break_index <= length(axis_labels)) {
                  return(as_grob_label(axis_labels[[break_index]]))
            }
      }
      return(as.character(break_value))
}

# Helper function to scale coordinates to NPC
scale_to_npc_coordinates <- function(x, y = NULL, plot_bounds) {

      if(!is.null(y)){
            x_scaled <- (x - plot_bounds[1]) / (plot_bounds[2] - plot_bounds[1])
            y_scaled <- (y - plot_bounds[3]) / (plot_bounds[4] - plot_bounds[3])
            if (is.na(x_scaled) || is.na(y_scaled)) {
                  return(NULL)
            }
            return(list(x = x_scaled, y = y_scaled))
      }else{ # assume x is a data frame with x and y variables
            x$x <- (x$x - plot_bounds[1]) / (plot_bounds[2] - plot_bounds[1])
            x$y <- (x$y - plot_bounds[3]) / (plot_bounds[4] - plot_bounds[3])
            return(x)
      }
}

# Helper function to create text grob
create_text_grob <- function(text, x_npc, y_npc, rotation_info, theme_elements,
                             is_title = FALSE, fontsize = NULL) {
      element_type <- if (is_title) "axis_title" else "axis_text"

      # Get appropriate theme elements
      colour <- theme_elements[[element_type]]$colour %||% "black"
      fontfamily <- theme_elements[[element_type]]$family %||% ""
      fontface <- theme_elements[[element_type]]$face %||% "plain"

      if (is.null(fontsize)) {
            fontsize <- resolve_fontsize(theme_elements[[element_type]]$size,
                                         if (is_title) 11 else 8.5)
      }

      tryCatch({
            grid::textGrob(
                  label = as_grob_label(text),
                  x = as.numeric(x_npc),
                  y = as.numeric(y_npc),
                  hjust = as.numeric(rotation_info$hjust),
                  vjust = as.numeric(rotation_info$vjust),
                  rot = as.numeric(rotation_info$angle),
                  default.units = "npc",
                  gp = grid::gpar(
                        fontsize = fontsize,
                        col = colour,
                        fontfamily = fontfamily,
                        fontface = fontface
                  )
            )
      }, error = function(e) {
            NULL
      })
}

# Axis ticks ---------------------------------------------------------------

# Projected length, in points, of a one-unit vector along each cube axis,
# measured at the cube centre. Used to convert a tick length in points into
# cube space so that ticks are built as ordinary 3D geometry.
projected_axis_lengths_pt <- function(proj, ctx) {
      vapply(1:3, function(i) {
            coords <- matrix(0, nrow = 2, ncol = 3)
            coords[1, i] <- -0.5
            coords[2, i] <- 0.5

            projected <- transform_3d_standard(
                  data.frame(x = coords[, 1], y = coords[, 2], z = coords[, 3]), proj)

            start <- plot_to_pt(projected$x[1], projected$y[1], ctx)
            end <- plot_to_pt(projected$x[2], projected$y[2], ctx)

            sqrt((end$x - start$x)^2 + (end$y - start$y)^2)
      }, numeric(1))
}

# Tick length in cube units.
#
# A tick is 3D geometry, so its length lives in cube space and foreshortens
# with the view like everything else. Calibrating against the longest
# projected axis means a tick pointing along the least foreshortened axis
# renders at the requested point size and every other tick comes in shorter,
# so `axis.ticks.length` reads as an upper bound.
tick_length_cube_units <- function(tick_length_pt, proj, ctx) {
      axis_lengths <- projected_axis_lengths_pt(proj, ctx)
      reference <- max(axis_lengths)

      if (!is.finite(reference) || reference <= 0) return(NA_real_)

      tick_length_pt / reference
}

# Build the tick marks for one axis.
#
# Ticks continue their gridlines past the cube edge: the direction is the
# face's free axis, signed outward by the edge's fixed coordinate. Geometry is
# assembled in cube space and then projected, so ticks share the perspective
# and depth behaviour of the gridlines they extend.
create_axis_ticks <- function(axis, standard_gridlines, theme_elements,
                              panel_params, ctx, chosen_edge, chosen_face,
                              effective_ratios, axis_angle, on_hull = TRUE,
                              ticks_depth_strength = 1) {

      empty <- list(ticks = list(), tick_reach = 0)

      # Geometry is computed even when ticks are not drawn, so that blanking
      # axis.ticks leaves label spacing unchanged, as it does in 2D ggplot2.
      element <- theme_elements$axis_ticks
      draw <- !is.null(element) && !inherits(element, "element_blank")

      length_cube <- tick_length_cube_units(theme_elements$tick_length %||% 0,
                                            panel_params$proj, ctx)
      if (!is.finite(length_cube) || length_cube == 0) return(empty)

      normal_axis <- substr(chosen_face, 1, 1)
      free_axis <- setdiff(c("x", "y", "z"), c(axis, normal_axis))
      if (length(free_axis) != 1) return(empty)

      fixed <- chosen_edge$fixed_coords[[free_axis]]
      if (is.null(fixed) || !is.finite(fixed)) return(empty)

      # Outward from the face, flipping with the labels when the edge is
      # interior to the rendered silhouette so the two stay on the same side.
      direction <- sign(fixed)
      if (direction == 0) return(empty)
      if (!on_hull) direction <- -direction

      free_edge_coord <- fixed * effective_ratios[match(free_axis, c("x", "y", "z"))]
      free_tip_coord <- free_edge_coord + direction * length_cube

      groups <- unique(standard_gridlines$group)
      if (length(groups) == 0) return(empty)

      starts <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))
      ends <- starts

      for (group_id in groups) {
            rows <- standard_gridlines[standard_gridlines$group == group_id, ]
            if (nrow(rows) == 0) next

            base <- rows[1, c("x", "y", "z"), drop = FALSE]

            start <- base
            start[[free_axis]] <- free_edge_coord
            end <- base
            end[[free_axis]] <- free_tip_coord

            starts <- rbind(starts, start)
            ends <- rbind(ends, end)
      }

      if (nrow(starts) == 0) return(empty)

      starts_2d <- transform_3d_standard(starts, panel_params$proj)
      ends_2d <- transform_3d_standard(ends, panel_params$proj)

      # How far the drawn ticks reach from the edge, perpendicular to it, in
      # points. This is the quantity label clearance must be measured against:
      # the tick runs along its gridline, which is generally oblique to the
      # edge, so its perpendicular reach is shorter than its drawn length.
      start_pt_x <- (starts_2d$x - ctx$plot_bounds[1]) * ctx$sx
      start_pt_y <- (starts_2d$y - ctx$plot_bounds[3]) * ctx$sy
      end_pt_x <- (ends_2d$x - ctx$plot_bounds[1]) * ctx$sx
      end_pt_y <- (ends_2d$y - ctx$plot_bounds[3]) * ctx$sy

      tick_dx <- end_pt_x - start_pt_x
      tick_dy <- end_pt_y - start_pt_y

      tick_reach <- 0
      lengths <- sqrt(tick_dx^2 + tick_dy^2)
      longest <- which.max(replace(lengths, !is.finite(lengths), 0))

      if (length(longest) == 1 && is.finite(lengths[longest]) && lengths[longest] > 0) {
            unit_dir <- c(tick_dx[longest], tick_dy[longest]) / lengths[longest]
            normal <- edge_normal(axis_angle, unit_dir)
            reaches <- tick_dx * normal[1] + tick_dy * normal[2]
            reaches <- reaches[is.finite(reaches)]
            if (length(reaches) > 0) tick_reach <- max(0, max(reaches))
      }

      if (!draw) return(list(ticks = list(), tick_reach = tick_reach))

      # scale_to_npc_coordinates() guards with scalar `||`, so npc is computed
      # directly here rather than element by element.
      pb <- ctx$plot_bounds
      x0 <- (starts_2d$x - pb[1]) / (pb[2] - pb[1])
      y0 <- (starts_2d$y - pb[3]) / (pb[4] - pb[3])
      x1 <- (ends_2d$x - pb[1]) / (pb[2] - pb[1])
      y1 <- (ends_2d$y - pb[3]) / (pb[4] - pb[3])

      keep <- is.finite(x0) & is.finite(y0) & is.finite(x1) & is.finite(y1)
      if (!any(keep)) return(list(ticks = list(), tick_reach = tick_reach))

      # Depth taken from the tick itself rather than its parent gridline,
      # since a tick sits at one end rather than spanning the face.
      depth <- (starts_2d$depth_scale + ends_2d$depth_scale) / 2
      depth[!is.finite(depth) | depth <= 0] <- 1

      base_lwd <- (element$linewidth %||% 0.5) * .pt

      tick_grob <- tryCatch({
            grid::segmentsGrob(
                  x0 = x0[keep], y0 = y0[keep],
                  x1 = x1[keep], y1 = y1[keep],
                  default.units = "npc",
                  gp = grid::gpar(
                        col = element$colour %||% "grey20",
                        lwd = safe_lwd(base_lwd * apply_depth_strength(depth[keep],
                                                                       ticks_depth_strength),
                                       base_lwd),
                        lty = element$linetype %||% 1,
                        lineend = element$lineend %||% "butt"
                  ),
                  name = paste0("axis.ticks.", axis, ".3d")
            )
      }, error = function(e) NULL)

      if (is.null(tick_grob)) return(list(ticks = list(), tick_reach = tick_reach))

      list(ticks = list(tick_grob), tick_reach = tick_reach)
}


# Label and title construction ---------------------------------------------

# Build the tick labels for one axis. `edge_gridlines` must already be in
# device points. Returns the grobs plus the furthest distance any label
# reaches from the axis edge, which titles use for spacing.
create_axis_labels <- function(axis, edge_gridlines, theme_elements, offsets,
                               panel_params, rotate_labels, ctx, chosen_edge,
                               axis_uses_start, axis_angle, on_hull = TRUE,
                               tick_reach = 0, text_depth_strength = 1) {

      margin <- max(tick_reach, 0) + offsets$text_offset

      axis_labels <- panel_params$scale_info[[axis]]$labels %||% NULL
      axis_breaks <- panel_params$scale_info[[axis]]$breaks %||% NULL

      base_fontsize <- resolve_fontsize(theme_elements$axis_text$size, 8.5)
      fontfamily <- theme_elements$axis_text$family %||% ""
      fontface <- theme_elements$axis_text$face %||% "plain"

      all_labels <- list()
      label_reach <- 0

      for (group_id in unique(edge_gridlines$group)) {
            gridline_data <- edge_gridlines[edge_gridlines$group == group_id, ]
            if (nrow(gridline_data) < 2) next

            base_pos <- calculate_gridline_position(gridline_data, axis_uses_start)
            offset_dir <- calculate_offset_direction(gridline_data, base_pos$x, base_pos$y)
            if (is.null(offset_dir)) next

            direction <- c(offset_dir$dx, offset_dir$dy)
            if (!on_hull) direction <- -direction
            normal <- edge_normal(axis_angle, direction)

            rotation_info <- calculate_text_rotation_and_justification(
                  gridline_data, rotate_labels, theme_elements,
                  is_title = FALSE, axis_angle = NULL)

            if (rotate_labels && is.null(rotation_info$hjust)) {
                  rotation_info$hjust <- resolve_end_anchor_hjust(rotation_info,
                                                                  base_pos$x, base_pos$y)
            }

            label_text <- resolve_label_text(gridline_data$break_value[1],
                                             axis_labels, axis_breaks)

            fontsize <- base_fontsize * apply_depth_strength(base_pos$depth_scale,
                                                             text_depth_strength)
            if (!is.finite(fontsize) || fontsize <= 0) fontsize <- base_fontsize

            box <- measure_text_box(label_text, fontsize, fontfamily, fontface)
            corners <- text_box_corner_offsets(box$width, box$height,
                                               rotation_info$angle,
                                               rotation_info$hjust,
                                               rotation_info$vjust)

            offset <- place_axis_text(direction, normal, margin, corners)
            label_reach <- max(label_reach,
                               text_box_reach(offset, direction, normal, corners))

            position_npc <- pt_to_npc(base_pos$x + direction[1] * offset,
                                      base_pos$y + direction[2] * offset,
                                      ctx)
            if (is.null(position_npc)) next

            label_grob <- create_text_grob(label_text, position_npc$x, position_npc$y,
                                           rotation_info, theme_elements,
                                           is_title = FALSE, fontsize = fontsize)

            if (!is.null(label_grob)) {
                  all_labels[[length(all_labels) + 1]] <- label_grob
            }
      }

      return(list(labels = all_labels,
                  label_reach = label_reach))
}

# Build the title for one axis. `edge_gridlines` must already be in device
# points. `label_reach` is how far the axis labels extend from the edge.
create_axis_title <- function(axis, edge_gridlines, theme_elements, offsets,
                              panel_params, rotate_labels, ctx, chosen_edge,
                              axis_uses_start, on_hull = TRUE, axis_selection = NULL,
                              title_position = "auto", label_reach = 0) {

      axis_name <- panel_params$scale_info[[axis]]$name

      if (is.null(axis_name) || inherits(axis_name, "waiver")) {
            axis_name <- axis
      }

      if (is.null(axis_name) || (is.character(axis_name) && axis_name == "")) {
            return(list())
      }

      axis_gridlines <- edge_gridlines[edge_gridlines$break_axis == axis, ]

      if (nrow(axis_gridlines) == 0) {
            return(list())
      }

      title_fontsize <- resolve_fontsize(theme_elements$axis_title$size, 11)
      title_family <- theme_elements$axis_title$family %||% ""
      title_face <- theme_elements$axis_title$face %||% "plain"

      if (!on_hull && title_position != "center" && !is.null(axis_selection)) {
            # Place title at the near (peripheral) end of the axis edge
            p1_depth <- axis_selection$edge_p1_2d$depth
            p2_depth <- axis_selection$edge_p2_2d$depth
            if (p1_depth <= p2_depth) {
                  title_pos <- axis_selection$edge_p1_2d
            } else {
                  title_pos <- axis_selection$edge_p2_2d
            }

            title_pos_pt <- plot_to_pt(title_pos$x, title_pos$y, ctx)

            cube_center_2d <- transform_3d_standard(data.frame(x = 0, y = 0, z = 0),
                                                    panel_params$proj)
            cube_center_pt <- plot_to_pt(cube_center_2d$x, cube_center_2d$y, ctx)

            offset_dx <- title_pos_pt$x - cube_center_pt$x
            offset_dy <- title_pos_pt$y - cube_center_pt$y
            offset_len <- sqrt(offset_dx^2 + offset_dy^2)
            if (offset_len > 0) {
                  offset_dx <- offset_dx / offset_len
                  offset_dy <- offset_dy / offset_len
            }

            title_offset <- offsets$title_margin
            final_x <- title_pos_pt$x + offset_dx * title_offset
            final_y <- title_pos_pt$y + offset_dy * title_offset

            position_npc <- pt_to_npc(final_x, final_y, ctx)
            if (is.null(position_npc)) return(list())

            edge_p1_pt <- plot_to_pt(axis_selection$edge_p1_2d$x,
                                     axis_selection$edge_p1_2d$y, ctx)
            edge_p2_pt <- plot_to_pt(axis_selection$edge_p2_2d$x,
                                     axis_selection$edge_p2_2d$y, ctx)
            axis_angle <- atan2(edge_p2_pt$y - edge_p1_pt$y,
                                edge_p2_pt$x - edge_p1_pt$x)

            rotation_info <- calculate_text_rotation_and_justification(
                  edge_gridlines[edge_gridlines$group == edge_gridlines$group[1], ],
                  rotate_labels, theme_elements, is_title = TRUE, axis_angle = axis_angle
            )

            # hjust: anchor text so it extends away from the plot
            angle_rad <- rotation_info$angle * pi / 180
            text_dir <- c(cos(angle_rad), sin(angle_rad))
            dot <- offset_dx * text_dir[1] + offset_dy * text_dir[2]
            rotation_info$hjust <- if (dot > 0) 0 else 1

            title_grob <- create_text_grob(axis_name, position_npc$x, position_npc$y,
                                           rotation_info, theme_elements,
                                           is_title = TRUE, fontsize = title_fontsize)
            if (!is.null(title_grob)) return(list(title_grob))
            return(list())
      }

      # Find the gridline closest to the center of the axis range
      axis_breaks <- panel_params$scale_info[[axis]]$breaks
      if(!is.numeric(axis_breaks)) axis_breaks <- attr(axis_breaks, "pos")
      axis_center_value <- mean(range(axis_breaks))

      center_distances <- abs(axis_gridlines$break_pos - axis_center_value)
      center_group <- axis_gridlines$group[which.min(center_distances)]
      center_gridline <- axis_gridlines[axis_gridlines$group == center_group, ]

      if (nrow(center_gridline) < 2) {
            return(list())
      }

      axis_angle <- calculate_axis_angle(axis_gridlines, axis_uses_start)

      base_pos <- calculate_gridline_position(center_gridline, axis_uses_start)
      offset_dir <- calculate_offset_direction(center_gridline, base_pos$x, base_pos$y)
      if (is.null(offset_dir)) return(list())

      direction <- c(offset_dir$dx, offset_dir$dy)
      if (!on_hull) direction <- -direction
      normal <- edge_normal(axis_angle, direction)

      rotation_info <- calculate_text_rotation_and_justification(
            center_gridline, rotate_labels, theme_elements,
            is_title = TRUE, axis_angle = axis_angle)

      box <- measure_text_box(axis_name, title_fontsize, title_family, title_face)
      corners <- text_box_corner_offsets(box$width, box$height,
                                         rotation_info$angle,
                                         rotation_info$hjust,
                                         rotation_info$vjust)

      # label_reach already accounts for the ticks, since label clearance is
      # measured from the tick tip. When labels are not drawn, the caller
      # passes the tick reach through in its place.
      margin <- max(label_reach, 0) + offsets$title_margin
      offset <- place_axis_text(direction, normal, margin, corners)

      position_npc <- pt_to_npc(base_pos$x + direction[1] * offset,
                                base_pos$y + direction[2] * offset,
                                ctx)
      if (is.null(position_npc)) {
            return(list())
      }

      title_grob <- create_text_grob(axis_name, position_npc$x, position_npc$y,
                                     rotation_info, theme_elements,
                                     is_title = TRUE, fontsize = title_fontsize)

      if (!is.null(title_grob)) {
            return(list(title_grob))
      } else {
            return(list())
      }
}

# Draw-time assembly -------------------------------------------------------

# Build every axis label and title for the panel. Runs at draw time, so
# `panel_width_pt` and `panel_height_pt` are the panel's true dimensions and
# all text can be measured directly.
render_axis_text <- function(self, panel_params, theme, panel_width_pt, panel_height_pt) {
      tryCatch({
            all_labels <- list()
            all_titles <- list()
            all_ticks <- list()

            theme_elements <- panel_params$theme_elements %||% list()
            should_render_axis_text <- theme_elements$show_axis_text %||% !inherits(calc_element("axis.text", theme), "element_blank")
            should_render_axis_title <- theme_elements$show_axis_title %||% !inherits(calc_element("axis.title", theme), "element_blank")
            should_render_axis_ticks <- theme_elements$show_axis_ticks %||% !inherits(calc_element("axis.ticks", theme), "element_blank")

            if (!should_render_axis_text && !should_render_axis_title && !should_render_axis_ticks) {
                  return(list(labels = list(), titles = list(), ticks = list()))
            }

            ctx <- make_device_context(panel_params$plot_bounds,
                                       panel_width_pt, panel_height_pt)

            effective_ratios <- compute_effective_ratios(
                  list(x = panel_params$scale_info$x$limits,
                       y = panel_params$scale_info$y$limits,
                       z = panel_params$scale_info$z$limits),
                  panel_params$scales,
                  panel_params$ratio
            )

            for (axis in c("x", "y", "z")) {

                  axis_selection <- get_axis_selection(axis, self, panel_params, effective_ratios)

                  if (is.null(axis_selection)) {
                        next
                  }

                  chosen_edge <- axis_selection$edge
                  chosen_face <- axis_selection$face

                  theme_elements_axis <- extract_axis_theme_elements(axis, theme)

                  offsets <- calculate_axis_offsets(theme_elements_axis, self$rotate_labels)

                  edge_gridlines <- panel_params$grid_transformed[
                        panel_params$grid_transformed$face == chosen_face &
                              panel_params$grid_transformed$break_axis == axis, ]

                  if (nrow(edge_gridlines) == 0) next

                  edge_gridlines <- gridlines_to_pt(edge_gridlines, ctx)

                  # Placement direction depends on externality relative to the
                  # rendered panels, not the full cube silhouette.
                  on_hull <- axis_selection$on_panel_hull %||% TRUE
                  label_reach <- 0
                  tick_reach <- 0

                  axis_uses_start <- determine_endpoint_preference_by_boundary(chosen_edge, edge_gridlines)
                  axis_angle <- calculate_axis_angle(edge_gridlines, axis_uses_start)

                  # Ticks first: label clearance is measured from the tick tip,
                  # and the drawn extent is only known once projected.
                  if (!is.null(panel_params$grid_standard)) {
                        standard_gridlines <- panel_params$grid_standard[
                              panel_params$grid_standard$face == chosen_face &
                                    panel_params$grid_standard$break_axis == axis, ]

                        if (nrow(standard_gridlines) > 0) {
                              tick_result <- create_axis_ticks(
                                    axis, standard_gridlines, theme_elements_axis,
                                    panel_params, ctx, chosen_edge, chosen_face,
                                    effective_ratios, axis_angle, on_hull,
                                    depth_strength(self, "ticks"))

                              tick_reach <- tick_result$tick_reach
                              if (should_render_axis_ticks) {
                                    all_ticks <- c(all_ticks, tick_result$ticks)
                              }
                        }
                  }

                  if (should_render_axis_text) {
                        label_result <- create_axis_labels(axis, edge_gridlines, theme_elements_axis,
                                                           offsets, panel_params, self$rotate_labels,
                                                           ctx, chosen_edge, axis_uses_start,
                                                           axis_angle, on_hull, tick_reach,
                                                           depth_strength(self, "text"))
                        all_labels <- c(all_labels, label_result$labels)
                        label_reach <- label_result$label_reach
                  } else {
                        label_reach <- max(tick_reach, 0)
                  }

                  if (should_render_axis_title) {
                        title_result <- create_axis_title(axis, edge_gridlines, theme_elements_axis,
                                                          offsets, panel_params, self$rotate_labels,
                                                          ctx, chosen_edge, axis_uses_start,
                                                          on_hull, axis_selection,
                                                          self$title_position %||% "auto",
                                                          label_reach)
                        all_titles <- c(all_titles, title_result)
                  }
            }

            return(list(labels = all_labels, titles = all_titles, ticks = all_ticks))
      }, error = function(e) {
            warning("Axis label/title rendering failed: ", e$message)
            return(list(labels = list(), titles = list(), ticks = list()))
      })
}

# Deferred grob holding everything needed to build axis furniture. Resolution
# happens in makeContent(), inside the panel viewport, where the panel's real
# dimensions and text metrics are available.
axis_furniture_grob <- function(self, panel_params, theme, show_text, show_title,
                                show_ticks = TRUE, name = "axis.furniture.3d") {
      grid::gTree(
            coord = self,
            panel_params = panel_params,
            plot_theme = theme,
            show_text = show_text,
            show_title = show_title,
            show_ticks = show_ticks,
            name = name,
            cl = "ggcube_axis_furniture"
      )
}

#' Resolve 3D axis labels and titles at draw time
#'
#' @param x A `ggcube_axis_furniture` gTree.
#' @return The gTree with its children populated.
#' @importFrom grid makeContent
#' @export
#' @keywords internal
makeContent.ggcube_axis_furniture <- function(x) {
      children <- tryCatch({
            panel_width_pt <- grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
            panel_height_pt <- grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE)

            elements <- render_axis_text(x$coord, x$panel_params, x$plot_theme,
                                         panel_width_pt, panel_height_pt)

            kids <- list()

            if (isTRUE(x$show_ticks)) {
                  ticks <- elements$ticks[!vapply(elements$ticks, is.null, logical(1))]
                  if (length(ticks) > 0) {
                        kids[[length(kids) + 1]] <- do.call(
                              grid::grobTree, c(list(name = "axis.ticks.3d"), ticks))
                  }
            }

            if (x$show_text) {
                  labels <- elements$labels[!vapply(elements$labels, is.null, logical(1))]
                  if (length(labels) > 0) {
                        kids[[length(kids) + 1]] <- do.call(
                              grid::grobTree, c(list(name = "axis.labels.3d"), labels))
                  }
            }

            if (x$show_title) {
                  titles <- elements$titles[!vapply(elements$titles, is.null, logical(1))]
                  if (length(titles) > 0) {
                        kids[[length(kids) + 1]] <- do.call(
                              grid::grobTree, c(list(name = "axis.titles.3d"), titles))
                  }
            }

            kids
      }, error = function(e) {
            warning("Axis furniture rendering failed: ", e$message)
            list()
      })

      if (length(children) == 0) {
            return(grid::setChildren(x, grid::gList()))
      }

      grid::setChildren(x, do.call(grid::gList, children))
}
