#' Convert text to polygon outlines
#'
#' Converts a text string into polygon vertices suitable for plotting with
#' ggplot2's geom_polygon. Uses systemfonts for font matching and glyph extraction.
#'
#' @param text Character string to convert
#' @param family Font family name (e.g., "Arial", "Helvetica", "sans")
#' @param weight Font weight ("normal", "bold", "thin", "light", etc.)
#' @param italic Logical; use italic variant
#' @param size Font size in points
#' @param tolerance Bezier curve tolerance; lower values give more detailed outlines
#' @param spacing Additional letter spacing in em units
#' @param hjust,vjust The justification of the textbox surrounding the text
#' @param width Maximum line width in inches for word wrapping; NA disables wrapping
#' @param align Text alignment: "left", "center", or "right"
#' @param lineheight Line height multiplier
#' @param internal_size Internal rendering size for kerning precision; generally
#'   shouldn't need adjustment
#'
#' @return A data frame with columns: x, y, contour, glyph, letter, poly_id. X and y
#'    are vertex coordinates, in point units.
#'
#' @examples
#' text_outlines("Howdy, partner", family = "Arial") |>
#'   ggplot2::ggplot(ggplot2::aes(x, y, group = letter, subgroup = poly_id)) +
#'   ggplot2::geom_polygon() +
#'   ggplot2::coord_fixed()
#'
#' @export
text_outlines <- function(text,
                          family = "sans",
                          weight = "normal",
                          italic = FALSE,
                          size = 12,
                          tolerance = 0.01,
                          spacing = 0,
                          hjust = 0.5,
                          vjust = 0.5,
                          width = NA,
                          align = "left",
                          lineheight = 1,
                          internal_size = 144) {
      scale_factor <- size / internal_size
      font_match <- systemfonts::match_fonts(family, weight = weight, italic = italic)
      f_path <- font_match$path
      f_index <- font_match$index
      internal_width <- if (!is.na(width)) width / scale_factor else NA
      shaped <- systemfonts::shape_string(
            text,
            path = f_path,
            index = f_index,
            size = internal_size,
            tracking = spacing * 1000,
            hjust = hjust, vjust = vjust,
            width = internal_width,
            align = align,
            lineheight = lineheight
      )
      glyph_positions <- shaped$shape
      all_glyphs <- lapply(seq_len(nrow(glyph_positions)), function(i) {
            outlines <- systemfonts::glyph_outline(
                  glyph = glyph_positions$index[i],
                  path = f_path,
                  index = f_index,
                  size = internal_size,
                  tolerance = tolerance / scale_factor
            )
            if (is.null(outlines) || nrow(outlines) == 0) {
                  return(NULL)
            }
            outlines$x <- (outlines$x + glyph_positions$x_offset[i]) * scale_factor
            outlines$y <- (outlines$y + glyph_positions$y_offset[i]) * scale_factor
            outlines$letter <- glyph_positions$glyph[i]
            outlines$poly_id <- paste0(i, "_", outlines$contour)
            outlines
      })
      dplyr::bind_rows(all_glyphs)
}


#' Compute camera-facing direction for 3D text
#'
#' Creates a specification for camera-facing "billboard" text that will compute
#' the appropriate facing direction for each label position.
#'
#' @param pitch,roll,yaw Rotation angles in degrees, matching the values
#'   you'll use in `coord_3d()`.
#' @param dist Distance from viewer to center of the data cube, matching
#'   the value you'll use in `coord_3d()`. Default is 2. Only used when
#'   `mode = "point"`.
#' @param mode Character string specifying how labels should face the camera:
#'   \itemize{
#'     \item `"plane"` (default): All labels face parallel to the view plane,
#'       like billboards. This is typically easier to read.
#'     \item `"point"`: Each label faces toward the camera position. Labels
#'       at the edges will angle inward, giving perspective-correct orientation.
#'   }
#'
#' @return A `camera_facing_spec` object to pass to the `facing` parameter
#'   of `geom_text_3d()`.
#'
#' @details
#' With `mode = "plane"`, all text labels face the same direction (parallel
#' to the viewing plane), similar to traditional billboard rendering. This
#' produces clean, readable labels.
#'
#' With `mode = "point"`, each label faces directly toward the camera's
#' position in 3D space. Labels near the edges of the plot will angle inward,
#' which is geometrically correct but can look less clean.
#'
#' Since `geom_text_3d()` computes vertex positions before the view is known,
#' you must specify the view parameters here and use the same values in
#' `coord_3d()`.
#'
#' @examples
#' \dontrun{
#' # Parallel billboard text (default) - all labels face same direction
#' ggplot(df, aes(x, y, z = z, label = label)) +
#'   geom_text_3d(facing = camera_facing(pitch = 20, roll = -60, yaw = -30)) +
#'   coord_3d(pitch = 20, roll = -60, yaw = -30)
#'
#' # Point-facing text - labels angle toward camera position
#' ggplot(df, aes(x, y, z = z, label = label)) +
#'   geom_text_3d(facing = camera_facing(pitch = 20, roll = -60, yaw = -30,
#'                                       mode = "point")) +
#'   coord_3d(pitch = 20, roll = -60, yaw = -30)
#' }
#'
#' @seealso [geom_text_3d()] for rendering 3D text labels
#'
#' @export
camera_facing <- function(coord = NULL,
                          pitch = 0, roll = -60, yaw = -30, dist = 2,
                          mode = c("plane", "point")) {
      mode <- match.arg(mode)

      if(!is.null(coord)){
            pitch <- coord[[1]]$pitch
            roll <- coord[[1]]$roll
            yaw <- coord[[1]]$yaw
            dist <- coord[[1]]$dist
      }

      # For plane mode, use infinite distance so all labels
      # face parallel to the view plane
      effective_dist <- if (mode == "plane") 1e6 else dist

      structure(
            list(pitch = pitch, roll = roll, yaw = yaw, dist = effective_dist),
            class = "camera_facing_spec"
      )
}


#' Compute per-point camera facing vectors
#'
#' @param anchors Data frame with x, y, z columns for anchor positions
#' @param spec A camera_facing_spec object
#' @param coord Optional coord_3d object for proper axis scaling
#' @return Matrix with one row per anchor, columns for facing x, y, z
#' @keywords internal
compute_camera_facing_vectors <- function(anchors, spec, coord = NULL) {
      # Extract view parameters
      pitch <- spec$pitch
      roll <- spec$roll
      yaw <- spec$yaw
      dist <- spec$dist

      # Convert angles to radians
      pitch_rad <- pitch * pi / 180
      roll_rad <- roll * pi / 180
      yaw_rad <- yaw * pi / 180

      # Build rotation matrix (same as in rotate_3d / transform_3d_standard)
      rot <- matrix(c(
            cos(yaw_rad) * cos(pitch_rad),
            cos(yaw_rad) * sin(pitch_rad) * sin(roll_rad) - sin(yaw_rad) * cos(roll_rad),
            cos(yaw_rad) * sin(pitch_rad) * cos(roll_rad) + sin(yaw_rad) * sin(roll_rad),

            sin(yaw_rad) * cos(pitch_rad),
            sin(yaw_rad) * sin(pitch_rad) * sin(roll_rad) + cos(yaw_rad) * cos(roll_rad),
            sin(yaw_rad) * sin(pitch_rad) * cos(roll_rad) - cos(yaw_rad) * sin(roll_rad),

            -sin(pitch_rad),
            cos(pitch_rad) * sin(roll_rad),
            cos(pitch_rad) * cos(roll_rad)
      ), nrow = 3, byrow = TRUE)

      # Get data ranges
      x_range <- range(anchors$x, na.rm = TRUE)
      y_range <- range(anchors$y, na.rm = TRUE)
      z_range <- range(anchors$z, na.rm = TRUE)

      # Compute effective ratios based on coord settings
      if (!is.null(coord)) {
            ratio <- coord$ratio %||% c(1, 1, 1)
            scales_mode <- coord$scales %||% "free"

            if (scales_mode == "free") {
                  effective_ratios <- ratio
            } else {
                  # Fixed scales: ratios depend on data ranges
                  x_span <- diff(x_range)
                  y_span <- diff(y_range)
                  z_span <- diff(z_range)

                  if (x_span == 0 || is.na(x_span)) x_span <- 1
                  if (y_span == 0 || is.na(y_span)) y_span <- 1
                  if (z_span == 0 || is.na(z_span)) z_span <- 1

                  scale_proportional <- c(x_span, y_span, z_span)
                  effective_ratios <- ratio * scale_proportional
            }

            # Normalize
            effective_ratios <- effective_ratios / max(effective_ratios)
      } else {
            effective_ratios <- c(1, 1, 1)
      }

      # Scale anchor points to standard domain with effective ratios applied
      scale_to_standard <- function(vals, rng, eff_ratio) {
            width <- diff(rng)
            if (width == 0 || is.na(width)) {
                  rep(0, length(vals))
            } else {
                  ((vals - mean(rng)) / width) * eff_ratio
            }
      }

      std_x <- scale_to_standard(anchors$x, x_range, effective_ratios[1])
      std_y <- scale_to_standard(anchors$y, y_range, effective_ratios[2])
      std_z <- scale_to_standard(anchors$z, z_range, effective_ratios[3])

      # Apply z-flip (as done in transform_3d_standard)
      std_z <- -std_z

      # Apply rotation to get points in view space
      std_coords <- rbind(std_x, std_y, std_z)

      # rotate_3d does: rotated <- xyz %*% rot
      # So for column vectors we need: t(t(std_coords) %*% rot) = t(rot) %*% std_coords
      rotated <- t(rot) %*% std_coords

      # Camera is at (0, 0, -dist) in rotated space
      # Direction from each point toward camera
      cam_x <- 0 - rotated[1, ]
      cam_y <- 0 - rotated[2, ]
      cam_z <- (-dist) - rotated[3, ]

      # Normalize these direction vectors
      lengths <- sqrt(cam_x^2 + cam_y^2 + cam_z^2)
      cam_x <- cam_x / lengths
      cam_y <- cam_y / lengths
      cam_z <- cam_z / lengths

      # These directions are in rotated space - transform back to standard space
      rotated_dirs <- rbind(cam_x, cam_y, cam_z)
      std_dirs <- rot %*% rotated_dirs

      # Undo z-flip
      std_dirs[3, ] <- -std_dirs[3, ]

      # Transform from standard space back to data space
      # Need to undo the effective ratio scaling
      data_dirs <- std_dirs
      data_dirs[1, ] <- data_dirs[1, ] / effective_ratios[1]
      data_dirs[2, ] <- data_dirs[2, ] / effective_ratios[2]
      data_dirs[3, ] <- data_dirs[3, ] / effective_ratios[3]

      # Re-normalize after undoing the scaling
      lengths <- sqrt(data_dirs[1, ]^2 + data_dirs[2, ]^2 + data_dirs[3, ]^2)
      data_dirs[1, ] <- data_dirs[1, ] / lengths
      data_dirs[2, ] <- data_dirs[2, ] / lengths
      data_dirs[3, ] <- data_dirs[3, ] / lengths

      # Return as data frame for easier handling
      data.frame(
            facing_x = data_dirs[1, ],
            facing_y = data_dirs[2, ],
            facing_z = data_dirs[3, ]
      )
}


# Helper functions for 3D text rendering --------------------------------------

#' Convert facing specification to normal vector
#'
#' @param facing Character string ("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
#'   or numeric vector of length 3
#' @return Normalized vector of length 3
#' @keywords internal
facing_to_normal <- function(facing) {
      if (is.numeric(facing)) {
            if (length(facing) != 3) {
                  stop("`facing` must be a length-3 numeric vector or a character string")
            }
            # Normalize
            return(facing / sqrt(sum(facing^2)))
      }

      if (!is.character(facing) || length(facing) != 1) {
            stop("`facing` must be a length-3 numeric vector or a character string")
      }

      switch(facing,
             xmin = c(-1, 0, 0),
             xmax = c(1, 0, 0),
             ymin = c(0, -1, 0),
             ymax = c(0, 1, 0),
             zmin = c(0, 0, -1),
             zmax = c(0, 0, 1),
             stop("`facing` must be one of: 'xmin', 'xmax', 'ymin', 'ymax', 'zmin', 'zmax', or a numeric vector")
      )
}


#' Compute rotation matrix from one vector to another using Rodrigues' formula
#'
#' @param from Unit vector to rotate from
#' @param to Unit vector to rotate to
#' @return 3x3 rotation matrix
#' @keywords internal
rotation_matrix_between_vectors <- function(from, to) {
      # Cross product for rotation axis
      v <- c(
            from[2] * to[3] - from[3] * to[2],
            from[3] * to[1] - from[1] * to[3],
            from[1] * to[2] - from[2] * to[1]
      )

      s <- sqrt(sum(v^2))  # sine of angle
      c <- sum(from * to)   # cosine of angle

      if (s < 1e-10) {
            # Vectors are aligned or opposite
            if (c > 0) {
                  return(diag(3))  # Identity
            } else {
                  # 180 degree rotation - find perpendicular axis
                  if (abs(from[1]) < 0.9) {
                        perp <- c(1, 0, 0)
                  } else {
                        perp <- c(0, 1, 0)
                  }
                  axis <- perp - from * sum(from * perp)
                  axis <- axis / sqrt(sum(axis^2))
                  # 180 degree rotation around axis: R = 2 * axis %*% t(axis) - I
                  return(2 * outer(axis, axis) - diag(3))
            }
      }

      # Rodrigues' rotation formula: R = I + [v]x + [v]x^2 * (1-c)/s^2
      v_cross <- matrix(c(
            0, -v[3], v[2],
            v[3], 0, -v[1],
            -v[2], v[1], 0
      ), nrow = 3, byrow = TRUE)

      diag(3) + v_cross + v_cross %*% v_cross * ((1 - c) / s^2)
}


#' Compute text rotation matrix with intuitive "up" direction
#'
#' Creates a rotation matrix that orients text to face a given direction,
#' with angle=0 producing the most natural baseline orientation (horizontal
#' when possible).
#'
#' @param facing_normal Unit vector for direction text should face
#' @param angle Additional rotation around facing axis (degrees)
#' @return 3x3 rotation matrix
#' @keywords internal
compute_text_rotation_matrix <- function(facing_normal, angle) {
      # We need to build an orthonormal basis where:
      # - The "forward" direction (original +z) maps to facing_normal
      # - The "up" direction (original +y) is as close to world +z as possible
      # - The "right" direction (original +x) completes the basis
      #
      # Text starts with:
      # - baseline along x-axis (right)
      # - ascenders along y-axis (up)
      # - face pointing along z-axis (forward)

      forward <- facing_normal

      # Choose world up hint: prefer +z, but if facing is parallel to z, use +y
      world_up <- c(0, 0, 1)
      if (abs(abs(sum(forward * world_up)) - 1) < 1e-10) {
            # Facing is parallel to z-axis, use y as up hint
            world_up <- c(0, 1, 0)
      }

      # Compute "right" as cross(world_up, forward), then normalize
      right <- c(
            world_up[2] * forward[3] - world_up[3] * forward[2],
            world_up[3] * forward[1] - world_up[1] * forward[3],
            world_up[1] * forward[2] - world_up[2] * forward[1]
      )
      right_len <- sqrt(sum(right^2))
      if (right_len < 1e-10) {
            # Fallback if cross product is degenerate
            right <- c(1, 0, 0)
      } else {
            right <- right / right_len
      }

      # Compute true "up" as cross(forward, right) to ensure orthogonality
      up <- c(
            forward[2] * right[3] - forward[3] * right[2],
            forward[3] * right[1] - forward[1] * right[3],
            forward[1] * right[2] - forward[2] * right[1]
      )
      # Should already be normalized, but just in case
      up <- up / sqrt(sum(up^2))

      # Build rotation matrix that maps:
      # (1,0,0) -> right
      # (0,1,0) -> up
      # (0,0,1) -> forward
      # This is just the matrix with right, up, forward as columns
      base_rotation <- cbind(right, up, forward)

      # Apply additional angle rotation around the facing axis
      if (angle != 0) {
            angle_rot <- rotation_matrix_around_axis(forward, angle)
            return(angle_rot %*% base_rotation)
      }

      base_rotation
}


#' Rotation matrix for rotation around an axis
#'
#' @param axis Unit vector defining rotation axis
#' @param angle Angle in degrees
#' @return 3x3 rotation matrix
#' @keywords internal
rotation_matrix_around_axis <- function(axis, angle) {
      if (angle == 0) return(diag(3))

      theta <- angle * pi / 180
      c <- cos(theta)
      s <- sin(theta)
      t <- 1 - c

      x <- axis[1]
      y <- axis[2]
      z <- axis[3]

      matrix(c(
            t*x*x + c,   t*x*y - s*z, t*x*z + s*y,
            t*x*y + s*z, t*y*y + c,   t*y*z - s*x,
            t*x*z - s*y, t*y*z + s*x, t*z*z + c
      ), nrow = 3, byrow = TRUE)
}


#' Compute aspect ratio correction for text
#'
#' Estimates the aspect ratio correction needed so text doesn't appear stretched
#' when the coord_3d scales axes differently. Returns a scalar to multiply
#' the text's y-coordinates by.
#'
#' @param data_ranges List with x, y, z elements, each a length-2 vector of [min, max]
#' @param coord A coord_3d object, or NULL
#' @return Scalar aspect ratio correction (1 = no correction needed)
#' @keywords internal
compute_text_aspect_ratios <- function(data_ranges, coord) {
      if (is.null(coord)) {
            return(1)
      }

      # Extract coord parameters
      ratio <- coord$ratio %||% c(1, 1, 1)
      scales_mode <- coord$scales %||% "free"

      # Compute effective ratios similar to compute_effective_ratios in transform-3d.R
      if (scales_mode == "free") {
            # With free scales, each axis is normalized independently
            # The ratio parameter applies directly
            effective_ratios <- ratio
      } else {
            # With fixed scales, ratios depend on data ranges
            x_span <- diff(data_ranges$x)
            y_span <- diff(data_ranges$y)
            z_span <- diff(data_ranges$z)

            # Handle zero-width ranges
            if (x_span == 0 || is.na(x_span)) x_span <- 1
            if (y_span == 0 || is.na(y_span)) y_span <- 1
            if (z_span == 0 || is.na(z_span)) z_span <- 1

            # Combine user ratios with scale-space proportions
            scale_proportional <- c(x_span, y_span, z_span)
            effective_ratios <- ratio * scale_proportional
      }

      # Normalize so largest dimension is 1
      max_ratio <- max(effective_ratios)
      effective_ratios <- effective_ratios / max_ratio

      # For text aspect correction, we need to know how much the x and y axes
      # differ in their effective scaling. Return ratio of y to x.
      # If y is compressed relative to x, we need to stretch text vertically.
      # Note: This is approximate since text can face any direction in 3D.
      # For now, use x/y ratio as a reasonable default.
      if (effective_ratios[1] == 0) return(1)

      return(effective_ratios[2] / effective_ratios[1])
}


#' Rotate and position text vertices in 3D space
#'
#' Takes 2D text vertices (in x-y plane) and transforms them to face the
#' specified direction at the specified position. With angle=0, the text
#' baseline will be as horizontal as possible (parallel to x-y plane when
#' the facing direction allows).
#'
#' @param vertices Data frame with vertex_x, vertex_y columns (text outline vertices in points)
#' @param facing_normal Unit vector for the direction the text should face
#' @param angle Rotation angle around the facing axis (degrees)
#' @param anchor_x,anchor_y,anchor_z Position to place the text
#' @param size_mm Text size in mm (same units as geom_text)
#' @param data_span The span of the data in data units (used to compute conversion)
#' @param aspect_adjust Scalar multiplier for text y-coordinates (height).
#'   Values > 1 make text taller, values < 1 make text wider.
#' @return Data frame with x, y, z columns in data coordinates
#' @keywords internal
rotate_text_to_facing <- function(vertices, facing_normal, angle,
                                  anchor_x, anchor_y, anchor_z,
                                  size_mm, data_span, aspect_adjust = 1) {
      # Convert text vertices from points to data units
      #
      # Goal: text specified at size_mm should appear roughly that size on a

      # typical 6" plot, similar to billboard text.
      #
      # For billboard: 3.88mm text on 152.4mm plot ≈ 2.5% of plot height
      # For polygon: we want text to be ~2.5% of the data span
      #
      # text_outlines() returns vertices in points. For size_pt point text,
      # the character height is roughly size_pt points.
      #
      # We want: (text height in data units) / data_span = size_mm / 152.4
      # So: text height in data units = size_mm * data_span / 152.4
      #
      # If vertices are at size_pt points, and we want height = size_mm * data_span / 152.4,
      # then scale factor = (size_mm * data_span / 152.4) / size_pt
      #
      # But size_pt = size_mm * 72.27 / 25.4 (mm to points conversion)
      # So scale factor = (size_mm * data_span / 152.4) / (size_mm * 72.27 / 25.4)
      #                 = (data_span / 152.4) * (25.4 / 72.27)
      #                 = data_span / 152.4 / 2.845
      #                 = data_span / 433.5
      #
      # Hmm, that's independent of size_mm, which is wrong.
      #
      # Let's think differently: vertices come in at size_pt points.
      # We want the final text height to be (size_mm / 152.4) * data_span data units.
      # Current vertex height is roughly size_pt points.
      # So we need to scale by: target_height / current_height
      #   = (size_mm / 152.4 * data_span) / size_pt
      #
      # Since size_pt = size_mm * 2.845:
      #   = (size_mm / 152.4 * data_span) / (size_mm * 2.845)
      #   = data_span / (152.4 * 2.845)
      #   = data_span / 433.5
      #
      # This is still size-independent! That's because larger size_mm gives
      # proportionally larger size_pt, and we're scaling both.
      #
      # The issue is that text_outlines already scaled the vertices.
      # Let's just use a simple empirical scale factor.

      # Simple approach: make text height roughly 10% of data_span at default size
      # Default size is 3.88mm. Scale proportionally from there.
      reference_size <- 3.88
      base_fraction <- 0.10  # text is 10% of data span at reference size

      # Target text height in data units
      target_height <- (size_mm / reference_size) * base_fraction * data_span

      # Vertices from text_outlines are in points. Approximate height is size_pt.
      size_pt <- size_mm * 72.27 / 25.4

      # Scale factor to convert from points to data units
      scale_factor <- target_height / size_pt

      vx <- vertices$vertex_x * scale_factor
      vy <- vertices$vertex_y * scale_factor * aspect_adjust
      vz <- rep(0, length(vx))

      # Get combined rotation matrix (handles both facing and angle)
      rotation <- compute_text_rotation_matrix(facing_normal, angle)

      # Apply rotation
      coords <- rbind(vx, vy, vz)
      rotated <- rotation %*% coords

      # Translate to anchor position
      data.frame(
            x = rotated[1, ] + anchor_x,
            y = rotated[2, ] + anchor_y,
            z = rotated[3, ] + anchor_z
      )
}


# Stat (Polygon method) -------------------------------------------------------

StatText3DPolygon <- ggproto("StatText3DPolygon", Stat,
                             required_aes = c("x", "y", "z", "label"),

                             compute_panel = function(data, scales, na.rm = FALSE,
                                                      family = "sans",
                                                      weight = "normal",
                                                      italic = FALSE,
                                                      size = 3.88,
                                                      hjust = 0.5,
                                                      vjust = 0.5,
                                                      lineheight = 1.2,
                                                      spacing = 0,
                                                      tolerance = 0.01,
                                                      facing = "zmax",
                                                      angle = 0,
                                                      nudge_x = 0,
                                                      nudge_y = 0,
                                                      nudge_z = 0,
                                                      coord = NULL,
                                                      aspect_adjust = 1,
                                                      light = NULL) {

                                   # Remove missing values if requested
                                   if (na.rm) {
                                         data <- data[complete.cases(data[c("x", "y", "z", "label")]), ]
                                   }

                                   if (nrow(data) == 0) {
                                         return(data.frame())
                                   }

                                   # Handle discrete scale conversion (preserve raw values for scale training)
                                   if ("x" %in% names(data)) {
                                         data$x_raw <- data$x
                                         if (is.factor(data$x) || is.character(data$x)) {
                                               data$x <- as.numeric(as.factor(data$x))
                                         }
                                   }
                                   if ("y" %in% names(data)) {
                                         data$y_raw <- data$y
                                         if (is.factor(data$y) || is.character(data$y)) {
                                               data$y <- as.numeric(as.factor(data$y))
                                         }
                                   }
                                   if ("z" %in% names(data)) {
                                         data$z_raw <- data$z
                                         if (is.factor(data$z) || is.character(data$z)) {
                                               data$z <- as.numeric(as.factor(data$z))
                                         }
                                   }

                                   # Compute anchor positions (with nudge applied)
                                   anchors <- data.frame(
                                         x = data$x + nudge_x,
                                         y = data$y + nudge_y,
                                         z = data$z + nudge_z
                                   )

                                   # Handle facing = "camera" when coord is provided
                                   if (is.character(facing) && facing == "camera") {
                                         if (is.null(coord)) {
                                               stop('facing = "camera" requires the `coord` parameter to be set')
                                         }
                                         # Create camera_facing_spec from coord params
                                         facing <- camera_facing(
                                               pitch = coord$pitch %||% 0,
                                               roll = coord$roll %||% -60,
                                               yaw = coord$yaw %||% -30,
                                               dist = coord$dist %||% 2
                                         )
                                   }

                                   # Handle camera_facing_spec: compute per-row facing vectors
                                   if (inherits(facing, "camera_facing_spec")) {
                                         facing_vectors <- compute_camera_facing_vectors(anchors, facing, coord)
                                         per_row_facing <- TRUE
                                   } else {
                                         # Fixed facing for all rows
                                         facing_normal <- facing_to_normal(facing)
                                         per_row_facing <- FALSE
                                   }

                                   # Compute aspect adjustment for text correction
                                   # Start with user-specified adjustment
                                   final_aspect_adjust <- aspect_adjust

                                   # If coord is provided, compute additional correction from coord params
                                   if (!is.null(coord)) {
                                         coord_aspect <- compute_text_aspect_ratios(
                                               data_ranges = list(
                                                     x = range(data$x, na.rm = TRUE),
                                                     y = range(data$y, na.rm = TRUE),
                                                     z = range(data$z, na.rm = TRUE)
                                               ),
                                               coord = coord
                                         )
                                         # Multiply user adjustment by coord-based correction
                                         final_aspect_adjust <- aspect_adjust * coord_aspect
                                   }

                                   # Compute data_span for size conversion
                                   # Use the maximum span across all axes as reference
                                   x_span <- diff(range(data$x, na.rm = TRUE))
                                   y_span <- diff(range(data$y, na.rm = TRUE))
                                   z_span <- diff(range(data$z, na.rm = TRUE))
                                   # Handle zero spans (single point)
                                   if (x_span == 0 || is.na(x_span)) x_span <- 1
                                   if (y_span == 0 || is.na(y_span)) y_span <- 1
                                   if (z_span == 0 || is.na(z_span)) z_span <- 1
                                   data_span <- max(x_span, y_span, z_span)

                                   # Process each label
                                   all_text <- lapply(seq_len(nrow(data)), function(i) {
                                         row <- data[i, ]
                                         label <- as.character(row$label)

                                         # Skip empty labels
                                         if (is.na(label) || label == "") {
                                               return(NULL)
                                         }

                                         # Get text outlines
                                         # Convert size from mm to points for text_outlines
                                         # 1 mm ≈ 2.835 points (72.27 pt/inch / 25.4 mm/inch)
                                         size_pt <- size * 72.27 / 25.4

                                         outlines <- text_outlines(
                                               text = label,
                                               family = family,
                                               weight = weight,
                                               italic = italic,
                                               size = size_pt,
                                               tolerance = tolerance,
                                               spacing = spacing,
                                               hjust = hjust,
                                               vjust = vjust,
                                               lineheight = lineheight
                                         )

                                         if (is.null(outlines) || nrow(outlines) == 0) {
                                               return(NULL)
                                         }

                                         # Add text-specific identifiers
                                         outlines$text_id <- i

                                         # Store vertex positions for rotation (already at correct size in points)
                                         outlines$vertex_x <- outlines$x
                                         outlines$vertex_y <- outlines$y

                                         # subgroup identifies contours within a text label (for holes)
                                         outlines$subgroup <- outlines$poly_id

                                         # Get anchor position (with nudge already applied)
                                         anchor_x <- anchors$x[i]
                                         anchor_y <- anchors$y[i]
                                         anchor_z <- anchors$z[i]

                                         # Get facing normal for this row
                                         if (per_row_facing) {
                                               facing_normal <- c(
                                                     facing_vectors$facing_x[i],
                                                     facing_vectors$facing_y[i],
                                                     facing_vectors$facing_z[i]
                                               )
                                         }

                                         # Rotate and position vertices in data space
                                         transformed <- rotate_text_to_facing(
                                               vertices = outlines,
                                               facing_normal = facing_normal,
                                               angle = angle,
                                               anchor_x = anchor_x,
                                               anchor_y = anchor_y,
                                               anchor_z = anchor_z,
                                               size_mm = size,
                                               data_span = data_span,
                                               aspect_adjust = final_aspect_adjust
                                         )

                                         # Store final positions as x, y, z for scale training
                                         outlines$x <- transformed$x
                                         outlines$y <- transformed$y
                                         outlines$z <- transformed$z

                                         # Store normal vectors for lighting (same for all vertices in this label)
                                         outlines$normal_x <- facing_normal[1]
                                         outlines$normal_y <- facing_normal[2]
                                         outlines$normal_z <- facing_normal[3]

                                         # Carry over aesthetics from original row
                                         for (col in setdiff(names(row), c("x", "y", "z", "label"))) {
                                               outlines[[col]] <- row[[col]]
                                         }

                                         outlines
                                   })

                                   result <- dplyr::bind_rows(all_text)

                                   if (nrow(result) == 0) {
                                         return(data.frame())
                                   }

                                   # Create hierarchical group for depth sorting
                                   # Group identifies each text label; subgroup identifies contours within
                                   if (!"group" %in% names(result) || all(result$group == -1)) {
                                         result$group <- paste0("1__text", result$text_id)
                                   } else {
                                         result$group <- paste0(result$group, "__text", result$text_id)
                                   }

                                   # Remove intermediate columns not needed downstream
                                   result$contour <- NULL
                                   result$glyph <- NULL
                                   result$letter <- NULL
                                   result$poly_id <- NULL
                                   result$vertex_x <- NULL
                                   result$vertex_y <- NULL

                                   # Attach lighting specification for downstream processing
                                   result <- attach_light(result, light)

                                   result
                             }
)


# Geom (Polygon method) -------------------------------------------------------

GeomText3DPolygon <- ggproto("GeomText3DPolygon", Geom,
                             required_aes = c("x", "y", "z", "group", "subgroup"),

                             default_aes = aes(
                                   colour = NA,
                                   fill = "black",
                                   alpha = 1,
                                   linewidth = 0
                             ),

                             draw_panel = function(data, panel_params, coord,
                                                   scale_depth = TRUE,
                                                   rule = "evenodd") {

                                   # Validate coord
                                   validate_coord3d(coord)

                                   if (nrow(data) == 0) {
                                         return(grid::nullGrob())
                                   }

                                   # Transform through coord (3D -> 2D projection)
                                   coords <- coord$transform(data, panel_params)

                                   # Scale linewidths by depth if enabled
                                   coords <- scale_depth(coords, scale_depth)

                                   # Apply light blending to colors
                                   coords <- blend_light(coords)

                                   # Render using pathGrob with evenodd rule for proper hole handling
                                   # Each text label is a group, each contour is a subgroup
                                   groups <- unique(coords$group)

                                   if (length(groups) == 0) {
                                         return(grid::nullGrob())
                                   }

                                   # Sort by group to maintain proper order
                                   coords <- coords[order(coords$group, coords$subgroup), ]

                                   # Create unique subgroup IDs across the whole dataset for pathGrob
                                   coords$path_id <- match(coords$subgroup, unique(coords$subgroup))

                                   # For pathGrob, we need one set of aesthetics per path (group),
                                   # not per subgroup. Get first row of each group.
                                   group_first_idx <- !duplicated(coords$group)

                                   grid::pathGrob(
                                         x = coords$x,
                                         y = coords$y,
                                         id = coords$path_id,
                                         pathId = as.integer(factor(coords$group)),
                                         rule = rule,
                                         default.units = "npc",
                                         gp = grid::gpar(
                                               col = coords$colour[group_first_idx],
                                               fill = coords$fill[group_first_idx],
                                               alpha = coords$alpha[group_first_idx],
                                               lwd = coords$linewidth[group_first_idx] * .pt
                                         )
                                   )
                             }
)


# Stat (Billboard method) -----------------------------------------------------

StatText3DBillboard <- ggproto("StatText3DBillboard", Stat,
                               required_aes = c("x", "y", "z", "label"),

                               default_aes = aes(
                                     colour = "black",
                                     size = 3.88,
                                     alpha = NA,
                                     family = "",
                                     fontface = 1,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     angle = 0,
                                     lineheight = 1.2
                               ),

                               compute_panel = function(data, scales, na.rm = FALSE,
                                                        nudge_x = 0,
                                                        nudge_y = 0,
                                                        nudge_z = 0,
                                                        size = 3.88,
                                                        hjust = 0.5,
                                                        vjust = 0.5,
                                                        angle = 0,
                                                        lineheight = 1.2,
                                                        family = "",
                                                        fontface = 1) {

                                     # Remove missing values if requested
                                     if (na.rm) {
                                           data <- data[complete.cases(data[c("x", "y", "z", "label")]), ]
                                     }

                                     if (nrow(data) == 0) {
                                           return(data.frame())
                                     }

                                     # Handle discrete scale conversion (preserve raw values for scale training)
                                     if ("x" %in% names(data)) {
                                           data$x_raw <- data$x
                                           if (is.factor(data$x) || is.character(data$x)) {
                                                 data$x <- as.numeric(as.factor(data$x))
                                           }
                                     }
                                     if ("y" %in% names(data)) {
                                           data$y_raw <- data$y
                                           if (is.factor(data$y) || is.character(data$y)) {
                                                 data$y <- as.numeric(as.factor(data$y))
                                           }
                                     }
                                     if ("z" %in% names(data)) {
                                           data$z_raw <- data$z
                                           if (is.factor(data$z) || is.character(data$z)) {
                                                 data$z <- as.numeric(as.factor(data$z))
                                           }
                                     }

                                     # Apply nudge
                                     data$x <- data$x + nudge_x
                                     data$y <- data$y + nudge_y
                                     data$z <- data$z + nudge_z

                                     # Set defaults for text parameters if not in data
                                     if (!"size" %in% names(data)) data$size <- size
                                     if (!"hjust" %in% names(data)) data$hjust <- hjust
                                     if (!"vjust" %in% names(data)) data$vjust <- vjust
                                     if (!"angle" %in% names(data)) data$angle <- angle
                                     if (!"lineheight" %in% names(data)) data$lineheight <- lineheight
                                     if (!"family" %in% names(data)) data$family <- family
                                     if (!"fontface" %in% names(data)) data$fontface <- fontface

                                     data
                               }
)


# Geom (Billboard method) -----------------------------------------------------

GeomText3DBillboard <- ggproto("GeomText3DBillboard", Geom,
                               required_aes = c("x", "y", "z", "label"),

                               default_aes = aes(
                                     colour = "black",
                                     size = 3.88,
                                     alpha = NA,
                                     family = "",
                                     fontface = 1,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     angle = 0,
                                     lineheight = 1.2
                               ),

                               draw_panel = function(data, panel_params, coord,
                                                     scale_depth = TRUE,
                                                     na.rm = FALSE) {

                                     # Validate coord
                                     validate_coord3d(coord)

                                     if (nrow(data) == 0) {
                                           return(grid::nullGrob())
                                     }

                                     # Transform through coord (3D -> 2D projection)
                                     coords <- coord$transform(data, panel_params)

                                     # Scale sizes by depth if enabled
                                     if (scale_depth && "depth_scale" %in% names(coords)) {
                                           coords$size <- coords$size * coords$depth_scale
                                     }

                                     # Convert size from mm to grid fontsize
                                     # ggplot2 size aesthetic is in mm, we need to convert appropriately
                                     fontsize_pts <- coords$size * .pt

                                     # Sort by depth (back to front) for proper rendering order
                                     if ("depth" %in% names(coords)) {
                                           coords <- coords[order(coords$depth, decreasing = TRUE), ]
                                     }

                                     # Create text grobs for each label
                                     grobs <- lapply(seq_len(nrow(coords)), function(i) {
                                           grid::textGrob(
                                                 label = coords$label[i],
                                                 x = coords$x[i],
                                                 y = coords$y[i],
                                                 default.units = "npc",
                                                 hjust = coords$hjust[i],
                                                 vjust = coords$vjust[i],
                                                 rot = coords$angle[i],
                                                 gp = grid::gpar(
                                                       col = alpha(coords$colour[i], coords$alpha[i]),
                                                       fontsize = fontsize_pts[i],
                                                       fontfamily = coords$family[i],
                                                       fontface = coords$fontface[i],
                                                       lineheight = coords$lineheight[i]
                                                 )
                                           )
                                     })

                                     do.call(grid::grobTree, grobs)
                               },

                               draw_key = ggplot2::draw_key_text
)


# User-facing functions -------------------------------------------------------

#' 3D text labels
#'
#' `geom_text_3d()` renders text labels in 3D space. Two rendering methods are
#' available: `"billboard"` (default) renders text as flat labels that always
#' face the camera, while `"polygon"` renders text as filled polygons that can
#' be oriented in any direction.
#'
#' @section Methods:
#' \itemize{
#'   \item `"billboard"` (default): Fast, simple text rendering using native
#'     fonts. Text is always parallel to the view plane (like a billboard).
#'     Only uses the `colour` aesthetic for text color.
#'   \item `"polygon"`: Renders text as filled polygon outlines. Text can be
#'     oriented to face any direction using the `facing` parameter. Uses both
#'     `fill` (text fill) and `colour` (text outline) aesthetics.
#' }
#'
#' @section Orientation (polygon method only):
#' The `facing` parameter controls which direction the text faces:
#' \itemize{
#'   \item `"zmax"` (default): Text faces upward (readable from above)
#'   \item `"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`, `"zmin"`: Text faces
#'     toward the specified cube face
#'   \item A numeric vector `c(x, y, z)`: Text faces the specified direction
#'   \item `camera_facing(...)`: Text faces the camera
#' }
#'
#' The `angle` parameter rotates the text around its facing axis (in degrees).
#' With `angle = 0`, the text baseline is oriented as horizontally as possible
#' (parallel to the x-y plane when the facing direction allows).
#'
#' @section Camera-facing text:
#' For billboard method, text automatically faces the camera - no configuration
#' needed.
#'
#' For polygon method, there are two ways to make text face the camera:
#'
#' 1. Use `facing = "camera"` with the `coord` parameter (recommended):
#' ```
#' my_coord <- coord_3d(pitch = 20, roll = -60, yaw = -30)
#' ggplot(df, aes(x, y, z = z, label = label)) +
#'   geom_text_3d(method = "polygon", facing = "camera", coord = my_coord) +
#'   my_coord
#' ```
#'
#' 2. Pass `camera_facing()` directly to `facing`:
#' ```
#' ggplot(df, aes(x, y, z = z, label = label)) +
#'   geom_text_3d(method = "polygon",
#'                facing = camera_facing(pitch = 20, roll = -60, yaw = -30)) +
#'   coord_3d(pitch = 20, roll = -60, yaw = -30)
#' ```
#'
#' @section Aspect Ratio Correction:
#' For polygon method, text can appear stretched if the x, y, and z axes have
#' different data ranges. Providing a `coord_3d()` object via the `coord`
#' parameter enables automatic aspect ratio correction based on the coord's
#' `ratio` and `scales` settings.
#'
#' If other layers in your plot have different data ranges that affect the
#' final scale limits, you can fine-tune the correction with `aspect_adjust`:
#' - `aspect_adjust = 1` (default): Use the computed correction
#' - `aspect_adjust > 1`: Make text taller
#' - `aspect_adjust < 1`: Make text wider
#'
#' @section Size and Scaling:
#' Text size is specified in mm via the `size` parameter, matching the units
#' used by [ggplot2::geom_text()]. The default size of 3.88 corresponds to
#' approximately 11pt text.
#'
#' Both methods use the same size units, so switching between `"billboard"`
#' and `"polygon"` methods with the same `size` value will produce similarly
#' sized text (assuming a typical 6 inch plot).
#'
#' When `scale_depth = TRUE` (the default), text farther from the viewer
#' appears smaller, creating a natural perspective effect. For billboard
#' method, this scales the font size. For polygon method, this scales the
#' outline linewidth (the polygon fill is always depth-scaled by the 3D
#' projection).
#'
#' @param mapping Set of aesthetic mappings. See [ggplot2::aes()].
#' @param data A data frame containing the variables to plot.
#' @param position Position adjustment. Defaults to `"identity"`.
#' @param ... Other arguments passed to the layer.
#' @param method Rendering method: `"billboard"` (default) for flat camera-facing
#'   text, or `"polygon"` for 3D-oriented filled text.
#' @param facing (Polygon method only) Direction the text should face. One of
#'   `"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`, `"zmin"`, `"zmax"` (default),
#'   `"camera"`, a numeric vector of length 3, or a `camera_facing()` specification.
#'   Use `"camera"` with the `coord` parameter to automatically face the camera.
#'   Ignored with a warning for billboard method.
#' @param coord (Polygon method only) A `coord_3d()` object. When provided:
#'   (1) if `facing = "camera"`, extracts rotation parameters automatically;
#'   (2) uses the coord's `ratio` and `scales` parameters to correct text aspect
#'   ratio so text doesn't appear stretched. Ignored for billboard method.
#' @param aspect_adjust (Polygon method only) Scalar multiplier for text height
#'   relative to width. Default is 1 (normal proportions). Values > 1 make text
#'   taller, values < 1 make text wider. When `coord` is provided, this is
#'   multiplied by the computed aspect correction. Ignored for billboard method.
#' @param light (Polygon method only) A lighting specification created by
#'   [light()], or `NULL` for no lighting. When provided, text polygons are
#'   shaded based on their facing direction. Ignored for billboard method.
#' @param angle Rotation angle in degrees. For polygon method, rotates around
#'   the facing axis. For billboard method, rotates in the view plane.
#' @param nudge_x,nudge_y,nudge_z Offset to apply to text position in data units.
#' @param family Font family.
#' @param fontface Font face (for billboard method): "plain", "bold", "italic",
#'   or "bold.italic".
#' @param weight (Polygon method only) Font weight: "normal", "bold", "thin",
#'   "light", etc.
#' @param italic (Polygon method only) Logical; use italic font variant.
#' @param size Font size in mm (same units as [ggplot2::geom_text()]). Default
#'   is 3.88, which corresponds to approximately 11pt text.
#' @param hjust,vjust Horizontal and vertical justification of text (0-1).
#' @param lineheight Line height multiplier for multi-line text.
#' @param spacing (Polygon method only) Additional letter spacing in em units.
#' @param tolerance (Polygon method only) Bezier curve tolerance for text
#'   outlines. Lower values give smoother curves but more vertices.
#' @param scale_depth Logical; if TRUE (default), scale by depth for perspective
#'   effect. For billboard method, this scales the font size. For polygon method,
#'   this scales the outline linewidth (the polygon fill is always depth-scaled
#'   by the 3D projection).
#' @param rule (Polygon method only) Fill rule for polygons with holes.
#'   Either "evenodd" (default) or "winding".
#' @param na.rm If TRUE, silently remove missing values.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If TRUE, inherit aesthetics from the plot's default.
#'
#' @section Aesthetics:
#' `geom_text_3d()` understands the following aesthetics (required in bold):
#'
#' **Both methods:**
#' \itemize{
#'   \item \strong{x}, \strong{y}, \strong{z}, \strong{label}
#'   \item `alpha`
#' }
#'
#' **Billboard method:**
#' \itemize{
#'   \item `colour` - text color
#'   \item `size` - font size in mm
#'   \item `family`, `fontface`
#'   \item `hjust`, `vjust`
#'   \item `angle` - rotation in view plane
#'   \item `lineheight`
#' }
#'
#' **Polygon method:**
#' \itemize{
#'   \item `fill` - text fill color
#'   \item `colour` - text outline color
#'   \item `linewidth` - outline thickness
#' }
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' df <- expand.grid(x = c("H", "B"), y = c("a", "o", "u"), z = c("g", "t"))
#' df$label <- paste0(df$x, df$y, df$z)
#'
#' # Billboard text (default) - automatically faces camera
#' ggplot(df, aes(x, y, z, label = label)) +
#'   geom_text_3d() +
#'   coord_3d(scales = "fixed")
#'
#' # Polygon text - can face any direction
#' ggplot(df, aes(x, y, z, label = label)) +
#'   geom_text_3d(method = "polygon", facing = "zmax") +
#'   coord_3d(scales = "fixed")
#'
#' # Polygon text facing camera (using coord parameter)
#' my_coord <- coord_3d(pitch = 0, roll = -60, yaw = -30, scales = "fixed")
#' ggplot(df, aes(x, y, z, label = label)) +
#'   geom_text_3d(method = "polygon", facing = "camera", coord = my_coord,
#'                color = "red", linewidth = .1) +
#'   my_coord
#'
#' # Larger text with styling
#' my_coord2 <- coord_3d(scales = "fixed")
#' ggplot(df, aes(x, y, z, label = label, fill = factor(x))) +
#'   geom_text_3d(method = "polygon", facing = "xmin", coord = my_coord2,
#'                family = "serif", weight = "bold", italic = TRUE) +
#'   my_coord2
#'
#' @seealso [camera_facing()] for camera-facing specifications, [text_outlines()]
#'   for text-to-polygon conversion
#' @export
geom_text_3d <- function(mapping = NULL, data = NULL,
                         position = "identity",
                         ...,
                         method = c("billboard", "polygon"),
                         # Polygon parameters
                         facing = "zmax",
                         coord = NULL,
                         aspect_adjust = 1,
                         light = NULL,
                         # Shared parameters
                         angle = 0,
                         nudge_x = 0,
                         nudge_y = 0,
                         nudge_z = 0,
                         size = 3.88,
                         hjust = 0.5,
                         vjust = 0.5,
                         lineheight = 1.2,
                         scale_depth = TRUE,
                         # Polygon-only parameters
                         family = "sans",
                         weight = "normal",
                         italic = FALSE,
                         fontface = "plain",
                         spacing = 0,
                         tolerance = 0.01,
                         rule = "evenodd",
                         # Common layer parameters
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

      method <- match.arg(method)

      if (method == "billboard") {
            # Warn if polygon-specific parameters are non-default
            if (!missing(facing) && !identical(facing, "zmax")) {
                  warning("`facing` is ignored for billboard method. ",
                          "Billboard text automatically faces the camera.", call. = FALSE)
            }
            if (!is.null(coord)) {
                  warning("`coord` is ignored for billboard method.", call. = FALSE)
            }
            if (aspect_adjust != 1) {
                  warning("`aspect_adjust` is ignored for billboard method.", call. = FALSE)
            }
            if (!is.null(light)) {
                  warning("`light` is ignored for billboard method.", call. = FALSE)
            }

            # Convert fontface string to numeric if needed
            fontface_num <- switch(as.character(fontface),
                                   "plain" = 1, "bold" = 2,
                                   "italic" = 3, "bold.italic" = 4,
                                   as.numeric(fontface))

            layer(
                  data = data,
                  mapping = mapping,
                  stat = StatText3DBillboard,
                  geom = GeomText3DBillboard,
                  position = position,
                  show.legend = show.legend,
                  inherit.aes = inherit.aes,
                  params = list(
                        nudge_x = nudge_x,
                        nudge_y = nudge_y,
                        nudge_z = nudge_z,
                        size = size,
                        hjust = hjust,
                        vjust = vjust,
                        angle = angle,
                        lineheight = lineheight,
                        family = family,
                        fontface = fontface_num,
                        scale_depth = scale_depth,
                        na.rm = na.rm,
                        ...
                  )
            )
      } else {
            # Polygon method
            layer(
                  data = data,
                  mapping = mapping,
                  stat = StatText3DPolygon,
                  geom = GeomText3DPolygon,
                  position = position,
                  show.legend = show.legend,
                  inherit.aes = inherit.aes,
                  params = list(
                        facing = facing,
                        coord = coord,
                        aspect_adjust = aspect_adjust,
                        light = light,
                        angle = angle,
                        nudge_x = nudge_x,
                        nudge_y = nudge_y,
                        nudge_z = nudge_z,
                        family = family,
                        weight = weight,
                        italic = italic,
                        size = size,
                        hjust = hjust,
                        vjust = vjust,
                        lineheight = lineheight,
                        spacing = spacing,
                        tolerance = tolerance,
                        scale_depth = scale_depth,
                        rule = rule,
                        na.rm = na.rm,
                        ...
                  )
            )
      }
}


#' @rdname geom_text_3d
#' @param geom The geometric object to use. Defaults based on method.
#' @param stat The statistical transformation to use. Defaults based on method.
#' @export
stat_text_3d <- function(mapping = NULL, data = NULL,
                         geom = NULL, position = "identity",
                         ...,
                         method = c("billboard", "polygon"),
                         # Polygon parameters
                         facing = "zmax",
                         coord = NULL,
                         aspect_adjust = 1,
                         light = NULL,
                         # Shared parameters
                         angle = 0,
                         nudge_x = 0,
                         nudge_y = 0,
                         nudge_z = 0,
                         size = 3.88,
                         hjust = 0.5,
                         vjust = 0.5,
                         lineheight = 1.2,
                         scale_depth = TRUE,
                         # Polygon-only parameters
                         family = "sans",
                         weight = "normal",
                         italic = FALSE,
                         fontface = "plain",
                         spacing = 0,
                         tolerance = 0.01,
                         # Common layer parameters
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

      method <- match.arg(method)

      if (method == "billboard") {
            if (!missing(facing) && !identical(facing, "zmax")) {
                  warning("`facing` is ignored for billboard method. ",
                          "Billboard text automatically faces the camera.", call. = FALSE)
            }
            if (!is.null(coord)) {
                  warning("`coord` is ignored for billboard method.", call. = FALSE)
            }
            if (aspect_adjust != 1) {
                  warning("`aspect_adjust` is ignored for billboard method.", call. = FALSE)
            }
            if (!is.null(light)) {
                  warning("`light` is ignored for billboard method.", call. = FALSE)
            }

            fontface_num <- switch(as.character(fontface),
                                   "plain" = 1, "bold" = 2,
                                   "italic" = 3, "bold.italic" = 4,
                                   as.numeric(fontface))

            geom <- geom %||% GeomText3DBillboard

            layer(
                  data = data,
                  mapping = mapping,
                  stat = StatText3DBillboard,
                  geom = geom,
                  position = position,
                  show.legend = show.legend,
                  inherit.aes = inherit.aes,
                  params = list(
                        nudge_x = nudge_x,
                        nudge_y = nudge_y,
                        nudge_z = nudge_z,
                        size = size,
                        hjust = hjust,
                        vjust = vjust,
                        angle = angle,
                        lineheight = lineheight,
                        family = family,
                        fontface = fontface_num,
                        scale_depth = scale_depth,
                        na.rm = na.rm,
                        ...
                  )
            )
      } else {
            geom <- geom %||% GeomText3DPolygon

            layer(
                  data = data,
                  mapping = mapping,
                  stat = StatText3DPolygon,
                  geom = geom,
                  position = position,
                  show.legend = show.legend,
                  inherit.aes = inherit.aes,
                  params = list(
                        facing = facing,
                        coord = coord,
                        aspect_adjust = aspect_adjust,
                        light = light,
                        angle = angle,
                        nudge_x = nudge_x,
                        nudge_y = nudge_y,
                        nudge_z = nudge_z,
                        family = family,
                        weight = weight,
                        italic = italic,
                        size = size,
                        hjust = hjust,
                        vjust = vjust,
                        lineheight = lineheight,
                        spacing = spacing,
                        tolerance = tolerance,
                        na.rm = na.rm,
                        ...
                  )
            )
      }
}
