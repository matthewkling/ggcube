#' Lighting specification for 3D surface rendering
#'
#' Creates a lighting specification object for use with 3D polygon layers.
#' Lighting modifies the brightness of fill and/or base color aesthetics based on surface
#' orientation (i.e., it implements form shadows but not cast shadows).
#' Various options are available to control light qualities and light source location.
#'
#' Note that light-like effects can also be achieved in some stats by mapping color
#' aesthestics to computed variables such as `after_stat(dzdx)`; see [stat_surface_3d()]
#' for examples.
#'
#' @param method Character string specifying lighting model:
#'   \itemize{
#'     \item \code{"diffuse"}: The default. Atmospheric lighting with soft shadows (only surfaces pointing
#'          directly away from light source are fully dark; base color occurs on surfaces perpendicular to light)
#'     \item \code{"direct"}: Direct lighting with hard shadows (all surfaces angled beyond 90 degrees from
#'          light source are fully dark; base color occurs on surfaces angled 45 degrees toward light)
#'     \item \code{"normal_rgb"}: Map surface normals to RGB colors
#'   }
#' @param mode Character string specifying color lighting mode:
#'   \itemize{
#'     \item \code{"hsv"}: The default. Modifies _value_ component of HSV color (fades to bright colors at high end, black at low end)
#'     \item \code{"hsl"}: Modifies _lightness_ component of HSL color (fades to white at high end, black at low end)
#'   }
#' @param contrast Numeric value greater than zero controlling the intensity of lighting effects.
#'   1.0 (the default) gives full black-to-white range. Values less than 1 give subtler effects, while
#'   values greater than 1 give more dramatic effects.
#' @param direction Numeric vector of length 3 specifying direction in 3D space that
#'   light comes from for directional lighting. The default is \code{c(1, 0, 1)}, giving
#'   diagonal lighting from the upper right edge with default rotation. Common examples: \code{c(0, 0, 1)} gives
#'   overhead lighting, \code{c(1, 0, 0)} lights surfaces facing the positive x
#'   direction, and \code{c(-1, -1, 0)} lights surfaces facing negative x-y edge. At least
#'   one value must be non-zero. Values are automatically normalized, so magnitude doesn't
#'   matter, only sign and relative magnitude. Direction is relative to the data axes,
#'   not the rotated figure. This argument is ignored if \code{position} is provided.
#' @param position Numeric vector of length 3 specifying light source position in
#'   data coordinate space for positional lighting. When specified, each face gets
#'   its own light direction calculated from the light position to the face center.
#'   Mutually exclusive with \code{direction}. Default is NULL (use directional lighting).
#' @param distance_falloff Logical indicating whether to apply distance-based
#'   intensity falloff for positional lighting using inverse square law
#'   (intensity ∝ 1/distance²). Only used when \code{position} is specified.
#'   Default is FALSE.
#' @param fill Logical indicating whether to apply lighting to fill colors.
#'   Default is TRUE.
#' @param color Logical indicating whether to apply lighting to border/line colors.
#'   Default is TRUE.
#'
#' @return A \code{lighting} object that can be passed to 3D surface stats.
#' @examples
#' # base plot used in examples
#' p <- ggplot(mountain, aes(x, y, z)) + coord_3d(ratio = c(1, 1.5, 1))
#'
#'
#' # Light qualities ------------------------
#'
#' # default diffuse lighting
#' p + stat_surface_3d(fill = "steelblue", color = "black")
#'
#' # use "hsl" mode to fade highlights to white
#' p + stat_surface_3d(fill = "steelblue", color = "black",
#'                     light = light(mode = "hsl"))
#'
#' # adjust lighting intensity with `contrast`
#' p + stat_surface_3d(fill = "steelblue", color = "black",
#'                     light = light(mode = "hsl", contrast = 2))
#'
#' # use "direct" lighting to apply full shade to unlit surfaces
#' p + stat_surface_3d(fill = "steelblue", color = "black",
#'                     light = light(method = "direct", contrast = .75))
#'
#' # use "rgb" to plot each face orientation in a unique color
#' p + stat_surface_3d(light = light(method = "normal_rgb"))
#'
#'
#' # Lighting targets -----------------------
#'
#' # use `fill` and `color` to select which aesthetics get lighting
#' p + stat_surface_3d(fill = "steelblue", color = "black",
#'                     light = light(fill = TRUE, color = FALSE))
#'
#' # disable lighting entirely
#' # (equivalent to specifying `light(fill = FALSE, color = FALSE`))
#' p + stat_surface_3d(fill = "steelblue", color = "black", light = NULL)
#'
#' # apply lighting on top of aesthetic mapping, with shaded guide
#' p + stat_surface_3d(aes(fill = z, color = z),
#'                     light = light(contrast = 2)) +
#'       scale_fill_viridis_c() +
#'       scale_color_viridis_c() +
#'       guides(fill = guide_colorbar_3d())
#'
#'
#' # Light sources -----------------------
#'
#' # set directional light as horizontal from back left corner
#' # (left = negative x, back = positive y, horizontal = neutral z)
#' p + stat_surface_3d(fill = "steelblue", color = "black",
#'                     light = light(direction = c(-1, 1, 0)))
#'
#' # specify positional light source within plot
#' p + stat_surface_3d(fill = "red", color = "red",
#'                     light = light(position = c(.5, .7, 95),
#'                                   distance_falloff = TRUE,
#'                                   mode = "hsl", contrast = .9))
#'
#' @seealso \code{\link{stat_surface_3d}}, \code{\link{stat_voxel_3d}}, \code{\link{stat_pillar_3d}}, \code{\link{scale_colorbar_shade}}
#' @export
light <- function(method = "diffuse",
                     direction = c(1, 0, 1),
                     position = NULL,
                     distance_falloff = FALSE,
                     fill = TRUE,
                     color = TRUE,
                     mode = "hsv",
                     contrast = 1.0) {

      # Validate method
      valid_methods <- c("direct", "diffuse", "normal_rgb")
      if (!method %in% valid_methods) {
            stop("method must be one of: ", paste(valid_methods, collapse = ", "))
      }

      # Validate direction
      if (!is.numeric(direction) || length(direction) != 3) {
            stop("direction must be a numeric vector of length 3")
      }

      if (all(direction == 0)) {
            stop("direction must contain at least one nonzero value")
      }

      # Validate position if provided
      if (!is.null(position)) {
            if (!is.numeric(position) || length(position) != 3) {
                  stop("position must be a numeric vector of length 3")
            }
      }

      # Validate distance_falloff
      if (!is.logical(distance_falloff) || length(distance_falloff) != 1) {
            stop("distance_falloff must be a single logical value")
      }

      # Validate fill and color
      if (!is.logical(fill) || length(fill) != 1) {
            stop("fill must be a single logical value")
      }
      if (!is.logical(color) || length(color) != 1) {
            stop("color must be a single logical value")
      }

      # Validate contrast
      if (!is.numeric(contrast) || length(contrast) != 1 || contrast < 0) {
            stop("contrast must be a single non-negative numeric value")
      }

      # Validate mode
      valid_modes <- c("hsv", "hsl")
      if (!mode %in% valid_modes) {
            stop("mode must be one of: ", paste(valid_modes, collapse = ", "))
      }

      # Convert new API to old internal representation for backward compatibility
      if (fill && color) {
            shade <- "both"
      } else if (fill) {
            shade <- "fill"
      } else if (color) {
            shade <- "colour"
      } else {
            shade <- "neither"
      }

      # Create lighting specification object using original internal structure
      structure(
            list(
                  method = method,
                  direction = direction,
                  position = position,
                  distance_falloff = distance_falloff,
                  shade = shade,
                  shade_strength = contrast,
                  shade_mode = mode
            ),
            class = "light"
      )
}

#' @export
print.light <- function(x, ...) {
      cat("Lighting specification:\n")
      cat("  Method:", x$method, "\n")

      if (!is.null(x$position)) {
            cat("  Position: [", paste(x$position, collapse = ", "), "] (positional lighting)\n")
            if (x$distance_falloff) {
                  cat("  Distance falloff: enabled\n")
            } else {
                  cat("  Distance falloff: disabled\n")
            }
      } else {
            cat("  Direction: [", paste(x$direction, collapse = ", "), "] (directional lighting)\n")
      }

      # Show what gets shaded using new API terms
      shading_targets <- c()
      if (x$shade %in% c("fill", "both")) shading_targets <- c(shading_targets, "fill")
      if (x$shade %in% c("colour", "both")) shading_targets <- c(shading_targets, "color")

      if (length(shading_targets) > 0) {
            cat("  Shading:", paste(shading_targets, collapse = " + "),
                "(contrast =", x$shade_strength, ", mode =", x$shade_mode, ")\n")
      } else {
            cat("  Shading: disabled\n")
      }

      invisible(x)
}


#' Apply lighting models to surface normals with positional light support
#'
#' Computes lighting values from surface normals using various lighting models.
#' Supports both directional lighting (parallel rays) and positional lighting
#' (point light sources with per-face light directions).
#'
#' @param normals Matrix with 3 columns (x, y, z normal components), where each
#'   row represents a face normal vector. Should be unit vectors (normalized).
#' @param light A lighting specification object created by \code{light()}
#' @param face_centers Matrix with 3 columns (x, y, z coordinates) representing
#'   the center position of each face in data coordinate space. Required for
#'   positional lighting, optional for directional lighting.
#' @return Vector of lighting values. For most methods, returns numeric values.
#'   For \code{method = "normal_rgb"}, returns hex color strings with \code{I()}
#'   class for identity scaling.
#' @keywords internal
compute_light <- function(normals, lighting, face_centers = NULL) {

      # Validate inputs
      if (!is.matrix(normals) || ncol(normals) != 3) {
            stop("normals must be a matrix with 3 columns (x, y, z)")
      }

      params <- lighting

      # Validate resolved parameters
      valid_light <- c("direct", "diffuse", "normal_rgb", "normal_x", "normal_y", "normal_z")
      if (!params$method %in% valid_light) {
            stop("lighting method must be one of: ", paste(valid_light, collapse = ", "))
      }

      # Check if positional lighting is requested
      use_positional <- !is.null(params$position)

      if (use_positional) {
            # Positional lighting: need face centers
            if (is.null(face_centers)) {
                  stop("face_centers required for positional lighting")
            }
            if (!is.matrix(face_centers) || ncol(face_centers) != 3 || nrow(face_centers) != nrow(normals)) {
                  stop("face_centers must be a matrix with 3 columns and same number of rows as normals")
            }

            # Calculate per-face light directions
            light_directions <- calculate_positional_light_directions(face_centers, params$position)

            # Calculate per-face dot products
            dot_products <- rowSums(normals * light_directions)

            # Apply distance falloff if requested
            if (params$distance_falloff) {
                  distances <- sqrt(rowSums((face_centers - matrix(rep(params$position, nrow(face_centers)),
                                                                   nrow = nrow(face_centers), byrow = TRUE))^2))
                  # Prevent division by zero and extreme values
                  distances <- pmax(distances, 0.1)
                  falloff_factor <- 1 / (distances^2)
                  # Normalize falloff to prevent extreme values
                  falloff_factor <- falloff_factor / max(falloff_factor)

                  # For diffuse lighting, interpolate between -1 (dark) and full lighting value
                  if (params$method == "diffuse") {
                        dot_products <- -1 + falloff_factor * (dot_products + 1)
                  } else {
                        # For other methods, scale toward appropriate dark value
                        if (params$method == "direct") {
                              # Direct: interpolate between 0 (dark) and full value
                              dot_products <- dot_products * falloff_factor
                        } else {
                              # General case: scale the lighting values
                              dot_products <- dot_products * falloff_factor
                        }
                  }
            }

      } else {
            # Directional lighting: uniform direction for all faces
            if (!is.numeric(params$direction) || length(params$direction) != 3) {
                  stop("light direction must be a numeric vector of length 3")
            }

            # Normalize light direction
            light_dir_norm <- normalize_light_direction(params$direction)

            # Compute dot products between normals and light direction
            dot_products <- compute_light_dot_products(normals, light_dir_norm)
      }

      # Apply lighting model (same for both directional and positional)
      light <- switch(params$method,
                      direct = pmax(0, dot_products),             # Direct lighting with hard shadows
                      diffuse = dot_products,                     # Atmospheric lighting: full -1 to +1 range
                      normal_rgb = {                              # Map normals to RGB hex colors
                            if (use_positional) {
                                  # For positional lighting, use average light direction for RGB mapping
                                  avg_light_dir <- colMeans(light_directions)
                                  compute_normal_rgb_light(normals, avg_light_dir)
                            } else {
                                  light_dir_norm <- normalize_light_direction(params$direction)
                                  compute_normal_rgb_light(normals, light_dir_norm)
                            }
                      },
                      normal_x = (normals[,1] + 1) / 2,           # X-normal as color (0-1 range)
                      normal_y = (normals[,2] + 1) / 2,           # Y-normal as color (0-1 range)
                      normal_z = (normals[,3] + 1) / 2,           # Z-normal as color (0-1 range)
                      stop("Unknown lighting method")
      )

      # Apply quantization if requested
      if (!is.null(params$quanta) && !params$method %in% c("normal_rgb", "normal_x", "normal_y", "normal_z")) {
            light <- apply_quantization(light, params$method, params$quanta)
      }

      # Auto-wrap RGB colors with I() for identity scaling
      if (params$method == "normal_rgb") {
            light <- I(light)
      }

      return(light)
}

#' Calculate per-face light directions for positional lighting
#'
#' @param face_centers Matrix with 3 columns (x, y, z face center coordinates)
#' @param light_position Numeric vector of length 3 (x, y, z light position)
#' @return Matrix with 3 columns (normalized light direction vectors)
#' @keywords internal
calculate_positional_light_directions <- function(face_centers, light_position) {
      # Calculate light directions from each face center TO the light position
      light_vectors <- matrix(rep(light_position, nrow(face_centers)),
                              nrow = nrow(face_centers), byrow = TRUE) - face_centers

      # Normalize each light direction vector
      light_lengths <- sqrt(rowSums(light_vectors^2))
      # Prevent division by zero
      light_lengths <- pmax(light_lengths, 1e-10)

      # Return normalized light directions
      light_vectors / light_lengths
}

#' Normalize light direction vector
#'
#' @param light_dir Numeric vector of length 3
#' @return Normalized light direction vector
#' @keywords internal
normalize_light_direction <- function(light_dir) {
      light_dir / sqrt(sum(light_dir^2))
}

#' Compute dot products between normals and light direction
#'
#' @param normals Matrix with 3 columns (x, y, z normal components)
#' @param light_dir_norm Normalized light direction vector
#' @return Vector of dot products
#' @keywords internal
compute_light_dot_products <- function(normals, light_dir_norm) {
      rowSums(normals * matrix(rep(light_dir_norm, nrow(normals)),
                               nrow = nrow(normals), byrow = TRUE))
}

#' Compute normal-to-RGB mapping with light direction rotation
#'
#' Maps surface normals to RGB colors with rotation so that normals aligned
#' with light_dir map to white/bright colors.
#'
#' @param normals Matrix with 3 columns (x, y, z normal components)
#' @param light_dir_norm Normalized light direction vector
#' @return Character vector of hex color codes
#' @keywords internal
compute_normal_rgb_light <- function(normals, light_dir_norm) {

      # Target direction for white color in RGB space
      white_dir <- c(1, 1, 1) / sqrt(3)

      # Create rotation matrix using Rodrigues' formula
      # We want to rotate FROM white_dir TO light_dir_norm
      # So we can apply the inverse rotation to normals

      # Cross product for rotation axis (white_dir × light_dir_norm)
      v <- c(
            white_dir[2] * light_dir_norm[3] - white_dir[3] * light_dir_norm[2],
            white_dir[3] * light_dir_norm[1] - white_dir[1] * light_dir_norm[3],
            white_dir[1] * light_dir_norm[2] - white_dir[2] * light_dir_norm[1]
      )

      s <- sqrt(sum(v^2))  # sine of angle
      c <- sum(white_dir * light_dir_norm)  # cosine of angle

      if (s < 1e-10) {
            # Vectors are already aligned or opposite
            if (c > 0) {
                  rot_matrix <- diag(3)  # Identity matrix
            } else {
                  # 180 degree rotation - use any perpendicular axis
                  rot_matrix <- -diag(3)
            }
      } else {
            # Rodrigues' rotation formula
            # R = I + [v]× + [v]×^2 * (1-c)/s^2
            v_cross <- matrix(c(
                  0, -v[3], v[2],
                  v[3], 0, -v[1],
                  -v[2], v[1], 0
            ), nrow = 3, byrow = TRUE)

            rot_matrix <- diag(3) + v_cross + v_cross %*% v_cross * ((1 - c) / s^2)
      }

      # Apply rotation to normals
      rotated_normals <- normals %*% rot_matrix

      # Scale normals from [-1,1] to [0,1] for RGB
      r <- (rotated_normals[,1] + 1) / 2
      g <- (rotated_normals[,2] + 1) / 2
      b <- (rotated_normals[,3] + 1) / 2

      # Convert to hex colors
      rgb(r, g, b)
}


#' Compute lighting from standardized coordinates in coord_3d
#'
#' @param data Original data frame with lighting_spec column
#' @param standardized_coords Data frame with standardized x, y, z coordinates
#' @return data frame with lighting values and normal components added
#' @keywords internal
compute_light_in_coord <- function(data, standardized_coords, scale_ranges, scales, ratio) {

      # Extract lighting specification from first row (all rows have same spec)
      light <- data$lighting_spec[[1]]

      # Transform light position, if applicable
      light$position <- transform_light_position(light$position, scale_ranges, scales, ratio)

      # Replace original coordinates with standardized ones for lighting computation
      data$x <- standardized_coords$x
      data$y <- standardized_coords$y
      data$z <- standardized_coords$z

      # Get unique faces for normal/lighting computation
      faces <- data %>%
            group_by(group) %>%
            slice(1) %>%  # One row per face
            ungroup()

      # Compute normals based on data type
      if (grepl("sh3d__hull", faces$group[1])) {
            # Hull/triangle data: compute normals from vertex coordinates
            normals <- compute_triangle_normals(data, faces)
      } else if ("face_type" %in% names(faces)) {
            # Voxel/pillar data: axis-aligned normals
            normals <- compute_axis_aligned_normals(faces)
      } else {
            # Surface-like data: normals from fitted plane gradients
            gradients <- compute_surface_gradients_from_vertices(data)
            normals <- compute_surface_normals(gradients)
      }

      # Compute face centers from standardized coordinates
      face_centers <- calculate_face_centers(data)

      # Apply lighting models
      faces <- apply_surface_light(faces, normals, face_centers, light)

      # Merge face-level lighting vars back into vertex-level data set
      data <- left_join(data, faces, by = join_by(group))

      if (light$method == "normal_rgb") {
            data$light <- I(data$light)
      }

      # Add lighting parameters for shade processing in geom
      data$shade_enabled <- light$shade
      data$shade_strength <- light$shade_strength
      data$shade_mode <- light$shade_mode
      data$lighting_method <- light$method

      # Remove lighting column (no longer needed)
      data$lighting_spec <- NULL

      return(data)
}

compute_surface_gradients_from_vertices <- function(data) {
      # Compute both gradients in one pass using least squares normal equations
      gradients <- data %>%
            group_by(group) %>%
            summarise(
                  n_pts = n(),
                  # Sums needed for least squares: z = a*x + b*y + c
                  sum_x = sum(x), sum_y = sum(y), sum_z = sum(z),
                  sum_xx = sum(x*x), sum_yy = sum(y*y),
                  sum_xy = sum(x*y), sum_xz = sum(x*z), sum_yz = sum(y*z),
                  .groups = "drop"
            ) %>%
            mutate(
                  # Means
                  mean_x = sum_x / n_pts, mean_y = sum_y / n_pts, mean_z = sum_z / n_pts,

                  # Centered sums of squares and cross products
                  sxx = sum_xx - n_pts * mean_x^2,
                  syy = sum_yy - n_pts * mean_y^2,
                  sxy = sum_xy - n_pts * mean_x * mean_y,
                  sxz = sum_xz - n_pts * mean_x * mean_z,
                  syz = sum_yz - n_pts * mean_y * mean_z,

                  # Determinant for solving 2x2 system
                  det = sxx * syy - sxy^2,

                  # Gradients from normal equations (or fallback to 0)
                  dzdx = ifelse(abs(det) > 1e-10, (sxz * syy - syz * sxy) / det, 0),
                  dzdy = ifelse(abs(det) > 1e-10, (syz * sxx - sxz * sxy) / det, 0)
            ) %>%
            select(group, dzdx, dzdy)

      return(gradients)
}

#' Compute triangle normals from vertex coordinates
#'
#' @param data Face vertex data
#' @param face_data Unique face data
#' @return Matrix with normalized normal vectors (one row per face, 3 columns)
#' @keywords internal
compute_triangle_normals <- function(data, face_data) {

      # Extract hull ID from hierarchical names (e.g., "hulls__hull53629_tri333" -> "hull53629")
      data <- data %>%
            mutate(hull_id = sub(".*__(hull[0-9]+)_.*", "\\1", group))

      # Compute hull center per hull group
      hull_centers <- data %>%
            group_by(hull_id) %>%
            summarise(
                  hull_center_x = mean(x),
                  hull_center_y = mean(y),
                  hull_center_z = mean(z),
                  .groups = "drop"
            )

      # Rest of computation using hull_id instead of top_group
      normals_data <- data %>%
            group_by(group) %>%
            summarise(
                  hull_id = first(hull_id),
                  # Cross product computation (same as before)
                  x1 = x[1], y1 = y[1], z1 = z[1],
                  x2 = x[2], y2 = y[2], z2 = z[2],
                  x3 = x[3], y3 = y[3], z3 = z[3],
                  .groups = "drop"
            ) %>%
            # Join with appropriate hull center
            left_join(hull_centers, by = "hull_id") %>%
            mutate(
                  # [rest of normal computation same as before]
                  v1_x = x2 - x1, v1_y = y2 - y1, v1_z = z2 - z1,
                  v2_x = x3 - x1, v2_y = y3 - y1, v2_z = z3 - z1,
                  normal_x = v1_y * v2_z - v1_z * v2_y,
                  normal_y = v1_z * v2_x - v1_x * v2_z,
                  normal_z = v1_x * v2_y - v1_y * v2_x,
                  normal_length = sqrt(normal_x^2 + normal_y^2 + normal_z^2),
                  normal_x = ifelse(normal_length > 0, normal_x / normal_length, 0),
                  normal_y = ifelse(normal_length > 0, normal_y / normal_length, 0),
                  normal_z = ifelse(normal_length > 0, normal_z / normal_length, 1),

                  face_center_x = (x1 + x2 + x3) / 3,
                  face_center_y = (y1 + y2 + y3) / 3,
                  face_center_z = (z1 + z2 + z3) / 3,

                  to_face_x = face_center_x - hull_center_x,
                  to_face_y = face_center_y - hull_center_y,
                  to_face_z = face_center_z - hull_center_z,

                  dot_product = normal_x * to_face_x + normal_y * to_face_y + normal_z * to_face_z,
                  normal_x = ifelse(dot_product < 0, -normal_x, normal_x),
                  normal_y = ifelse(dot_product < 0, -normal_y, normal_y),
                  normal_z = ifelse(dot_product < 0, -normal_z, normal_z)
            )

      return(as.matrix(normals_data[, c("normal_x", "normal_y", "normal_z")]))
}

#' Compute surface normals from face gradients
#'
#' @param face_data Data frame with unique faces containing dzdx and dzdy
#' @return Matrix with normalized normal vectors (one row per face, 3 columns)
#' @keywords internal
compute_surface_normals <- function(face_data) {
      # Compute surface normals from gradients
      normals <- matrix(nrow = nrow(face_data), ncol = 3)
      normals[, 1] <- -face_data$dzdx  # Normal x component
      normals[, 2] <- -face_data$dzdy  # Normal y component
      normals[, 3] <- 1                # Normal z component

      # Normalize the normal vectors
      normal_lengths <- sqrt(rowSums(normals^2))
      normals <- normals / normal_lengths

      return(normals)
}

#' Calculate face centers for positional lighting
#'
#' @param faces Data frame with all face vertices
#' @return Matrix with face centers (one row per face, 3 columns)
#' @keywords internal
calculate_face_centers <- function(faces) {

      # Vectorized computation using group_by instead of loops
      centers <- faces %>%
            group_by(group) %>%
            summarise(
                  center_x = mean(x),
                  center_y = mean(y),
                  center_z = mean(z),
                  .groups = "drop"
            )

      # Convert to matrix format expected by lighting functions
      face_centers <- as.matrix(centers[, c("center_x", "center_y", "center_z")])

      return(face_centers)
}

#' Compute normals for axis-aligned faces (voxels/pillars)
#'
#' @param face_data Data frame with unique faces containing face_type column
#' @return Matrix with normalized normal vectors (one row per face, 3 columns)
#' @keywords internal
compute_axis_aligned_normals <- function(face_data) {

      # Lookup table for axis-aligned face normals
      normal_lookup <- data.frame(
            face_type = c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
            normal_x = c(-1, 1, 0, 0, 0, 0),
            normal_y = c(0, 0, -1, 1, 0, 0),
            normal_z = c(0, 0, 0, 0, -1, 1),
            stringsAsFactors = FALSE
      )

      # Vectorized lookup - merge face_data with normals
      face_normals <- face_data %>%
            left_join(normal_lookup, by = "face_type") %>%
            mutate(
                  # Handle any unknown face types with default upward normal
                  normal_x = ifelse(is.na(normal_x), 0, normal_x),
                  normal_y = ifelse(is.na(normal_y), 0, normal_y),
                  normal_z = ifelse(is.na(normal_z), 1, normal_z)
            )

      # Return as matrix format expected by lighting functions
      normals <- as.matrix(face_normals[, c("normal_x", "normal_y", "normal_z")])

      return(normals)
}

#' Apply lighting models to surface normals
#'
#' @param face_data Data frame with unique faces (group column)
#' @param normals Matrix of surface normals
#' @param face_centers Matrix of face centers
#' @param light Lighting specification object
#' @return Data frame with lighting values and normal components
#' @keywords internal
apply_surface_light <- function(face_data, normals, face_centers, light) {
      # Apply lighting models to the normals
      light_vals <- compute_light(normals, light, face_centers)

      face_data_with_light <- select(face_data, group) %>%
            mutate(light = light_vals,
                   normal_x = normals[, 1],
                   normal_y = normals[, 2],
                   normal_z = normals[, 3])

      return(face_data_with_light)
}


#' Convert RGB to HSL color space
#'
#' @param rgb_matrix 3xN matrix with RGB values in [0,1] range
#' @return 3xN matrix with HSL values (H in [0,1], S in [0,1], L in [0,1])
#' @keywords internal
rgb2hsl <- function(rgb_matrix) {
      if (!is.matrix(rgb_matrix) || nrow(rgb_matrix) != 3) {
            stop("rgb_matrix must be a 3xN matrix")
      }

      r <- rgb_matrix[1, ]
      g <- rgb_matrix[2, ]
      b <- rgb_matrix[3, ]

      max_val <- pmax(r, g, b)
      min_val <- pmin(r, g, b)
      diff <- max_val - min_val

      # Lightness
      l <- (max_val + min_val) / 2

      # Initialize hue and saturation
      h <- rep(0, ncol(rgb_matrix))
      s <- rep(0, ncol(rgb_matrix))

      # Calculate saturation
      non_zero_diff <- diff > 0
      s[non_zero_diff] <- ifelse(
            l[non_zero_diff] < 0.5,
            diff[non_zero_diff] / (max_val[non_zero_diff] + min_val[non_zero_diff]),
            diff[non_zero_diff] / (2 - max_val[non_zero_diff] - min_val[non_zero_diff])
      )

      # Calculate hue
      if (any(non_zero_diff)) {
            # Red is max
            red_max <- non_zero_diff & (r == max_val)
            h[red_max] <- ((g[red_max] - b[red_max]) / diff[red_max]) %% 6

            # Green is max
            green_max <- non_zero_diff & (g == max_val)
            h[green_max] <- (b[green_max] - r[green_max]) / diff[green_max] + 2

            # Blue is max
            blue_max <- non_zero_diff & (b == max_val)
            h[blue_max] <- (r[blue_max] - g[blue_max]) / diff[blue_max] + 4

            h[non_zero_diff] <- h[non_zero_diff] / 6
      }

      return(rbind(h, s, l))
}

#' Convert HSL to RGB color space
#'
#' @param hsl_matrix 3xN matrix with HSL values (H in [0,1], S in [0,1], L in [0,1])
#' @return 3xN matrix with RGB values in [0,1] range
#' @keywords internal
hsl2rgb <- function(hsl_matrix) {
      if (!is.matrix(hsl_matrix) || nrow(hsl_matrix) != 3) {
            stop("hsl_matrix must be a 3xN matrix")
      }

      h <- hsl_matrix[1, ]
      s <- hsl_matrix[2, ]
      l <- hsl_matrix[3, ]

      # Initialize RGB
      r <- rep(0, ncol(hsl_matrix))
      g <- rep(0, ncol(hsl_matrix))
      b <- rep(0, ncol(hsl_matrix))

      # Check for grayscale (s == 0)
      grayscale <- s == 0
      r[grayscale] <- l[grayscale]
      g[grayscale] <- l[grayscale]
      b[grayscale] <- l[grayscale]

      # Process colored pixels
      colored <- !grayscale
      if (any(colored)) {
            q <- ifelse(l[colored] < 0.5,
                        l[colored] * (1 + s[colored]),
                        l[colored] + s[colored] - l[colored] * s[colored])
            p <- 2 * l[colored] - q

            r[colored] <- hue2rgb(p, q, h[colored] + 1/3)
            g[colored] <- hue2rgb(p, q, h[colored])
            b[colored] <- hue2rgb(p, q, h[colored] - 1/3)
      }

      return(rbind(r, g, b))
}

# Helper function for hue to RGB conversion
hue2rgb <- function(p, q, t) {
      t <- ifelse(t < 0, t + 1, t)
      t <- ifelse(t > 1, t - 1, t)

      result <- p
      result <- ifelse(t < 1/6, p + (q - p) * 6 * t, result)
      result <- ifelse(t >= 1/6 & t < 1/2, q, result)
      result <- ifelse(t >= 1/2 & t < 2/3, p + (q - p) * (2/3 - t) * 6, result)

      return(result)
}


# Blend lighting values with base colors using HSV or HSL color spaces
blend_light_with_colors <- function(base_colors, light_values, lighting) {

      if (length(base_colors) != length(light_values)) {
            stop("base_colors and light_values must have the same length")
      }

      if (lighting$method == "normal_rgb") {
            warning("Color shading is not supported with normal_rgb lighting method")
            return(base_colors)
      }

      # Handle any invalid values
      valid_mask <- !is.na(base_colors) & !is.na(light_values) & is.finite(light_values)
      if (!any(valid_mask)) {
            return(base_colors)
      }

      result_colors <- base_colors

      # Process only valid entries
      valid_base <- base_colors[valid_mask]
      valid_light <- light_values[valid_mask]

      # Normalize lighting values to [0, 1] if needed
      if (lighting$method == "diffuse") {
            valid_light <- (valid_light + 1) / 2
      }
      valid_light <- pmax(0, pmin(1, valid_light))

      # Convert to RGB matrix
      base_rgb <- col2rgb(valid_base)

      if (lighting$shade_mode == "hsl") {
            # HSL blending mode
            base_rgb_norm <- base_rgb / 255  # Normalize to [0,1] for HSL
            base_hsl <- rgb2hsl(base_rgb_norm)

            # Modify only lightness (L component)
            new_hsl <- base_hsl

            for (i in seq_along(valid_light)) {
                  h_val <- base_hsl[1, i]
                  s_val <- base_hsl[2, i]
                  l_val <- base_hsl[3, i]
                  light_val <- valid_light[i]

                  if (light_val > 0.5) {
                        # Brighten toward white (L = 1)
                        blend_factor <- (light_val - 0.5) * 2 * lighting$shade_strength
                        new_l <- l_val + blend_factor * (1 - l_val)
                  } else {
                        # Darken toward black (L = 0)
                        blend_factor <- (0.5 - light_val) * 2 * lighting$shade_strength
                        new_l <- l_val * (1 - blend_factor)
                  }

                  new_hsl[1, i] <- h_val  # Preserve hue
                  new_hsl[2, i] <- s_val  # Preserve saturation
                  new_hsl[3, i] <- pmax(0, pmin(1, new_l))  # Clamp lightness
            }

            # Convert back to RGB
            new_rgb <- hsl2rgb(new_hsl)
            new_colors <- rgb(new_rgb[1, ], new_rgb[2, ], new_rgb[3, ])

      } else {
            # HSV blending mode
            base_hsv <- rgb2hsv(base_rgb)

            # Modify only brightness (V component)
            new_hsv <- base_hsv

            for (i in seq_along(valid_light)) {
                  h_val <- base_hsv[1, i]
                  s_val <- base_hsv[2, i]
                  v_val <- base_hsv[3, i]
                  light_val <- valid_light[i]

                  # Handle NaN hue (gray colors)
                  if (is.nan(h_val)) h_val <- 0

                  if (light_val > 0.5) {
                        # Brighten
                        blend_factor <- (light_val - 0.5) * 2 * lighting$shade_strength
                        new_v <- v_val + blend_factor * (1 - v_val)
                  } else {
                        # Darken
                        blend_factor <- (0.5 - light_val) * 2 * lighting$shade_strength
                        new_v <- v_val * (1 - blend_factor)
                  }

                  new_hsv[1, i] <- h_val
                  new_hsv[2, i] <- s_val
                  new_hsv[3, i] <- pmax(0, pmin(1, new_v))
            }

            # Convert back to colors
            new_colors <- hsv(new_hsv[1, ], new_hsv[2, ], new_hsv[3, ])
      }

      result_colors[valid_mask] <- new_colors

      return(result_colors)
}


blend_light <- function(coords) {

      # Extract lighting parameters from special columns
      if ("shade_enabled" %in% names(coords) && coords$shade_enabled[1] != "neither") {
            # Reconstruct lighting object from columns
            light <- list(
                  shade = coords$shade_enabled[1],
                  shade_strength = coords$shade_strength[1],
                  shade_mode = coords$shade_mode[1],
                  method = coords$lighting_method[1]
            )

            # Apply shading if enabled
            if (light$shade != "neither" && "light" %in% names(coords)) {
                  # Blend fill colors if requested
                  if (light$shade %in% c("fill", "both")) {
                        coords$fill <- blend_light_with_colors(coords$fill, coords$light, light)
                  }

                  # Blend colour/border colors if requested
                  if (light$shade %in% c("colour", "both")) {
                        coords$colour <- blend_light_with_colors(coords$colour, coords$light, light)
                  }
            }

            # Clean up lighting parameter columns
            coords <- coords[, !names(coords) %in% c("shade_enabled", "shade_strength", "shade_mode", "lighting_method")]
      }

      return(coords)
}

# convert light position from data units to cube units
transform_light_position <- function(position, scale_ranges, scales, ratio) {
      if(is.null(position)) return(NULL)
      light_data <- data.frame(x = position[1], y = position[2], z = position[3])
      standardized <- scale_to_standard(light_data, scale_ranges, scales, ratio)
      return(c(standardized$x, standardized$y, standardized$z))
}

# store lighting specification to data frame for downstream use
attach_light <- function(data, light){
      if(!is.null(light)) data$lighting_spec <- I(list(light))
      data
}
