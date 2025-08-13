#' Create lighting specification for 3D surface rendering
#'
#' Creates a lighting specification object that supports both directional lighting
#' (parallel rays like sunlight) and positional lighting (point light sources with
#' per-face light directions and optional distance falloff), for use with 3D polygon geoms.
#'
#' @param method Character string specifying lighting model:
#'   \itemize{
#'     \item \code{"diffuse"}: Atmospheric lighting with soft shadows (includes negative values for subsurface scattering effect)
#'     \item \code{"direct"}: Direct lighting with hard shadows (surfaces facing away are completely dark)
#'     \item \code{"normal_rgb"}: Map surface normals to RGB colors
#'   }
#'   Default is "diffuse".
#' @param direction Numeric vector of length 3 specifying direction in 3D space that
#'   light comes from for directional lighting. The default is \code{c(1, 0, 1)}, giving
#'   diagonal lighting from the upper right edge with default rotation. Common examples: \code{c(0, 0, 1)} gives
#'   overhead lighting, \code{c(1, 0, 0)} lights surfaces facing the positive x
#'   direction, and \code{c(-1, -1, 0)} lights surfaces facing negative x-y edge. At least
#'   one value must be non-zero. Values are automatically normalized, so magnitude doesn't
#'   matter, only sign and relative magnitude.
#'   This argument is ignored if \code{position} is provided.
#' @param position Numeric vector of length 3 specifying light source position in
#'   data coordinate space for positional lighting. When specified, each face gets
#'   its own light direction calculated from the light position to the face center.
#'   Mutually exclusive with \code{direction}. Default is NULL (use directional lighting).
#' @param distance_falloff Logical indicating whether to apply distance-based
#'   intensity falloff for positional lighting using inverse square law
#'   (intensity ∝ 1/distance²). Only used when \code{position} is specified.
#'   Default is FALSE.
#' @param fill Logical indicating whether to apply lighting effects to fill colors.
#'   Default is FALSE.
#' @param color Logical indicating whether to apply lighting effects to border/line colors.
#'   Default is FALSE.
#' @param mode Character string specifying color shading mode:
#'   \itemize{
#'     \item \code{"hsv"}: Modifies HSV value component (fades to bright colors at high end, black at low end)
#'     \item \code{"hsl"}: Modifies HSL lightness component (fades to white at high end, black at low end)
#'   }
#'   Default is "hsv".
#' @param strength Numeric value in the range 0--1 controlling the intensity of lighting effects.
#'   1.0 gives full black-to-white range, 0.5 gives subtle lighting effects.
#'   Default is 1.0.
#'
#' @details
#' Lighting in this package works through shading - modifying the brightness
#' of fill and/or color aesthetics based on computed lighting values. This approach
#' allows lighting effects while still using standard ggplot2 color specifications.
#'
#' @return A \code{lighting} object that can be passed to 3D surface stats.
#' @examples
#' # Default diffuse lighting (no shading)
#' lighting()
#'
#' # Directional lighting with fill shading
#' lighting(method = "direct", direction = c(1, 0, 0.5), fill = TRUE)
#'
#' # Rainbow normal mapping
#' lighting(method = "normal_rgb")
#'
#' # Apply to both fill and color
#' lighting(fill = TRUE, color = TRUE, strength = 0.8)
#'
#' # Positional lighting with distance falloff
#' lighting(position = c(10, 10, 20), distance_falloff = TRUE, fill = TRUE)
#' @export
lighting <- function(method = "diffuse",
                     direction = c(1, 0, 1),
                     position = NULL,
                     distance_falloff = FALSE,
                     fill = FALSE,
                     color = FALSE,
                     mode = "hsv",
                     strength = 1.0) {

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

      # Validate strength
      if (!is.numeric(strength) || length(strength) != 1 || strength < 0) {
            stop("strength must be a single non-negative numeric value")
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
                  shade_strength = strength,
                  shade_mode = mode
            ),
            class = "lighting"
      )
}

#' @export
print.lighting <- function(x, ...) {
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
                "(strength =", x$shade_strength, ", mode =", x$shade_mode, ")\n")
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
#' @param lighting A lighting specification object created by \code{lighting()}
#' @param face_centers Matrix with 3 columns (x, y, z coordinates) representing
#'   the center position of each face in data coordinate space. Required for
#'   positional lighting, optional for directional lighting.
#' @return Vector of lighting values. For most methods, returns numeric values.
#'   For \code{method = "normal_rgb"}, returns hex color strings with \code{I()}
#'   class for identity scaling.
#' @keywords internal
compute_lighting <- function(normals, lighting = lighting(), face_centers = NULL) {

      # Validate inputs
      if (!is.matrix(normals) || ncol(normals) != 3) {
            stop("normals must be a matrix with 3 columns (x, y, z)")
      }

      params <- lighting

      # Validate resolved parameters
      valid_lighting <- c("direct", "diffuse", "normal_rgb", "normal_x", "normal_y", "normal_z")
      if (!params$method %in% valid_lighting) {
            stop("lighting method must be one of: ", paste(valid_lighting, collapse = ", "))
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
                                  compute_normal_rgb_lighting(normals, avg_light_dir)
                            } else {
                                  light_dir_norm <- normalize_light_direction(params$direction)
                                  compute_normal_rgb_lighting(normals, light_dir_norm)
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
compute_normal_rgb_lighting <- function(normals, light_dir_norm) {

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

#' Apply quantization to continuous lighting values
#'
#' @param light_values Numeric vector of continuous lighting values
#' @param method Character string: "direct" or "diffuse"
#' @param quanta Integer number of quantization levels
#' @return Quantized lighting values mapped to [0, 1] range
#' @keywords internal
apply_quantization <- function(light_values, method, quanta) {
      if (method == "diffuse") {
            # Create n equal bins across [-1, 1]
            breaks <- seq(-1, 1, length.out = quanta + 1)
            quantized <- cut(light_values, breaks = breaks, labels = FALSE, include.lowest = TRUE)
            # Map back to [-1, 1] range (preserve original range for shading)
            -1 + 2 * (quantized - 1) / (quanta - 1)
      } else if (method == "direct") {
            # One bin for negatives, (quanta-1) bins for [0, 1]
            result <- numeric(length(light_values))

            # All negative values get level 0
            negative_mask <- light_values <= 0
            result[negative_mask] <- 0

            # Positive values get quantized into (quanta-1) bins
            positive_mask <- light_values > 0
            if (any(positive_mask)) {
                  pos_vals <- light_values[positive_mask]
                  # Create (quanta-1) bins across [0, 1]
                  breaks <- seq(0, 1, length.out = quanta)
                  quantized <- cut(pos_vals, breaks = breaks, labels = FALSE, include.lowest = TRUE)
                  # Map to levels 1/(quanta-1), 2/(quanta-1), ..., 1
                  result[positive_mask] <- quantized / (quanta - 1)
            }

            result
      } else {
            # Should not reach here for valid methods
            light_values
      }
}
