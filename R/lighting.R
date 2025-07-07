#' Create lighting specification for 3D surface rendering with positional light support
#'
#' Creates a lighting specification object that supports both directional lighting
#' (parallel rays like sunlight) and positional lighting (point light sources with
#' per-face light directions and optional distance falloff).
#'
#' @param method Character string specifying lighting model:
#'   \itemize{
#'     \item \code{"lambert"}: Standard diffuse lighting (surfaces facing away are dark)
#'     \item \code{"signed"}: Continuous lighting gradient including negative values
#'     \item \code{"ambient"}: Uniform lighting with no directional component
#'     \item \code{"quantize"}: Quantized lighting with discrete levels
#'     \item \code{"normal_rgb"}: Map surface normals to RGB colors
#'     \item \code{"normal_x"}, \code{"normal_y"}, \code{"normal_z"}: Individual normal components
#'   }
#' @param direction Numeric vector of length 3 specifying light direction in 3D space
#'   for directional lighting. Only used if \code{position} is NULL. Default is \code{c(1, 1, 1)}.
#' @param position Numeric vector of length 3 specifying light source position in
#'   data coordinate space for positional lighting. When specified, each face gets
#'   its own light direction calculated from the light position to the face center.
#'   Mutually exclusive with \code{direction}. Default is NULL (use directional lighting).
#' @param distance_falloff Logical indicating whether to apply distance-based
#'   intensity falloff for positional lighting using inverse square law
#'   (intensity ∝ 1/distance²). Only used when \code{position} is specified.
#'   Default is FALSE.
#' @param levels Integer number of discrete levels for \code{method = "quantize"}.
#'   Default is 3.
#' @param clamp_negative Logical indicating whether to clamp negative lighting values
#'   to the lowest level when using \code{method = "quantize"}. Default is TRUE.
#' @param blend Logical indicating whether to blend lighting with existing fill colors
#'   rather than replacing them. When TRUE, lighting modulates the base fill colors
#'   to create highlights and shadows. Default is FALSE.
#' @param blend_strength Numeric value controlling the intensity of lighting blending.
#'   1.0 gives full black-to-white range, 0.5 gives subtle lighting effects.
#'   Only used when \code{blend = TRUE}. Default is 1.0.
#' @param highlight_color Color to blend toward for bright areas when blending.
#'   Default is "white".
#' @param shadow_color Color to blend toward for dark areas when blending.
#'   Default is "black".
#'
#' @return A \code{lighting} object that can be passed to 3D surface stats.
#'
#' @examples
#' library(ggplot2)
#' data(mountain)
#'
#' # Directional lighting (classic hillshading)
#' ggplot(mountain, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)),
#'                light = lighting("lambert", direction = c(1, 1, 1))) +
#'   coord_3d()
#'
#' # Blended lighting with material colors
#' ggplot(mountain, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = z),
#'                light = lighting("lambert", blend = TRUE)) +
#'   scale_fill_viridis_c() +
#'   coord_3d()
#'
#' # Custom highlight/shadow colors
#' ggplot(mountain, aes(x, y, z = z)) +
#'   stat_surface(fill = "red",
#'                light = lighting("lambert", blend = TRUE,
#'                                highlight_color = "yellow",
#'                                shadow_color = "darkred")) +
#'   coord_3d()
#'
#' # Positional lighting (point light source)
#' ggplot(mountain, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)),
#'                light = lighting("lambert", position = c(50, 30, 200))) +
#'   coord_3d()
#'
#' # Positional lighting with distance falloff
#' ggplot(mountain, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)),
#'                light = lighting("lambert", position = c(50, 30, 200),
#'                                distance_falloff = TRUE)) +
#'   coord_3d()
#'
#' # Voxel scene with positional lighting
#' voxel_data <- data.frame(
#'   x = c(1, 2, 3, 2, 1),
#'   y = c(1, 1, 2, 3, 2),
#'   z = c(1, 2, 1, 1, 3)
#' )
#' ggplot(voxel_data, aes(x, y, z)) +
#'   stat_voxel(aes(fill = after_stat(light)),
#'              light = lighting("lambert", position = c(2, 2, 5))) +
#'   coord_3d()
#'
#' @seealso \code{\link{stat_surface}}, \code{\link{stat_voxel}}, \code{\link{stat_pillar}}
#' @export
lighting <- function(method = "lambert",
                     direction = c(1, 1, 1),
                     position = NULL,
                     distance_falloff = FALSE,
                     levels = 3,
                     clamp_negative = TRUE,
                     blend = FALSE,
                     blend_strength = 1.0,
                     highlight_color = "white",
                     shadow_color = "black") {

      # Validate method
      valid_methods <- c("lambert", "signed", "ambient", "quantize",
                         "normal_rgb", "normal_x", "normal_y", "normal_z")
      if (!method %in% valid_methods) {
            stop("method must be one of: ", paste(valid_methods, collapse = ", "))
      }

      # Validate mutually exclusive direction/position
      if (!is.null(position) && !identical(direction, c(1, 1, 1))) {
            stop("direction and position are mutually exclusive. Use either direction for directional lighting or position for positional lighting.")
      }

      # Validate direction
      if (!is.numeric(direction) || length(direction) != 3) {
            stop("direction must be a numeric vector of length 3")
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

      # Validate levels
      if (!is.numeric(levels) || length(levels) != 1 || levels < 2) {
            stop("levels must be a single integer >= 2")
      }

      # Validate clamp_negative
      if (!is.logical(clamp_negative) || length(clamp_negative) != 1) {
            stop("clamp_negative must be a single logical value")
      }

      # Validate blend
      if (!is.logical(blend) || length(blend) != 1) {
            stop("blend must be a single logical value")
      }

      # Validate blend_strength
      if (!is.numeric(blend_strength) || length(blend_strength) != 1 || blend_strength < 0) {
            stop("blend_strength must be a single non-negative numeric value")
      }

      # Validate colors
      tryCatch({
            col2rgb(highlight_color)
            col2rgb(shadow_color)
      }, error = function(e) {
            stop("highlight_color and shadow_color must be valid color specifications")
      })

      # Create lighting specification object
      structure(
            list(
                  method = method,
                  direction = direction,
                  position = position,
                  distance_falloff = distance_falloff,
                  levels = as.integer(levels),
                  clamp_negative = clamp_negative,
                  blend = blend,
                  blend_strength = blend_strength,
                  highlight_color = highlight_color,
                  shadow_color = shadow_color
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

      if (x$method == "quantize") {
            cat("  Levels:", x$levels, "\n")
            cat("  Clamp negative:", x$clamp_negative, "\n")
      }

      if (x$blend) {
            cat("  Blending: enabled (strength =", x$blend_strength, ")\n")
            cat("  Highlight color:", x$highlight_color, "\n")
            cat("  Shadow color:", x$shadow_color, "\n")
      } else {
            cat("  Blending: disabled\n")
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
compute_lighting <- function(normals, lighting = lighting("lambert"), face_centers = NULL) {

      # Validate inputs
      if (!is.matrix(normals) || ncol(normals) != 3) {
            stop("normals must be a matrix with 3 columns (x, y, z)")
      }

      params <- lighting

      # Validate resolved parameters
      valid_lighting <- c("lambert", "signed", "ambient", "quantize",
                          "normal_rgb", "normal_x", "normal_y", "normal_z")
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

                  # For signed lighting, interpolate between -1 (dark) and full lighting value
                  if (params$method == "signed") {
                        dot_products <- -1 + falloff_factor * (dot_products + 1)
                  } else {
                        # For other methods, scale toward appropriate dark value
                        if (params$method == "lambert") {
                              # Lambert: interpolate between 0 (dark) and full value
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
                      lambert = pmax(0, dot_products),          # Standard diffuse lighting
                      signed = dot_products,                    # Unclamped: full -1 to +1 range
                      ambient = rep(0.5, length(dot_products)), # Uniform lighting
                      quantize = {                              # Quantized lighting with parameters
                            if(params$clamp_negative) {
                                  # Negative values → lowest level (0)
                                  # Positive values → quantized across remaining levels
                                  result <- numeric(length(dot_products))

                                  # All negative values get level 0
                                  negative_mask <- dot_products <= 0
                                  result[negative_mask] <- 0

                                  # Positive values get quantized across levels 1 to n_levels
                                  positive_mask <- dot_products > 0
                                  if(any(positive_mask)) {
                                        pos_vals <- dot_products[positive_mask]
                                        # Quantize positive values into (n_levels - 1) bins
                                        breaks <- seq(0, 1, length.out = params$levels)  # n_levels-1 intervals
                                        quantized <- cut(pos_vals, breaks = breaks, labels = FALSE, include.lowest = TRUE)
                                        # Map to levels 1/(n_levels-1), 2/(n_levels-1), ..., 1
                                        result[positive_mask] <- quantized / (params$levels - 1)
                                  }
                                  result
                            } else {
                                  # Quantize full range [-1, 1]
                                  breaks <- seq(-1, 1, length.out = params$levels + 1)
                                  quantized <- cut(dot_products, breaks = breaks, labels = FALSE, include.lowest = TRUE)
                                  (quantized - 1) / (params$levels - 1)
                            }
                      },
                      normal_rgb = {                            # Map normals to RGB hex colors
                            if (use_positional) {
                                  # For positional lighting, use average light direction for RGB mapping
                                  avg_light_dir <- colMeans(light_directions)
                                  compute_normal_rgb_lighting(normals, avg_light_dir)
                            } else {
                                  light_dir_norm <- normalize_light_direction(params$direction)
                                  compute_normal_rgb_lighting(normals, light_dir_norm)
                            }
                      },
                      normal_x = (normals[,1] + 1) / 2,         # X-normal as color (0-1 range)
                      normal_y = (normals[,2] + 1) / 2,         # Y-normal as color (0-1 range)
                      normal_z = (normals[,3] + 1) / 2,         # Z-normal as color (0-1 range)
                      stop("Unknown lighting method")
      )

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
normalize_light_direction <- function(light_dir) {
      light_dir / sqrt(sum(light_dir^2))
}

#' Compute dot products between normals and light direction
#'
#' @param normals Matrix with 3 columns (x, y, z normal components)
#' @param light_dir_norm Normalized light direction vector
#' @return Vector of dot products
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
