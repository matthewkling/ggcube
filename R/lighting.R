#' Create lighting specification for 3D surface rendering
#'
#' Creates a lighting specification object that encapsulates lighting parameters
#' for use with 3D surface stats like \code{stat_surface()} and \code{stat_terrain()}.
#' This provides a clean, reusable way to specify complex lighting configurations.
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
#' @param direction Numeric vector of length 3 specifying light direction in 3D space.
#'   For most lighting methods, this defines the illumination direction.
#'   For \code{method = "normal_rgb"}, this defines the orientation of the RGB
#'   coordinate system (surfaces facing this direction will be brightest).
#'   Default is \code{c(1, 1, 1)} (diagonal lighting from upper-right-front).
#' @param levels Integer number of discrete levels for \code{method = "quantize"}.
#'   Default is 3.
#' @param clamp_negative Logical indicating whether to clamp negative lighting values
#'   to the lowest level when using \code{method = "quantize"}. If \code{TRUE},
#'   surfaces facing away from light get the darkest level. If \code{FALSE},
#'   the full range from -1 to 1 is quantized. Default is \code{TRUE}.
#'
#' @return A \code{lighting} object that can be passed to 3D surface stats.
#'
#' @examples
#' library(ggplot2)
#' data(mountain)
#'
#' # Basic Lambert lighting (classic hillshading)
#' ggplot(mountain, aes(x, y, z = z)) +
#'   stat_terrain(aes(fill = after_stat(light)),
#'                lighting = lighting("lambert")) +
#'   scale_fill_gradient(low = "black", high = "white") +
#'   coord_3d()
#'
#' # Quantized lighting (cel shading effect)
#' ggplot(mountain, aes(x, y, z = z)) +
#'   stat_terrain(aes(fill = after_stat(light)),
#'                lighting = lighting("quantize", levels = 5)) +
#'   scale_fill_gradient(low = "darkgreen", high = "white") +
#'   coord_3d()
#'
#' # Normal-to-RGB coloring with custom light orientation
#' ggplot(mountain, aes(x, y, z = z)) +
#'   stat_terrain(aes(fill = after_stat(light)),
#'                lighting = lighting("normal_rgb", direction = c(-1, 1, 1))) +
#'   coord_3d()
#'
#' # Custom light direction for dramatic shadows
#' ggplot(mountain, aes(x, y, z = z)) +
#'   stat_surface(aes(fill = after_stat(light)),
#'                lighting = lighting("lambert", direction = c(1, 0, 0.5))) +
#'   scale_fill_gradient2(low = "navy", mid = "grey", high = "white") +
#'   coord_3d()
#'
#' @seealso \code{\link{stat_surface}}, \code{\link{stat_terrain}} for using lighting specifications
#' @export
lighting <- function(method = "lambert",
                     direction = c(1, 1, 1),
                     levels = 3,
                     clamp_negative = TRUE) {

      # Validate method
      valid_methods <- c("lambert", "signed", "ambient", "quantize",
                         "normal_rgb", "normal_x", "normal_y", "normal_z")
      if (!method %in% valid_methods) {
            stop("method must be one of: ", paste(valid_methods, collapse = ", "))
      }

      # Validate direction
      if (!is.numeric(direction) || length(direction) != 3) {
            stop("direction must be a numeric vector of length 3")
      }

      # Validate levels
      if (!is.numeric(levels) || length(levels) != 1 || levels < 2) {
            stop("levels must be a single integer >= 2")
      }

      # Validate clamp_negative
      if (!is.logical(clamp_negative) || length(clamp_negative) != 1) {
            stop("clamp_negative must be a single logical value")
      }

      # Create lighting specification object
      structure(
            list(
                  method = method,
                  direction = direction,
                  levels = as.integer(levels),
                  clamp_negative = clamp_negative
            ),
            class = "lighting"
      )
}

#' @export
print.lighting <- function(x, ...) {
      cat("Lighting specification:\n")
      cat("  Method:", x$method, "\n")
      cat("  Direction: [", paste(x$direction, collapse = ", "), "]\n")
      if (x$method == "quantize") {
            cat("  Levels:", x$levels, "\n")
            cat("  Clamp negative:", x$clamp_negative, "\n")
      }
      invisible(x)
}



#' Apply lighting models to surface normals
#'
#' Computes lighting values from surface normals using various lighting models.
#' This function is shared between stat_surface and stat_terrain to ensure
#' consistent lighting behavior.
#'
#' @param normals Matrix with 3 columns (x, y, z normal components), where each
#'   row represents a face normal vector. Should be unit vectors (normalized).
#' @param lighting A lighting specification object created by \code{lighting()}
#' @return Vector of lighting values. For most methods, returns numeric values.
#'   For \code{method = "normal_rgb"}, returns hex color strings with \code{I()}
#'   class for identity scaling.
compute_lighting <- function(normals, lighting = lighting("lambert")) {

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

      if (!is.numeric(params$direction) || length(params$direction) != 3) {
            stop("light direction must be a numeric vector of length 3")
      }

      # Normalize light direction
      light_dir_norm <- normalize_light_direction(params$direction)

      # Compute dot products between normals and light direction
      dot_products <- compute_light_dot_products(normals, light_dir_norm)

      # Apply lighting model
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
                            compute_normal_rgb_lighting(normals, light_dir_norm)
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
