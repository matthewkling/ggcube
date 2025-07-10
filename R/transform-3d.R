
#' Rotate points in 3D space
#'
#' @param xyz Matrix of x, y, z coordinates
#' @param pitch Rotation around x-axis in degrees
#' @param roll Rotation around y-axis in degrees
#' @param yaw Rotation around z-axis in degrees
#' @return Rotated coordinates matrix
rotate_3d <- function(xyz, pitch, roll, yaw) {
      # Convert angles to radians
      pitch_rad <- pitch * pi / 180
      roll_rad <- roll * pi / 180
      yaw_rad <- yaw * pi / 180

      # Create rotation matrix
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

      # Apply rotation
      rotated <- xyz %*% rot

      return(rotated)
}

#' Apply perspective effect to 3D points
#'
#' @param rotated Matrix of rotated x, y, z coordinates
#' @param dist Distance from viewer to center of scene
#' @return Matrix with perspective effect applied
apply_perspective <- function(rotated, dist) {
      z_offset <- rotated[, 3] + dist
      perspective_factor <- dist / z_offset
      rotated[, 1] <- rotated[, 1] * perspective_factor
      rotated[, 2] <- rotated[, 2] * perspective_factor
      return(rotated)
}

#' Transform 3D points using rotation and optional perspective with viewpoint distance
#'
#' @param data Data frame with x, y, z columns (in standard [-0.5, 0.5] domain)
#' @param proj A list of projection parameters
#' @return Data frame with transformed coordinates and depth for sorting
transform_3d_standard <- function(data, proj = list(pitch = 0, roll = 0, yaw = 0, persp = TRUE, dist = 2)) {

      pitch <- proj$pitch
      roll <- proj$roll
      yaw <- proj$yaw
      persp <- proj$persp
      dist <- proj$dist

      # Apply rotation
      rotated <- rotate_3d(as.matrix(data[, c("x", "y", "z")]), pitch, roll, yaw)

      if (persp) {
            # Calculate true distance from viewpoint (camera at 0,0,-dist in rotated space)
            depth <- sqrt(rotated[, 1]^2 + rotated[, 2]^2 + (rotated[, 3] + dist)^2)

            # Apply perspective transformation
            transformed <- apply_perspective(rotated, dist)

            # Return transformed coordinates with viewpoint depth
            return(data.frame(
                  x = transformed[, 1],
                  y = transformed[, 2],
                  z = transformed[, 3],  # Keep z for face visibility calculations
                  depth = depth          # True viewpoint distance for sorting
            ))
      } else {
            # Orthographic: depth is just negative z (farther = larger depth)
            return(data.frame(
                  x = rotated[, 1],
                  y = rotated[, 2],
                  z = rotated[, 3],      # Keep z for face visibility
                  depth = -rotated[, 3]  # Use for sorting
            ))
      }
}

' Scale data to standard domain with aspect ratio control
#'
#' @param values Vector of values to scale (for single axis) OR data frame with x,y,z columns (for multi-axis)
#' @param data_range Original range of the data [min, max] (for single axis) OR list with x,y,z scale ranges (for multi-axis)
#' @param scales Aspect ratio behavior ("free" or "fixed") (only used for multi-axis)
#' @param ratio Length-3 numeric vector of axis ratios (only used for multi-axis)
#' @return Scaled values in [-0.5, 0.5] domain (single axis) OR data frame with scaled coordinates (multi-axis)
scale_to_standard <- function(values, data_range, scales = "free", ratio = c(1, 1, 1)) {

      # Handle single-axis case
      if (is.numeric(data_range) && length(data_range) == 2) {
            # Handle case where data has zero range (single point or constant values)
            range_width <- diff(data_range)
            if (range_width == 0 || is.na(range_width)) {
                  # If all values are the same, center them at 0 in standard domain
                  return(rep(0, length(values)))
            }

            # for discrete scales, get the position rather than the raw value
            if(!is.numeric(values)) values <- attr(values, "pos")

            # Scale to [0, 1] then shift to [-0.5, 0.5]
            return((values - data_range[1]) / range_width - 0.5)
      }

      # Handle multi-axis case
      data <- values
      scale_ranges <- data_range  # This is actually scale limits, not raw data ranges

      # Compute effective ratios based on aspect setting
      effective_ratios <- compute_effective_ratios(scale_ranges, scales, ratio)

      # Apply standard scaling for each axis
      return(data.frame(x = scale_to_standard(data$x, scale_ranges$x) * effective_ratios[1],
                        y = scale_to_standard(data$y, scale_ranges$y) * effective_ratios[2],
                        z = scale_to_standard(data$z, scale_ranges$z) * effective_ratios[3]))
}

# Helper function to compute effective ratios
compute_effective_ratios <- function(scale_ranges, scales, ratio) {
      if (scales == "free") {
            # Apply ratios directly to standardized coordinates
            effective_ratios <- ratio
      } else {
            # scales == "fixed": adjust ratios based on scale ranges (including expansion)
            x_span <- diff(scale_ranges$x)
            y_span <- diff(scale_ranges$y)
            z_span <- diff(scale_ranges$z)

            # Handle zero-width ranges
            if (x_span == 0) x_span <- 1
            if (y_span == 0) y_span <- 1
            if (z_span == 0) z_span <- 1

            # Apply user ratios to scale-space proportions
            scale_proportional <- c(x_span, y_span, z_span)
            effective_ratios <- ratio * scale_proportional
      }

      # Normalize so largest dimension gets [-0.5, 0.5]
      max_ratio <- max(effective_ratios)
      return(effective_ratios / max_ratio)
}
