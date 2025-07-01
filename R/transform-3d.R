
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

#' Transform 3D points using rotation and optional perspective
#'
#' @param data Data frame with x, y, z columns (in standard [-0.5, 0.5] domain)
#' @param proj A list of projection parameters
#' @param dist Distance from viewer to center (for perspective)
#' @return Data frame with transformed coordinates
transform_3d_standard <- function(data, proj = list(pitch = 0, roll = 0, yaw = 0, persp = TRUE, dist = 2)) {

      pitch <- proj$pitch
      roll <- proj$roll
      yaw <- proj$yaw
      persp <- proj$persp
      dist <- proj$dist

      # Apply rotation
      rotated <- rotate_3d(as.matrix(data[, c("x", "y", "z")]), pitch, roll, yaw)

      # Apply perspective if needed
      if (persp) {
            transformed <- apply_perspective(rotated, dist)
      } else {
            transformed <- rotated
      }

      # Return transformed coordinates
      data.frame(
            x = transformed[, 1],
            y = transformed[, 2],
            z = transformed[, 3]
      )
}

#' Scale data to standard domain [-0.5, 0.5]
#'
#' @param values Vector of values to scale
#' @param data_range Original range of the data [min, max]
#' @return Scaled values in [-0.5, 0.5] domain
scale_to_standard <- function(values, data_range) {
      # Handle case where data has zero range (single point or constant values)
      range_width <- diff(data_range)
      if (range_width == 0 || is.na(range_width)) {
            # If all values are the same, center them at 0 in standard domain
            return(rep(0, length(values)))
      }

      # Scale to [0, 1] then shift to [-0.5, 0.5]
      (values - data_range[1]) / range_width - 0.5
}
