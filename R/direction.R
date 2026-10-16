#' Specify a direction using angles
#'
#' Converts a spherical direction given as azimuth and elevation into the
#' length-3 direction vector used by [coord_3d()] and [light()]. Both of those
#' arguments take a vector of the form `c(x, y, z)` whose magnitude is ignored;
#' `direction()` is an alternative way to express such a vector for people who
#' find angles easier to reason about than components.
#'
#' Azimuth is a compass-style bearing measured clockwise from the positive y
#' axis, matching the convention used by terrain shading functions such as
#' `terra::shade()`. Elevation is measured upward from the xy plane, so
#' `elevation = 90` points along the positive z axis and `elevation = -90`
#' points along the negative z axis.
#'
#' The returned vector is scaled to unit length, but since both `coord_3d()` and
#' `light()` normalize their inputs, the magnitude carries no meaning.
#'
#' @param azimuth Numeric bearing in degrees, measured clockwise from the
#'   positive y axis. Values outside `[0, 360)` are wrapped.
#' @param elevation Numeric angle in degrees above the xy plane, between -90
#'   and 90.
#'
#' @return A unit-length numeric vector of length 3.
#'
#' @seealso [coord_3d()] and [light()], which accept the result.
#'
#' @examples
#' # The default viewing angle, expressed as a direction
#' direction(210, 30)
#'
#' # The default lighting angle
#' direction(270, 63.43)
#'
#' # Light from directly overhead
#' light(direction = direction(0, 90))
#'
#' @export
direction <- function(azimuth, elevation) {
      if (!is.numeric(azimuth) || length(azimuth) != 1 || !is.finite(azimuth)) {
            stop("`azimuth` must be a single finite numeric value.")
      }
      if (!is.numeric(elevation) || length(elevation) != 1 || !is.finite(elevation)) {
            stop("`elevation` must be a single finite numeric value.")
      }
      if (elevation < -90 || elevation > 90) {
            stop("`elevation` must be between -90 and 90 degrees.")
      }

      az <- azimuth * pi / 180
      el <- elevation * pi / 180

      c(sin(az) * cos(el),
        cos(az) * cos(el),
        sin(el))
}

# Inverse of direction(): decompose a direction vector into azimuth and
# elevation in degrees. Azimuth is returned in [0, 360).
#
# A purely vertical direction has no azimuth, since its horizontal component
# vanishes. Those cases resolve to 180 degrees, which is the bearing whose
# corresponding rotation leaves the data y axis pointing up the screen.
direction_angles <- function(v) {
      v <- validate_direction(v)

      horizontal <- sqrt(v[1]^2 + v[2]^2)
      azimuth <- if (horizontal < 1e-10) 180 else atan2(v[1], v[2]) * 180 / pi
      elevation <- asin(v[3] / sqrt(sum(v^2))) * 180 / pi

      list(azimuth = azimuth %% 360, elevation = elevation)
}

validate_direction <- function(v, arg = "direction") {
      if (!is.numeric(v) || length(v) != 3 || any(!is.finite(v))) {
            stop("`", arg, "` must be a finite numeric vector of length 3.")
      }
      if (all(v == 0)) {
            stop("`", arg, "` must have at least one non-zero value.")
      }
      v
}

# Convert a camera direction into the Euler angles used by Coord3D.
#
# A direction fixes only where the camera sits, which is two degrees of
# freedom, while pitch/roll/yaw carry three. The extra degree of freedom is
# twist about the view axis, resolved here by keeping the data z axis pointing
# up the screen. That convention forces pitch to zero, leaving a closed form
# for the remaining two angles.
direction_rotation <- function(v) {
      angles <- direction_angles(v)
      list(pitch = 0,
           roll = angles$elevation - 90,
           yaw = 180 - angles$azimuth)
}
