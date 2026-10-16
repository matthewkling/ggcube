test_that("direction() returns a unit vector with the documented convention", {
      # Azimuth is a compass bearing clockwise from +y
      expect_equal(direction(0, 0), c(0, 1, 0))
      expect_equal(direction(90, 0), c(1, 0, 0))
      expect_equal(direction(180, 0), c(0, -1, 0))
      expect_equal(direction(270, 0), c(-1, 0, 0))

      # Elevation is measured up from the xy plane
      expect_equal(direction(0, 90), c(0, 0, 1))
      expect_equal(direction(123, -90), c(0, 0, -1))

      expect_equal(sqrt(sum(direction(37, 21)^2)), 1)
})

test_that("direction() validates its arguments", {
      expect_error(direction("a", 0), "azimuth")
      expect_error(direction(c(1, 2), 0), "azimuth")
      expect_error(direction(0, NA), "elevation")
      expect_error(direction(0, 91), "between -90 and 90")
      expect_error(direction(0, -91), "between -90 and 90")
})

test_that("direction_angles() inverts direction()", {
      for (az in seq(0, 345, by = 15)) {
            for (el in c(-75, -40, 0, 25, 60)) {
                  angles <- direction_angles(direction(az, el))
                  expect_equal(angles$azimuth, az)
                  expect_equal(angles$elevation, el)
            }
      }
})

test_that("direction_angles() ignores magnitude and handles vertical directions", {
      expect_equal(direction_angles(c(0, 2, 0))$azimuth, 0)
      expect_equal(direction_angles(c(0, 0.001, 0))$azimuth, 0)

      # A vertical direction has no bearing; it resolves to the one that leaves
      # the data y axis pointing up the plot.
      expect_equal(direction_angles(c(0, 0, 1))$azimuth, 180)
      expect_equal(direction_angles(c(0, 0, -1))$azimuth, 180)
      expect_equal(direction_angles(c(0, 0, 1))$elevation, 90)
})

test_that("direction_angles() validates its argument", {
      expect_error(direction_angles(c(0, 0, 0)), "non-zero")
      expect_error(direction_angles(c(1, 2)), "length 3")
      expect_error(direction_angles(c(1, 2, NA)), "finite")
})

test_that("documented defaults round-trip through the angular parameterization", {
      # coord_3d()'s default rotation
      view <- direction_angles(direction(210, 30))
      expect_equal(view$azimuth, 210)
      expect_equal(view$elevation, 30)

      rotation <- direction_rotation(direction(210, 30))
      expect_equal(rotation$pitch, 0)
      expect_equal(rotation$roll, -60)
      expect_equal(rotation$yaw, -30)

      # light()'s default direction
      angles <- direction_angles(c(-.5, 0, 1))
      expect_equal(angles$azimuth, 270)
      expect_equal(angles$elevation, 63.43495, tolerance = 1e-5)
})

test_that("coord_3d(direction =) reproduces the equivalent Euler angles", {
      by_angles <- coord_3d(pitch = 0, roll = -60, yaw = -30)[[1]]
      by_vector <- coord_3d(direction = direction(210, 30))[[1]]

      expect_equal(by_vector$pitch, by_angles$pitch)
      expect_equal(by_vector$roll, by_angles$roll)
      expect_equal(by_vector$yaw, by_angles$yaw)

      # Magnitude is ignored
      expect_equal(coord_3d(direction = c(3, 3, 3))[[1]]$yaw,
                   coord_3d(direction = c(1, 1, 1))[[1]]$yaw)
})

test_that("coord_3d(direction =) keeps the z axis pointing up the plot", {
      for (az in seq(0, 315, by = 45)) {
            for (el in c(-60, -15, 0, 35, 70)) {
                  coord <- coord_3d(direction = direction(az, el))[[1]]

                  # Image of the data z axis, which the renderer flips before
                  # rotating. Its horizontal component should vanish and its
                  # vertical component should be positive.
                  z_axis <- rotate_3d(matrix(c(0, 0, -1), nrow = 1),
                                      coord$pitch, coord$roll, coord$yaw)
                  expect_equal(z_axis[1, 1], 0)
                  expect_true(z_axis[1, 2] > 0)
            }
      }
})

test_that("coord_3d(direction =) places the viewer where asked", {
      # Bearings and elevations chosen to avoid axis-aligned directions, which
      # would leave the nearest corner ambiguous.
      for (az in c(30, 75, 210, 300)) {
            for (el in c(-50, 20, 80)) {
                  coord <- coord_3d(direction = direction(az, el))[[1]]

                  # Points further from the viewer project to larger z, so the
                  # nearest corner of the cube is the one in the direction the
                  # viewer sits.
                  corners <- expand.grid(x = c(-0.5, 0.5),
                                         y = c(-0.5, 0.5),
                                         z = c(-0.5, 0.5))
                  projected <- transform_3d_standard(
                        corners,
                        list(pitch = coord$pitch, roll = coord$roll,
                             yaw = coord$yaw, persp = FALSE, dist = 2)
                  )

                  nearest <- as.numeric(corners[which.min(projected$z), ])
                  expect_equal(nearest, sign(direction(az, el)) * 0.5)
            }
      }
})

test_that("coord_3d() rejects mixed parameterizations", {
      expect_error(coord_3d(direction = c(1, 1, 1), pitch = 10), "cannot be combined")
      expect_error(coord_3d(direction = c(1, 1, 1), roll = -60), "cannot be combined")
      expect_error(coord_3d(direction = c(1, 1, 1), yaw = 0), "cannot be combined")

      # Passing the documented defaults explicitly still counts as supplying them
      expect_error(coord_3d(direction = c(1, 1, 1), pitch = 0), "cannot be combined")

      expect_no_error(coord_3d(direction = c(1, 1, 1)))
      expect_no_error(coord_3d(pitch = 0, roll = -60, yaw = -30))
})

test_that("print.light() reports the angular form of the direction", {
      output <- capture.output(print(light(direction = c(-.5, 0, 1))))
      expect_true(any(grepl("azimuth: 270", output)))
      expect_true(any(grepl("elevation: 63.4", output)))

      # Positional lighting has no direction to report
      output <- capture.output(print(light(position = c(1, 1, 1))))
      expect_false(any(grepl("azimuth", output)))
})
