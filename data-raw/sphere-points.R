## code to prepare `sphere_points` dataset

set.seed(123)
theta <- runif(200, 0, 2*pi)
phi <- acos(runif(200, -1, 1))
sphere_points <- data.frame(
  x = sin(phi) * cos(theta),
  y = sin(phi) * sin(theta),
  z = cos(phi)
)

usethis::use_data(sphere_points, overwrite = TRUE)
