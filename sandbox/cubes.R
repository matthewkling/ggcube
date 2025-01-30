
library(tidyverse)
library(devtools)
load_all()


##### projections ######
# https://en.wikipedia.org/wiki/Isometric_projection#/media/File:Comparison_of_graphical_projections.svg

d <- data.frame(x = 1:2, y = 1:2, z = 1:2)

pp <- function(prj, ...){
      ggplot(d, aes(x, y, z = z)) +
            proj_panel(prj = prj, faces = "all", alpha = .75, ...) +
            coord_fixed() +
            theme_cube()
}

# one-point
pp(projection(persp = T, dist = 2,
              hjust = 1.5, vjust = 1.5, data = d))

# two-point
pp(projection(persp = T, roll = 35))

# three-point
pp(projection(persp = T, roll = 35, pitch = -25))

# isometric
pp(projection(roll = 45, pitch = -35.3))

# dimetric
pp(projection(roll = 45, pitch = -25))

# trimetric
pp(projection(roll = 65, pitch = -15))

# cabinet
pp(projection(shear_xz = -.5, shear_yz = -.5))


pp(projection(persp = T, pitch = 0, roll = 0, yaw = 0))
pp(projection(persp = T, pitch = 10, roll = 0, yaw = 0))
pp(projection(persp = T, pitch = 0, roll = 10, yaw = 0))
pp(projection(persp = T, pitch = 0, roll = 0, yaw = 10))

