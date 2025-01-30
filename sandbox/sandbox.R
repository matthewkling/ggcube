
library(tidyverse)
library(devtools)
load_all()

# library(ggcube)

d <- select(mpg, displ, hwy, cty)

d <- data.frame(displ = rnorm(100),
                hwy = rnorm(100),
                cty = rnorm(100))

prj <- projection(pitch = -20, roll = 130,
                  persp = TRUE, dist = 2, data = d)
# prj <- projection(pitch = runif(1, -180, 180),
#                   roll = runif(1, -180, 180),
#                   yaw = runif(1, -180, 180),
#                   persp = TRUE, dist = 2, data = d)

d %>%
      ggplot(aes(displ, hwy, z = cty, color = displ)) +
      proj_panel(prj = prj) +
      proj_gridlines(prj = prj, color = "white") +
      proj_margin(prj = prj, geom = "point",
                  color = "gray40", size = 1, alpha = .5) +
      proj_data(prj = prj, geom = "point", size = 3) +
      proj_label(prj = prj, axis = "x", title = "displacement",
                 edge = c("ymin", "zmin")) +
      proj_label(prj = prj, axis = "y", title = "highway MPG",
                 edge = c("zmax", "xmax")) +
      proj_label(prj = prj, axis = "z", title = "city MPG",
                 edge = c("xmin", "ymax")) +
      scale_color_viridis_c() +
      coord_fixed() +
      theme_cube()


prj <- projection(pitch = 75, yaw = 55,
                  persp = T, dist = 1,
                  data = mountain)

# ridgeline
mountain %>%
      ggplot(aes(x, y, z = z)) +
      proj_ridgeline(prj = prj, geom = "polygon", piece = "y",
                     fill = "black", color = "white", linewidth = .15) +
      theme_void() +
      coord_fixed(ratio = .6)

# mesh
mountain %>%
      ggplot(aes(x, y, z = z, color = z)) +
      proj_mesh(prj = prj) +
      theme_void() +
      coord_fixed(ratio = .6) +
      scale_color_viridis_c()

# surface
mountain %>%
      ggplot(aes(x, y, z = z, fill = after_stat(slope))) +
      proj_surface(prj = prj, color = "black", linewidth = .1) +
      theme_void() +
      coord_fixed(ratio = .6) +
      scale_fill_viridis_c()

# surface
mountain %>%
      ggplot(aes(x, y, z = z, fill = after_stat(dzdy))) +
      proj_surface(prj = prj, color = "black", linewidth = .1) +
      theme_void() +
      coord_fixed(ratio = .6) +
      scale_fill_viridis_c()
