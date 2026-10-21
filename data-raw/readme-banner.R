# Contact sheet of ggcube plots for the top of the README
#
# Renders each tile as a square PNG, then assembles them into a grid, written
# to man/figures/README-banner.jpg (a JPEG, at well under half the size of a
# PNG, since the files in man/figures are part of the package). Re-run after changes that affect how plots
# look. Run from the package root:
#
#   Rscript data-raw/readme-banner.R
#
# Needs ragg, png, magick, alphashape3d, mgcv, and jsonlite, plus network
# access for the mammoth data.

library(ggplot2)
library(ggcube)

out_file <- file.path("man", "figures", "README-banner.jpg")

ncol <- 6
nrow <- 3
gap_px <- 6

tile_px <- 800            # was 400
dpi <- 200                # was 100
banner_width_px <- 3600   # was 1800


# Shared styling ----------------------------------------------------------------

# Strip text and legends so tiles read as pictures; each tile keeps its own
# theme otherwise
bare <- theme(legend.position = "none",
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = margin(0, 0, 0, 0))


# Tiles -------------------------------------------------------------------------

tiles <- list(

      terrain = function() {
            ggplot(mountain, aes(x, y, z)) +
                  geom_surface_3d(aes(fill = z, colour = z), linewidth = .1) +
                  scale_fill_viridis_c() + scale_colour_viridis_c() +
                  coord_3d(ratio = c(1.5, 2, 1), expand = FALSE, panels = "zmin") +
                  light(direction = c(1, 0, 0.5), mode = "hsl", method = "direct", contrast = .7) +
                  theme_light() + bare
      },

      droplines = function() {
            ggplot(mpg, aes(displ, hwy, drv, fill = class)) +
                  geom_dropline_3d(faces = c("ymax", "xmax")) +
                  geom_point_3d(position = position_on_face(c("ymax", "xmax")), flat = TRUE,
                                size = 3, shape = 21, stroke = .1, alpha = .5) +
                  geom_point_3d(size = 3, shape = 21, colour = "black", stroke = .1) +
                  coord_3d() +
                  bare
      },

      ridgelines = function() {
            g <- expand.grid(x = seq(-50, 50, 10),
                             y = seq(-50, 50, 10))
            g$z <- with(g, x^3 - 3*x*y^2)

            base <- min(g$z) - diff(range(g$z)) * .1

            ggplot(g, aes(x, y, z)) +
                  geom_ridgeline_3d(aes(x = ifelse(x %in% range(x), NA, x)),
                                    colour = "white", linewidth = .2, direction = "x", fill = "black", base = base) +
                  geom_ridgeline_3d(aes(y = ifelse(y %in% range(y), NA, y)),
                                    colour = "white", linewidth = .2, direction = "y", fill = "gray40", base = base) +
                  scale_fill_viridis_c(option = "magma", begin = .2) +
                  coord_3d(yaw = 60) +
                  light("none") +
                  theme_void() + bare
      },

      sphere_lit = function() {
            ggplot(sphere_points, aes(x, y, z)) +
                  geom_hull_3d(fill = "#8a2900", colour = "#8a2900",
                               light = light(method = "direct", mode = "hsl",
                                             direction = c(1, -.5, 1))) +
                  coord_3d(scales = "fixed") +
                  theme_dark() + bare
      },

      text = function() {

            g <- data.frame(x = c(-1, 1, 0, 0, 0, 0) * .5,
                            y = c(0, 0, -1, 1, 0, 0) * .5,
                            z = c(0, 0, 0, 0, -1, 1) * .5)

            ggplot(data.frame()) +
                  geom_text_3d(aes(x = 1, y = 0, z = 0, label = "North"), size = 20, weight = "bold",
                               method = "polygon", facing = "ymin", hjust = 0) +
                  geom_text_3d(aes(x = -1, y = 0, z = 0, label = "South"),  size = 20, weight = "bold",
                               method = "polygon", facing = "ymin", hjust = 1) +
                  geom_text_3d(aes(x = 0, y = -1, z = 0, label = "East"),  size = 20, weight = "bold",
                               method = "polygon", facing = "xmin", hjust = 0) +
                  geom_text_3d(aes(x = 0, y = 1, z = 0, label = "West"),  size = 20, weight = "bold",
                               method = "polygon", facing = "xmin", hjust = 1) +
                  geom_text_3d(aes(x = 0, y = 0, z = 1, label = "Up"),  size = 20, angle = 90, weight = "bold",
                               method = "polygon", facing = "xmin", hjust = 0) +
                  geom_text_3d(aes(x = 0, y = 0, z = -1, label = "Down"),  size = 20, angle = 90, weight = "bold",
                               method = "polygon", facing = "xmin", hjust = 1) +

                  geom_segment_3d(aes(x = c(-.9, 0, 0), xend = c(.9, 0, 0),
                                      y = c(0, -.9, 0), yend = c(0, .9, 0),
                                      z = c(0, 0, -.9), zend = c(0, 0, .9)),
                                  arrow = arrow(ends = "both", type = "closed", angle = 20, length = unit(.1, "in")),
                                  linewidth = 1, color = "darkred") +

                  geom_hull_3d(data = g, aes(x, y, z), color = "black", fill = "darkred", linewidth = .25) +
                  light("direct", color = F) +

                  coord_3d(scales = "fixed", expand = F, roll = -70, yaw = -45, dist = 1, zoom = 2) +


                  zlim(NA, 3) +
                  theme_void()
      },

      fun = function() {

            d <- expand.grid(x = seq(-pi, pi, length.out = 30),
                             y = seq(-pi*2, pi*2, length.out = 60))
            d$z <- cos(d$x) * sin(d$y)

            ggplot(d, aes(x, y, z)) +
                  geom_contour(aes(linewidth = after_stat(level)),
                               breaks = breaks, color = "black",
                               position = position_on_face("zmin", axes = c("x", "y"))) +
                  geom_surface_3d(fill = "#7a2100", colour = "#b3725b",
                                   grid = "right1", linewidth = .2) +
                  scale_linewidth_continuous(range = c(1, .1)) +
                  coord_3d(yaw = 160, roll = -70, scales = "fixed", ratio = c(1, 1, 2),
                           panels = "all", expand = F) +
                  theme_light() + bare +
                  theme(panel.background = element_blank(),
                        panel.border = element_rect(color = "black", linewidth = .15),
                        panel.grid = element_blank())
      },

      smooth = function() {
            set.seed(123)
            d <- data.frame(x = rnorm(200), y = rnorm(200))
            d$z <- d$x + d$x^2 - d$y^2 + rnorm(200)
            ggplot(d, aes(x, y, z)) +
                  geom_smooth_3d(aes(fill = after_stat(level)),
                                 method = "gam", formula = z ~ te(x, y),
                                 se = TRUE, level = 0.99, colour = "black",
                                 linewidth = .1, grid = "equilateral", n = 20) +
                  geom_point_3d(size = .8) +
                  scale_fill_manual(values = c("red", "darkorchid4", "steelblue")) +
                  coord_3d() +
                  bare
      },


      # distributions = function() {
      #       ggplot(iris, aes(y = Sepal.Length, x = Species, fill = Species)) +
      #             stat_distributions_3d(alpha = .8, colour = "black", linewidth = .2) +
      #             scale_z_continuous(expand = expansion(mult = c(0, NA))) +
      #             coord_3d(yaw = 40) +
      #             theme_classic() + bare
      # },


      torus_rgb = function() {

            set.seed(1)
            n <- 20000
            theta <- runif(n, 0, 2 * pi)  # position around the ring
            phi <- runif(n, 0, 2 * pi)    # position around the tube
            rho <- sqrt(runif(n))         # distance from the tube center
            torus <- data.frame(
                  x = (3 + rho * cos(phi)) * cos(theta),
                  y = (3 + rho * cos(phi)) * sin(theta),
                  z = rho * sin(phi)
            )

            ggplot(torus, aes(x, y, z)) +
                  geom_hull_3d(method = "alpha", radius = 0.6, fill = "steelblue", color = "steelblue") +
                  coord_3d(scales = "fixed", zoom = 1.5) +
                  light("rgb", direction = c(0, -1, 0)) +
                  theme_void() + bare
      },

      density = function() {
            ggplot(faithful, aes(eruptions, waiting)) +
                  geom_density_3d(grid = "equilateral", n = 30, direction = "y",
                                  colour = "white", linewidth = .1,
                                  light = light("direct", direction = c(-1, 0, 1))) +
                  scale_fill_viridis_c(option = "plasma") +
                  coord_3d() +
                  theme_dark() + bare
      },


      vectors = function() {
            u <- c(1, .2, .4); v <- c(.3, 1, .7)
            vecs <- data.frame(xend = c(u[1], v[1]), yend = c(u[2], v[2]), zend = c(u[3], v[3]),
                               label = c("u", "v"))
            ggplot(vecs, aes(0, 0, 0, xend = xend, yend = yend, zend = zend)) +
                  annotate_3d("polygon", x = c(0, u[1], v[1]), y = c(0, u[2], v[2]),
                              z = c(0, u[3], v[3]), fill = "steelblue") +
                  geom_plane_3d(zintercept = .3, fill = "tomato", alpha = .4) +
                  geom_axes_3d(labels = NULL) +
                  geom_segment_3d(arrow = arrow(type = "closed", length = unit(6, "pt")),
                                  linewidth = .8) +
                  geom_text_3d(aes(xend * 1.12, yend * 1.12, zend * 1.12, label = label),
                               size = 5, on_top = TRUE) +
                  lims(x = c(-.25, 1.1), y = c(-.25, 1.1)) + zlim(-.25, 1.1) +
                  coord_3d(panels = "none") +
                  theme_void() + bare
      },

      voxels = function() {
            set.seed(2)
            v <- expand.grid(x = 1:12, y = 1:12, z = 1:12)
            r <- with(v, sqrt((x - 6.5)^2 + (y - 6.5)^2 + (z - 6.5)^2))
            v <- v[r < 6 & r > 3.5 & runif(nrow(v)) < .35, ]
            ggplot(v, aes(x, y, z, fill = z)) +
                  geom_voxel_3d(colour = "black", linewidth = .1) +
                  scale_fill_viridis_c(option = "mako", begin = .3) +
                  light("rgb", direction = c(1, -1, 1), color = F) +
                  coord_3d() +
                  theme_light() + bare
      },


      layers = function(){
            d <- expand_grid(lon = 1:50, lat = 1:50, time = 1:8) %>%
                  mutate(value = sin(lon * lat * time / 500) + sin(time+4))

            palette <- c("black", "darkblue", "red", "yellow")

            ggplot(d, aes(lon, lat, time, group = time,
                          fill = value, color = value)) +
                  geom_surface_3d(light = "none") +
                  coord_3d() +
                  bare +
                  theme_void() +
                  theme(legend.position = "none") +
                  scale_fill_gradientn(colors = palette) +
                  scale_color_gradientn(colors = palette)
      },


      linear_system = function() {
            ggplot(data.frame(x = -.375, y = .5, z = .0625), aes(x, y, z)) +
                  geom_plane_3d(yintercept = .5,
                                fill = "tomato", color = "darkred", alpha = .8) +
                  geom_plane_3d(slope_x = .5, slope_y = .5,
                                fill = "dodgerblue", color = "darkblue", alpha = .8) +
                  geom_plane_3d(slope_x = -.5, slope_y = -.25,
                                fill = "seagreen", color = "darkgreen", alpha = .8) +
                  geom_point_3d(aes(x = 0, y = 0, z = 0), size = 3) +
                  geom_segment_3d(aes(xend = 0, yend = 0, zend = 0),
                                  arrow = arrow(20, unit(.1, "in"), "first", "closed")) +
                  lims(x = c(-1, 1), y = c(-1, 1)) + zlim(-1, 1) +
                  coord_3d(yaw = 60, light = "none") +
                  theme_void()

      },

      histogram = function() {
            ggplot(faithful, aes(waiting, eruptions)) +
                  geom_bar_3d(fill = "steelblue", colour = "steelblue", bins = 15, width = .9) +
                  scale_z_continuous(expand = c(0, 0)) +
                  coord_3d(yaw = 60) +
                  bare
      },

      mammoth = function() {
            m <- jsonlite::fromJSON(paste0("https://raw.githubusercontent.com/PAIR-code/",
                                           "understanding-umap/master/raw_data/mammoth_3d_50k.json"))
            m <- as.data.frame(m)
            colnames(m) <- c("Y", "X", "Z")
            set.seed(1)
            m <- m[sample(nrow(m), 15000), ]
            ggplot(m, aes(X, Y, Z)) +
                  geom_hull_3d(method = "alpha", radius = 10, fill = "darkred", colour = "darkred") +
                  light(mode = "hsl", direction = c(-1, 1, 0), anchor = "camera") +
                  coord_3d(roll = -90, yaw = -45, dist = 1.5,
                           scales = "fixed", panels = "zmin", expand = FALSE) +
                  bare
      },

      lorenz = function() {
            butterfly <- ggcube:::lorenz_attractor(n_points = 8000, dt = .01)
            ggplot(butterfly, aes(x, y, z, colour = time)) +
                  geom_path_3d(linewidth = 0.1, colour = "black",
                               position = position_on_face(c("xmax", "ymax", "zmin"))) +
                  geom_path_3d(linewidth = 0.3) +
                  scale_colour_gradientn(colours = c("blue", "purple", "red", "orange")) +
                  coord_3d() +
                  theme_light() + bare
      },

      contours = function() {
            d <- terra::aggregate(terra::rast(topoclimate.pred::moonshine), 10)
            d <- terra::as.data.frame(d, xy = T)
            names(d)[3] <- "z"

            # ggplot(d, aes(x, y, z)) +
            #       geom_contour_3d(fill = "black", colour = "white", linewidth = .2, bins = 40) +
            #       coord_3d(direction = c(1, 1, 1), ratio = c(2, 1, .5), light = "none") +
            #       theme_void() + bare


            ggplot(d[d$x > quantile(d$x, .45) & d$x < quantile(d$x, .85),],
                   aes(x, y, z, color = after_stat(z))) +
                  geom_contour_3d(fill = "black", linewidth = .4, bins = 30) +
                  coord_3d(direction = c(1, 0, .5), ratio = c(1, 1, .5),
                           dist = 1.05, light = "none", zoom = 1.5) +
                  theme_void() + bare +
                  scale_color_gradientn(colors = c("dodgerblue", "orangered", "gold", "white"))
      },


      iris_faces = function() {
            ggplot(iris, aes(Sepal.Length, Sepal.Width, Petal.Length,
                             colour = Species, fill = Species)) +
                  stat_density_2d(position = position_on_face(faces = "zmin", axes = c("x", "y")),
                                  geom = "polygon", alpha = .1, linewidth = .25) +
                  geom_hull_3d(position = position_on_face("ymax"), alpha = .5) +
                  geom_voxel_3d(aes(round(Sepal.Length), round(Sepal.Width), round(Petal.Length)),
                                position = position_on_face("xmax"), alpha = .15, light = NULL) +
                  geom_point_3d(shape = 21, colour = "black", stroke = .25, size = 1) +
                  xlim(4, 8) +
                  coord_3d() +
                  bare
      }
)


# Render and assemble -------------------------------------------------------------

render_tile <- function(make, file) {
      ragg::agg_png(file, width = tile_px, height = tile_px, res = dpi, background = "white")
      on.exit(grDevices::dev.off())
      print(make())
}

tile_dir <- tempfile("banner-tiles")
dir.create(tile_dir)
files <- character(0)
for (nm in names(tiles)) {
      message("Rendering ", nm)
      f <- file.path(tile_dir, paste0(nm, ".png"))
      suppressWarnings(render_tile(tiles[[nm]], f))
      files <- c(files, f)
}
stopifnot(length(files) == ncol * nrow)

width <- ncol * tile_px + (ncol - 1) * gap_px
height <- nrow * tile_px + (nrow - 1) * gap_px
scale <- banner_width_px / width

sheet <- tempfile(fileext = ".png")
ragg::agg_png(sheet, width = round(width * scale), height = round(height * scale),
              res = dpi, background = "white")
grid::grid.newpage()
for (k in seq_along(files)) {
      col <- (k - 1) %% ncol
      row <- (k - 1) %/% ncol
      img <- png::readPNG(files[k])
      grid::grid.raster(img,
                        x = grid::unit((col * (tile_px + gap_px)) / width, "npc"),
                        y = grid::unit(1 - (row * (tile_px + gap_px)) / height, "npc"),
                        width = grid::unit(tile_px / width, "npc"),
                        height = grid::unit(tile_px / height, "npc"),
                        just = c("left", "top"))
}
grDevices::dev.off()
magick::image_write(magick::image_read(sheet), out_file, format = "jpeg", quality = 90)
message("Wrote ", out_file)
