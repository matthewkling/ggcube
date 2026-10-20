# Regression cases for ggcube rendering
#
# Each case is a function returning a ggplot, so that plots are constructed
# with whichever version of ggcube is loaded when they are rendered. Every
# case sets its own seed. Cases use only API available in the baseline
# version (except those tagged `new`); arguments that later versions
# deprecate (e.g. a layer's `sort_method`, `annotate =`) are kept so that
# diffs show their effect.
#
# Tags group cases for review:
#   single  - one ggcube layer (expected pixel-identical unless sorting is
#             the subject of an intended change)
#   multi   - several ggcube layers (scene sorting changes these by design)
#   sort    - exercises depth sorting directly
#   ann     - uses annotate_3d()
#   new     - uses API added after the baseline (errors in older versions)

regression_cases <- function() {

      mountain <- ggcube::mountain
      grid_surface <- function(n = 25) {
            d <- expand.grid(x = seq(-2, 2, length.out = n), y = seq(-2, 2, length.out = n))
            d$z <- with(d, sin(x * 1.3) * cos(y))
            d
      }
      # Reference lines were annotate_3d("line") before 0.4.0
      ann_line <- function(...) {
            type <- if (exists("geom_refline_3d", asNamespace("ggcube"))) "refline" else "line"
            annotate_3d(type, ...)
      }
      cloud <- function(n, seed = 1) {
            set.seed(seed)
            data.frame(x = runif(n), y = runif(n), z = runif(n))
      }

      list(

            # Surfaces and polygons ----------------------------------------

            surface_default = list(tags = "single", plot = function() {
                  ggplot(mountain, aes(x, y, z)) +
                        geom_surface_3d(fill = "grey70", colour = "grey20", linewidth = 0.1) +
                        coord_3d(ratio = c(1.5, 2, 1))
            }),

            surface_painter_ortho = list(tags = c("single", "sort"), plot = function() {
                  ggplot(grid_surface(), aes(x, y, z, fill = z)) +
                        geom_surface_3d(sort_method = "painter", colour = "black", linewidth = 0.1) +
                        scale_fill_viridis_c() +
                        coord_3d(persp = FALSE, roll = -50, yaw = -20)
            }),

            surface_pairwise_persp = list(tags = c("single", "sort"), plot = function() {
                  ggplot(grid_surface(20), aes(x, y, z, fill = z)) +
                        geom_surface_3d(sort_method = "pairwise", colour = "black", linewidth = 0.1) +
                        scale_fill_viridis_c() +
                        coord_3d(dist = 1.5, roll = -55, yaw = 30)
            }),

            surface_lit_hsl = list(tags = "single", plot = function() {
                  d <- expand.grid(x = -10:10, y = -10:10)
                  d$z <- with(d, cos(sqrt(x^2 + y^2) / 1.5) - sqrt(x^2 + y^2) / 1.5)
                  ggplot(d, aes(x, y, z)) +
                        geom_surface_3d(fill = "steelblue", colour = "steelblue", linewidth = 0.2) +
                        coord_3d(light = light(mode = "hsl", direction = c(1, 0, 0)))
            }),

            surface_backface_cull = list(tags = "single", plot = function() {
                  ggplot(ggcube::sphere_points, aes(x, y, z)) +
                        geom_hull_3d(method = "convex", fill = "orange", colour = "black",
                                     linewidth = 0.1, cull_backfaces = TRUE) +
                        coord_3d(scales = "fixed")
            }),

            function_equilateral = list(tags = "single", plot = function() {
                  ggplot() +
                        geom_function_3d(fun = function(x, y) x^2 - y^2,
                                         xlim = c(-2, 2), ylim = c(-2, 2),
                                         aes(fill = after_stat(slope)),
                                         grid = "equilateral", n = 20) +
                        scale_fill_viridis_c() +
                        coord_3d()
            }),

            polygon_intersecting = list(tags = c("single", "sort"), plot = function() {
                  sq <- function(id, x, y, z) data.frame(id = id, x = x, y = y, z = z)
                  d <- rbind(
                        sq("a", c(0, 1, 1, 0), c(0, 0, 1, 1), c(0.2, 0.2, 0.8, 0.8)),
                        sq("b", c(0, 1, 1, 0), c(0, 0, 1, 1), c(0.8, 0.8, 0.2, 0.2)),
                        sq("c", c(0.5, 0.5, 0.5, 0.5), c(0, 1, 1, 0), c(0, 0, 1, 1))
                  )
                  ggplot(d, aes(x, y, z, group = id, fill = id)) +
                        geom_polygon_3d(sort_method = "pairwise", colour = "black", alpha = 0.9) +
                        coord_3d(light = "none", dist = 2)
            }),

            # Bars, columns, voxels -----------------------------------------

            col_grid = list(tags = "single", plot = function() {
                  set.seed(2)
                  d <- expand.grid(x = 1:5, y = 1:5)
                  d$z <- d$x + d$y + rnorm(25, 0, 0.5)
                  ggplot(d, aes(x, y, z, fill = z)) +
                        geom_col_3d(colour = "white", linewidth = 0.2) +
                        scale_fill_viridis_c() +
                        coord_3d()
            }),

            bar_histogram = list(tags = "single", plot = function() {
                  set.seed(3)
                  d <- data.frame(x = rnorm(600), y = rnorm(600))
                  ggplot(d, aes(x, y)) +
                        geom_bar_3d(bins = 8, fill = "tomato", colour = "black", linewidth = 0.1) +
                        coord_3d()
            }),

            voxel = list(tags = "single", plot = function() {
                  set.seed(4)
                  v <- unique(data.frame(x = round(rnorm(80, 0, 2)), y = round(rnorm(80, 0, 2)),
                                         z = round(rnorm(80, 0, 2))))
                  ggplot(v, aes(x, y, z, fill = z)) +
                        geom_voxel_3d(colour = "black", linewidth = 0.1) +
                        scale_fill_viridis_c() +
                        coord_3d()
            }),

            # Stats ------------------------------------------------------------

            smooth_residuals = list(tags = "single", plot = function() {
                  set.seed(123)
                  d <- data.frame(x = runif(60, -1, 3), y = runif(60, -3, 3))
                  d$z <- abs(1 + d$x^2 - d$y^2 + rnorm(60, 0, 1))
                  ggplot(d, aes(x, y, z)) +
                        geom_smooth_3d(points = TRUE, residuals = TRUE, n = 15) +
                        scale_fill_viridis_c() +
                        coord_3d(light = NULL)
            }),

            contour = list(tags = "single", plot = function() {
                  ggplot(mountain, aes(x, y, z, fill = after_stat(z))) +
                        geom_contour_3d(bins = 10, colour = "white", linewidth = 0.1) +
                        scale_fill_viridis_c() +
                        coord_3d(light = "none", ratio = c(1.5, 2, 1))
            }),

            ridgeline = list(tags = "single", plot = function() {
                  g <- expand.grid(x = seq(-3, 3, 0.5), y = seq(-3, 3, 0.1))
                  g$z <- with(g, dnorm(y) * dnorm(x) * 10)
                  ggplot(g, aes(x, y, z = z, fill = x)) +
                        geom_ridgeline_3d(colour = "white", linewidth = 0.3) +
                        scale_fill_viridis_c() +
                        coord_3d()
            }),

            density = list(tags = "single", plot = function() {
                  ggplot(faithful, aes(eruptions, waiting)) +
                        geom_density_3d(n = 25, colour = "white", linewidth = 0.05) +
                        scale_fill_viridis_c() +
                        coord_3d()
            }),

            distributions = list(tags = "single", plot = function() {
                  ggplot(iris, aes(y = Sepal.Length, x = Species, fill = Species)) +
                        stat_distributions_3d() +
                        scale_z_continuous(expand = expansion(mult = c(0, NA))) +
                        coord_3d() +
                        theme(legend.position = "none")
            }),

            # Points, paths, segments, text -----------------------------------

            points_default = list(tags = "single", plot = function() {
                  ggplot(cloud(300), aes(x, y, z, colour = z)) +
                        geom_point_3d(size = 3) +
                        scale_colour_viridis_c() +
                        coord_3d()
            }),

            points_overlapping_pairwise = list(tags = c("single", "sort"), plot = function() {
                  set.seed(5)
                  d <- data.frame(x = rnorm(150, 0, .3), y = rnorm(150, 0, .3), z = rnorm(150, 0, .3))
                  ggplot(d, aes(x, y, z, fill = z)) +
                        geom_point_3d(size = 8, shape = 21, colour = "black", sort_method = "pairwise") +
                        scale_fill_viridis_c() +
                        coord_3d(dist = 1.5)
            }),

            points_reference = list(tags = "single", plot = function() {
                  ggplot(cloud(40, 6), aes(x, y, z, colour = x)) +
                        geom_point_3d(size = 2, ref_lines = TRUE, ref_points = "circles",
                                      ref_faces = c("zmin", "ymax")) +
                        coord_3d()
            }),

            points_reference_layers = list(tags = c("multi", "new"), plot = function() {
                  ggplot(cloud(40, 6), aes(x, y, z, colour = x)) +
                        geom_dropline_3d(faces = c("zmin", "ymax")) +
                        geom_point_3d(position = position_on_face(c("zmin", "ymax")), flat = TRUE,
                                      size = 2, alpha = 0.5) +
                        geom_point_3d(size = 2) +
                        coord_3d()
            }),

            points_flat_shapes = list(tags = c("single", "new"), plot = function() {
                  shapes <- c(0, 1, 2, 5, 6, 15:25)
                  d <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4), z = 2, s = factor(shapes))
                  ggplot(d, aes(x, y, z, shape = s)) +
                        scale_shape_manual(values = shapes) +
                        geom_point_3d(position = position_on_face(c("3D", "zmin")), flat = TRUE,
                                      size = 5, fill = "gold") +
                        zlim(0, 2) +
                        coord_3d()
            }),

            points_plane_pairwise = list(tags = c("single", "sort", "ann"), plot = function() {
                  ggplot(cloud(200, 7), aes(x, y, z)) +
                        geom_point_3d(size = 2, colour = "black", sort_method = "pairwise",
                                      annotate = annotate_3d("plane", zintercept = 0.5,
                                                             fill = "steelblue", alpha = 0.6)) +
                        coord_3d()
            }),

            points_plane_painter = list(tags = c("single", "sort", "ann"), plot = function() {
                  ggplot(cloud(200, 7), aes(x, y, z)) +
                        geom_point_3d(size = 2, colour = "black",
                                      annotate = annotate_3d("plane", zintercept = 0.5,
                                                             fill = "steelblue", alpha = 0.6)) +
                        coord_3d()
            }),

            path_spiral = list(tags = "single", plot = function() {
                  t <- seq(0, 12 * pi, length.out = 600)
                  ggplot(data.frame(x = sin(t), y = cos(t), z = t), aes(x, y, z, colour = z)) +
                        geom_path_3d(linewidth = 1, lineend = "round") +
                        scale_colour_viridis_c() +
                        coord_3d()
            }),

            segments_arrows = list(tags = "single", plot = function() {
                  set.seed(8)
                  d <- data.frame(x = runif(25), y = runif(25), z = runif(25))
                  d <- transform(d, xend = x + rnorm(25, 0, .2), yend = y + rnorm(25, 0, .2),
                                 zend = z + rnorm(25, 0, .2))
                  ggplot(d, aes(x, y, z, xend = xend, yend = yend, zend = zend)) +
                        geom_segment_3d(arrow = grid::arrow(length = grid::unit(0.1, "inches"),
                                                            type = "closed"),
                                        colour = "darkred", linewidth = 0.6) +
                        coord_3d()
            }),

            text_billboard = list(tags = "single", plot = function() {
                  df <- expand.grid(x = c("B", "H", "N"), y = c("a", "o", "u"), z = c("g", "t"))
                  df$label <- paste0(df$x, df$y, df$z)
                  ggplot(df, aes(x, y, z, label = label)) +
                        geom_text_3d() +
                        coord_3d(scales = "fixed")
            }),

            text_polygon = list(tags = "single", plot = function() {
                  df <- expand.grid(x = c("B", "H"), y = c("a", "o"), z = c("g", "t"))
                  df$label <- paste0(df$x, df$y, df$z)
                  ggplot(df, aes(x, y, z, label = label, fill = z)) +
                        geom_text_3d(method = "polygon", facing = "zmax") +
                        coord_3d(scales = "fixed")
            }),

            polygon_winding_lighting = list(tags = c("single"), plot = function() {
                  sq <- function(x, y, z, g) data.frame(x = x, y = y, z = z, g = g)
                  d <- rbind(sq(c(0, 1, 1, 0), c(0, 0, 1, 1), 0, "ccw"),
                             sq(c(1.5, 1.5, 2.5, 2.5), c(0, 1, 1, 0), 0, "cw"),
                             sq(c(0, 1, 1, 0), 1.5, c(0, 0, 1, 1), "vertical"),
                             sq(c(1.5, 2.5, 2.5, 1.5), c(1.5, 1.5, 2.5, 2.5), c(0, .5, 1, .5), "slanted"))
                  ggplot(d, aes(x, y, z, group = g)) +
                        geom_polygon_3d(fill = "steelblue", colour = "black") +
                        coord_3d()
            }),

            # Annotations ------------------------------------------------------

            annotation_mix_surface = list(tags = c("single", "ann"), plot = function() {
                  summit <- mountain[which.max(mountain$z), ]
                  ggplot(mountain, aes(x, y, z)) +
                        geom_contour_3d(
                              annotate = list(
                                    annotate_3d("plane", a = 0, b = 1, c = 0, d = .5,
                                                fill = "blue", alpha = .25),
                                    annotate_3d("point", x = summit$x, y = summit$y, z = summit$z,
                                                colour = "red"),
                                    annotate_3d("text", x = summit$x, y = summit$y, z = summit$z,
                                                colour = "red", label = "Summit",
                                                fontface = "bold", vjust = -1)),
                              fill = "black") +
                        coord_3d(ratio = c(2, 3, 1.5), light = "none")
            }),

            annotation_planes_surface = list(tags = c("single", "sort", "ann"), plot = function() {
                  ggplot(grid_surface(18), aes(x, y, z)) +
                        geom_surface_3d(sort_method = "pairwise", fill = "grey80",
                                        colour = "grey30", linewidth = 0.1,
                                        annotate = list(
                                              annotate_3d("plane", zintercept = 0, fill = "tomato",
                                                          alpha = 0.7),
                                              annotate_3d("plane", xintercept = 0.5,
                                                          fill = "steelblue", alpha = 0.7))) +
                        coord_3d(dist = 2)
            }),

            annotation_axes_line_segment = list(tags = c("single", "ann"), plot = function() {
                  ggplot(cloud(30, 9), aes(x - .5, y - .5, z - .5)) +
                        geom_point_3d(size = 1.5, sort_method = "pairwise",
                                      annotate = list(
                                            annotate_3d("axes"),
                                            ann_line(dx = 1, dy = 1, dz = 0.5, colour = "purple"),
                                            annotate_3d("segment", x = -.4, y = -.4, z = -.4,
                                                        xend = .4, yend = .4, zend = .4,
                                                        colour = "darkgreen", linewidth = 1,
                                                        arrow = grid::arrow(type = "closed",
                                                                            length = grid::unit(8, "pt"))),
                                            annotate_3d("text", x = 0, y = 0, z = .45,
                                                        label = "on top", on_top = TRUE))) +
                        coord_3d()
            }),

            annotation_layers_new_api = list(tags = c("multi", "ann", "new"), plot = function() {
                  planes <- data.frame(slope_x = c(-0.4, 0.4), model = c("a", "b"))
                  ggplot(cloud(60, 12), aes(x - .5, y - .5, z - .5)) +
                        geom_point_3d(size = 1.5) +
                        geom_plane_3d(aes(slope_x = slope_x, fill = model), data = planes) +
                        geom_refline_3d(dx = c(1, 0), dy = c(0, 1), dz = 0.3, colour = "purple") +
                        geom_axes_3d(colour = c("red", "darkgreen", "blue")) +
                        annotate_3d("polygon", x = c(-.4, .4, 0), y = c(-.4, -.4, .4), z = .3,
                                    fill = "gold") +
                        annotate_3d("text", x = 0, y = 0, z = .45, label = "on top", on_top = TRUE) +
                        coord_3d()
            }),

            # Multi-layer scenes -----------------------------------------------

            multi_surface_scatter = list(tags = c("multi", "sort"), plot = function() {
                  set.seed(10)
                  pts <- data.frame(x = runif(60, -2, 2), y = runif(60, -2, 2))
                  pts$z <- with(pts, sin(x * 1.3) * cos(y)) + rnorm(60, 0, 0.4)
                  ggplot(grid_surface(18), aes(x, y, z)) +
                        geom_surface_3d(fill = "grey80", colour = "grey40", linewidth = 0.1) +
                        geom_point_3d(data = pts, colour = "red", size = 2) +
                        coord_3d()
            }),

            multi_matlib_diagram = list(tags = c("multi", "sort", "ann"), plot = function() {
                  vecs <- data.frame(x = 0, y = 0, z = 0,
                                     xend = c(1, 0, 0.6), yend = c(0, 1, 0.6), zend = c(0, 0, 1),
                                     v = c("a", "b", "c"))
                  tri <- data.frame(x = c(1, 0, 0.6), y = c(0, 1, 0.6), z = c(0, 0, 1))
                  labs <- transform(vecs, x = xend * 1.1, y = yend * 1.1, z = zend * 1.1)
                  ggplot() +
                        geom_polygon_3d(data = tri, aes(x, y, z), fill = "gold", alpha = 0.8,
                                        colour = "black", sort_method = "pairwise",
                                        annotate = annotate_3d("plane", zintercept = 0.4,
                                                               fill = "steelblue", alpha = 0.5)) +
                        geom_segment_3d(data = vecs, aes(x, y, z, xend = xend, yend = yend,
                                                         zend = zend, colour = v),
                                        linewidth = 1.2, sort_method = "pairwise",
                                        arrow = grid::arrow(type = "closed",
                                                            length = grid::unit(8, "pt"))) +
                        geom_text_3d(data = labs, aes(x, y, z, label = v), size = 6) +
                        coord_3d(light = "none", xlim = c(-.1, 1.2), ylim = c(-.1, 1.2),
                                 zlim = c(-.1, 1.2)) +
                        theme(legend.position = "none")
            }),

            multi_plane_layer_scatter = list(tags = c("multi", "sort"), plot = function() {
                  plane <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(.2, .2, .8, .8))
                  ggplot(cloud(150, 11), aes(x, y, z)) +
                        geom_point_3d(colour = "black", size = 2) +
                        geom_polygon_3d(data = plane, fill = "orange", alpha = 0.8) +
                        coord_3d()
            }),

            # Coord, facets, themes ---------------------------------------------

            facet_wrap_surface = list(tags = "single", plot = function() {
                  d <- rbind(transform(grid_surface(12), f = "a"),
                             transform(grid_surface(12), z = -z, f = "b"))
                  ggplot(d, aes(x, y, z, fill = z)) +
                        geom_surface_3d(colour = "black", linewidth = 0.1) +
                        facet_wrap(~f) +
                        scale_fill_viridis_c() +
                        coord_3d()
            }),

            facet_views = list(tags = "single", plot = function() {
                  ggplot(mountain, aes(x, y, z, fill = z)) +
                        geom_surface_3d(linewidth = .1, colour = "black") +
                        coord_3d(ratio = c(1.5, 2, 1)) +
                        scale_fill_viridis_c() +
                        theme_void() +
                        facet_wrap_views(~yaw, n = 3)
            }),

            coord_direction_classic = list(tags = "single", plot = function() {
                  ggplot(grid_surface(15), aes(x, y, z)) +
                        geom_surface_3d(fill = "lightblue", colour = "navy", linewidth = 0.1) +
                        coord_3d(direction = c(1, -1, 1), panels = c("zmin", "xmin")) +
                        theme_classic()
            }),

            scale_depth_points = list(tags = "single", plot = function() {
                  ggplot(cloud(100, 12), aes(x, y, z)) +
                        geom_point_3d(size = 4, scale_depth = 2, colour = "darkblue") +
                        coord_3d(dist = 1.5, scale_depth = c(grid = 0, text = 1))
            })
      )
}
