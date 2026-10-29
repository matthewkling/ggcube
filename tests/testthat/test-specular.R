# Tests for specular highlights.

# Orthographic view from straight above: the viewer is toward +z in scene space
top_view <- list(pitch = 0, roll = 0, yaw = 0, persp = FALSE)

# Unit normals tilted from +z toward +x by the given angles in degrees
tilted <- function(angle) cbind(sinpi(angle / 180), 0, cospi(angle / 180))

centers <- function(n) matrix(0, n, 3)

# Drawn polygon fills of a plot
drawn_fills <- function(p) {
      layer <- find_grobs(drawn_grob(p), c("ggcube_layer", "ggcube_scene"))[[1]]
      polys <- c(find_grobs(layer, "polygon"), find_grobs(layer, "pathgrob"))
      unname(unlist(lapply(polys, function(g) g$gp$fill)))
}

oklab_L <- function(cols) linear_to_oklab(srgb_to_linear(t(col2rgb(cols)) / 255))[, 1]

sphere_plot <- function(...) {
      ggplot() +
            geom_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2 - 1,
                                xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), zlim = c(-1.1, 1.1),
                                n = 16, ...) +
            coord_3d(scales = "fixed")
}


test_that("glints are full strength where faces reflect the light toward the viewer", {
      l <- light(direction = c(0, 0, 1), specular = .7)
      s <- compute_specular(tilted(c(0, 15, 40)), centers(3), l, top_view)
      expect_equal(s[1], .7)
      # Half strength at `specular_size` from the reflecting angle
      expect_equal(s[2], .35)
      expect_lt(s[3], .35)
      expect_gt(s[3], 0)
})

test_that("glint size sets the angle of half strength", {
      l <- light(direction = c(0, 0, 1), specular = 1, specular_size = 30)
      expect_equal(compute_specular(tilted(30), centers(1), l, top_view), .5)
})

test_that("glints are found halfway between the light and the viewer", {
      # Light from 60 degrees off the view direction: the half vector is 30
      # degrees off
      l <- light(direction = c(sinpi(1/3), 0, cospi(1/3)), specular = 1)
      s <- compute_specular(tilted(c(0, 30, 60)), centers(3), l, top_view)
      expect_equal(which.max(s), 2)
      expect_equal(s[2], 1)
})

test_that("faces not facing the light get no glint", {
      l <- light(direction = c(1, 0, 0), specular = 1)
      # Facing the viewer but at 90 degrees to the light, and facing away from it
      s <- compute_specular(rbind(c(0, 0, 1), c(-1, 0, 0)), centers(2), l, top_view)
      expect_equal(s, c(0, 0))

      # Light from directly behind the faces, opposite the viewer
      l <- light(direction = c(0, 0, -1), specular = 1)
      s <- compute_specular(tilted(c(0, 10)), centers(2), l, top_view)
      expect_equal(s, c(0, 0))
})

test_that("faces are glinted on the side the viewer sees", {
      l <- light(direction = c(0, 0, 1), specular = 1)
      expect_equal(compute_specular(rbind(c(0, 0, -1)), centers(1), l, top_view), 1)
})

test_that("stepped ramps give flat glints ending at their size", {
      l <- light(ramp_steps(breaks = 90), direction = c(0, 0, 1), specular = .6)
      s <- compute_specular(tilted(c(0, 10, 15, 16, 40)), centers(5), l, top_view)
      expect_equal(s, c(.6, .6, .6, 0, 0))
})

test_that("glints need a view, and are off by default", {
      l <- light(direction = c(0, 0, 1))
      expect_equal(compute_specular(tilted(0), centers(1), l, top_view), 0)
      l <- light(direction = c(0, 0, 1), specular = 1)
      expect_equal(compute_specular(tilted(0), centers(1), l, NULL), 0)
})

test_that("camera-space normals and directions give the same glints as scene space", {
      proj <- list(pitch = 20, roll = -50, yaw = 130, persp = FALSE)
      set.seed(1)
      normals <- matrix(rnorm(30), ncol = 3)
      normals <- normals / sqrt(rowSums(normals^2))
      dir <- c(-.3, .5, .8)

      scene <- compute_specular(normals, centers(10), light(direction = dir, specular = 1), proj)

      camera_normals <- rotate_normals_to_camera(normals, proj)
      camera_dir <- as.vector(rotate_normals_to_camera(matrix(dir, 1), proj))
      camera <- compute_specular(camera_normals, centers(10),
                                 light(direction = camera_dir, anchor = "camera", specular = 1),
                                 proj, normals_in_camera = TRUE)
      expect_equal(camera, scene)
      expect_true(any(scene > 0))
})

test_that("perspective glints depend on each face's position", {
      proj <- list(pitch = 0, roll = 0, yaw = 0, persp = TRUE, dist = 3)
      eye <- eye_in_cube_space(proj)$v
      l <- light(direction = c(0, 0, 1), specular = 1)
      # Two faces facing straight up: the one under the eye reflects toward it
      s <- compute_specular(rbind(c(0, 0, 1), c(0, 0, 1)),
                            rbind(c(eye[1], eye[2], 0), c(eye[1] + 1, eye[2], 0)), l, proj)
      expect_equal(s[1], 1)
      expect_lt(s[2], 1)
})

test_that("distance falloff dims glints", {
      l <- light(position = c(0, 0, 10), specular = 1)
      lf <- light(position = c(0, 0, 10), distance_falloff = TRUE, specular = 1)
      ctr <- rbind(c(0, 0, 0), c(0, 0, 5))
      n <- rbind(c(0, 0, 1), c(0, 0, 1))
      expect_equal(compute_specular(n, ctr, l, top_view), c(1, 1))
      expect_equal(compute_specular(n, ctr, lf, top_view), c(1, 1) * distance_falloff_factor(ctr, c(0, 0, 10)))
})

test_that("glints move colors toward the tone's full highlight", {
      lit <- light(direction = c(0, 0, 1), specular = 1)
      expect_equal(blend_light_with_colors("#9E2602", 1, lit, specular = 1), "#FFFFFF")
      expect_equal(blend_light_with_colors("#9E2602", 1, lit, specular = 0),
                   blend_light_with_colors("#9E2602", 1, lit))

      tinted <- light(tone = tone_tint("navy", "gold"), specular = 1)
      expect_equal(blend_light_with_colors("#9E2602", 1, tinted, specular = 1),
                   toupper(rgb(t(col2rgb("gold")), maxColorValue = 255)))

      # Halfway from the diffuse amount toward full highlight
      half <- blend_light_with_colors("#9E2602", cospi(1/4), lit, specular = .5)
      expect_equal(half, tone_colors(lit$tone, "#9E2602", .5))

      normals_tone <- light(tone = "normals", specular = 1)
      expect_equal(blend_light_with_colors("#9E2602", .5, normals_tone, normals = tilted(10), specular = 1),
                   blend_light_with_colors("#9E2602", .5, normals_tone, normals = tilted(10)))
})

test_that("plots with specular lighting draw glints", {
      plain <- drawn_fills(sphere_plot(fill = "#9e2602") + light(direction = direction(270, 60)))
      glossy <- drawn_fills(sphere_plot(fill = "#9e2602") + light(specular = 1, specular_size = 30))
      expect_equal(length(glossy), length(plain))
      expect_gt(max(oklab_L(glossy)), max(oklab_L(plain)) + .1)
      expect_gt(max(oklab_L(glossy)), .9)
})

test_that("specular lighting without glints leaves plots unchanged", {
      b <- ggplot_build(sphere_plot(fill = "#9e2602") + light())
      expect_false(".specular" %in% names(b$data[[1]]))
      expect_identical(drawn_fills(sphere_plot(fill = "#9e2602") + light()),
                       drawn_fills(sphere_plot(fill = "#9e2602")))
})

test_that("two-sided polygons get glints", {
      p <- ggplot(mtcars, aes(wt, hp, mpg)) + geom_point_3d() +
            geom_plane_3d(zintercept = 20, fill = "steelblue", light = light(specular = 1, specular_size = 80)) +
            coord_3d(pitch = 0, roll = 0, yaw = 0)
      plain <- ggplot(mtcars, aes(wt, hp, mpg)) + geom_point_3d() +
            geom_plane_3d(zintercept = 20, fill = "steelblue", light = light()) +
            coord_3d(pitch = 0, roll = 0, yaw = 0)
      expect_gt(max(oklab_L(drawn_fills(p))), max(oklab_L(drawn_fills(plain))))
})

test_that("specular arguments are validated and printed", {
      expect_error(light(specular = 1.5), "`specular`")
      expect_error(light(specular = -.1), "`specular`")
      expect_error(light(specular_size = 0), "`specular_size`")
      expect_error(light(specular_size = 90), "`specular_size`")
      expect_error(light(specular_size = c(10, 20)), "`specular_size`")
      expect_output(print(light(specular = .5)), "Specular: 0.5 , size 15 deg")
      expect_false(any(grepl("Specular", capture.output(print(light())))))
})
