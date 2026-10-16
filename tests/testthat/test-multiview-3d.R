# Tests for multiview_3d(): panel order and angles, shared framing, labels,
# layout, and lighting. These inspect the returned patchwork and its built
# panels rather than rendered pixels.

skip_if_not_installed("patchwork", "1.3.1")

sphere_plot <- function() {
      ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() + coord_3d()
}

# All panels of a multiview, in order. patchwork keeps every panel but the last
# in `patches$plots`; the last is the returned object itself.
panels_of <- function(figure) {
      c(figure$patches$plots, list(figure))
}

panel_angles <- function(figure, param) {
      vapply(panels_of(figure), function(panel) panel$coordinates[[param]],
             numeric(1))
}

panel_titles <- function(figure) {
      lapply(panels_of(figure), function(panel) panel$labels$title)
}

# The built light of panel i. Only panels held in `patches$plots` are plain
# ggplots, so this is used on the leading panels.
built_panel_light <- function(figure, i) {
      ggplot_build(figure$patches$plots[[i]])$layout$panel_params[[1]]$light
}

# --- Panels and framing ---------------------------------------------------------

test_that("panels follow the orbit grid, columns varying fastest", {
      figure <- multiview_3d(sphere_plot(), yaw = c(0, 360), roll = c(-90, 0),
                             n = c(4, 3))
      expect_length(panels_of(figure), 12)
      expect_equal(panel_angles(figure, "yaw"), rep(c(0, 90, 180, 270), times = 3))
      expect_equal(panel_angles(figure, "roll"), rep(c(-90, -45, 0), each = 4))
      # Parameters that aren't ranged are held at the plot's coord.
      expect_true(all(panel_angles(figure, "pitch") == 0))
})

test_that("a grid lays out columns and rows from the two ranges", {
      figure <- multiview_3d(sphere_plot(), yaw = c(0, 360), roll = c(-90, 0),
                             n = c(4, 3))
      expect_equal(figure$patches$layout$ncol, 4)
      expect_equal(figure$patches$layout$nrow, 3)
      expect_true(figure$patches$layout$byrow)

      # A single sequence is left for patchwork to wrap.
      figure <- multiview_3d(sphere_plot(), yaw = c(0, 360), n = 4)
      expect_null(figure$patches$layout$ncol)
      expect_null(figure$patches$layout$nrow)
})

test_that("every panel shares one framing", {
      p <- sphere_plot()
      figure <- multiview_3d(p, yaw = c(0, 90), n = 3)
      bounds <- lapply(panels_of(figure), function(panel) panel$coordinates$fixed_bounds)

      expect_false(is.null(bounds[[1]]))
      for (b in bounds[-1]) expect_equal(b, bounds[[1]])

      seqs <- data.frame(pitch = 0, roll = -60, yaw = c(0, 45, 90))
      expect_equal(bounds[[1]],
                   compute_frame_bounds(p, extract_static_coord_params(p$coordinates),
                                        seqs))
})

test_that("panels keep the plot's other coord settings", {
      p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() +
            coord_3d(persp = FALSE, ratio = c(1, 2, 1), panels = "all")
      panel <- panels_of(multiview_3d(p, yaw = c(0, 90), n = 2))[[1]]
      expect_false(panel$coordinates$persp)
      expect_equal(panel$coordinates$ratio, c(1, 2, 1))
      expect_equal(panel$coordinates$panels, "all")
})

# --- Labels -----------------------------------------------------------------

test_that("panel titles name only the varying angles, counting in order", {
      figure <- multiview_3d(sphere_plot(), yaw = c(0, 360), roll = c(-90, 0),
                             n = c(4, 2))
      expect_equal(unlist(panel_titles(figure)),
                   c("yaw 0, roll -90", "yaw 90, roll -90",
                     "yaw 180, roll -90", "yaw 270, roll -90",
                     "yaw 0, roll 0", "yaw 90, roll 0",
                     "yaw 180, roll 0", "yaw 270, roll 0"))

      figure <- multiview_3d(sphere_plot(), yaw = c(-180, 180), n = 4)
      expect_equal(unlist(panel_titles(figure)),
                   c("yaw -180", "yaw -90", "yaw 0", "yaw 90"))
})

test_that("light panels are titled with the light angle as given", {
      p <- sphere_plot() + light("direct")
      figure <- multiview_3d(p, light_azimuth = c(-90, 270), n = 4)
      expect_equal(unlist(panel_titles(figure)),
                   c("light azimuth -90", "light azimuth 0",
                     "light azimuth 90", "light azimuth 180"))
})

test_that("labels = FALSE leaves panels untitled", {
      figure <- multiview_3d(sphere_plot(), yaw = c(0, 90), n = 3, labels = FALSE)
      expect_true(all(vapply(panel_titles(figure), is.null, logical(1))))
})

test_that("the plot's own title, subtitle, and caption go over the whole figure", {
      p <- sphere_plot() + labs(title = "T", subtitle = "S", caption = "C")
      figure <- multiview_3d(p, yaw = c(0, 90), n = 2)
      expect_equal(figure$patches$annotation$title, "T")
      expect_equal(figure$patches$annotation$subtitle, "S")
      expect_equal(figure$patches$annotation$caption, "C")

      first <- panels_of(figure)[[1]]
      expect_equal(first$labels$title, "yaw 0")
      expect_null(first$labels$subtitle)
      expect_null(first$labels$caption)

      # Without angle labels, panels are untitled rather than repeating the
      # plot's title.
      figure <- multiview_3d(p, yaw = c(0, 90), n = 2, labels = FALSE)
      expect_null(panels_of(figure)[[1]]$labels$title)
      expect_equal(figure$patches$annotation$title, "T")
})

# --- Lighting ----------------------------------------------------------------

test_that("a light added before coord_3d() still lights every panel", {
      p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() +
            light("direct", direction = c(1, 0, 0)) + coord_3d()
      figure <- multiview_3d(p, yaw = c(0, 90), n = 3)
      light <- built_panel_light(figure, 1)
      expect_equal(light$method, "direct")
      expect_equal(light$direction, c(1, 0, 0))
})

test_that("light panels each carry their own light direction", {
      p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() +
            light("direct") + coord_3d()
      figure <- multiview_3d(p, light_azimuth = c(0, 180), light_elevation = 30,
                             n = 3)
      expect_equal(built_panel_light(figure, 1)$direction, direction(0, 30))
      expect_equal(built_panel_light(figure, 2)$direction, direction(90, 30))
      expect_true(all(panel_angles(figure, "yaw") == -30))
})

test_that("light angles follow the same rules as in orbit_3d()", {
      p <- sphere_plot() + light(position = c(1, 1, 1))
      expect_error(multiview_3d(p, light_azimuth = c(0, 180)), "directional")
      expect_error(multiview_3d(sphere_plot(), light_elevation = c(0, 100)),
                   "between -90 and 90")
})

# --- Validation ----------------------------------------------------------------

test_that("multiview_3d() validates its inputs", {
      expect_error(multiview_3d(ggplot(), yaw = c(0, 90)), "coord_3d")
      expect_error(multiview_3d(sphere_plot(), yaw = 30), "at least one")
      expect_error(multiview_3d(sphere_plot(), yaw = c(0, 90), roll = c(-90, 0),
                                n = 4), "`n` has length 1")
      expect_error(multiview_3d(sphere_plot(), yaw = c(0, 90), n = 0),
                   "positive integers")
      expect_error(multiview_3d(sphere_plot(), yaw = c(0, 90), labels = NA),
                   "TRUE or FALSE")
      expect_error(multiview_3d(sphere_plot(), yaw = c(0, 90), loop = "yes"),
                   "NULL, TRUE, or FALSE")
})

test_that("multiview_labels() names and formats each varying axis", {
      seqs <- data.frame(pitch = 0, roll = c(-60, 300), yaw = c(330, 10),
                         light_azimuth = c(-90, 45), light_elevation = c(30, 60))
      expect_equal(multiview_labels(seqs, c("yaw", "light_elevation")),
                   c("yaw 330, light elevation 30", "yaw 10, light elevation 60"))
      expect_equal(multiview_labels(seqs, "roll"), c("roll -60", "roll 300"))
      expect_equal(multiview_labels(seqs, "light_azimuth"),
                   c("light azimuth -90", "light azimuth 45"))
})
