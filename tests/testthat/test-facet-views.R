# Tests for facet_wrap_views() and facet_grid_views(): which views panels get,
# how they're laid out and labeled, shared framing, lighting, and errors.
# These inspect built plots rather than rendered pixels.

sphere_plot <- function() {
      ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() + coord_3d()
}

built <- function(p) ggplot_build(p)$layout

panel_angle <- function(layout, param) {
      vapply(layout$panel_params, function(pp) pp$proj[[param]], numeric(1))
}

# --- Which views panels get ---------------------------------------------------

test_that("facet_wrap_views() gives each panel its own view", {
      l <- built(sphere_plot() + facet_wrap_views(~yaw))
      expect_equal(nrow(l$layout), 4)
      expect_equal(panel_angle(l, "yaw"), c(0, 90, 180, 270))
      # Angles not in the formula are held at the coord's values.
      expect_true(all(panel_angle(l, "roll") == -60))
      expect_true(all(panel_angle(l, "pitch") == 0))
})

test_that("each panel's view matches its layout values", {
      l <- built(sphere_plot() + facet_grid_views(roll ~ yaw, n = c(3, 4)))
      expect_equal(as.character(l$layout$roll),
                   format_angle(panel_angle(l, "roll")))
      expect_equal(as.character(l$layout$yaw),
                   format_angle(panel_angle(l, "yaw")))
})

test_that("each angle has a default range", {
      spec <- new_view_spec(view_angle_names, list(), 4)
      expect_equal(spec$values$yaw, c(0, 90, 180, 270))
      expect_equal(spec$values$light_azimuth, c(0, 90, 180, 270))
      expect_equal(spec$values$roll, c(-90, -60, -30, 0))
      expect_equal(spec$values$pitch, c(-45, -15, 15, 45))
      expect_equal(spec$values$light_elevation, c(0, 30, 60, 90))
})

test_that("ranges and fixed angles can be set", {
      l <- built(sphere_plot() +
                       facet_wrap_views(~yaw, yaw = c(-90, 90), n = 3, roll = -20))
      expect_equal(panel_angle(l, "yaw"), c(-90, 0, 90))
      expect_true(all(panel_angle(l, "roll") == -20))
})

test_that("n follows the order angles appear in the formula", {
      spec <- new_view_spec(c("roll", "yaw"), list(), c(3, 5))
      expect_length(spec$values$roll, 3)
      expect_length(spec$values$yaw, 5)
      spec <- new_view_spec(c("yaw", "roll"), list(), c(3, 5))
      expect_length(spec$values$yaw, 3)
      expect_length(spec$values$roll, 5)
})

# --- Layout ---------------------------------------------------------------

test_that("a grid puts the left side of the formula in rows and the right in columns", {
      l <- built(sphere_plot() + facet_grid_views(roll ~ yaw, n = c(3, 4)))
      lay <- l$layout
      expect_equal(max(lay$ROW), 3)
      expect_equal(max(lay$COL), 4)
      # Each row shares a roll and each column a yaw.
      roll <- panel_angle(l, "roll")
      yaw <- panel_angle(l, "yaw")
      expect_true(all(tapply(roll, lay$ROW, function(x) length(unique(x))) == 1))
      expect_true(all(tapply(yaw, lay$COL, function(x) length(unique(x))) == 1))
      expect_equal(as.vector(tapply(roll, lay$ROW, unique)), c(-90, -45, 0))
      expect_equal(as.vector(tapply(yaw, lay$COL, unique)), c(0, 90, 180, 270))
})

test_that("a side of a grid formula can be empty", {
      lay <- built(sphere_plot() + facet_grid_views(. ~ yaw))$layout
      expect_equal(c(max(lay$ROW), max(lay$COL)), c(1, 4))

      lay <- built(sphere_plot() + facet_grid_views(roll ~ .))$layout
      expect_equal(c(max(lay$ROW), max(lay$COL)), c(4, 1))

      lay <- built(sphere_plot() + facet_grid_views(~yaw))$layout
      expect_equal(c(max(lay$ROW), max(lay$COL)), c(1, 4))
})

test_that("several angles on one side are crossed", {
      p <- sphere_plot() + light("direct")
      l <- built(p + facet_grid_views(roll ~ yaw + light_azimuth, n = c(2, 3, 2)))
      lay <- l$layout
      expect_equal(c(max(lay$ROW), max(lay$COL)), c(2, 6))
      combos <- unique(lay[c("yaw", "light_azimuth")])
      expect_equal(nrow(combos), 6)

      l <- built(p + facet_wrap_views(~yaw + light_elevation, n = c(2, 3)))
      expect_equal(nrow(l$layout), 6)
})

test_that("wrap layout arguments pass through", {
      lay <- built(sphere_plot() + facet_wrap_views(~yaw, nrow = 1))$layout
      expect_equal(c(max(lay$ROW), max(lay$COL)), c(1, 4))
      lay <- built(sphere_plot() + facet_wrap_views(~yaw, ncol = 1))$layout
      expect_equal(c(max(lay$ROW), max(lay$COL)), c(4, 1))
})

test_that("strips name each angle and its value by default", {
      p <- sphere_plot() + facet_wrap_views(~yaw)
      b <- ggplot_build(p)
      labels <- b$layout$facet$format_strip_labels(b$layout$layout,
                                                   b$layout$facet_params)
      expect_equal(unname(unlist(labels$facets)), c("yaw: 0", "yaw: 90", "yaw: 180", "yaw: 270"))

      p <- sphere_plot() + facet_wrap_views(~yaw, labeller = "label_value")
      b <- ggplot_build(p)
      labels <- b$layout$facet$format_strip_labels(b$layout$layout,
                                                   b$layout$facet_params)
      expect_equal(unname(unlist(labels$facets)), c("0", "90", "180", "270"))
})

test_that("every panel gets all of the data", {
      b <- ggplot_build(sphere_plot() + facet_wrap_views(~yaw))
      plain <- ggplot_build(sphere_plot())
      counts <- table(b$data[[1]]$PANEL)
      expect_true(all(counts == nrow(plain$data[[1]])))
})

test_that("view facets widen the default panel spacing", {
      b <- ggplot_build(sphere_plot() + facet_wrap_views(~yaw))
      expect_equal(b$plot$theme$panel.spacing, grid::unit(20, "pt"))
})

# --- Framing ---------------------------------------------------------------

test_that("every panel shares one padded frame", {
      l <- built(sphere_plot() + facet_grid_views(roll ~ yaw, n = c(2, 3)))
      bounds <- lapply(l$panel_params, `[[`, "plot_bounds")
      for (b in bounds[-1]) expect_equal(b, bounds[[1]])

      # The shared frame contains a single view's own frame, padded.
      single <- built(sphere_plot())$panel_params[[1]]$plot_bounds
      expect_true(diff(bounds[[1]][1:2]) > diff(single[1:2]))
      expect_true(diff(bounds[[1]][3:4]) > diff(single[3:4]))
})

test_that("the shared frame is every view's frame, together, padded by 15%", {
      p <- sphere_plot()
      l <- built(p + facet_wrap_views(~yaw, n = 2))
      shared <- l$panel_params[[1]]$plot_bounds

      own <- lapply(c(0, 180), function(y) {
            built(ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() +
                        coord_3d(yaw = y))$panel_params[[1]]$plot_bounds
      })
      b <- do.call(rbind, own)
      union <- c(min(b[, 1]), max(b[, 2]), min(b[, 3]), max(b[, 4]))
      pad <- c(diff(union[1:2]), diff(union[3:4])) * 0.15
      expect_equal(shared, union + c(-pad[1], pad[1], -pad[2], pad[2]))
})

test_that("panels use the plot's trained x and y scales", {
      plain <- built(sphere_plot())$panel_params[[1]]
      for (pp in built(sphere_plot() + facet_wrap_views(~yaw))$panel_params) {
            expect_equal(pp$scale_info$x[c("limits", "breaks", "name")],
                         plain$scale_info$x[c("limits", "breaks", "name")])
            expect_equal(pp$scale_info$y[c("limits", "breaks", "name")],
                         plain$scale_info$y[c("limits", "breaks", "name")])
      }
})

test_that("panels keep the plot's other coord settings", {
      p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() +
            coord_3d(persp = FALSE, ratio = c(1, 2, 1))
      pp <- built(p + facet_wrap_views(~yaw, n = 2))$panel_params[[2]]
      expect_false(pp$proj$persp)
      expect_equal(pp$ratio, c(1, 2, 1))
})

# --- Lighting ----------------------------------------------------------------

test_that("light angles give each panel its own light direction", {
      p <- sphere_plot() + light("direct")
      l <- built(p + facet_wrap_views(~light_azimuth, light_elevation = 30))
      dirs <- lapply(l$panel_params, function(pp) pp$light$direction)
      expect_equal(dirs, lapply(c(0, 90, 180, 270), direction, elevation = 30))
      expect_equal(l$panel_params[[1]]$light$method, "direct")
      # The camera is untouched.
      expect_true(all(panel_angle(l, "yaw") == -30))
})

test_that("a light added before coord_3d() is the one faceted", {
      p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() +
            light("direct", mode = "hsl") + coord_3d()
      l <- built(p + facet_wrap_views(~light_elevation))
      expect_equal(l$panel_params[[1]]$light$shade_mode, "hsl")
})

test_that("a camera facet leaves the plot's light alone", {
      p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() +
            light("direct", direction = c(1, 0, 0)) + coord_3d()
      l <- built(p + facet_wrap_views(~yaw))
      for (pp in l$panel_params) expect_equal(pp$light$direction, c(1, 0, 0))
})

# --- Errors --------------------------------------------------------------------

test_that("formulas may only name viewing angles, once each", {
      expect_error(facet_wrap_views(~species), "not a viewing angle")
      expect_error(facet_grid_views(roll ~ species), "use facet_grid\\(\\)")
      expect_error(facet_grid_views(yaw ~ yaw), "more than once")
      expect_error(facet_wrap_views(~.), "at least one angle")
      expect_error(facet_wrap_views(1), "must be a formula")
})

test_that("angle arguments must agree with the formula", {
      expect_error(facet_wrap_views(~yaw, yaw = 30), "needs a range")
      expect_error(facet_wrap_views(~yaw, roll = c(-90, 0)), "isn't in the formula")
      expect_error(facet_wrap_views(~yaw, yaw = "a"), "must be numeric")
      expect_error(facet_wrap_views(~light_elevation, light_elevation = c(0, 100)),
                   "between -90 and 90")
      expect_error(facet_wrap_views(~yaw, yaw = c(10, 10)), "repeated angles")
})

test_that("n must match the formula", {
      expect_error(facet_grid_views(roll ~ yaw, n = c(2, 3, 4)), "names 2 angles")
      expect_error(facet_wrap_views(~yaw, n = 0), "positive integers")
})

test_that("light angles need a directional plot-level light", {
      p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() +
            geom_hull_3d(aes(x = x + 3), light = light("direct")) + coord_3d()
      expect_error(ggplot_build(p + facet_wrap_views(~light_azimuth)),
                   "sets its own")

      p <- sphere_plot() + light(position = c(1, 1, 1))
      expect_error(ggplot_build(p + facet_wrap_views(~light_azimuth)),
                   "directional")
})

test_that("orbit_3d() and animate_3d() can't move angles a view facet sets", {
      p <- sphere_plot() + facet_wrap_views(~yaw, roll = -20)
      expect_error(orbit_3d(p, yaw = c(0, 90)), "sets `yaw`")
      expect_error(animate_3d(p, roll = c(-90, 0)), "sets `roll`")
      expect_no_error(check_view_facet_angles(p, list(pitch = c(0, 30)), "orbit_3d"))
      expect_no_error(check_view_facet_angles(sphere_plot(), list(yaw = c(0, 90)),
                                              "orbit_3d"))
})

# --- Ordinary plots are unaffected ---------------------------------------------

test_that("plots without view facets share panel params as before", {
      p <- sphere_plot() + facet_wrap(~ x > 0)
      lay <- built(p)$layout
      expect_true(all(lay$COORD == 1))
})

test_that("the coord's aspect ratio comes from the panel's own frame", {
      pp <- built(sphere_plot())$panel_params[[1]]
      coord <- sphere_plot()$coordinates
      expect_equal(coord$aspect(pp),
                   diff(pp$plot_bounds[3:4]) / diff(pp$plot_bounds[1:2]))
})

test_that("view_map_data() copies every row into every panel", {
      layout <- data.frame(PANEL = factor(1:3))
      out <- view_map_data(data.frame(a = 1:2), layout)
      expect_equal(out$a, rep(1:2, 3))
      expect_equal(as.integer(out$PANEL), rep(1:3, each = 2))

      empty <- view_map_data(data.frame(a = integer(0)), layout)
      expect_equal(nrow(empty), 0)
      expect_equal(levels(empty$PANEL), c("1", "2", "3"))
})
