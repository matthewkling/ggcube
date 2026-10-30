# Axis furniture is laid out in points beyond the cube, so in a small plot it
# reaches past the panel unless the panel's contents are shrunk to make room.

mtcars_3d <- ggplot(mtcars, aes(mpg, wt, qsec)) +
      geom_point_3d() +
      labs(x = "Miles per gallon", y = "Weight (1000 lbs)", z = "Quarter mile (s)")

# How far each axis title reaches past its panel, in points, as drawn on a
# `width` by `height` inch device: the largest overshoot past any side of the
# panel, negative when the title is inside it. Measured with grid's own
# geometry, by sampling the boundary of each rotated title.
title_overshoot_pt <- function(p, width, height) {
      grDevices::pdf(NULL, width = width, height = height)
      on.exit(grDevices::dev.off())
      grid::grid.newpage()
      print(p, newpage = FALSE)
      grid::grid.force()

      listing <- grid::grid.ls(grobs = TRUE, viewports = TRUE, print = FALSE)
      hits <- which(listing$name == "axis.titles.3d")
      to_pt <- function(x) as.numeric(x) * 72.27

      overshoot <- numeric(0)
      for (k in hits) {
            parts <- strsplit(listing$vpPath[k], "::")[[1]]
            parts <- parts[parts != "ROOT"]
            panel <- utils::tail(grep("^panel\\.", parts), 1)

            grid::downViewport(do.call(grid::vpPath, as.list(parts[seq_len(panel)])))
            lower <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"))
            upper <- grid::deviceLoc(grid::unit(1, "npc"), grid::unit(1, "npc"))
            grid::upViewport(0)

            grid::downViewport(do.call(grid::vpPath, as.list(parts)))
            path <- c(strsplit(listing$gPath[k], "::")[[1]], listing$name[k])
            titles <- grid::grid.get(do.call(grid::gPath, as.list(path)))
            for (title in titles$children) {
                  at <- lapply(seq(0, 355, by = 5), function(a) {
                        grid::deviceLoc(grid::grobX(title, a), grid::grobY(title, a))
                  })
                  xs <- to_pt(vapply(at, function(l) as.numeric(l$x), numeric(1)))
                  ys <- to_pt(vapply(at, function(l) as.numeric(l$y), numeric(1)))
                  overshoot <- c(overshoot, max(to_pt(lower$x) - min(xs), max(xs) - to_pt(upper$x),
                                                to_pt(lower$y) - min(ys), max(ys) - to_pt(upper$y)))
            }
            grid::upViewport(0)
      }
      overshoot
}

# Titles get room only for as much of them as is as long as their axis, so
# these use plots large enough for the titles to be no longer than that.
test_that("axis titles stay inside the panel of a small plot", {
      views <- list(
            coord_3d(direction = direction(210, -30)),
            coord_3d(pitch = 30, roll = -60, yaw = -30),
            coord_3d(direction = direction(45, 80)),
            coord_3d(direction = direction(0, 0))
      )
      for (view in views) {
            overshoot <- title_overshoot_pt(mtcars_3d + view, 3.5, 3.5)
            expect_length(overshoot, 3)
            expect_true(all(overshoot <= 1), info = paste(round(overshoot, 1), collapse = ", "))
      }
})

test_that("axis titles stay inside their panels in a patchwork grid", {
      skip_if_not_installed("patchwork")
      p <- patchwork::wrap_plots(
            mtcars_3d + coord_3d(direction = direction(210, -30)),
            mtcars_3d + coord_3d(pitch = 30, roll = -60, yaw = -30),
            nrow = 1)
      overshoot <- title_overshoot_pt(p, 7, 3.5)
      expect_length(overshoot, 6)
      expect_true(all(overshoot <= 1), info = paste(round(overshoot, 1), collapse = ", "))
})

test_that("the panel's contents are shrunk only when the furniture doesn't fit", {
      fit <- find_grobs(ggplotGrob(mtcars_3d + coord_3d()), "ggcube_panel_fit")
      expect_length(fit, 1)
      fit <- fit[[1]]

      grDevices::pdf(NULL, width = 6, height = 6)
      on.exit(grDevices::dev.off())

      expect_identical(fit_panel_contents(fit$furniture, 1000, 1000, fit$plot_bounds),
                       list(scale = 1, x = 0, y = 0))

      small <- fit_panel_contents(fit$furniture, 200, 200, fit$plot_bounds)
      expect_lt(small$scale, 1)
      expect_gte(small$scale, 0.5)
})

test_that("a title longer than its axis overflows rather than shrinking the cube", {
      long <- "Fuel economy in miles per US gallon, combined city and highway driving"
      fit_scale <- function(p) {
            fit <- find_grobs(ggplotGrob(p), "ggcube_panel_fit")[[1]]
            fit_panel_contents(fit$furniture, 288, 288, fit$plot_bounds)$scale
      }

      grDevices::pdf(NULL, width = 6, height = 6)
      on.exit(grDevices::dev.off())

      short_scale <- fit_scale(mtcars_3d + coord_3d())
      expect_lt(short_scale, 1)
      expect_equal(fit_scale(mtcars_3d + labs(x = long) + coord_3d()), short_scale,
                   tolerance = 0.01)
})

test_that("zoom applies on top of the fitted framing", {
      fits <- lapply(c(1, 1.25, 0.8), function(z) {
            find_grobs(ggplotGrob(mtcars_3d + coord_3d(zoom = z)), "ggcube_panel_fit")[[1]]
      })
      bounds <- function(f) f$furniture$panel_params$plot_bounds

      # The fit frames every zoom alike, while the panel is drawn zoomed
      expect_equal(fits[[2]]$plot_bounds, fits[[1]]$plot_bounds)
      expect_equal(fits[[3]]$plot_bounds, fits[[1]]$plot_bounds)
      expect_lt(diff(bounds(fits[[2]])[1:2]), diff(bounds(fits[[1]])[1:2]))
      expect_gt(diff(bounds(fits[[3]])[1:2]), diff(bounds(fits[[1]])[1:2]))
})

test_that("fixed frames and view facets are not fitted, and keep a wide margin", {
      fixed <- coord_3d(fixed_bounds = c(-1, 1, -1, 1))
      expect_length(find_grobs(ggplotGrob(mtcars_3d + fixed), "ggcube_panel_fit"), 0)
      expect_equal(fixed[[2]]$plot.margin, margin(20, 20, 20, 20, "pt"))

      faceted <- mtcars_3d + coord_3d() + facet_wrap_views(~yaw)
      expect_length(find_grobs(ggplotGrob(faceted), "ggcube_panel_fit"), 0)
      expect_equal(faceted$theme$plot.margin, margin(20, 20, 20, 20, "pt"))
})

test_that("coord_3d() leaves the plot margin to the theme", {
      expect_length(coord_3d(), 1)
      expect_null((mtcars_3d + coord_3d())$theme$plot.margin)
})
