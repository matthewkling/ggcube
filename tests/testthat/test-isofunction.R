# Tests for isosurfaces of functions

radius <- function(x, y, z) sqrt(x^2 + y^2 + z^2)
sphere <- function(x, y, z) x^2 + y^2 + z^2 - 1
cube <- list(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), zlim = c(-1.5, 1.5))

isofunction_data <- function(..., plot = ggplot()) {
      p <- plot + geom_isofunction_3d(...) + coord_3d()
      layer_data(p, length(p$layers))
}

# The stat's output, without building a plot, so that its errors and warnings
# reach the caller directly
isofunction_stat <- function(..., scales = list()) {
      StatIsofunction3D$compute_panel(data.frame(group = -1L), scales = scales, ...)
}


test_that("the default surface is where the function is zero", {
      skip_if_not_installed("rmarchingcubes")
      d <- do.call(isofunction_data, c(list(fun = sphere), cube))
      expect_equal(unique(d$level), 0)
      expect_equal(radius(d$x, d$y, d$z), rep(1, nrow(d)), tolerance = 0.01)

      d <- do.call(isofunction_data, c(list(fun = sphere, levels = c(-0.75, 0)), cube))
      expect_equal(median(radius(d$x, d$y, d$z)[d$level == -0.75]), 0.5, tolerance = 0.02)
})


test_that("surfaces match those of the same field given as gridded data", {
      skip_if_not_installed("rmarchingcubes")
      s <- seq(-1.5, 1.5, length.out = 20)
      g <- expand.grid(x = s, y = s, z = s)
      g$value <- sphere(g$x, g$y, g$z)
      gridded <- layer_data(ggplot(g, aes(x, y, z, value = value)) +
                                  geom_isosurface_3d(levels = c(0, 0.5), inside = "below", cap = TRUE) +
                                  zlim(NA, 0.6) + coord_3d())
      fn <- isofunction_data(fun = sphere, levels = c(0, 0.5), cap = TRUE, n = 20,
                             xlim = range(s), ylim = range(s), zlim = range(s),
                             plot = ggplot() + zlim(NA, 0.6))
      cols <- c("x", "y", "z", "level", "cap", "normal_x", "normal_y", "normal_z")
      expect_equal(fn[, cols], gridded[, cols], ignore_attr = TRUE)
})


test_that("surfaces enclose where the function is below the level, unless asked otherwise", {
      skip_if_not_installed("rmarchingcubes")
      outward <- function(d) {
            tri <- d[!duplicated(d$group), ]
            sign(tri$normal_x * tri$x + tri$normal_y * tri$y + tri$normal_z * tri$z)
      }
      d <- do.call(isofunction_data, c(list(fun = sphere), cube))
      expect_true(all(outward(d) == 1))
      d <- do.call(isofunction_data, c(list(fun = sphere, inside = "above"), cube))
      expect_true(all(outward(d) == -1))
      expect_error(do.call(isofunction_stat, c(list(fun = sphere, inside = "out"), cube)), "`inside`")
})


test_that("the domain defaults to the ranges of the plot's scales", {
      skip_if_not_installed("rmarchingcubes")
      # Points spanning a domain that cuts the unit sphere at z = 0
      corners <- data.frame(x = c(-2, 2), y = c(-2, 2), z = c(0, 2))
      d <- isofunction_data(fun = sphere, cap = TRUE,
                            plot = ggplot(corners, aes(x, y, z)) + geom_point_3d() + zlim(0, 2))
      expect_equal(min(d$z), 0)
      expect_true(all(d$z[d$cap] == 0))
      expect_equal(radius(d$x, d$y, d$z)[!d$cap], rep(1, sum(!d$cap)), tolerance = 0.02)

      # The z domain isn't taken from other layers' data, which isn't yet known
      expect_warning(
            layer_data(ggplot(corners, aes(x, y, z)) + geom_point_3d() +
                             geom_isofunction_3d(fun = sphere) + coord_3d(), 2),
            "`zlim` must be given")
})


test_that("a domain that can't be resolved, or isn't valid, is an error", {
      skip_if_not_installed("rmarchingcubes")
      expect_error(isofunction_stat(fun = sphere), "`xlim` must be given")
      expect_error(isofunction_stat(fun = sphere, xlim = c(1, -1), ylim = c(-1, 1), zlim = c(-1, 1)),
                   "`xlim` must be two finite increasing numbers")
      expect_error(isofunction_stat(fun = sphere, xlim = c(-1, 1), ylim = c(-1, NA), zlim = c(-1, 1)),
                   "`ylim`")
})


test_that("functions that can't be evaluated as required are errors", {
      skip_if_not_installed("rmarchingcubes")
      run <- function(fun, ...) do.call(isofunction_stat, c(list(fun = fun, ...), cube))
      expect_error(run(NULL), "`fun` must be a function")
      expect_error(run(function(x, y, z) stop("nope")), "`fun` failed")
      expect_error(run(function(x, y, z) 1), "one number for each")
      expect_error(run(function(x, y, z) log(x)), "finite values")
      expect_error(run(sphere, n = 1), "`n`")
      expect_error(run(sphere, levels = NA), "`levels`")
      expect_error(run(sphere, cap = "yes"), "`cap`")
})


test_that("functions with no surface at the levels give a warning with their range", {
      skip_if_not_installed("rmarchingcubes")
      expect_warning(d <- do.call(isofunction_stat, c(list(fun = sphere, levels = 10), cube)),
                     "`fun` ranges from -0.99")
      expect_equal(nrow(d), 0)
})


test_that("caps close surfaces at the edges of the domain", {
      skip_if_not_installed("rmarchingcubes")
      half <- list(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), zlim = c(0, 1.5))
      open <- do.call(isofunction_data, c(list(fun = sphere), half))
      d <- do.call(isofunction_data, c(list(fun = sphere, cap = TRUE), half))
      expect_false(any(open$cap))
      expect_true(all(d$z[d$cap] == 0))
      expect_equal(unique(d$normal_z[d$cap]), -1)
})


test_that("the surface is drawn in every facet panel", {
      skip_if_not_installed("rmarchingcubes")
      pts <- data.frame(x = 0, y = 0, z = 0, f = c("a", "b"))
      d <- layer_data(ggplot(pts, aes(x, y, z)) + geom_point_3d() +
                            do.call(geom_isofunction_3d, c(list(fun = sphere), cube)) +
                            facet_wrap(~f) + coord_3d(), 2)
      expect_setequal(unique(d$PANEL), factor(1:2))
      expect_equal(sum(d$PANEL == 1), sum(d$PANEL == 2))
})


test_that("a missing rmarchingcubes is reported", {
      local_mocked_bindings(isosurface_engine_available = function() FALSE)
      expect_error(do.call(isofunction_stat, c(list(fun = sphere), cube)), "rmarchingcubes")
})
