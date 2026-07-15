# Tests for orbit_3d front-end logic: axis assignment, validation, grid
# construction, and per-axis loop detection. These exercise the pure helpers
# and do not render (grob/render behavior is covered elsewhere / skipped on
# CRAN).

# A minimal stand-in for a Coord3D carrying default rotation angles, used as
# the source of held-constant values in build_param_grid().
fake_coord <- function(pitch = 10, roll = 20, yaw = 30) {
      list(pitch = pitch, roll = roll, yaw = yaw)
}

# --- angles_return_home ------------------------------------------------------

test_that("angles_return_home detects full-circle returns modulo 360", {
      expect_true(angles_return_home(0, 360))
      expect_true(angles_return_home(0, 720))
      expect_true(angles_return_home(0, 0))
      expect_true(angles_return_home(10, 370))
      expect_false(angles_return_home(0, 90))
      expect_false(angles_return_home(-45, 45))
})

# --- resolve_orbit_axes: classification and axis assignment ---------------

test_that("a single ranged parameter yields a 1D orbit on that axis", {
      expect_equal(resolve_orbit_axes(yaw = c(0, 360))$axes, "yaw")
      expect_equal(resolve_orbit_axes(roll = c(0, 360))$axes, "roll")
      expect_equal(resolve_orbit_axes(pitch = c(0, 90))$axes, "pitch")
})

test_that("two ranges assign x and y by priority", {
      # yaw is top x priority; roll is top y priority.
      expect_equal(resolve_orbit_axes(yaw = c(0, 360), roll = c(-45, 45))$axes,
                   c("yaw", "roll"))
      # yaw on x, pitch falls to y.
      expect_equal(resolve_orbit_axes(yaw = c(0, 360), pitch = c(0, 90))$axes,
                   c("yaw", "pitch"))
      # no yaw: pitch takes x (next x priority), roll takes y.
      expect_equal(resolve_orbit_axes(pitch = c(0, 90), roll = c(-45, 45))$axes,
                   c("pitch", "roll"))
})

test_that("fixed parameters are recorded and not treated as axes", {
      spec <- resolve_orbit_axes(yaw = c(0, 360), pitch = 15, roll = c(-45, 45))
      expect_equal(spec$axes, c("yaw", "roll"))
      expect_equal(spec$fixed$pitch, 15)
})

test_that("resolve_orbit_axes errors on invalid range counts", {
      # No ranges at all.
      expect_error(resolve_orbit_axes(pitch = 5),
                   "at least one rotation parameter")
      # A length-3 parameter is not a valid range.
      expect_error(resolve_orbit_axes(yaw = c(0, 180, 360)),
                   "length 3")
      # Three ranged parameters.
      expect_error(
            resolve_orbit_axes(yaw = c(0, 360), pitch = c(0, 90),
                                  roll = c(-1, 1)),
            "at most two")
})

# --- build_param_grid: dimensions, ordering, constants -----------------------

test_that("1D grid has the requested number of rows and holds others constant", {
      spec <- resolve_orbit_axes(yaw = c(0, 90))
      grid <- build_param_grid(fake_coord(pitch = 10, roll = 20, yaw = 30),
                               spec, n = 5, loop = NULL)
      expect_equal(nrow(grid$param_seqs), 5)
      expect_equal(grid$dims, 5L)
      # Non-swept params held at coord defaults.
      expect_true(all(grid$param_seqs$pitch == 10))
      expect_true(all(grid$param_seqs$roll == 20))
      # Swept param spans the range inclusively (non-looping).
      expect_equal(grid$param_seqs$yaw, seq(0, 90, length.out = 5))
})

test_that("2D grid is the row-major outer product with x varying fastest", {
      spec <- resolve_orbit_axes(yaw = c(0, 100), roll = c(-10, 10))
      grid <- build_param_grid(fake_coord(), spec, n = c(3, 2), loop = NULL)
      # 3 x 2 = 6 rows.
      expect_equal(nrow(grid$param_seqs), 6)
      expect_equal(grid$dims, c(3L, 2L))
      # x (yaw) varies fastest: first 3 rows sweep yaw at the first roll value.
      yaw_vals <- seq(0, 100, length.out = 3)
      roll_vals <- seq(-10, 10, length.out = 2)
      expect_equal(grid$param_seqs$yaw, rep(yaw_vals, times = 2))
      expect_equal(grid$param_seqs$roll, rep(roll_vals, each = 3))
})

test_that("fixed parameter overrides the coord default in the grid", {
      spec <- resolve_orbit_axes(yaw = c(0, 90), pitch = 42)
      grid <- build_param_grid(fake_coord(pitch = 10), spec, n = 4, loop = NULL)
      expect_true(all(grid$param_seqs$pitch == 42))
})

# --- Per-axis loop detection and endpoint handling ---------------------------

test_that("a full-turn axis loops and drops its duplicate endpoint", {
      spec <- resolve_orbit_axes(yaw = c(0, 360))
      grid <- build_param_grid(fake_coord(), spec, n = 4, loop = NULL)
      expect_true(grid$loop_flags[1])
      # 4 unique frames over [0, 360): 0, 90, 180, 270 (no 360 duplicate).
      expect_equal(grid$param_seqs$yaw, c(0, 90, 180, 270))
      expect_equal(grid$dims, 4L)
})

test_that("a partial-sweep axis clamps and samples inclusively", {
      spec <- resolve_orbit_axes(roll = c(-90, 90))
      grid <- build_param_grid(fake_coord(), spec, n = 5, loop = NULL)
      expect_false(grid$loop_flags[1])
      expect_equal(grid$param_seqs$roll, seq(-90, 90, length.out = 5))
})

test_that("per-axis auto-detect mixes loop and clamp in 2D", {
      # yaw full turn (loops) x roll partial (clamps).
      spec <- resolve_orbit_axes(yaw = c(0, 360), roll = c(-45, 45))
      grid <- build_param_grid(fake_coord(), spec, n = c(6, 4), loop = NULL)
      expect_true(grid$loop_flags[1])
      expect_false(grid$loop_flags[2])
})

test_that("loop = FALSE forces clamp even on a full turn", {
      spec <- resolve_orbit_axes(yaw = c(0, 360))
      grid <- build_param_grid(fake_coord(), spec, n = 4, loop = FALSE)
      expect_false(grid$loop_flags[1])
      # Inclusive sampling keeps the endpoint (360 present).
      expect_equal(grid$param_seqs$yaw, seq(0, 360, length.out = 4))
})

test_that("loop = TRUE forces wrap and endpoint drop", {
      spec <- resolve_orbit_axes(roll = c(-90, 90))
      grid <- build_param_grid(fake_coord(), spec, n = 4, loop = TRUE)
      expect_true(grid$loop_flags[1])
      expect_equal(nrow(grid$param_seqs), 4)
      # Endpoint dropped: 4 samples over [-90, 90).
      expect_equal(grid$param_seqs$roll, seq(-90, 90, length.out = 5)[-5])
})

# --- Starting-frame selection ------------------------------------------------

test_that("start defaults to the sweep midpoint per axis", {
      spec <- resolve_orbit_axes(yaw = c(0, 350))
      grid <- build_param_grid(fake_coord(), spec, n = 36, loop = FALSE,
                               start = NULL)
      expect_equal(grid$start, 18L)  # floor(36 / 2)

      spec2 <- resolve_orbit_axes(yaw = c(0, 100), roll = c(-90, 90))
      grid2 <- build_param_grid(fake_coord(), spec2, n = c(6, 4), loop = FALSE,
                                start = NULL)
      expect_equal(grid2$start, c(3L, 2L))  # floor(6/2), floor(4/2)
})

test_that("start snaps to the frame nearest a named angle", {
      spec <- resolve_orbit_axes(yaw = c(0, 350))
      # 36 frames every 10 degrees; yaw = 30 -> index 3 (0-based).
      grid <- build_param_grid(fake_coord(), spec, n = 36, loop = TRUE,
                               start = c(yaw = 30))
      expect_equal(grid$start, 3L)
      # Nearest-frame snapping: 32 still rounds to the 30 frame.
      grid2 <- build_param_grid(fake_coord(), spec, n = 36, loop = TRUE,
                                start = c(yaw = 32))
      expect_equal(grid2$start, 3L)
})

test_that("a bare scalar start is accepted for a 1D orbit", {
      spec <- resolve_orbit_axes(yaw = c(0, 350))
      grid <- build_param_grid(fake_coord(), spec, n = 36, loop = TRUE,
                               start = 30)
      expect_equal(grid$start, 3L)
})

test_that("an unnamed swept axis in start falls back to its midpoint", {
      spec <- resolve_orbit_axes(yaw = c(0, 100), roll = c(-90, 90))
      grid <- build_param_grid(fake_coord(), spec, n = c(6, 4), loop = FALSE,
                               start = c(yaw = 0))
      expect_equal(grid$start[1], 0L)  # yaw = 0 -> first frame
      expect_equal(grid$start[2], 2L)  # roll unspecified -> midpoint
})

test_that("start naming a non-swept axis is an error", {
      spec <- resolve_orbit_axes(yaw = c(0, 350))
      expect_error(
            build_param_grid(fake_coord(), spec, n = 36, loop = TRUE,
                             start = c(roll = 10)),
            "fixed rather than variable")
})

test_that("a bare scalar start errors for a 2D orbit", {
      spec <- resolve_orbit_axes(yaw = c(0, 100), roll = c(-90, 90))
      expect_error(
            build_param_grid(fake_coord(), spec, n = c(6, 4), loop = FALSE,
                             start = 30),
            "named vector")
})
