test_that("resolve_scale_depth defaults every element to full strength", {
      resolved <- resolve_scale_depth(TRUE)

      expect_named(resolved, ggcube_depth_elements)
      expect_true(all(resolved == 1))
})

test_that("resolve_scale_depth treats FALSE as off everywhere", {
      resolved <- resolve_scale_depth(FALSE)

      expect_named(resolved, ggcube_depth_elements)
      expect_true(all(resolved == 0))
})

test_that("resolve_scale_depth broadcasts a single number", {
      resolved <- resolve_scale_depth(0.4)

      expect_true(all(resolved == 0.4))
      expect_length(resolved, length(ggcube_depth_elements))
})

test_that("resolve_scale_depth applies named values and defaults the rest", {
      resolved <- resolve_scale_depth(c(text = 0.4))

      expect_equal(unname(resolved["text"]), 0.4)
      expect_equal(unname(resolved["grid"]), 1)
      expect_equal(unname(resolved["border"]), 1)
})

test_that("resolve_scale_depth accepts values above one", {
      resolved <- resolve_scale_depth(c(grid = 2.5))
      expect_equal(unname(resolved["grid"]), 2.5)
})

test_that("resolve_scale_depth is idempotent", {
      once <- resolve_scale_depth(c(text = 0.4))
      twice <- resolve_scale_depth(once)

      expect_equal(once, twice)
})

test_that("resolve_scale_depth rejects negative values", {
      expect_error(resolve_scale_depth(-1), "non-negative")
      expect_error(resolve_scale_depth(c(text = -0.5)), "non-negative")
})

test_that("resolve_scale_depth rejects non-finite values", {
      expect_error(resolve_scale_depth(NA_real_), "finite")
      expect_error(resolve_scale_depth(c(grid = Inf)), "finite")
})

test_that("resolve_scale_depth rejects unknown element names", {
      expect_error(resolve_scale_depth(c(title = 1)), "title")
      expect_error(resolve_scale_depth(c(grid = 1, panel = 1)), "panel")
})

test_that("resolve_scale_depth covers every documented element", {
      resolved <- resolve_scale_depth(c(grid = 0.2, border = 0.4,
                                        ticks = 0.6, text = 0.8))

      expect_equal(unname(resolved["ticks"]), 0.6)
      expect_setequal(names(resolved), c("grid", "border", "ticks", "text"))
})

test_that("resolve_scale_depth rejects malformed input", {
      expect_error(resolve_scale_depth("grid"))
      expect_error(resolve_scale_depth(c(1, 2)))
      expect_error(resolve_scale_depth(c(grid = 1, 2)))
      expect_error(resolve_scale_depth(c(grid = 1, grid = 2)), "duplicate")
      expect_error(resolve_scale_depth(c(TRUE, FALSE)))
      expect_error(resolve_scale_depth(NA))
})

test_that("apply_depth_strength is the identity at strength one", {
      depths <- c(0.5, 0.8, 1, 1.25, 2)
      expect_equal(apply_depth_strength(depths, 1), depths)
})

test_that("apply_depth_strength flattens to one at strength zero", {
      depths <- c(0.5, 0.8, 1, 1.25, 2)
      expect_true(all(apply_depth_strength(depths, 0) == 1))
})

test_that("apply_depth_strength interpolates geometrically", {
      expect_equal(apply_depth_strength(4, 0.5), 2)
      expect_equal(apply_depth_strength(0.25, 0.5), 0.5)
})

test_that("apply_depth_strength composes multiplicatively", {
      depths <- c(0.7, 1, 1.75)
      chained <- apply_depth_strength(apply_depth_strength(depths, 0.5), 0.4)

      expect_equal(chained, apply_depth_strength(depths, 0.5 * 0.4))
})

test_that("apply_depth_strength treats near and far symmetrically", {
      # A depth factor and its reciprocal stay reciprocal at any strength
      expect_equal(apply_depth_strength(2, 0.3) * apply_depth_strength(0.5, 0.3), 1)
})

test_that("apply_depth_strength stays positive and finite for large strengths", {
      depths <- c(0.5, 0.7, 1, 1.75)
      result <- apply_depth_strength(depths, 10)

      expect_true(all(result > 0))
      expect_true(all(is.finite(result)))
})

test_that("apply_depth_strength falls back to identity for invalid strength", {
      depths <- c(0.8, 1.2)
      expect_equal(apply_depth_strength(depths, NA), depths)
      expect_equal(apply_depth_strength(depths, NULL), depths)
      expect_equal(apply_depth_strength(depths, c(1, 2)), depths)
})

test_that("depth_strength reads an element off a coord", {
      coord <- list(scale_depth = c(grid = 1, border = 0.5, text = 0))

      expect_equal(depth_strength(coord, "grid"), 1)
      expect_equal(depth_strength(coord, "border"), 0.5)
      expect_equal(depth_strength(coord, "text"), 0)
})

test_that("depth_strength defaults to full strength when unavailable", {
      # Coord predating the parameter
      expect_equal(depth_strength(list(), "text"), 1)
      # Element not present in the stored vector
      expect_equal(depth_strength(list(scale_depth = c(grid = 0)), "text"), 1)
})

test_that("safe_lwd replaces invalid linewidths with the fallback", {
      expect_equal(safe_lwd(c(1, 2, 3), fallback = 5), c(1, 2, 3))
      expect_equal(safe_lwd(c(1, NA, -1, Inf), fallback = 5), c(1, 5, 5, 5))
})

test_that("coord_3d stores a resolved scale_depth vector", {
      coord <- coord_3d()[[1]]

      expect_named(coord$scale_depth, ggcube_depth_elements)
      expect_true(all(coord$scale_depth == 1))

      subdued <- coord_3d(scale_depth = c(text = 0.4))[[1]]
      expect_equal(unname(subdued$scale_depth["text"]), 0.4)
})

test_that("coord_3d rejects invalid scale_depth at construction", {
      expect_error(coord_3d(scale_depth = -1), "non-negative")
      expect_error(coord_3d(scale_depth = c(title = 1)), "title")
})
