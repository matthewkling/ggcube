test_that("point_segment_distance measures perpendicular distance", {
      expect_equal(point_segment_distance(0, 3, -5, 0, 5, 0), 3)
      expect_equal(point_segment_distance(2, 0, 0, 0, 10, 0), 0)
})

test_that("point_segment_distance clamps beyond the segment ends", {
      # Projection falls past the far end, so distance is to the endpoint
      expect_equal(point_segment_distance(13, 4, 0, 0, 10, 0), 5)
      expect_equal(point_segment_distance(-3, 4, 0, 0, 10, 0), 5)
})

test_that("point_segment_distance handles a degenerate segment", {
      expect_equal(point_segment_distance(3, 4, 0, 0, 0, 0), 5)
})


# A unit square in perimeter order, standing in for a projected silhouette
square_hull <- function() {
      data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
}

pt <- function(x, y) data.frame(x = x, y = y)

test_that("edge_on_hull accepts edges lying on the boundary", {
      hull <- square_hull()

      expect_true(edge_on_hull(pt(0, 0), pt(1, 0), hull))
      expect_true(edge_on_hull(pt(1, 0), pt(1, 1), hull))
      expect_true(edge_on_hull(pt(0, 0), pt(0, 1), hull))
})

test_that("edge_on_hull rejects a diagonal between hull vertices", {
      # Both endpoints are hull vertices, but the midpoint is interior.
      # This is the case that endpoint matching alone would get wrong.
      hull <- square_hull()

      expect_false(edge_on_hull(pt(0, 0), pt(1, 1), hull))
      expect_false(edge_on_hull(pt(1, 0), pt(0, 1), hull))
})

test_that("edge_on_hull rejects fully interior edges", {
      hull <- square_hull()

      expect_false(edge_on_hull(pt(0.25, 0.25), pt(0.75, 0.25), hull))
      expect_false(edge_on_hull(pt(0.5, 0.1), pt(0.5, 0.9), hull))
})

test_that("edge_on_hull accepts a sub-segment of a boundary edge", {
      # An edge shorter than the hull side it lies on is still external.
      # Vertex matching would reject this; the midpoint test does not.
      hull <- square_hull()

      expect_true(edge_on_hull(pt(0.2, 0), pt(0.8, 0), hull))
})

test_that("edge_on_hull treats a NULL hull as unconstrained", {
      expect_true(edge_on_hull(pt(0, 0), pt(1, 1), NULL))
})

test_that("edge_on_hull tolerance scales with hull size", {
      small <- square_hull()
      large <- data.frame(x = c(0, 1000, 1000, 0), y = c(0, 0, 1000, 1000))

      # An offset that is negligible relative to the large hull, but not the
      # small one, should be judged accordingly
      offset <- 0.01

      expect_false(edge_on_hull(pt(0, offset), pt(1, offset), small))
      expect_true(edge_on_hull(pt(0, 0), pt(1000, 0), large))
})

test_that("edge_on_hull handles a degenerate two-point hull", {
      # A face seen exactly edge-on collapses to a line segment
      line_hull <- data.frame(x = c(0, 10), y = c(0, 0))

      expect_true(edge_on_hull(pt(2, 0), pt(8, 0), line_hull))
      expect_false(edge_on_hull(pt(2, 5), pt(8, 5), line_hull))
})

test_that("classify_edge_externality reports both silhouettes independently", {
      cube_hull <- data.frame(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
      panels_hull <- square_hull()

      # On the panel boundary, interior to the cube
      result <- classify_edge_externality(pt(0, 0), pt(1, 0), cube_hull, panels_hull)
      expect_true(result$on_panel_hull)
      expect_false(result$on_cube_hull)

      # On the cube boundary, outside and so not on the panel boundary
      result <- classify_edge_externality(pt(-1, -1), pt(1, -1), cube_hull, panels_hull)
      expect_true(result$on_cube_hull)
      expect_false(result$on_panel_hull)
})
