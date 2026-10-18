# geom_contour_3d() and geom_ridgeline_3d() convert their whole layer in
# setup_data(), which sees every facet panel at once. These check that each
# panel's output matches what that panel's data gives on its own.

facet_grid_data <- function() {
      g <- expand.grid(x = 1:12, y = 1:10)
      g$z <- sin(g$x / 3) + cos(g$y / 4)
      rbind(transform(g, k = "a"), transform(g, k = "b", z = z * 2))
}

# Coordinates of a built layer, sorted so row order doesn't matter.
layer_coords <- function(data) {
      out <- data[c("x", "y", "z")]
      out <- out[do.call(order, out), ]
      rownames(out) <- NULL
      out
}

expect_panels_match_alone <- function(layer) {
      d <- facet_grid_data()
      faceted <- ggplot_build(ggplot(d, aes(x, y, z)) + layer + coord_3d() +
                                    facet_wrap(~k))$data[[1]]
      for (i in 1:2) {
            alone <- ggplot_build(ggplot(subset(d, k == c("a", "b")[i]), aes(x, y, z)) +
                                        layer + coord_3d())$data[[1]]
            expect_equal(layer_coords(faceted[faceted$PANEL == i, ]),
                         layer_coords(alone))
      }
}

test_that("geom_contour_3d() keeps facet panels separate", {
      expect_panels_match_alone(geom_contour_3d())
})

test_that("geom_ridgeline_3d() keeps facet panels separate", {
      expect_panels_match_alone(geom_ridgeline_3d())
})

test_that("contours and ridgelines appear in every view facet panel", {
      d <- subset(facet_grid_data(), k == "a")
      for (layer in list(geom_contour_3d(), geom_ridgeline_3d())) {
            plain <- ggplot_build(ggplot(d, aes(x, y, z)) + layer + coord_3d())$data[[1]]
            faceted <- ggplot_build(ggplot(d, aes(x, y, z)) + layer + coord_3d() +
                                          facet_wrap_views(~yaw, n = 3))$data[[1]]
            for (i in 1:3) {
                  expect_equal(layer_coords(faceted[faceted$PANEL == i, ]),
                               layer_coords(plain))
            }
      }
})
