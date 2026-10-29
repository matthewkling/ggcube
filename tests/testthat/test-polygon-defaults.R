# Tests for polygon layers' default lighting and outlines.

# Fill and border colors of a plot's drawn polygons
drawn_polygon_colors <- function(p) {
      layer <- find_grobs(drawn_grob(p), "ggcube_layer")[[1]]
      polys <- c(find_grobs(layer, "polygon"), find_grobs(layer, "pathgrob"))
      list(fill = unlist(lapply(polys, function(g) g$gp$fill)),
           col = unlist(lapply(polys, function(g) g$gp$col)))
}

grid_data <- function() {
      d <- expand.grid(x = seq(0, 1, length.out = 6), y = seq(0, 1, length.out = 6))
      d$z <- with(d, sin(3 * x) + cos(3 * y))
      d
}


test_that("contours and ridgelines aren't lit by the default light", {
      for (layer in list(geom_contour_3d(fill = "steelblue", bins = 5),
                         geom_ridgeline_3d(fill = "steelblue"))) {
            p <- ggplot(grid_data(), aes(x, y, z)) + layer + coord_3d()
            fills <- unique(toupper(drawn_polygon_colors(p)$fill))
            expect_equal(fills, "STEELBLUE", info = class(layer$geom)[1])
      }
})

test_that("contours and ridgelines are lit by explicitly set lighting", {
      lit <- list(
            plot = function(layer) ggplot(grid_data(), aes(x, y, z)) + layer + coord_3d() + light(),
            coord = function(layer) ggplot(grid_data(), aes(x, y, z)) + layer + coord_3d(light = light())
      )
      for (route in names(lit)) {
            for (layer in list(geom_contour_3d(fill = "steelblue", bins = 5),
                               geom_ridgeline_3d(fill = "steelblue"))) {
                  fills <- unique(toupper(drawn_polygon_colors(lit[[route]](layer))$fill))
                  expect_false(identical(fills, "STEELBLUE"),
                               info = paste(route, class(layer$geom)[1]))
            }
      }

      # Layer-level lighting
      p <- ggplot(grid_data(), aes(x, y, z)) +
            geom_ridgeline_3d(fill = "steelblue", light = light()) + coord_3d()
      expect_false(identical(unique(toupper(drawn_polygon_colors(p)$fill)), "STEELBLUE"))
})

test_that("other polygon layers are lit by the default light", {
      p <- ggplot(grid_data(), aes(x, y, z)) + geom_surface_3d(fill = "steelblue") + coord_3d()
      expect_gt(length(unique(drawn_polygon_colors(p)$fill)), 1)
})

test_that("the default light is marked as such, and copies keep the mark", {
      default <- resolve_light(waiver())
      expect_true(is_default_light(default))
      expect_false(is_default_light(light()))
      expect_false(is_default_light(NULL))

      # Animations and view facets move a copy of the light
      default$direction <- direction(90, 30)
      expect_true(is_default_light(default))
})

test_that("opaque polygons are outlined in their fill color, translucent ones not at all", {
      p <- ggplot(grid_data(), aes(x, y, z, fill = z)) + geom_surface_3d(light = "none") + coord_3d()
      cols <- drawn_polygon_colors(p)
      expect_equal(cols$col, cols$fill)

      p <- ggplot(grid_data(), aes(x, y, z)) +
            geom_surface_3d(fill = "steelblue", alpha = .5, light = "none") + coord_3d()
      cols <- drawn_polygon_colors(p)
      expect_true(all(is.na(cols$col) | cols$col == "transparent"))

      # Hulls, columns, and other polygon layers do the same
      b <- ggplot_build(ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d(fill = "tomato") + coord_3d())
      expect_equal(unique(b$data[[1]]$colour), "tomato")
})

test_that("a mapped or set color overrides fill-colored outlines", {
      d <- grid_data()
      b <- ggplot_build(ggplot(d, aes(x, y, z)) + geom_surface_3d(color = "black") + coord_3d())
      expect_equal(unique(b$data[[1]]$colour), "black")

      b <- ggplot_build(ggplot(d, aes(x, y, z, color = z > 0)) + geom_surface_3d() + coord_3d())
      expect_gt(length(unique(b$data[[1]]$colour)), 1)
      expect_false(any(b$data[[1]]$colour == b$data[[1]]$fill))
})

test_that("contours, ridgelines, and text keep their own outlines", {
      d <- grid_data()
      b <- ggplot_build(ggplot(d, aes(x, y, z)) + geom_contour_3d(bins = 5) + coord_3d())
      expect_equal(unique(b$data[[1]]$colour), "white")
      b <- ggplot_build(ggplot(d, aes(x, y, z)) + geom_ridgeline_3d() + coord_3d())
      expect_equal(unique(b$data[[1]]$colour), "black")
})

test_that("points and lines of composite polygon layers keep their own colors", {
      set.seed(1)
      d <- data.frame(x = runif(20), y = runif(20))
      d$z <- d$x + d$y + rnorm(20, 0, .1)
      p <- ggplot(d, aes(x, y, z)) + geom_smooth_3d(residuals = TRUE) + coord_3d()
      out <- ggplot_build(p)$data[[1]]
      seg <- out$.prim %in% "segment"
      expect_true(any(seg))
      expect_false(any(out$colour[seg] == out$fill[seg], na.rm = TRUE))
})

test_that("polygon layers default to a grey50 fill", {
      for (geom in list(GeomPolygon3D, GeomSurface3D, GeomIsosurface3D, GeomPlane3D)) {
            expect_equal(geom$default_aes$fill, "grey50")
      }
})
