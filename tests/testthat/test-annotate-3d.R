# Tests for annotate_3d(), which builds annotation layers.

# Constructor ------------------------------------------------------------------

test_that("annotate_3d builds a layer of the matching geom for each type", {
      geoms <- list(
            point = list(annotate_3d("point", x = 1, y = 2, z = 3), "GeomPoint3D"),
            text = list(annotate_3d("text", x = 0, y = 0, z = 0, label = "hi"), "GeomText3DBillboard"),
            segment = list(annotate_3d("segment", x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1),
                           "GeomSegment3D"),
            polygon = list(annotate_3d("polygon", x = c(0, 1, 0), y = c(0, 0, 1), z = 0), "GeomPolygon3D"),
            axes = list(annotate_3d("axes"), "GeomAxes3D"),
            refline = list(annotate_3d("refline", dz = 1), "GeomRefline3D"),
            plane = list(annotate_3d("plane", zintercept = 0), "GeomPlane3D")
      )
      for (type in names(geoms)) {
            l <- geoms[[type]][[1]]
            expect_s3_class(l, "LayerInstance")
            expect_true(inherits(l$geom, geoms[[type]][[2]]), info = type)
            expect_false(l$inherit.aes, info = type)
            expect_false(isTRUE(l$show.legend), info = type)
      }
})

test_that("annotate_3d puts positions in the layer's data, and other values in its parameters", {
      l <- annotate_3d("point", x = 1:3, y = 0, z = 0, colour = c("red", "green", "blue"))
      expect_equal(l$data, data.frame(x = 1:3, y = c(0, 0, 0), z = c(0, 0, 0)))
      expect_equal(l$aes_params$colour, c("red", "green", "blue"))

      l <- annotate_3d("text", x = 1, y = 2, z = 3, label = "hi", on_top = TRUE)
      expect_equal(l$data$label, "hi")
      expect_true(l$geom_params$on_top)
})

test_that("annotate_3d validates its arguments", {
      expect_error(annotate_3d("point", x = 1, y = 2), "requires `z`")
      expect_error(annotate_3d("text", x = 0, y = 0, z = 0), "requires `label`")
      expect_error(annotate_3d("segment", x = 0, y = 0, z = 0), "requires `xend`, `yend`, `zend`")
      expect_error(annotate_3d("sphere", z = 0), "must be one of")
      expect_error(annotate_3d("line", dz = 1), "must be one of")
      expect_error(annotate_3d("point", x = 1:3, y = 1:2, z = 0), "incompatible")
      expect_error(annotate_3d("point", x = 1:3, y = 0, z = 0, colour = c("red", "blue")), "incompatible")
})

test_that("annotate_3d normalizes color to colour and keeps the annotation defaults", {
      l <- annotate_3d("point", x = 0, y = 0, z = 0, color = "red")
      expect_equal(l$aes_params$colour, "red")
      expect_equal(l$aes_params$size, 2)
      expect_equal(annotate_3d("point", x = 0, y = 0, z = 0, size = 5)$aes_params$size, 5)
})


# Arrows ------------------------------------------------------------------------

test_that("segment and axes annotations accept an arrow, and only they do", {
      a <- grid::arrow(length = grid::unit(6, "pt"), type = "closed")
      l <- annotate_3d("segment", x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1, arrow = a)
      expect_identical(l$geom_params$arrow, a)
      expect_identical(annotate_3d("axes", arrow = a)$geom_params$arrow, a)

      expect_error(annotate_3d("point", x = 0, y = 0, z = 0, arrow = a), "only to segment")
      expect_error(annotate_3d("segment", x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1,
                               arrow = "yes"), "grid::arrow")
})


test_that("layer arrows are filled with the segment colour unless arrow.fill is given", {
      rows <- data.frame(x = c(0, 1, 0, 1), y = c(0, 1, 1, 0), group = c(1, 1, 2, 2),
                         point_type = c("start", "end", "start", "end"),
                         colour = c("red", "red", "blue", "blue"), linewidth = 0.5,
                         linetype = 1, alpha = 1, stringsAsFactors = FALSE)
      closed <- grid::arrow(type = "closed")
      expect_equal(render_segments(rows, arrow = closed)$gp$fill, c("red", "blue"))
      expect_equal(render_segments(rows, arrow = closed, arrow.fill = "white")$gp$fill,
                   c("white", "white"))
      expect_null(render_segments(rows)$gp$fill)

      p <- ggplot(data.frame(x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1),
                  aes(x, y, z, xend = xend, yend = yend, zend = zend)) +
            geom_segment_3d(colour = "#00FF00", arrow = closed, arrow.fill = "#0000FF") +
            coord_3d()
      layer <- find_grobs(drawn_grob(p), "ggcube_layer")[[1]]
      g <- find_grobs(layer, "segments")[[1]]
      expect_equal(g$gp$fill, "#0000FF")
      expect_equal(substr(g$gp$col, 1, 7), "#00FF00")
})


test_that("an annotation's arrow applies only to it, and its head stays at the end when split", {
      g <- expand.grid(x = seq(-1, 1, length.out = 6), y = seq(-1, 1, length.out = 6))
      g$z <- 0
      p <- ggplot(g, aes(x, y, z)) +
            geom_surface_3d(grid = "right2") +
            annotate_3d("segment", x = 0.1, y = 0.1, z = -1, xend = 0.1, yend = 0.1, zend = 1,
                        colour = "#FF0000", arrow = grid::arrow(type = "closed")) +
            geom_segment_3d(aes(x, y, z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE,
                            data = data.frame(x = -1, y = -1, z = 0.5, xend = 1, yend = -1, zend = 0.5),
                            colour = "#0000FF") +
            coord_3d()

      rows <- ggcube:::validation_scene(p)$panels[[1]]$rows
      ann <- rows[rows$.layer == 2, ]
      expect_gt(length(unique(ann$group)), 1)                      # split by the surface
      tip <- ann[ann$point_type == "end" & ann$.seg_last %in% TRUE, ]
      expect_equal(nrow(tip), 1)

      segs <- find_grobs(drawn_grob(p), "segments")
      with_arrow <- Filter(function(k) !is.null(k$arrow), segs)
      expect_length(with_arrow, 1)
      expect_length(as.numeric(with_arrow[[1]]$x1), 1)
      expect_equal(as.numeric(with_arrow[[1]]$x1), tip$x)
      expect_equal(substr(with_arrow[[1]]$gp$col, 1, 7), "#FF0000")
})


# Polygons ----------------------------------------------------------------------

test_that("polygon annotations group vertices and style each polygon", {
      l <- annotate_3d("polygon", x = c(0, 1, 1, 0, 2, 3, 2), y = c(0, 0, 1, 1, 0, 0, 1), z = 0,
                       group = c("a", "a", "a", "a", "b", "b", "b"), fill = c("red", "blue"))
      expect_equal(l$data$group, c(1, 1, 1, 1, 2, 2, 2))
      expect_equal(l$aes_params$fill, rep(c("red", "blue"), c(4, 3)))
      expect_equal(l$aes_params$colour, "grey30")
      expect_equal(l$aes_params$alpha, 0.5)

      # Without group, all vertices form one polygon
      expect_equal(unique(annotate_3d("polygon", x = 1:3, y = c(0, 1, 0), z = 0)$data$group), 1)

      expect_error(annotate_3d("polygon", x = 1:3, y = 1:3), "requires `z`")
      expect_error(annotate_3d("polygon", x = 1:4, y = 1:4, z = 0, group = c(1, 1, 2, 2)),
                   "at least 3 vertices")
      expect_error(annotate_3d("polygon", x = 1:3, y = 1:3, z = 0, group = 1:2), "one value per vertex")
      expect_error(annotate_3d("polygon", x = 1:6, y = 1:6, z = 0, group = rep(1:2, each = 3),
                               fill = c("red", "blue", "green")),
                   "length 1 or one per polygon \\(2\\): `fill`")
})


test_that("polygon annotations expand the axis ranges and are drawn per group", {
      p <- ggplot(data.frame(x = 1:2, y = 1:2, z = 1:2), aes(x, y, z)) +
            geom_point_3d() +
            annotate_3d("polygon", x = c(0, 5, 5, 0, 1, 1), y = c(0, 0, 5, 3, 3, 4),
                        z = c(0, 0, 8, 1, 1, 1), group = rep(1:2, each = 3),
                        fill = c("#FF00FF", "#00FFFF"), alpha = 1)
      p <- p + coord_3d(light = "none")
      lim <- ggplot_build(p)$layout$panel_params[[1]]$scale_info
      expect_true(all(c(0, 5) >= min(lim$x$limits) & c(0, 5) <= max(lim$x$limits)))
      expect_true(8 <= max(lim$z$limits))
      polys <- find_grobs(drawn_grob(p), "polygon")
      fills <- toupper(substr(unlist(lapply(polys, function(g) g$gp$fill)), 1, 7))
      expect_setequal(intersect(fills, c("#FF00FF", "#00FFFF")), c("#FF00FF", "#00FFFF"))
})


# Integration -------------------------------------------------------------------

test_that("annotations expand scale ranges", {
      p <- ggplot(sphere_points, aes(x, y, z)) +
            geom_hull_3d(fill = "dodgerblue") +
            annotate_3d("point", x = 0, y = 0, z = 5, colour = "red") +
            coord_3d()
      z_range <- ggplot_build(p)$layout$panel_params[[1]]$scale_info$z$limits
      expect_true(z_range[2] >= 5)
})


test_that("annotations are depth-sorted with the plot's other layers", {
      g <- expand.grid(x = seq(-1, 1, length.out = 6), y = seq(-1, 1, length.out = 6))
      g$z <- 0
      p <- ggplot(g, aes(x, y, z)) +
            geom_surface_3d(fill = "grey50") +
            annotate_3d("point", x = 0.1, y = 0.1, z = c(-0.5, 0.5), size = 4) +
            zlim(-1, 1) + coord_3d(pitch = 0, roll = 0, yaw = 0)       # seen from above
      rows <- ggcube:::validation_scene(p)$panels[[1]]$rows
      pts <- which(rows$.layer == 2)
      surface <- which(rows$.layer == 1)
      expect_length(pts, 2)
      below <- pts[rows$.z3d[pts] < 0]
      above <- pts[rows$.z3d[pts] > 0]
      expect_lt(below, max(surface))
      expect_gt(above, max(surface))
})


test_that("annotations are drawn in every facet panel", {
      d <- data.frame(x = c(0, 1, 0, 1), y = c(0, 1, 0, 1), z = c(0, 1, 0, 1), f = c("a", "a", "b", "b"))
      p <- ggplot(d, aes(x, y, z)) +
            geom_point_3d() +
            annotate_3d("point", x = 0.5, y = 0.5, z = 0.5, colour = "red") +
            annotate_3d("plane", zintercept = 0.25) +
            facet_wrap(~f) + coord_3d()
      sc <- ggcube:::validation_scene(p)
      expect_length(sc$panels, 2)
      for (pn in sc$panels) expect_setequal(unique(pn$rows$.layer), 1:3)
})


# Labels on top -----------------------------------------------------------------

test_that("on_top layers are drawn after the rest of the scene, in depth order", {
      drawn <- function(on_top) {
            p <- ggplot(data.frame(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1)), aes(x, y, z)) +
                  geom_point_3d() +
                  annotate_3d("plane", zintercept = 0, alpha = 1) +
                  annotate_3d("text", x = c(0, 0.5), y = c(0, 0.5), z = c(-0.5, -0.8),
                              label = c("near", "far"), on_top = on_top) +
                  coord_3d()
            rows <- ggcube:::validation_scene(p)$panels[[1]]$rows
            rows[!duplicated(paste(rows$.layer, rows$group)), c(".layer", "label")]
      }
      # Labels under an opaque plane, seen from above, are drawn before it ...
      out <- drawn(FALSE)
      expect_lt(max(which(out$.layer == 3)), max(which(out$.layer == 2)))

      # ... unless they're on top, in which case they come last, far before near
      out <- drawn(TRUE)
      expect_gt(min(which(out$.layer == 3)), max(which(out$.layer != 3)))
      expect_equal(out$label[out$.layer == 3], c("far", "near"))
})


test_that("on_top applies to any ggcube layer and is validated", {
      d <- data.frame(x = c(0, 1), y = c(0, 1), z = c(-1, -1))
      p <- ggplot(d, aes(x, y, z)) +
            geom_point_3d(on_top = TRUE, size = 4) +
            geom_polygon_3d(aes(x, y, z), inherit.aes = FALSE,
                            data = data.frame(x = c(-1, 2, 2, -1), y = c(-1, -1, 2, 2), z = 0)) +
            coord_3d()
      rows <- ggcube:::validation_scene(p)$panels[[1]]$rows
      expect_equal(unique(rows$.layer), c(2, 1))
      expect_error(geom_point_3d(on_top = NA), "`on_top` must be")
      expect_error(geom_text_3d(on_top = "yes"), "`on_top` must be")
})


# Deprecated in-layer annotations ---------------------------------------------------

test_that("a layer's annotate argument is deprecated, and becomes annotation layers", {
      ann <- list(annotate_3d("point", x = 0, y = 0, z = 0),
                  list(annotate_3d("text", x = 0, y = 0, z = 0, label = "a")))
      lifecycle::expect_deprecated(out <- geom_point_3d(annotate = ann))
      expect_type(out, "list")
      expect_length(out, 3)
      expect_true(inherits(out[[1]]$geom, "GeomPoint3D"))
      expect_identical(out[[2]], ann[[1]])
      expect_identical(out[[3]], ann[[2]][[1]])

      expect_silent(l <- geom_point_3d())
      expect_s3_class(l, "LayerInstance")
      expect_error(suppressWarnings(geom_point_3d(annotate = "plane")), "built by `annotate_3d\\(\\)`")
})
