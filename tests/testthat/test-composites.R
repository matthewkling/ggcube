# Tests for layers that replace composite layer options: drop lines, flat
# markers, and the deprecated data points of smooths and reference elements
# of point layers.

scene_rows_of <- function(p, ...) ggcube:::validation_scene(p, ...)$panels[[1]]$rows


# Smooth data points ------------------------------------------------------------

smooth_data <- function() {
      set.seed(1)
      d <- data.frame(x = runif(30), y = runif(30))
      d$z <- d$x + d$y + rnorm(30, 0, 0.3)
      d
}

test_that("a smooth's points argument is deprecated, and becomes a point layer", {
      rlang::local_options(lifecycle_verbosity = "warning")
      warnings <- character(0)
      out <- withCallingHandlers(
            geom_smooth_3d(aes(x, y, z, fill = after_stat(se)), method = "lm",
                           points = TRUE, point_colour = "red", point_size = 3),
            warning = function(w) {
                  warnings <<- c(warnings, conditionMessage(w))
                  invokeRestart("muffleWarning")
            })
      expect_length(warnings, 3)
      expect_match(warnings[1], "`points` argument of `geom_smooth_3d\\(\\)` is deprecated")
      expect_match(warnings[2], "`point_colour` argument")
      expect_type(out, "list")
      expect_length(out, 2)
      pts <- out[[2]]
      expect_true(inherits(pts$geom, "GeomPoint3D"))
      expect_equal(names(pts$mapping), c("x", "y", "z"))        # not the smooth's fill
      expect_equal(pts$aes_params$colour, "red")
      expect_equal(pts$aes_params$size, 3)
      expect_equal(pts$aes_params$shape, 19)
      expect_false(pts$show.legend)

      # Styling without points warns and adds nothing
      expect_warning(out <- stat_smooth_3d(point_colour = "red"), "`point_colour` argument")
      expect_s3_class(out, "LayerInstance")
      expect_silent(geom_smooth_3d())
})

test_that("a smooth's deprecated points are drawn, depth-sorted with the surface", {
      d <- smooth_data()
      p <- ggplot(d, aes(x, y, z)) +
            suppressWarnings(geom_smooth_3d(method = "lm", points = TRUE, point_colour = "#FF0000")) +
            coord_3d()
      rows <- scene_rows_of(p)
      pts <- rows[rows$.layer == 2, ]
      expect_equal(nrow(pts), nrow(d))
      expect_true(all(pts$colour == "#FF0000" | substr(pts$colour, 1, 7) == "#FF0000"))
      # Interleaved with the surface's tiles, not all drawn after them
      expect_lt(min(which(rows$.layer == 2)), max(which(rows$.layer == 1)))
})


# Drop lines ----------------------------------------------------------------------

test_that("drop lines run from each point to its projection on each face", {
      d <- data.frame(x = c(1, 2, 3), y = c(4, 6, 5), z = c(7, 9, 8))
      p <- ggplot(d, aes(x, y, z)) +
            geom_dropline_3d(faces = c("zmin", "xmax")) +
            coord_3d(scales = "fixed")
      rows <- scene_rows_of(p)
      expect_equal(unique(rows$.prim), "segment")
      expect_equal(length(unique(rows$group)), 6)

      # Cube-space faces lie at +/- half the cube's extent along their axis
      start <- rows[rows$point_type == "start", ]
      end <- rows[rows$point_type == "end", ]
      end <- end[match(start$group, end$group), ]
      to_floor <- abs(end$.z3d - min(rows$.z3d)) < 1e-9
      to_side <- abs(end$.x3d - max(rows$.x3d)) < 1e-9
      expect_equal(sum(to_floor), 3)
      expect_equal(sum(to_side), 3)
      expect_true(all(xor(to_floor, to_side)))
      # Each line keeps the point's other coordinates
      expect_equal(end$.x3d[to_floor], start$.x3d[to_floor])
      expect_equal(end$.y3d[to_floor], start$.y3d[to_floor])
      expect_equal(end$.y3d[to_side], start$.y3d[to_side])
      expect_equal(end$.z3d[to_side], start$.z3d[to_side])
})

test_that("drop lines work with discrete axes and validate their faces", {
      p <- ggplot(mpg[1:20, ], aes(displ, hwy, drv)) +
            geom_dropline_3d(faces = "ymax") + geom_point_3d() + coord_3d()
      expect_equal(sum(scene_rows_of(p)$.layer == 1), 40)
      expect_error(geom_dropline_3d(faces = "floor"), "`faces` must contain only")
      expect_error(geom_dropline_3d(faces = "3D"), "`faces` must contain only")
})


# Flat markers -------------------------------------------------------------------

flat_plot <- function(shape = 19, size = 10, faces = "zmin", persp = FALSE, ...) {
      ggplot(data.frame(x = c(-0.5, 0.5), y = 0, z = 0.5), aes(x, y, z)) +
            geom_point_3d(position = position_on_face(faces), flat = TRUE, shape = shape,
                          size = size, scale_depth = FALSE, ...) +
            lims(x = c(-1, 1), y = c(-1, 1)) + zlim(0, 1) +
            coord_3d(direction = c(0, 0, 1), persp = persp)
}

test_that("flat markers need a face, and a shape with a flat outline", {
      expect_error(geom_point_3d(flat = TRUE), "needs a face")
      expect_error(geom_point_3d(flat = TRUE, position = position_on_face("3D")), "needs a face")
      expect_error(geom_point_3d(flat = NA), "`flat` must be")
      expect_no_error(geom_point_3d(flat = TRUE, position = position_on_face("zmin")))
      expect_error(ggplotGrob(flat_plot(shape = 3)), "can't be drawn with shape `3`")
})

test_that("flat markers lie in their face, at the size of the drawn symbol", {
      sc <- ggcube:::validation_scene(flat_plot(), 6, 6)
      rows <- sc$panels[[1]]$rows
      expect_equal(unique(rows$.prim), "polygon")
      expect_equal(as.vector(table(rows$group)), c(32, 32))
      expect_true(all(abs(rows$.z3d - min(rows$.z3d)) < 1e-12))       # on the floor

      # Seen square-on without perspective, each marker's drawn radius is that
      # of the plotting symbol: 0.375 times its font size
      for (g in unique(rows$group)) {
            r <- rows[rows$group == g, ]
            radius_pt <- diff(range(r$x)) / 2 * sc$panel_pt[1]
            expect_equal(radius_pt, 0.375 * 10 * .pt, tolerance = 1e-6)
            expect_equal(diff(range(r$y)) / 2 * sc$panel_pt[2], 0.375 * 10 * .pt, tolerance = 1e-3)
      }
})

test_that("flat markers follow each symbol's outline and styling", {
      outline <- function(shape) {
            rows <- scene_rows_of(flat_plot(shape = shape, size = 4, colour = "#FF0000", fill = "#0000FF"))
            r <- rows[rows$group == rows$group[1], ]
            list(n = nrow(r), fill = r$fill[1], colour = r$colour[1])
      }
      expect_equal(outline(0)$n, 4)
      expect_equal(outline(2)$n, 3)
      expect_equal(outline(5)$n, 4)
      expect_equal(outline(1)$n, 32)

      open <- outline(1)
      expect_true(is.na(open$fill))
      expect_equal(open$colour, "#FF0000")
      solid <- outline(17)
      expect_equal(solid$fill, "#FF0000")
      expect_true(is.na(solid$colour))
      filled <- outline(22)
      expect_equal(filled$fill, "#0000FF")
      expect_equal(filled$colour, "#FF0000")

      expect_equal(flat_shape_outline(15)[, 1], c(-1, 1, 1, -1) * 0.375)
      expect_equal(max(abs(flat_shape_outline(22))), 0.375 * sqrt(pi / 4))
})

test_that("points kept in 3D are drawn as usual alongside flat ones", {
      p <- ggplot(mtcars, aes(mpg, wt, qsec)) +
            geom_point_3d(position = position_on_face(c("3D", "zmin")), flat = TRUE) +
            coord_3d()
      rows <- scene_rows_of(p)
      expect_equal(sum(is.na(rows$project_to_face)), nrow(mtcars))
      expect_true(all(rows$.prim[is.na(rows$project_to_face)] == "point"))
      expect_true(all(rows$.prim[!is.na(rows$project_to_face)] == "polygon"))
      expect_equal(length(unique(rows$group[!is.na(rows$project_to_face)])), nrow(mtcars))
})

test_that("flat markers shrink with distance under perspective", {
      p <- ggplot(data.frame(x = 0, y = 0, z = 0.5), aes(x, y, z)) +
            geom_point_3d(position = position_on_face(c("zmin", "zmax")), flat = TRUE, size = 10) +
            lims(x = c(-1, 1), y = c(-1, 1)) + zlim(0, 1) +
            coord_3d(direction = c(0, 0, 1), persp = TRUE)
      rows <- scene_rows_of(p)
      top <- rows$.z3d > 0
      expect_gt(diff(range(rows$x[top])), diff(range(rows$x[!top])))   # the top face is nearer
})


test_that("flat marker outlines scale with perspective, like the markers, whatever scale_depth is", {
      for (persp in c(TRUE, FALSE)) {
            for (scale_depth in c(TRUE, FALSE)) {
                  p <- ggplot(data.frame(x = 0, y = 0, z = 0.5), aes(x, y, z)) +
                        geom_point_3d(position = position_on_face(c("zmin", "zmax")), flat = TRUE,
                                      shape = 21, size = 10, stroke = 2, scale_depth = scale_depth) +
                        lims(x = c(-1, 1), y = c(-1, 1)) + zlim(0, 1) +
                        coord_3d(direction = c(0.3, -0.4, 1), persp = persp)
                  rows <- scene_rows_of(p)
                  top <- rows$.z3d > 0
                  width <- c(diff(range(rows$x[top])), diff(range(rows$x[!top])))
                  stroke <- c(unique(rows$linewidth[top]), unique(rows$linewidth[!top]))
                  info <- paste("persp", persp, "scale_depth", scale_depth)
                  expect_length(stroke, 2)
                  expect_equal(stroke[1] / stroke[2], width[1] / width[2], tolerance = 0.02, info = info)
                  if (!persp) expect_equal(stroke, c(2, 2), info = info)
            }
      }
})


# Point reference elements ----------------------------------------------------------

test_that("point layers' reference element arguments are deprecated but still work", {
      rlang::local_options(lifecycle_verbosity = "warning")
      expect_warning(geom_point_3d(ref_lines = TRUE), "`ref_lines` argument of `geom_point_3d\\(\\)`")
      expect_warning(stat_point_3d(ref_points = "points"), "`ref_points` argument of `stat_point_3d\\(\\)`")
      expect_warning(geom_point_3d(ref_line_colour = "red"), "`ref_line_colour` argument")
      expect_silent(geom_point_3d())

      l <- suppressWarnings(geom_point_3d(ref_lines = TRUE, ref_faces = c("zmin", "xmax"),
                                          ref_point_colour = "blue"))
      expect_true(l$stat_params$ref_lines)
      expect_equal(l$stat_params$ref_faces, c("zmin", "xmax"))
      expect_equal(l$geom_params$ref_point_colour, "blue")

      p <- ggplot(data.frame(x = 1:3, y = 1:3, z = 1:3), aes(x, y, z)) +
            suppressWarnings(geom_point_3d(ref_lines = TRUE, ref_points = TRUE)) +
            coord_3d()
      rows <- scene_rows_of(p)
      expect_setequal(unique(rows$.prim), c("point", "segment", "polygon"))
})
