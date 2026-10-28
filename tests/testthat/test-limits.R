# Scale limits: z limits applied before stats, polygons cut at limits, and
# polygons with missing vertices removed whole.

# Warnings from building a plot, with the plot's layer data
build_with_warnings <- function(p, i = 1) {
      warnings <- character(0)
      d <- withCallingHandlers(
            layer_data(p, i),
            warning = function(w) {
                  warnings <<- c(warnings, conditionMessage(w))
                  invokeRestart("muffleWarning")
            })
      list(data = d, warnings = warnings)
}

# Polygons drawn in a ggcube layer, as lists of vertex counts
drawn_polygon_sizes <- function(p) {
      g <- suppressWarnings(drawn_grob(p))
      polys <- find_grobs(find_grobs(g, "ggcube_layer")[[1]], "polygon")
      unlist(lapply(polys, function(pg) {
            if (is.null(pg$id)) length(pg$x) else as.vector(table(pg$id))
      }))
}

shoelace <- function(u, v) 0.5 * abs(sum(u * c(v[-1], v[1]) - c(u[-1], u[1]) * v))


# Limit planes ---------------------------------------------------------------------

test_that("clip_ring() clips a polygon exactly at each plane", {
      # Unit square in x-y, z rising with y from 0 to 2, cut at z = 1
      P <- cbind(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 0, 2, 2))
      out <- clip_ring(P, list(list(axis = "z", bound = 1, sign = 1)))
      expect_equal(nrow(out$P), 4)
      expect_equal(max(out$P[, "z"]), 1)
      expect_equal(sort(unique(out$P[, "y"])), c(0, 0.5))
      expect_equal(shoelace(out$P[, "x"], out$P[, "y"]), 0.5)

      # Weights reproduce the new vertices from the old ones
      expect_equal(out$W %*% P, out$P, ignore_attr = TRUE)
      expect_equal(rowSums(out$W), rep(1, 4))

      # A corner cut by two planes gives a pentagon
      out <- clip_ring(P, list(list(axis = "x", bound = 0.5, sign = 1),
                               list(axis = "y", bound = 0.5, sign = -1)))
      expect_equal(shoelace(out$P[, "x"], out$P[, "y"]), 0.25)
      expect_true(all(out$P[, "x"] <= 0.5 & out$P[, "y"] >= 0.5))

      # Nothing remains beyond a plane, and nothing is cut within one
      expect_null(clip_ring(P, list(list(axis = "z", bound = -1, sign = 1))))
      expect_equal(clip_ring(P, list(list(axis = "z", bound = 3, sign = 1)))$P, P)
})

test_that("clip_segment() shortens segments to the limits", {
      P <- cbind(x = c(0, 4), y = c(0, 0), z = c(0, 2))
      out <- clip_segment(P, list(list(axis = "x", bound = 1, sign = -1),
                                  list(axis = "z", bound = 1.5, sign = 1)))
      expect_equal(out$P[, "x"], c(1, 3))
      expect_equal(out$P[, "z"], c(0.5, 1.5))
      expect_null(clip_segment(P, list(list(axis = "x", bound = 5, sign = -1))))
})


# z limits before stats ------------------------------------------------------------

test_that("z limits remove rows before stats that summarize observations", {
      cut <- 5
      inside <- iris[iris$Petal.Length <= cut, ]
      hull <- function(d) ggplot(d, aes(Sepal.Length, Sepal.Width, Petal.Length)) +
            geom_hull_3d() + coord_3d()

      limited <- build_with_warnings(hull(iris) + zlim(NA, cut))
      expect_true(any(grepl("Removed \\d+ rows .*stat_hull3d", limited$warnings)))
      expect_equal(limited$data[c("x", "y", "z")],
                   layer_data(hull(inside) + zlim(NA, cut))[c("x", "y", "z")])
      expect_lte(max(limited$data$z), cut)

      # A smooth is fit to the data within the limits only
      smooth <- function(d) ggplot(d, aes(mpg, wt, qsec)) + geom_smooth_3d(method = "lm") +
            coord_3d() + zlim(NA, 19)
      a <- suppressWarnings(layer_data(smooth(mtcars)))
      b <- suppressWarnings(layer_data(smooth(mtcars[mtcars$qsec <= 19, ])))
      expect_equal(a$fitted, b$fitted)

      # Segments with an end beyond the limits are removed, as by x and y limits
      segs <- data.frame(x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = c(1, 3))
      d <- suppressWarnings(layer_data(
            ggplot(segs, aes(x, y, z, xend = xend, yend = yend, zend = zend)) +
                  geom_segment_3d() + zlim(NA, 2) + coord_3d()))
      expect_equal(nrow(d), 2)
      expect_true(all(d$z <= 2))
})

test_that("z limits before stats follow the scale's oob function", {
      p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, Petal.Length)) + geom_hull_3d() + coord_3d()
      free <- layer_data(p)

      kept <- build_with_warnings(p + scale_z_continuous(limits = c(NA, 5), oob = scales::oob_keep))
      expect_length(kept$warnings, 0)
      expect_equal(kept$data[c("x", "y", "z")], free[c("x", "y", "z")])

      squished <- layer_data(p + scale_z_continuous(limits = c(NA, 5), oob = scales::squish))
      expect_equal(max(squished$z), 5)
})

test_that("points beyond z limits are removed, whatever the stat", {
      p <- ggplot(mtcars, aes(mpg, wt, qsec)) + zlim(NA, 19) + coord_3d()
      n_inside <- sum(mtcars$qsec <= 19)

      d <- suppressWarnings(layer_data(p + geom_point_3d()))
      expect_equal(nrow(d), n_inside)

      # With a stat that keeps them, the geom removes them rather than failing
      # Points pushed beyond the limits after the stat are removed by the geom,
      # rather than drawn with missing sizes
      p <- p + geom_point_3d(aes(z = stage(qsec, after_stat = z + 2)))
      g <- NULL
      warnings <- capture_warnings(g <- drawn_grob(p))
      expect_true(any(grepl("Removed \\d+ rows .*geom_point3d", warnings)))
      expect_s3_class(g, "grob")
})

test_that("paths break at points beyond the limits rather than joining across them", {
      # Two segments, each between consecutive points, none jumping the gap
      check_breaks <- function(d, along) {
            expect_equal(length(unique(d$segment_id)), 2)
            span <- tapply(d[[along]], d$segment_id, function(v) abs(diff(v)))
            expect_true(all(span == 1))
      }

      path <- data.frame(x = 1:6, y = 0, z = c(1, 2, 3, 4, 2, 1))
      check_breaks(suppressWarnings(layer_data(ggplot(path, aes(x, y, z)) + geom_path_3d() +
                                                     zlim(NA, 2.5) + coord_3d())), "x")

      path <- data.frame(x = c(1, 2, 3, 4, 2, 1), y = 0, z = 1:6)
      check_breaks(suppressWarnings(layer_data(ggplot(path, aes(x, y, z)) + geom_path_3d() +
                                                     xlim(NA, 2.5) + coord_3d())), "z")
})


# Polygons cut at limits -------------------------------------------------------------

test_that("polygons are cut where they cross limits on any axis", {
      sq <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 0, 2, 2))
      d <- layer_data(ggplot(sq, aes(x, y, z)) + geom_polygon_3d() + zlim(NA, 1) + coord_3d())
      expect_false(anyNA(d[c("x", "y", "z")]))
      expect_equal(max(d$z), 1)
      expect_equal(sort(unique(d$y)), c(0, 0.5))

      # A computed surface cut by x and z limits
      p <- ggplot() + coord_3d() +
            geom_function_3d(fun = function(x, y) sin(x) * cos(y), xlim = c(-3, 3), ylim = c(-3, 3))
      d <- suppressWarnings(layer_data(p + xlim(-2, 2) + zlim(-0.5, NA)))
      expect_false(anyNA(d[c("x", "y", "z")]))
      expect_equal(range(d$x), c(-2, 2))
      expect_equal(min(d$z), -0.5)

      # Vertices cut by several planes stay within all of them, despite rounding
      d <- suppressWarnings(layer_data(p + xlim(-0.3, 0.7) + ylim(0.1, 0.9) + zlim(-0.3, 0.3)))
      expect_false(anyNA(d[c("x", "y", "z")]))

      # Every polygon drawn keeps at least three vertices
      expect_true(all(drawn_polygon_sizes(p + zlim(-0.5, 0.5)) >= 3))
})

test_that("surface data are cut at z limits, not trimmed", {
      p <- ggplot(mountain, aes(x, y, z)) + coord_3d()
      free <- layer_data(p + geom_surface_3d())
      lim <- 90
      cut <- build_with_warnings(p + geom_surface_3d() + zlim(NA, lim))

      # No rows removed before the stat, and none left with missing vertices
      expect_length(cut$warnings, 0)
      d <- cut$data
      expect_false(anyNA(d$z))
      expect_equal(max(d$z), lim)

      # Tiles entirely below the limit are unchanged
      below <- tapply(free$z, free$group, max) <= lim
      kept <- names(below)[below]
      expect_equal(d[d$group %in% kept, c("x", "y", "z")],
                   free[free$group %in% kept, c("x", "y", "z")], ignore_attr = TRUE)

      # Tiles crossing the limit are cut rather than removed
      crossing <- names(which(tapply(free$z, free$group, min) < lim & !below))
      expect_gt(length(crossing), 0)
      expect_true(all(crossing %in% d$group))

      # Contours and ridgelines too
      for (geom in list(geom_contour_3d(), geom_ridgeline_3d())) {
            d <- layer_data(p + geom + zlim(NA, lim))
            expect_false(anyNA(d$z))
            expect_lte(max(d$z), lim)
      }
})

test_that("varying numeric columns are interpolated at new vertices", {
      sq <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 0, 2, 2), w = c(0, 0, 4, 4))
      d <- layer_data(ggplot(sq, aes(x, y, z, linewidth = w)) + geom_polygon_3d() +
                            scale_linewidth_identity() + zlim(NA, 1) + coord_3d())
      expect_equal(d$linewidth[d$z == 1], c(2, 2))
})

test_that("columns and voxels crossing limits are removed whole", {
      g <- expand.grid(x = 1:6, y = 1:6)
      g$z <- with(g, x * y)

      # Columns at x = 4 reach x = 4.5, beyond the limit
      d <- suppressWarnings(layer_data(ggplot(g, aes(x, y, z)) + geom_col_3d() +
                                             xlim(NA, 4.2) + coord_3d()))
      expect_false(anyNA(d[c("x", "y", "z")]))
      expect_equal(max(d$x), 3.5)
      expect_equal(length(unique(d$col_id)), 18)

      # Columns rising beyond a z limit
      d <- suppressWarnings(layer_data(ggplot(g, aes(x, y, z)) + geom_col_3d() +
                                             zlim(NA, 20) + coord_3d()))
      expect_false(anyNA(d$z))
      expect_equal(length(unique(d$col_id)), sum(g$z <= 20))

      # Voxels centered within a limit but extending beyond it
      vox <- data.frame(x = 1:3, y = 1, z = c(1, 2, 3))
      d <- suppressWarnings(layer_data(ggplot(vox, aes(x, y, z)) + geom_voxel_3d() +
                                             zlim(NA, 2.2) + coord_3d()))
      expect_false(anyNA(d$z))
      expect_equal(length(unique(d$voxel_id)), 1)
})

test_that("text drawn as polygons is cut at limits", {
      d <- data.frame(x = 0, y = 0, z = 0, label = "Clipped")
      p <- ggplot(d, aes(x, y, z, label = label)) +
            geom_text_3d(method = "polygon", size = 20) + coord_3d()
      free <- layer_data(p)

      # A limit through the middle of the first glyph
      first <- free[free$.subgroup == free$.subgroup[1], ]
      lim <- mean(range(first$x))
      cut <- layer_data(p + xlim(NA, lim))
      expect_false(anyNA(cut[c("x", "y", "z")]))
      expect_equal(max(cut$x), lim)
      expect_equal(unique(cut$text_id), 1)
})

test_that("limits whose scale doesn't censor are left to the scale", {
      p <- ggplot(mountain, aes(x, y, z)) + geom_surface_3d() + coord_3d()
      free <- layer_data(p)
      d <- layer_data(p + scale_z_continuous(limits = c(NA, 90), oob = scales::squish))
      expect_equal(d[c("x", "y", "group")], free[c("x", "y", "group")])
      expect_equal(max(d$z), 90)
})


# Missing vertices ---------------------------------------------------------------------

test_that("polygons with missing vertices are removed whole", {
      d <- data.frame(x = c(0, 1, 1, 0, 2, 3, 3, 2), y = c(0, 0, 1, 1, 0, 0, 1, 1),
                      z = c(0, 0, 0, 0, 0, NA, 0, 0), group = rep(c("a", "b"), each = 4))
      p <- ggplot(d, aes(x, y, z, group = group)) + geom_polygon_3d() + coord_3d()
      expect_equal(unname(drawn_polygon_sizes(p)), 4)
      expect_warning(drawn_grob(p), "Removed 4 rows")

      out <- remove_partial_polygons(d, na.rm = TRUE)
      expect_equal(unique(out$group), "a")
})
