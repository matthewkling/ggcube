# Tests for geom_plane_3d(), geom_refline_3d(), and geom_axes_3d(), which are
# built from the panel's limits when drawn.

base_plot <- function() {
      ggplot(data.frame(x = c(-2, 3), y = c(-1, 4), z = c(0, 5)), aes(x, y, z)) +
            geom_point_3d() + coord_3d()
}

# Panel limits of a plot, and the data of its layer `i` as drawn
built <- function(p, i = 2) {
      b <- ggplot_build(p)
      pp <- b$layout$panel_params[[1]]
      list(data = b$data[[i]], limits = ggcube:::panel_limits(pp),
           ranges = lapply(pp$scale_info[c("x", "y", "z")], function(s) range(s$limits)),
           params = b$plot$layers[[i]]$computed_geom_params)
}

# ggcube layer grobs of a plot, before drawing, including those merged into scenes
layer_grobs <- function(p) {
      g <- ggplotGrob(p)
      scenes <- find_grobs(g, "ggcube_scene")
      c(find_grobs(g, "ggcube_layer"), unlist(lapply(scenes, `[[`, "layers"), recursive = FALSE))
}

within_limits <- function(rows, ranges) {
      all(vapply(c("x", "y", "z"), function(a) {
            all(rows[[a]] >= ranges[[a]][1] - 1e-9 & rows[[a]] <= ranges[[a]][2] + 1e-9)
      }, logical(1)))
}


# Planes ------------------------------------------------------------------------

test_that("plane forms convert to general coefficients", {
      coefs <- function(...) unname(as.matrix(plane_coefficients(list(...))))

      expect_equal(coefs(xintercept = 3), matrix(c(1, 0, 0, 3), 1))
      expect_equal(coefs(yintercept = 2), matrix(c(0, 1, 0, 2), 1))
      expect_equal(coefs(zintercept = c(0, 5)), rbind(c(0, 0, 1, 0), c(0, 0, 1, 5)))

      # z = 1 + 2x - y  <=>  -2x + y + z = 1
      expect_equal(coefs(intercept = 1, slope_x = 2, slope_y = -1), matrix(c(-2, 1, 1, 1), 1))
      expect_equal(coefs(slope_y = 3), matrix(c(0, -3, 1, 0), 1))
      expect_equal(coefs(intercept = c(a = 1, b = 2)), rbind(c(0, 0, 1, 1), c(0, 0, 1, 2)))

      expect_equal(coefs(a = 1, b = 2, c = 3), matrix(c(1, 2, 3, 0), 1))
      expect_equal(nrow(coefs(a = 1:3, b = 0, c = 1, d = c(0, 1, 2))), 3)
})


test_that("plane coefficients are validated", {
      expect_error(geom_plane_3d(), "Planes need coefficients")
      expect_error(geom_plane_3d(fill = "red"), "Planes need coefficients")
      expect_error(geom_plane_3d(yintercept = 1, a = 1, b = 0, c = 0), "can't mix")
      expect_error(geom_plane_3d(zintercept = 1, slope_x = 1), "can't mix")
      expect_error(geom_plane_3d(xintercept = 1, yintercept = 1), "only one of")
      expect_error(geom_plane_3d(yintercept = NA), "`yintercept` must contain only finite")
      expect_error(geom_plane_3d(slope_x = "a"), "`slope_x` must contain only finite")
      expect_error(geom_plane_3d(d = 1), "need `a`, `b`, `c`")
      expect_error(geom_plane_3d(a = 1, b = 2), "need `c`")
      expect_error(geom_plane_3d(a = 0, b = 0, c = 0), "can't all be zero")
      expect_error(geom_plane_3d(a = 1:3, b = 1:2, c = 1), "length 1 or a common length")
      expect_error(geom_plane_3d(aes(a = a), data = data.frame(a = 1), zintercept = 1), "not both")
})


test_that("planes given as parameters are vectorized, with per-plane styling", {
      l <- geom_plane_3d(a = c(1, 0, 0), b = c(0, 1, 0), c = c(0, 0, 1), d = 1:3,
                         fill = c("red", "green", "blue"))
      expect_equal(l$data, data.frame(a = c(1, 0, 0), b = c(0, 1, 0), c = c(0, 0, 1), d = 1:3))
      expect_equal(l$aes_params$fill, c("red", "green", "blue"))
      expect_false(l$inherit.aes)
      expect_false(l$show.legend)
})


test_that("format_plane_equation writes readable equations", {
      expect_equal(format_plane_equation(c(0, 1, 0, 2)), "y = 2")
      expect_equal(format_plane_equation(c(-2, 1, 1, 1)), "-2x + y + z = 1")
      expect_equal(format_plane_equation(c(1, -0.5, 0, 0)), "x - 0.5y = 0")
})


test_that("clip_plane_to_box returns the plane's convex cross-section of the box", {
      lim <- list(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1))
      check <- function(normal, offset, n_expected) {
            v <- clip_plane_to_box(normal, offset, lim)
            expect_equal(nrow(v), n_expected)
            expect_equal(as.vector(v %*% normal), rep(offset, nrow(v)), tolerance = 1e-12)
            for (k in 1:3) expect_true(all(v[, k] >= -1 - 1e-12 & v[, k] <= 1 + 1e-12))
            # Vertices are in convex order: consecutive cross products all point the same way
            nxt <- c(2:nrow(v), 1)
            nxt2 <- c(3:nrow(v), 1:2)
            turns <- t(vapply(seq_len(nrow(v)), function(i) {
                  cross3(v[nxt[i], ] - v[i, ], v[nxt2[i], ] - v[nxt[i], ])
            }, numeric(3)))
            expect_true(all(turns %*% normal > 0) || all(turns %*% normal < 0))
      }
      check(c(0, 0, 1), 0.3, 4)          # axis-aligned: a square
      check(c(1, 1, 1), 2.5, 3)          # cuts off a corner: a triangle
      check(c(1, 1, 1), 0, 6)            # through the centre: a hexagon
      check(c(1, 2, 0), 0.5, 4)          # vertical plane

      expect_null(clip_plane_to_box(c(0, 0, 1), 5, lim))
      expect_equal(nrow(clip_plane_to_box(c(1, 1, 1), 3, lim)), NULL)  # touches one corner only
})


test_that("planes are clipped to the panel and don't affect the axis ranges", {
      b <- built(base_plot() + geom_plane_3d(a = 1, b = -1, c = 0.5, d = 1))
      rows <- plane_rows(b$data, b$limits)
      expect_true(all(rows$.prim == "polygon"))
      expect_gte(nrow(rows), 3)
      expect_equal(rows$x - rows$y + 0.5 * rows$z, rep(1, nrow(rows)), tolerance = 1e-12)
      expect_true(within_limits(rows, b$ranges))

      base <- built(base_plot(), 1)
      for (a in c("x", "y", "z")) expect_equal(b$ranges[[a]], base$ranges[[a]])

      far <- built(base_plot() + geom_plane_3d(zintercept = 100))
      expect_warning(plane_rows(far$data, far$limits), "The plane z = 100 doesn't cross the panel")
})


test_that("intercept and regression planes are drawn where they belong", {
      b <- built(base_plot() + geom_plane_3d(yintercept = 2))
      rows <- plane_rows(b$data, b$limits)
      expect_equal(rows$y, rep(2, nrow(rows)))
      expect_equal(sort(unique(round(rows$x, 9))), round(b$ranges$x, 9))
      expect_equal(sort(unique(round(rows$z, 9))), round(b$ranges$z, 9))

      b <- built(base_plot() + geom_plane_3d(intercept = 2, slope_x = 0.5, slope_y = -0.25))
      rows <- plane_rows(b$data, b$limits)
      expect_gte(nrow(rows), 3)
      expect_equal(rows$z, 2 + 0.5 * rows$x - 0.25 * rows$y, tolerance = 1e-12)
})


test_that("planes can be mapped from data, with scales for their styling", {
      planes <- data.frame(slope_x = c(-0.5, 0, 0.5, NA), model = c("a", "b", "c", "d"))
      p <- base_plot() + geom_plane_3d(aes(slope_x = slope_x, fill = model), data = planes)
      expect_warning(b <- built(p), "Removed 1 row containing non-finite plane coefficients")
      expect_equal(b$data$a, c(0.5, 0, -0.5))
      expect_equal(b$data$c, c(1, 1, 1))
      expect_equal(length(unique(b$data$fill)), 3)
      expect_false(any(c("intercept", "slope_x", "slope_y") %in% names(b$data)))

      general <- data.frame(a = 1, b = 1, c = 1, d = 2)
      b <- built(base_plot() + geom_plane_3d(aes(a = a, b = b, c = c, d = d), data = general))
      expect_equal(unlist(b$data[c("a", "b", "c", "d")]), c(a = 1, b = 1, c = 1, d = 2))

      expect_error(built(base_plot() + geom_plane_3d(aes(fill = model), data = planes)),
                   "Planes need coefficients")
})


test_that("planes are two-sided: lit from the side facing the viewer, never culled", {
      for (yaw in c(-30, 150)) {
            p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d() +
                  geom_plane_3d(a = 1, b = 1, c = 0.5, fill = "purple") +
                  coord_3d(yaw = yaw, persp = FALSE)
            plane <- Filter(function(g) ".two_sided" %in% names(g$rows),
                            layer_grobs(p))
            expect_length(plane, 1)
            rows <- plane[[1]]$rows
            expect_gt(nrow(rows), 2)
            expect_false(anyNA(rows$light), info = paste("yaw", yaw))

            # The default light is scene-anchored, so normals stay in scene space
            normal <- unlist(rows[1, c("normal_x", "normal_y", "normal_z")])
            toward_viewer <- eye_in_cube_space(ggplot_build(p)$layout$panel_params[[1]]$proj)$v
            expect_gt(sum(normal * toward_viewer), 0)
      }
})


test_that("intersecting planes are split along their intersection", {
      p <- base_plot() + geom_plane_3d(a = c(1, 0), b = c(0, 1), c = c(1, 1), d = c(1, 3))
      rows <- ggcube:::validation_scene(p)$panels[[1]]$rows
      expect_gt(length(unique(rows$group[rows$.layer == 2])), 2)
})


# Reference lines -----------------------------------------------------------------

test_that("reference lines validate their points and directions", {
      l <- geom_refline_3d(dz = 1)
      expect_equal(l$data, data.frame(dx = 0, dy = 0, dz = 1))
      expect_equal(l$geom_params[c("x", "y", "z")], list(x = 0, y = 0, z = 0))
      l <- geom_refline_3d(x = 1:3, dx = 1)
      expect_equal(nrow(l$data), 3)
      expect_equal(l$geom_params$x, 1:3)
      expect_equal(l$geom_params$y, c(0, 0, 0))
      expect_equal(geom_refline_3d(dx = c(a = 2))$data$dx, 2)

      expect_error(geom_refline_3d(x = 1), "need a direction")
      expect_error(geom_refline_3d(dx = 0, dy = 0), "can't all be zero")
      expect_error(geom_refline_3d(dx = NA), "`dx` must contain only finite")
      expect_error(geom_refline_3d(x = NA, dx = 1), "`x` must contain only finite")
      expect_error(geom_refline_3d(x = 1:2, dx = 1:3), "length 1 or a common length")
      expect_error(geom_refline_3d(aes(dx = u), data = data.frame(u = 1), x = 1:2),
                   "must each be a single number")
      expect_error(annotate_3d("refline", dx = 1, arrow = grid::arrow()), "only to segment and axes")
})


test_that("clip_line_to_box returns the line's chord through the box", {
      lim <- list(x = c(-1, 1), y = c(-1, 1), z = c(0, 2))

      # Axis-parallel
      expect_equal(clip_line_to_box(c(0.5, -0.5, 7), c(0, 0, 3), lim),
                   rbind(c(0.5, -0.5, 0), c(0.5, -0.5, 2)))

      # Oblique: endpoints lie on the line and on the box boundary
      p <- c(0.2, 0.1, 1); v <- c(1, 2, -0.5)
      e <- clip_line_to_box(p, v, lim)
      for (k in 1:2) {
            t <- sum((e[k, ] - p) * v) / sum(v^2)
            expect_equal(e[k, ], p + t * v, tolerance = 1e-12)
            on_face <- abs(e[k, ] - c(-1, -1, 0)) < 1e-12 | abs(e[k, ] - c(1, 1, 2)) < 1e-12
            expect_true(any(on_face))
            expect_true(all(e[k, ] >= c(-1, -1, 0) - 1e-12 & e[k, ] <= c(1, 1, 2) + 1e-12))
      }

      expect_null(clip_line_to_box(c(3, 0, 1), c(0, 1, 0), lim))   # parallel, outside
      expect_null(clip_line_to_box(c(5, 5, 5), c(1, 0, 0), lim))   # misses
      expect_null(clip_line_to_box(c(1, 1, 2), c(1, -1, 1), lim))  # touches one corner only
})


test_that("reference lines are clipped to the panel and don't affect the axis ranges", {
      b <- built(base_plot() + geom_refline_3d(x = c(0, 1), y = 1, z = 2, dx = 1, dy = 1, dz = 1,
                                               colour = c("red", "blue")))
      rows <- refline_rows(b$data, b$params[c("x", "y", "z")], b$limits)
      expect_equal(nrow(rows), 4)
      expect_equal(rows$.prim, rep("segment", 4))
      expect_equal(rows$point_type, rep(c("start", "end"), 2))
      expect_equal(rows$colour, rep(c("red", "blue"), each = 2))
      expect_equal(rows$y - rows$x, c(1, 1, 0, 0), tolerance = 1e-12)
      expect_equal(rows$z - rows$x, c(2, 2, 1, 1), tolerance = 1e-12)
      expect_true(within_limits(rows, b$ranges))

      base <- built(base_plot(), 1)
      for (a in c("x", "y", "z")) expect_equal(b$ranges[[a]], base$ranges[[a]])

      expect_warning(refline_rows(b$data[1, ], list(x = 0, y = 0, z = 100), b$limits),
                     "The line through \\(0, 0, 100\\) in direction \\(1, 1, 1\\) doesn't cross")
})


test_that("reference line directions can be mapped from data, through a shared point", {
      dirs <- data.frame(u = c(1, 0), w = c(0, 1), g = c("a", "b"))
      p <- base_plot() + geom_refline_3d(aes(dx = u, dz = w, colour = g), data = dirs, x = 1, y = 2, z = 3)
      b <- built(p)
      rows <- refline_rows(b$data, b$params[c("x", "y", "z")], b$limits)
      expect_equal(nrow(rows), 4)
      expect_equal(rows$y, rep(2, 4))                     # neither line moves in y
      expect_equal(rows$z[1:2], c(3, 3))                  # the first runs along x at z = 3
      expect_equal(rows$x[3:4], c(1, 1))                  # the second along z at x = 1
      expect_equal(length(unique(rows$colour)), 2)
})


# Axes ----------------------------------------------------------------------------

test_that("axes validate their arguments and style each axis", {
      l <- geom_axes_3d()
      expect_equal(l$geom_params[c("x", "y", "z")], list(x = 0, y = 0, z = 0))
      expect_equal(l$geom_params$labels, c("x", "y", "z"))
      expect_s3_class(l$geom_params$arrow, "arrow")

      l <- geom_axes_3d(x = 1, colour = c("red", "green", "blue"), linewidth = 2)
      expect_equal(l$aes_params$colour, c("red", "green", "blue"))
      expect_null(geom_axes_3d(arrow = NULL)$geom_params$arrow)
      expect_null(geom_axes_3d(labels = NULL)$geom_params$labels)

      expect_error(geom_axes_3d(x = 1:2), "single finite number")
      expect_error(geom_axes_3d(labels = c("a", "b")), "length 3")
      expect_error(geom_axes_3d(colour = c("red", "blue")), "length 1 or 3")
      expect_error(geom_axes_3d(arrow = "yes"), "grid::arrow")
})


test_that("axes span the panel's limits through the origin", {
      b <- built(base_plot() + geom_axes_3d(x = 1, y = 2, z = 3, colour = c("red", "green", "blue")))
      rows <- axes_rows(b$data, c(x = 1, y = 2, z = 3), c("x", "y", "z"), b$limits)
      origin <- c(x = 1, y = 2, z = 3)
      cols <- c(x = "red", y = "green", z = "blue")

      for (a in c("x", "y", "z")) {
            seg <- rows[rows$group == paste0("axis__", a), ]
            expect_equal(seg[[a]][seg$point_type == "start"], b$ranges[[a]][1])
            expect_equal(seg[[a]][seg$point_type == "end"], b$ranges[[a]][2])
            for (other in setdiff(c("x", "y", "z"), a)) {
                  expect_equal(seg[[other]], rep(origin[[other]], 2))
            }
            expect_equal(unique(seg$colour), cols[[a]])
            label <- rows[rows$group == paste0("axis__", a, "_label"), ]
            expect_equal(label$label, a)
            expect_equal(label$.prim, "text")
            expect_gt(label[[a]], b$ranges[[a]][2])
      }

      expect_false(any(axes_rows(b$data, origin, NULL, b$limits)$.prim == "text"))
      expect_warning(axes_rows(b$data, c(x = 0, y = 100, z = 0), NULL, b$limits),
                     "outside the panel range on y")

      base <- built(base_plot(), 1)
      for (a in c("x", "y", "z")) expect_equal(b$ranges[[a]], base$ranges[[a]])
})


test_that("axes are drawn with arrowheads in each facet panel", {
      d <- data.frame(x = c(-1, 1, -1, 1), y = c(-1, 1, -1, 1), z = c(-1, 1, -1, 1), f = c("a", "a", "b", "b"))
      p <- ggplot(d, aes(x, y, z)) + geom_point_3d() + geom_axes_3d() + facet_wrap(~f) + coord_3d()
      segs <- find_grobs(drawn_grob(p), "segments")
      with_arrow <- Filter(function(g) !is.null(g$arrow), segs)
      expect_equal(sum(lengths(lapply(with_arrow, function(g) as.numeric(g$x0)))), 6)
})


# Coincident elements ------------------------------------------------------------

test_that("lines and segments lying in a plane are drawn on top of it", {
      drawn_order <- function(ann, persp, yaw) {
            p <- ggplot(data.frame(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1)), aes(x, y, z)) +
                  geom_point_3d() +
                  ann +                                        # added before the plane
                  geom_plane_3d(zintercept = 0) +
                  coord_3d(persp = persp, yaw = yaw)
            rows <- ggcube:::validation_scene(p)$panels[[1]]$rows
            rows$.prim[!duplicated(paste(rows$.layer, rows$group))]
      }
      for (ann in list(annotate_3d("segment", x = -0.8, y = -0.5, z = 0, xend = 0.7, yend = 0.6, zend = 0),
                       geom_refline_3d(y = 0.2, dx = 1, dy = 0.5))) {
            for (persp in c(TRUE, FALSE)) {
                  for (yaw in c(-30, 60, 150, 240)) {
                        prim <- drawn_order(ann, persp, yaw)
                        expect_true(any(prim == "segment") && any(prim == "polygon"))
                        expect_gt(min(which(prim == "segment")), max(which(prim == "polygon")))
                  }
            }
      }

      g_plane <- list(pts = rbind(c(-1, -1, 0), c(2, -1, 0), c(2, 2, 0), c(-1, 2, 0)))
      g_seg <- list(pts = rbind(c(0, 0, 0), c(1, 1, 0)))
      g_off <- list(pts = rbind(c(0, 0, 0), c(1, 1, 0.1)))
      expect_equal(coplanar_order(g_plane, g_seg, "polygon", "segment"), 1)
      expect_equal(coplanar_order(g_seg, g_plane, "segment", "polygon"), -1)
      expect_true(is.na(coplanar_order(g_plane, g_off, "polygon", "segment")))
})
