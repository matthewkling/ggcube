# test-annotate-3d.R

# Constructor ------------------------------------------------------------------

test_that("annotate_3d creates valid specs", {
      pt <- annotate_3d("point", x = 1, y = 2, z = 3)
      expect_s3_class(pt, "annotate_3d")
      expect_equal(pt$type, "point")
      expect_equal(nrow(pt$data), 1)

      txt <- annotate_3d("text", x = 0, y = 0, z = 0, label = "hi")
      expect_equal(txt$type, "text")
      expect_equal(txt$data$label, "hi")

      seg <- annotate_3d("segment", x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1)
      expect_equal(seg$type, "segment")
      expect_equal(seg$data$xend, 1)
})

test_that("annotate_3d validates required params", {
      expect_error(annotate_3d("point", x = 1, y = 2), "requires")
      expect_error(annotate_3d("text", x = 0, y = 0, z = 0), "requires.*label")
      expect_error(annotate_3d("segment", x = 0, y = 0, z = 0), "requires")
})

test_that("annotate_3d rejects invalid types", {
      expect_error(annotate_3d("sphere", z = 0), "must be one of")
})

test_that("annotate_3d normalizes color to colour", {
      pt <- annotate_3d("point", x = 0, y = 0, z = 0, color = "red")
      expect_equal(pt$data$colour, "red")
      expect_false("color" %in% names(pt$data))
})

test_that("annotate_3d supports vectorized params", {
      pt <- annotate_3d("point", x = 1:3, y = 0, z = 0)
      expect_equal(nrow(pt$data), 3)
      expect_equal(pt$data$x, 1:3)
      expect_equal(pt$data$y, c(0, 0, 0))

      pt2 <- annotate_3d("point", x = 1:3, y = 0, z = 0, colour = c("red", "green", "blue"))
      expect_equal(nrow(pt2$data), 3)
      expect_equal(pt2$data$colour, c("red", "green", "blue"))
})

test_that("annotate_3d errors on incompatible lengths", {
      expect_error(annotate_3d("point", x = 1:3, y = 1:2, z = 0), "incompatible")
})


# Build annotations ------------------------------------------------------------

test_that("build_annotations handles NULL", {
      expect_null(build_annotations(NULL))
})

test_that("build_annotations handles single spec", {
      pt <- annotate_3d("point", x = 1, y = 2, z = 3)
      rows <- build_annotations(pt)
      expect_equal(nrow(rows), 1)
      expect_equal(rows$.prim, "point")
      expect_true(rows$.ann)
})

test_that("build_annotations handles list of mixed types", {
      specs <- list(
            annotate_3d("point", x = 0, y = 0, z = 0),
            annotate_3d("segment", x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1),
            annotate_3d("text", x = 0, y = 0, z = 0, label = "hi")
      )
      rows <- build_annotations(specs)
      expect_equal(nrow(rows), 4) # 1 point + 2 segment rows + 1 text
      expect_equal(sum(rows$.prim == "point"), 1)
      expect_equal(sum(rows$.prim == "segment"), 2)
      expect_equal(sum(rows$.prim == "text"), 1)
      expect_true(all(rows$.ann))
})

test_that("build_annotations assigns unique groups", {
      specs <- list(
            annotate_3d("point", x = 1:3, y = 0, z = 0),
            annotate_3d("point", x = 4, y = 0, z = 0)
      )
      rows <- build_annotations(specs)
      expect_equal(nrow(rows), 4)
      expect_equal(length(unique(rows$group)), 4)
})


# Apply annotation styles ------------------------------------------------------

test_that("apply_annotation_styles copies .ann_ columns to standard columns", {
      data <- data.frame(
            x = c(0, 1), y = c(0, 1),
            colour = c("blue", "blue"),
            .ann = c(FALSE, TRUE),
            .ann_colour = c(NA, "red"),
            stringsAsFactors = FALSE
      )
      result <- apply_annotation_styles(data)
      expect_equal(result$colour, c("blue", "red"))
})

test_that("apply_annotation_styles is a no-op without .ann column", {
      data <- data.frame(x = 1, y = 1, colour = "blue", stringsAsFactors = FALSE)
      result <- apply_annotation_styles(data)
      expect_equal(result$colour, "blue")
})


# Integration: rendering without error -----------------------------------------

test_that("annotations render on polygon-based geoms", {
      ann <- annotate_3d("point", x = 0, y = 0, z = 0, colour = "red")

      expect_no_error({
            ggplot(sphere_points, aes(x, y, z)) +
                  geom_hull_3d(fill = "dodgerblue", annotate = ann) +
                  coord_3d()
      })

      expect_no_error({
            ggplot(sphere_points, aes(x, y, z)) +
                  geom_polygon_3d(annotate = ann) +
                  coord_3d()
      })
})

test_that("annotations render on geom_point_3d", {
      d <- expand.grid(x = 1:3, y = 1:3, z = 1:3)
      ann <- annotate_3d("text", x = 2, y = 2, z = 2, label = "center")

      expect_no_error({
            ggplot(d, aes(x, y, z)) +
                  geom_point_3d(annotate = ann) +
                  coord_3d()
      })
})

test_that("annotations render on geom_segment_3d", {
      d <- data.frame(x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1)
      ann <- annotate_3d("point", x = 0.5, y = 0.5, z = 0.5, colour = "red", size = 5)

      expect_no_error({
            ggplot(d, aes(x, y, z, xend = xend, yend = yend, zend = zend)) +
                  geom_segment_3d(annotate = ann) +
                  coord_3d()
      })
})

test_that("mixed annotation types render together", {
      ann <- list(
            annotate_3d("point", x = 0, y = 0, z = 1, colour = "red", size = 3),
            annotate_3d("text", x = 0, y = 0, z = 1.2, label = "top"),
            annotate_3d("segment", x = 0, y = 0, z = -1,
                        xend = 0, yend = 0, zend = 1, colour = "grey50")
      )

      expect_no_error({
            ggplot(sphere_points, aes(x, y, z)) +
                  geom_hull_3d(fill = "dodgerblue", annotate = ann) +
                  coord_3d()
      })
})

test_that("vectorized annotations render", {
      ann <- annotate_3d("point", x = c(-1, 0, 1), y = 0, z = 0,
                         colour = c("red", "white", "blue"), size = 4)

      expect_no_error({
            ggplot(sphere_points, aes(x, y, z)) +
                  geom_hull_3d(fill = "dodgerblue", annotate = ann) +
                  coord_3d()
      })
})

test_that("annotations expand scale ranges", {
      # Annotation at z = 5 should expand z scale beyond sphere_points range (~-1 to 1)
      ann <- annotate_3d("point", x = 0, y = 0, z = 5, colour = "red")
      p <- ggplot(sphere_points, aes(x, y, z)) +
            geom_hull_3d(fill = "dodgerblue", annotate = ann) +
            coord_3d()
      built <- ggplot_build(p)
      z_range <- built$layout$panel_params[[1]]$scale_info$z$limits
      expect_true(z_range[2] >= 5)
})


# Segment arrows ----------------------------------------------------------------

test_that("segment annotations accept an arrow, and only segments do", {
      a <- grid::arrow(length = grid::unit(6, "pt"), type = "closed")
      spec <- annotate_3d("segment", x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1, arrow = a)
      expect_identical(spec$arrow, a)
      expect_false("arrow" %in% names(spec$data))

      expect_error(annotate_3d("point", x = 0, y = 0, z = 0, arrow = a), "only to segment")
      expect_error(annotate_3d("segment", x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1,
                               arrow = "yes"), "grid::arrow")
})


test_that("annotation arrows are rebuilt at render time and filled with the segment colour", {
      a <- grid::arrow(angle = 25, length = grid::unit(6, "pt"), ends = "both", type = "closed")
      rows <- build_annotations(annotate_3d("segment", x = 0, y = 0, z = 0, xend = 1, yend = 1,
                                            zend = 1, arrow = a))
      rows$x <- c(0, 1); rows$y <- c(0, 1)
      rows <- apply_annotation_styles(rows)

      g <- render_segments(rows)
      expect_equal(g$arrow$angle, 25)
      expect_equal(g$arrow$ends, 3L)
      expect_equal(g$arrow$type, 2L)
      expect_equal(as.numeric(g$arrow$length), 6)
      expect_equal(grid::unitType(g$arrow$length), "points")
      expect_equal(g$gp$fill, "black")

      # The layer's own arrow keeps grid's default fill
      plain <- rows[setdiff(names(rows), grep("^\\.arrow_", names(rows), value = TRUE))]
      g <- render_segments(plain, arrow = grid::arrow(type = "closed"))
      expect_null(g$gp$fill)
})


test_that("an annotation's own arrow overrides the layer's, and its heads stay at the ends when split", {
      g <- expand.grid(x = seq(-1, 1, length.out = 6), y = seq(-1, 1, length.out = 6))
      g$z <- 0
      ann <- annotate_3d("segment", x = 0.1, y = 0.1, z = -1, xend = 0.1, yend = 0.1, zend = 1,
                         colour = "red", arrow = grid::arrow(type = "closed"))
      p <- ggplot(g, aes(x, y, z)) +
            geom_surface_3d(grid = "right2", sort_method = "pairwise", annotate = ann) + coord_3d()

      b <- ggplot_build(p)
      pp <- b$layout$panel_params[[1]]
      ld <- b$data[[1]]
      ld$.sort_method <- "pairwise"
      ld$.prim[is.na(ld$.prim)] <- "polygon"                       # as draw_panel() does
      coords <- b$plot$coordinates$transform(ld, pp)
      segs <- coords[coords$.prim %in% "segment", ]
      expect_gt(length(unique(segs$group)), 1)                     # split by the surface

      grob <- render_segments(apply_annotation_styles(segs))
      kids <- if (inherits(grob, "gTree")) grob$children else list(grob)
      with_arrow <- Filter(function(k) !is.null(k$arrow), kids)
      expect_length(with_arrow, 1)
      expect_length(as.numeric(with_arrow[[1]]$x0), 1)
      tip <- segs[segs$point_type == "end" & segs$.seg_last %in% TRUE, ]
      expect_equal(as.numeric(with_arrow[[1]]$x1), tip$x)
})


# Axes --------------------------------------------------------------------------

test_that("axes specs validate their arguments and recycle styling per axis", {
      spec <- annotate_3d("axes")
      expect_equal(unname(spec$origin), c(0, 0, 0))
      expect_equal(spec$labels, c("x", "y", "z"))
      expect_s3_class(spec$arrow, "arrow")

      spec <- annotate_3d("axes", x = 1, colour = c("red", "green", "blue"), linewidth = 2)
      expect_equal(spec$data$colour, c("red", "green", "blue"))
      expect_equal(spec$data$linewidth, rep(2, 3))
      expect_null(annotate_3d("axes", arrow = NULL)$arrow)
      expect_null(annotate_3d("axes", labels = NULL)$labels)

      expect_error(annotate_3d("axes", x = 1:2), "single finite number")
      expect_error(annotate_3d("axes", labels = c("a", "b")), "length 3")
      expect_error(annotate_3d("axes", colour = c("red", "blue")), "length 1 or 3")
})


axes_plot <- function(ann, data = data.frame(x = c(-2, 3), y = c(-1, 4), z = c(0, 5))) {
      ggplot(data, aes(x, y, z)) + geom_point_3d(annotate = ann) + coord_3d()
}

axes_rows <- function(p) {
      b <- ggplot_build(p)
      pp <- b$layout$panel_params[[1]]
      ann <- b$plot$layers[[1]]$computed_geom_params$annotate
      rows <- prepare_annotations(b$data[[1]], pp, ann)
      list(rows = rows[rows$.ann %in% TRUE, ], pp = pp)
}


test_that("axes span the panel's limits through the origin", {
      res <- axes_rows(axes_plot(annotate_3d("axes", x = 1, y = 2, z = 3)))
      rows <- res$rows
      lim <- lapply(res$pp$scale_info[c("x", "y", "z")], function(s) range(s$limits))
      origin <- c(x = 1, y = 2, z = 3)

      for (a in c("x", "y", "z")) {
            seg <- rows[rows$group == paste0("ann__axes1_", a), ]
            expect_equal(seg[[a]][seg$point_type == "start"], lim[[a]][1])
            expect_equal(seg[[a]][seg$point_type == "end"], lim[[a]][2])
            for (other in setdiff(c("x", "y", "z"), a)) {
                  expect_equal(seg[[other]], rep(origin[[other]], 2))
            }
            label <- rows[rows$group == paste0("ann__axes1_", a, "_label"), ]
            expect_equal(label$label, a)
            expect_gt(label[[a]], lim[[a]][2])
      }
      expect_true(all(rows$.arrow_type[rows$.prim == "segment"] == 2L))

      no_labels <- axes_rows(axes_plot(annotate_3d("axes", labels = NULL)))$rows
      expect_false(any(no_labels$.prim == "text"))
})


test_that("axes do not affect the plot's axis ranges", {
      base <- ggplot_build(axes_plot(NULL))$layout$panel_params[[1]]$scale_info
      with_axes <- ggplot_build(axes_plot(annotate_3d("axes", x = 1, y = 2, z = 3)))$layout$panel_params[[1]]$scale_info
      for (a in c("x", "y", "z")) expect_equal(with_axes[[a]]$limits, base[[a]]$limits)
})


test_that("axes warn when the origin is outside the panel", {
      expect_warning(axes_rows(axes_plot(annotate_3d("axes", y = 100))), "outside the panel range on y")
})


# Planes ------------------------------------------------------------------------

test_that("plane specs validate their coefficients and default d to 0", {
      spec <- annotate_3d("plane", a = 1, b = 2, c = 3)
      expect_equal(spec$data$d, 0)
      expect_equal(nrow(annotate_3d("plane", a = 1:3, b = 0, c = 1, d = c(0, 1, 2))$data), 3)
      expect_error(annotate_3d("plane", a = 1, b = 2), "requires: c")
      expect_error(annotate_3d("plane", a = 0, b = 0, c = 0), "can't all be zero")
      expect_error(annotate_3d("plane", a = 1, b = NA, c = 1), "finite")
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


plane_plot <- function(ann, geom = geom_point_3d) {
      ggplot(data.frame(x = c(-2, 3), y = c(-1, 4), z = c(0, 5)), aes(x, y, z)) +
            geom(annotate = ann) + coord_3d()
}


test_that("planes are clipped to the panel and don't affect the axis ranges", {
      p <- plane_plot(annotate_3d("plane", a = 1, b = -1, c = 0.5, d = 1))
      b <- ggplot_build(p)
      pp <- b$layout$panel_params[[1]]
      lim <- lapply(pp$scale_info[c("x", "y", "z")], function(s) range(s$limits))
      rows <- prepare_annotations(b$data[[1]], pp, b$plot$layers[[1]]$computed_geom_params$annotate)
      rows <- rows[rows$.ann %in% TRUE, ]
      expect_true(all(rows$.prim == "polygon"))
      expect_equal(rows$x - rows$y + 0.5 * rows$z, rep(1, nrow(rows)), tolerance = 1e-12)
      for (a in c("x", "y", "z")) {
            expect_true(all(rows[[a]] >= lim[[a]][1] - 1e-9 & rows[[a]] <= lim[[a]][2] + 1e-9))
      }

      base <- ggplot_build(plane_plot(NULL))$layout$panel_params[[1]]$scale_info
      for (a in c("x", "y", "z")) expect_equal(pp$scale_info[[a]]$limits, base[[a]]$limits)

      expect_warning(prepare_annotations(b$data[[1]], pp, annotate_3d("plane", a = 0, b = 0, c = 1, d = 100)),
                     "doesn't cross the panel")
})


test_that("planes are never culled as back faces", {
      ann <- annotate_3d("plane", a = 1, b = 1, c = 0.5, fill = "purple")
      for (yaw in c(-30, 150)) {
            p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d(annotate = ann) + coord_3d(yaw = yaw)
            b <- ggplot_build(p)
            pp <- b$layout$panel_params[[1]]
            ld <- b$data[[1]]
            ld <- prepare_annotations(ld, pp, ann)
            ld$.prim[is.na(ld$.prim)] <- "polygon"
            ld$.sort_method <- "painter"
            out <- b$plot$coordinates$transform(ld, pp)
            expect_true(any(out$.ann %in% TRUE), info = paste("yaw", yaw))
      }
})


test_that("annotation planes are lit from the side facing the viewer", {
      ann <- annotate_3d("plane", a = 1, b = 1, c = 0.5, fill = "purple")
      for (yaw in c(-30, 150)) {
            p <- ggplot(sphere_points, aes(x, y, z)) + geom_hull_3d(annotate = ann) +
                  coord_3d(yaw = yaw, persp = FALSE)
            b <- ggplot_build(p)
            pp <- b$layout$panel_params[[1]]
            ld <- prepare_annotations(b$data[[1]], pp, ann)
            ld$.prim[is.na(ld$.prim)] <- "polygon"
            ld$.sort_method <- "painter"
            out <- b$plot$coordinates$transform(ld, pp)
            plane <- out[out$.ann %in% TRUE, ]
            expect_false(anyNA(plane$light), info = paste("yaw", yaw))

            # The default light is scene-anchored, so normals stay in scene space
            normal <- unlist(plane[1, c("normal_x", "normal_y", "normal_z")])
            toward_viewer <- eye_in_cube_space(pp$proj)$v
            expect_gt(sum(normal * toward_viewer), 0)
      }
})


test_that("intersecting plane annotations are split along their intersection", {
      ann <- annotate_3d("plane", a = c(1, 0), b = c(0, 1), c = c(1, 1), d = c(0, 0))
      p <- ggplot(data.frame(x = c(-1, 1, 1), y = c(-1, 1, -1), z = c(-1, 1, 0), g = 1),
                  aes(x, y, z, group = g)) +
            geom_polygon_3d(sort_method = "pairwise", annotate = ann) + coord_3d()
      b <- ggplot_build(p)
      pp <- b$layout$panel_params[[1]]
      ld <- prepare_annotations(b$data[[1]], pp, ann)
      ld$.prim[is.na(ld$.prim)] <- "polygon"
      ld$.sort_method <- "pairwise"
      out <- b$plot$coordinates$transform(ld, pp)
      # Group names are integer-coded by the coord, so count the pieces instead
      expect_gt(length(unique(out$group[out$.ann %in% TRUE])), 2)
})
