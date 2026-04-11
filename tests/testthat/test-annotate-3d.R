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
      expect_error(annotate_3d("plane", z = 0), "must be one of")
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
