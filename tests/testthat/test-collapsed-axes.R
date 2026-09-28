test_that("round_range_outward uses break spacing for precision", {
      # hwy-like: breaks every 10, limits already whole numbers
      expect_equal(round_range_outward(c(12, 44), c(20, 30, 40)), c(12, 44))

      # narrow range away from zero: precision comes from the 0.002 spacing,
      # not from the magnitude of the values
      expect_equal(round_range_outward(c(0.5023, 0.5061), c(0.502, 0.504, 0.506)),
                   c(0.502, 0.507))
})

test_that("round_range_outward never reports a narrower range than the data", {
      limits <- c(-4.7, 12.3)
      rounded <- round_range_outward(limits, c(-5, 0, 5, 10))

      expect_lte(rounded[1], limits[1])
      expect_gte(rounded[2], limits[2])
})

test_that("round_range_outward leaves limits already on the grid alone", {
      # Guards the epsilon: without it, floating point can push an endpoint a
      # whole unit outward.
      expect_equal(round_range_outward(c(0.7, 0.9), c(0.7, 0.8, 0.9)), c(0.7, 0.9))
})

test_that("round_range_outward falls back when breaks give no spacing", {
      limits <- c(11.8, 44.3)

      expect_equal(round_range_outward(limits, 20), limits)
      expect_equal(round_range_outward(limits, numeric(0)), limits)
      expect_equal(round_range_outward(limits, c(20, 20)), limits)
})

test_that("chosen_justification distinguishes set values from inherited ones", {
      expect_null(chosen_justification(0.5, 0.5))
      expect_null(chosen_justification(NULL, 0.5))
      expect_null(chosen_justification("top", 0.5))
      expect_null(chosen_justification(NA_real_, 0.5))

      expect_equal(chosen_justification(2, 0.5), 2)
      expect_equal(chosen_justification(0.5, 1), 0.5)
})

test_that("element_is_blank detects blanked elements", {
      expect_true(element_is_blank(ggplot2::element_blank()))
      expect_true(element_is_blank(NULL))
      expect_false(element_is_blank(ggplot2::element_text()))
})

test_that("readable_angle_degrees flips upside-down text only", {
      expect_equal(readable_angle_degrees(45), 45)
      expect_equal(readable_angle_degrees(-90), -90)
      expect_equal(readable_angle_degrees(-135), 45)
      expect_equal(readable_angle_degrees(180), 0)
      expect_equal(readable_angle_degrees(NA_real_), 0)
})

test_that("axis_is_collapsed keys on projected length, not exact coincidence", {
      ctx <- make_device_context(c(0, 1, 0, 1), 400, 400)

      collapsed <- list(edge_p1_2d = list(x = 0.2, y = 0.2),
                        edge_p2_2d = list(x = 0.2, y = 0.2))
      # Half a point apart: shorter than the text it would carry.
      nearly <- list(edge_p1_2d = list(x = 0.2, y = 0.2),
                     edge_p2_2d = list(x = 0.2 + 0.5 / 400, y = 0.2))
      live <- list(edge_p1_2d = list(x = 0.2, y = 0.2),
                   edge_p2_2d = list(x = 0.8, y = 0.2))

      expect_true(axis_is_collapsed(collapsed, ctx))
      expect_true(axis_is_collapsed(nearly, ctx))
      expect_false(axis_is_collapsed(live, ctx))
      expect_true(axis_is_collapsed(list(), ctx))
})

test_that("collapsed_range_label brackets a continuous range", {
      info <- list(range_labels = c("12", "44"), labels = c("20", "30", "40"))

      expect_equal(collapsed_range_label(info, 500, 8.5, "", "plain"), "[12, 44]")
})

test_that("collapsed_range_label lists discrete levels within budget", {
      info <- list(labels = c("4", "f", "r"))

      expect_equal(collapsed_range_label(info, 500, 8.5, "", "plain"), "[4, f, r]")
})

test_that("collapsed_range_label counts levels that overrun the budget", {
      info <- list(labels = c("compact", "subcompact", "midsize", "minivan"))

      expect_equal(collapsed_range_label(info, 1, 8.5, "", "plain"), "[4 levels]")
})

test_that("collapsed_range_label returns NULL with nothing to report", {
      expect_null(collapsed_range_label(NULL, 500, 8.5, "", "plain"))
      expect_null(collapsed_range_label(list(), 500, 8.5, "", "plain"))
      expect_null(collapsed_range_label(list(labels = character(0)), 500, 8.5, "", "plain"))
})

test_that("get_scale_info rounds a transformed range in data space", {
      scale_obj <- ggplot2::scale_y_continuous(transform = "log10")
      scale_obj$train(log10(c(12, 44)))

      info <- get_scale_info(scale_obj, expand = TRUE, axis_name = "y")

      # Rounding in transformed space would give values like 50.11872 here.
      expect_equal(info$range_labels, c("12", "44"))
      expect_equal(info$data_limits, c(12, 44), tolerance = 1e-8)
})

test_that("get_scale_info leaves untransformed scales in data space", {
      scale_obj <- ggplot2::scale_x_continuous()
      scale_obj$train(c(1.6, 7))

      info <- get_scale_info(scale_obj, expand = TRUE, axis_name = "x")

      expect_equal(info$data_limits, c(1.6, 7))
      expect_length(info$range_labels, 2)
})

test_that("get_scale_info reports no range for a discrete scale", {
      scale_obj <- ggplot2::scale_x_discrete()
      scale_obj$train(c("4", "f", "r"))

      info <- get_scale_info(scale_obj, expand = TRUE, axis_name = "x")

      expect_null(info$range_labels)
      expect_equal(as.character(info$labels), c("4", "f", "r"))
})
