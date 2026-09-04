test_that("as_grob_label passes character and numeric through as strings", {
      expect_identical(as_grob_label("foo"), "foo")
      expect_identical(as_grob_label(5), "5")
      expect_identical(as_grob_label(NA_character_), NA_character_)
})

test_that("as_grob_label preserves language objects", {
      lab <- as_grob_label(quote(alpha))
      expect_true(is.language(lab))
      expect_false(is.expression(lab))
      expect_identical(lab, quote(alpha))

      call_lab <- as_grob_label(quote(beta[1]))
      expect_true(is.language(call_lab))
      expect_false(is.expression(call_lab))
})

test_that("as_grob_label reduces an expression vector to its first element", {
      lab <- as_grob_label(expression(alpha, beta, gamma))
      # Should be the bare first element, not the whole vector
      expect_true(is.language(lab))
      expect_false(is.expression(lab))
      expect_identical(lab, quote(alpha))
})

test_that("as_grob_label handles a single-element expression", {
      lab <- as_grob_label(expression(alpha))
      expect_true(is.language(lab))
      expect_false(is.expression(lab))
      expect_identical(lab, quote(alpha))
})

test_that("as_grob_label returns empty string for an empty expression", {
      expect_identical(as_grob_label(expression()), "")
})

test_that("resolve_label_text falls back to break value as character", {
      # No custom labels supplied
      expect_identical(resolve_label_text(2, NULL, NULL), "2")

      # Break not present among axis_breaks: fall back to the value itself
      expect_identical(
            resolve_label_text(99, c("a", "b"), c(1, 2)),
            "99"
      )
})

test_that("resolve_label_text returns character labels for character vectors", {
      out <- resolve_label_text(2, c("Low", "Med", "High"), c(1, 2, 3))
      expect_identical(out, "Med")
})

test_that("resolve_label_text returns the matched language element for plotmath", {
      labels <- expression(alpha, beta, gamma)
      breaks <- c(1, 2, 3)

      out2 <- resolve_label_text(2, labels, breaks)
      expect_true(is.language(out2))
      expect_false(is.expression(out2))
      expect_identical(out2, quote(beta))

      out3 <- resolve_label_text(3, labels, breaks)
      expect_identical(out3, quote(gamma))
})

test_that("resolve_label_text handles a label vector mixing math and plain text", {
      labels <- expression(alpha, "beta", gamma^2)
      breaks <- c(1, 2, 3)

      # Math element stays language
      expect_true(is.language(resolve_label_text(1, labels, breaks)))
      # The plain-text element is a character constant inside the expression;
      # indexing yields a length-1 character string
      expect_identical(resolve_label_text(2, labels, breaks), "beta")
      # Superscript element stays language
      expect_true(is.language(resolve_label_text(3, labels, breaks)))
})

test_that("resolve_label_text does not error when break has no match", {
      labels <- expression(alpha, beta)
      breaks <- c(1, 2)
      # match() returns NA; must fall back without indexing error
      expect_identical(resolve_label_text(5, labels, breaks), "5")
})

test_that("create_text_grob builds a grob for plain, language, and expression labels", {
      rot <- list(hjust = 0.5, vjust = 0.5, angle = 0)
      theme_elements <- list(axis_text = list())

      g_plain <- create_text_grob("foo", 0.5, 0.5, rot, theme_elements)
      expect_s3_class(g_plain, "text")

      g_lang <- create_text_grob(quote(alpha), 0.5, 0.5, rot, theme_elements)
      expect_s3_class(g_lang, "text")

      g_expr <- create_text_grob(expression(beta[1]), 0.5, 0.5, rot, theme_elements)
      expect_s3_class(g_expr, "text")
})

test_that("create_text_grob carries a language label into the grob unchanged", {
      rot <- list(hjust = 0.5, vjust = 0.5, angle = 0)
      g <- create_text_grob(quote(beta[1]), 0.5, 0.5, rot, list(axis_title = list()),
                            is_title = TRUE)
      # The grob's label should remain a language object, not a string
      expect_true(is.language(g$label))
})

# Text box geometry and placement -------------------------------------------

test_that("text_box_corner_offsets returns the four corners about the anchor", {
      corners <- text_box_corner_offsets(width = 10, height = 4,
                                         angle_degrees = 0,
                                         hjust = 0.5, vjust = 0.5)

      expect_equal(nrow(corners), 4)
      expect_equal(ncol(corners), 2)
      expect_equal(sort(unique(corners[, 1])), c(-5, 5))
      expect_equal(sort(unique(corners[, 2])), c(-2, 2))
})

test_that("text_box_corner_offsets honours justification", {
      # hjust = 0 anchors the left edge, so the box extends only rightward
      corners <- text_box_corner_offsets(10, 4, 0, hjust = 0, vjust = 0.5)
      expect_equal(min(corners[, 1]), 0)
      expect_equal(max(corners[, 1]), 10)

      # hjust = 1 anchors the right edge
      corners <- text_box_corner_offsets(10, 4, 0, hjust = 1, vjust = 0.5)
      expect_equal(min(corners[, 1]), -10)
      expect_equal(max(corners[, 1]), 0)
})

test_that("text_box_corner_offsets rotates the box rather than its bounding box", {
      corners <- text_box_corner_offsets(10, 4, angle_degrees = 90,
                                         hjust = 0.5, vjust = 0.5)

      # A 90 degree rotation swaps the extents exactly; an axis-aligned
      # bounding box would not shrink the long dimension
      expect_equal(sort(unique(round(corners[, 1], 10))), c(-2, 2))
      expect_equal(sort(unique(round(corners[, 2], 10))), c(-5, 5))
})

test_that("text_box_corner_offsets falls back to sane values for bad input", {
      corners <- text_box_corner_offsets(10, 4, angle_degrees = NA,
                                         hjust = NULL, vjust = NA)
      expect_equal(nrow(corners), 4)
      expect_true(all(is.finite(corners)))
})

test_that("place_axis_text clears a centred box by margin plus half its extent", {
      # Offsetting straight along the edge normal, box 10 wide and 4 tall
      direction <- c(0, 1)
      normal <- c(0, 1)
      corners <- text_box_corner_offsets(10, 4, 0, hjust = 0.5, vjust = 0.5)

      # Nearest corner sits 2 below the anchor, so offset = margin + 2
      expect_equal(place_axis_text(direction, normal, margin = 5, corners), 7)
})

test_that("place_axis_text needs only the margin when the box extends outward", {
      # vjust = 0 anchors the bottom edge, so nothing lies below the anchor
      direction <- c(0, 1)
      normal <- c(0, 1)
      corners <- text_box_corner_offsets(10, 4, 0, hjust = 0.5, vjust = 0)

      expect_equal(place_axis_text(direction, normal, margin = 5, corners), 5)
})

test_that("place_axis_text divides by the direction-normal cosine", {
      # Direction 45 degrees off the normal: travel is margin / cos(45)
      direction <- c(sin(pi / 4), cos(pi / 4))
      normal <- c(0, 1)
      corners <- text_box_corner_offsets(10, 4, 0, hjust = 0.5, vjust = 0)

      expect_equal(place_axis_text(direction, normal, margin = 5, corners),
                   5 / cos(pi / 4))
})

test_that("place_axis_text caps the offset for near-parallel directions", {
      # Direction almost along the edge: without the clamp this diverges
      direction <- c(1, 1e-6)
      normal <- c(0, 1)
      corners <- text_box_corner_offsets(10, 4, 0, hjust = 0.5, vjust = 0)

      offset <- place_axis_text(direction, normal, margin = 5, corners,
                                min_cosine = 0.15)
      expect_equal(offset, 5 / 0.15)
      expect_true(is.finite(offset))
})

test_that("place_axis_text always returns at least the margin", {
      normal <- c(0, 1)
      corners <- text_box_corner_offsets(10, 4, 0, hjust = 0.5, vjust = 0)

      for (angle in seq(0, 80, by = 10)) {
            rad <- angle * pi / 180
            direction <- c(sin(rad), cos(rad))
            expect_gte(place_axis_text(direction, normal, margin = 5, corners), 5)
      }
})

test_that("text_box_reach measures the far edge of a placed box", {
      direction <- c(0, 1)
      normal <- c(0, 1)
      corners <- text_box_corner_offsets(10, 4, 0, hjust = 0.5, vjust = 0.5)

      offset <- place_axis_text(direction, normal, margin = 5, corners)
      # Box centre lands at 7, far edge is 2 beyond that
      expect_equal(text_box_reach(offset, direction, normal, corners), 9)
})

test_that("edge_normal is a unit vector agreeing with the offset direction", {
      n <- edge_normal(axis_angle = 0, direction = c(0, 1))
      expect_equal(sqrt(sum(n^2)), 1)
      expect_gt(sum(n * c(0, 1)), 0)

      # Flipping the offset direction flips the normal
      n_flipped <- edge_normal(axis_angle = 0, direction = c(0, -1))
      expect_equal(n_flipped, -n)
})

test_that("edge_normal is perpendicular to the edge for arbitrary angles", {
      for (angle in seq(0, 2 * pi, length.out = 9)) {
            u <- c(cos(angle), sin(angle))
            n <- edge_normal(angle, direction = c(-u[2], u[1]))
            expect_equal(sum(u * n), 0)
            expect_equal(sqrt(sum(n^2)), 1)
      }
})


# Device-space conversion ---------------------------------------------------

test_that("make_device_context round-trips through plot_to_pt and pt_to_npc", {
      ctx <- make_device_context(plot_bounds = c(-1, 1, -2, 2),
                                 panel_width_pt = 400,
                                 panel_height_pt = 800)

      pt <- plot_to_pt(0, 0, ctx)
      # Centre of the bounds sits at the centre of the panel
      expect_equal(pt$x, 200)
      expect_equal(pt$y, 400)

      npc <- pt_to_npc(pt$x, pt$y, ctx)
      expect_equal(npc$x, 0.5)
      expect_equal(npc$y, 0.5)
})

test_that("make_device_context guards degenerate plot bounds", {
      ctx <- make_device_context(c(0, 0, 0, 0), 400, 400)
      expect_true(is.finite(ctx$sx))
      expect_true(is.finite(ctx$sy))
})

test_that("pt_to_npc returns NULL for non-finite input", {
      ctx <- make_device_context(c(-1, 1, -1, 1), 400, 400)
      expect_null(pt_to_npc(NA_real_, 0, ctx))
      expect_null(pt_to_npc(0, Inf, ctx))
})


# Depth-scaled label sizing -------------------------------------------------

test_that("calculate_gridline_position picks the requested endpoint", {
      gridline <- data.frame(x = c(0, 10), y = c(0, 20),
                             depth_scale = c(0.8, 1.2))

      start <- calculate_gridline_position(gridline, axis_uses_start = TRUE)
      expect_equal(start$x, 0)
      expect_equal(start$y, 0)
      expect_equal(start$depth_scale, 0.8)

      end <- calculate_gridline_position(gridline, axis_uses_start = FALSE)
      expect_equal(end$x, 10)
      expect_equal(end$depth_scale, 1.2)
})

test_that("calculate_gridline_position defaults depth_scale when absent or invalid", {
      no_column <- data.frame(x = c(0, 1), y = c(0, 1))
      expect_equal(calculate_gridline_position(no_column, TRUE)$depth_scale, 1)

      bad <- data.frame(x = c(0, 1), y = c(0, 1), depth_scale = c(NA, -1))
      expect_equal(calculate_gridline_position(bad, TRUE)$depth_scale, 1)
      expect_equal(calculate_gridline_position(bad, FALSE)$depth_scale, 1)
})

test_that("resolve_fontsize falls back for missing or invalid sizes", {
      expect_equal(resolve_fontsize(NULL, 8.5), 8.5)
      expect_equal(resolve_fontsize(0, 8.5), 8.5)
      expect_equal(resolve_fontsize(-2, 8.5), 8.5)
      expect_equal(resolve_fontsize(c(1, 2), 8.5), 8.5)
      expect_equal(resolve_fontsize(12, 8.5), 12)
})


# Margin resolution ---------------------------------------------------------

test_that("calculate_axis_offsets adds tick length to the text margin", {
      elements <- list(
            axis_text = list(margin = margin(2, 2, 2, 2)),
            axis_title = list(margin = margin(4, 4, 4, 4)),
            parent_text = list(margin = margin(0, 0, 0, 0)),
            tick_length = 3
      )

      offsets <- calculate_axis_offsets(elements, rotate_labels = TRUE)
      expect_equal(offsets$text_offset, 5)
      expect_equal(offsets$tick_extent, 3)
})

test_that("calculate_axis_offsets ignores negative tick lengths for clearance", {
      elements <- list(
            axis_text = list(margin = margin(2, 2, 2, 2)),
            axis_title = list(margin = margin(4, 4, 4, 4)),
            parent_text = list(margin = margin(0, 0, 0, 0)),
            tick_length = -3
      )

      offsets <- calculate_axis_offsets(elements, rotate_labels = TRUE)
      expect_equal(offsets$text_offset, 2)
      expect_equal(offsets$tick_extent, 0)
})
