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

test_that("label_width_proxy matches nchar for plain strings", {
      expect_identical(label_width_proxy("hello"), 5L)
      expect_identical(label_width_proxy("12.5"), 4L)
})

test_that("label_width_proxy returns a positive count for language objects", {
      w <- label_width_proxy(quote(alpha))
      expect_true(is.numeric(w))
      expect_gt(w, 0)
})

test_that("label_width_proxy ranks longer language labels above shorter ones", {
      expect_gt(label_width_proxy(quote(alphabeta)),
                label_width_proxy(quote(a)))
})

test_that("label_width_proxy handles expression vectors and NA", {
      expect_gt(label_width_proxy(expression(alpha)), 0)
      expect_identical(label_width_proxy(NA), 0L)
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
