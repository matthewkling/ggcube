test_that("element_rect() returns a valid ggplot2 element", {
      el <- element_rect(fill = "blue")
      expect_s3_class(el, "element_rect")
      expect_s3_class(el, "element")
      expect_no_error(ggplot2::element_grob(el))
})

test_that("element_rect() round-trips the alpha field", {
      expect_equal(element_rect(alpha = 0.5)$alpha, 0.5)
      expect_null(element_rect()$alpha)
})

test_that("element_rect() honours the color/colour alias", {
      expect_equal(element_rect(color = "blue")$colour, "blue")
})
