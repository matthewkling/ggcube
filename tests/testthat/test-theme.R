test_that("element_rect() returns a valid ggplot2 element", {
      el <- element_rect(fill = "blue")
      expect_s3_class(el, "element_rect")
      expect_s3_class(el, "element")
      expect_no_error(ggplot2::element_grob(el))
})

test_that("element_rect() blends alpha into the fill", {
      expect_equal(element_rect(fill = "blue", alpha = 0.5)$fill,
                   scales::alpha("blue", 0.5))
      expect_equal(element_rect(fill = "blue")$fill, "blue")
})

test_that("element_rect() honours the color/colour alias", {
      expect_equal(element_rect(color = "blue")$colour, "blue")
})
