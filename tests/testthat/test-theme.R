test_that("element_rect() returns a valid ggplot2 element", {
      el <- element_rect(fill = "blue")
      expect_s3_class(el, "element_rect")
      expect_s3_class(el, "element")
      expect_no_error(ggplot2::element_grob(el))
})

test_that("element_rect() attaches alpha as an attribute", {
      expect_equal(attr(element_rect(alpha = 0.5), "ggcube_alpha"), 0.5)
      expect_equal(attr(element_rect(fill = "blue", alpha = 0.3), "ggcube_alpha"), 0.3)
      # No alpha specified: attribute defaults to NA so the renderer can
      # detect "use the package default" via the same code path that handles
      # inherited elements stripped of their attribute by merge_element().
      expect_true(is.na(attr(element_rect(fill = "blue"), "ggcube_alpha")))
})

test_that("element_rect() honours the color/colour alias", {
      expect_equal(element_rect(color = "blue")[["colour"]], "blue")
})
