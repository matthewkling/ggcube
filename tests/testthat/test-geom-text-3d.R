test_that("text_outlines produces net-CCW winding per glyph", {
      # Helper: signed area via shoelace formula (positive = CCW)
      signed_area <- function(x, y) {
            sum(x * dplyr::lead(y, default = y[1]) -
                      dplyr::lead(x, default = x[1]) * y)
      }

      # Word with holes (letters like "a", "o" have interior contours)
      # Individual hole contours may be CW (negative area), but the net
      # signed area per glyph (outer + holes) should be positive (CCW dominant)
      outlines_holes <- text_outlines("ao", family = "sans", size = 24)
      net_areas_holes <- outlines_holes %>%
            dplyr::group_by(poly_id) %>%
            dplyr::summarise(area = signed_area(x, y), .groups = "drop") %>%
            dplyr::mutate(glyph = sub("_.*", "", poly_id)) %>%
            dplyr::group_by(glyph) %>%
            dplyr::summarise(net_area = sum(area), .groups = "drop")
      expect_true(all(net_areas_holes$net_area > 0),
                  info = "Net signed area per glyph in 'ao' should be positive (CCW dominant)")

      # Word without holes — all contours should be CCW
      outlines_no_holes <- text_outlines("xx", family = "sans", size = 24)
      areas_no_holes <- outlines_no_holes %>%
            dplyr::group_by(poly_id) %>%
            dplyr::summarise(area = signed_area(x, y), .groups = "drop")
      expect_true(all(areas_no_holes$area > 0),
                  info = "All contours in 'xx' should be CCW (positive signed area)")

      # Multi-word: net area per glyph should be positive
      outlines_mixed <- text_outlines("Hello World", family = "sans", size = 24)
      net_areas_mixed <- outlines_mixed %>%
            dplyr::group_by(poly_id) %>%
            dplyr::summarise(area = signed_area(x, y), .groups = "drop") %>%
            dplyr::mutate(glyph = sub("_.*", "", poly_id)) %>%
            dplyr::group_by(glyph) %>%
            dplyr::summarise(net_area = sum(area), .groups = "drop")
      expect_true(all(net_areas_mixed$net_area > 0),
                  info = "Net signed area per glyph in 'Hello World' should be positive")
})


test_that("text_outlines outer contours are CCW", {
      signed_area <- function(x, y) {
            sum(x * dplyr::lead(y, default = y[1]) -
                      dplyr::lead(x, default = x[1]) * y)
      }

      # For letters with holes, the outer contour should have the largest
      # absolute area, and it should be positive (CCW)
      outlines <- text_outlines("ao", family = "sans", size = 24)
      contour_areas <- outlines %>%
            dplyr::group_by(poly_id) %>%
            dplyr::summarise(area = signed_area(x, y), .groups = "drop") %>%
            dplyr::mutate(glyph = sub("_.*", "", poly_id))

      # Within each glyph, the contour with largest absolute area (the outer)
      # should be positive
      outer_areas <- contour_areas %>%
            dplyr::group_by(glyph) %>%
            dplyr::slice_max(abs(area), n = 1, with_ties = FALSE) %>%
            dplyr::ungroup()
      expect_true(all(outer_areas$area > 0),
                  info = "Outer contours should be CCW (positive signed area)")
})


test_that("transform_normals_to_standard preserves axis-aligned normals under uniform scaling", {
      normals <- matrix(c(0, 0, 1), nrow = 1)
      scale_ranges <- list(x = c(0, 10), y = c(0, 10), z = c(0, 10))

      result <- ggcube:::transform_normals_to_standard(
            normals, scale_ranges, scales = "free", ratio = c(1, 1, 1),
            anchor = "scene", proj = NULL
      )

      expect_equal(result[1, ], c(0, 0, 1), tolerance = 1e-10)
})


test_that("transform_normals_to_standard applies inverse-transpose under non-uniform scaling", {
      normals <- matrix(c(1, 0, 0), nrow = 1)
      scale_ranges <- list(x = c(0, 10), y = c(0, 20), z = c(0, 10))

      result <- ggcube:::transform_normals_to_standard(
            normals, scale_ranges, scales = "free", ratio = c(1, 1, 1),
            anchor = "scene", proj = NULL
      )

      # Axis-aligned normal should remain axis-aligned regardless of scaling
      expect_equal(result[1, ], c(1, 0, 0), tolerance = 1e-10)

      # Diagonal normal should rotate under non-uniform scaling
      normals_diag <- matrix(c(1, 1, 0) / sqrt(2), nrow = 1)
      result_diag <- ggcube:::transform_normals_to_standard(
            normals_diag, scale_ranges, scales = "free", ratio = c(1, 1, 1),
            anchor = "scene", proj = NULL
      )

      # x gets scaled by 10, y by 20, so inverse-transpose scales x by 10, y by 20
      # After normalization: (10, 20, 0) / sqrt(500)
      expected <- c(10, 20, 0) / sqrt(10^2 + 20^2)
      expect_equal(result_diag[1, ], expected, tolerance = 1e-6)
})


test_that("transform_normals_to_standard handles fixed scales", {
      normals <- matrix(c(0, 0, 1), nrow = 1)
      scale_ranges <- list(x = c(0, 10), y = c(0, 5), z = c(0, 20))

      result <- ggcube:::transform_normals_to_standard(
            normals, scale_ranges, scales = "fixed", ratio = c(1, 1, 1),
            anchor = "scene", proj = NULL
      )

      expect_equal(result[1, 1], 0, tolerance = 1e-10)
      expect_equal(result[1, 2], 0, tolerance = 1e-10)
      expect_equal(abs(result[1, 3]), 1, tolerance = 1e-10)
})


test_that("transform_normals_to_standard applies camera rotation", {
      normals <- matrix(c(0, 0, 1), nrow = 1)
      scale_ranges <- list(x = c(0, 10), y = c(0, 10), z = c(0, 10))
      proj <- list(pitch = 0, roll = 0, yaw = 0)

      result_scene <- ggcube:::transform_normals_to_standard(
            normals, scale_ranges, scales = "free", ratio = c(1, 1, 1),
            anchor = "scene", proj = proj
      )

      result_camera <- ggcube:::transform_normals_to_standard(
            normals, scale_ranges, scales = "free", ratio = c(1, 1, 1),
            anchor = "camera", proj = proj
      )

      expect_equal(result_scene[1, ], c(0, 0, 1), tolerance = 1e-10)
      expect_equal(result_camera[1, ], c(0, 0, -1), tolerance = 1e-10)
})


test_that("process_backfaces correctly identifies frontfaces with .subgroup", {
      outer <- data.frame(
            x = c(0, 1, 1, 0),
            y = c(0, 0, 1, 1),
            group = "g1",
            .subgroup = "g1_outer"
      )
      hole <- data.frame(
            x = c(0.25, 0.75, 0.75, 0.25),
            y = c(0.25, 0.25, 0.75, 0.75),
            group = "g1",
            .subgroup = "g1_hole"
      )
      data <- rbind(outer, hole)

      result <- ggcube:::process_backfaces(data)

      expect_equal(nrow(result), 8)
      expect_false(".backface" %in% names(result))
})


test_that("process_backfaces correctly identifies backfaces with .subgroup", {
      outer <- data.frame(
            x = c(0, 0, 1, 1),
            y = c(0, 1, 1, 0),
            group = "g1",
            .subgroup = "g1_outer"
      )
      hole <- data.frame(
            x = c(0.25, 0.25, 0.75, 0.75),
            y = c(0.25, 0.75, 0.75, 0.25),
            group = "g1",
            .subgroup = "g1_hole"
      )
      data <- rbind(outer, hole)
      data$cull_backfaces <- TRUE

      result <- ggcube:::process_backfaces(data)

      expect_equal(nrow(result), 0)
})


test_that("process_backfaces works without .subgroup column", {
      data <- data.frame(
            x = c(0, 1, 0.5),
            y = c(0, 0, 1),
            group = "g1"
      )

      result <- ggcube:::process_backfaces(data)
      expect_equal(nrow(result), 3)

      data_cw <- data.frame(
            x = c(0, 0.5, 1),
            y = c(0, 1, 0),
            group = "g1",
            cull_backfaces = TRUE
      )

      result_cw <- ggcube:::process_backfaces(data_cw)
      expect_equal(nrow(result_cw), 0)
})


test_that("process_backfaces handles mixed front/back groups", {
      front <- data.frame(
            x = c(0, 1, 1, 0),
            y = c(0, 0, 1, 1),
            group = "front",
            .subgroup = "front_1"
      )
      back <- data.frame(
            x = c(2, 2, 3, 3),
            y = c(0, 1, 1, 0),
            group = "back",
            .subgroup = "back_1"
      )
      data <- rbind(front, back)
      data$cull_backfaces <- TRUE

      result <- ggcube:::process_backfaces(data)

      expect_equal(nrow(result), 4)
      expect_true(all(result$group == "front"))
})
