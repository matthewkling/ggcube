test_that("continuous and discrete scales run without error for various stats", {
      library(tidyverse)

      cont <- expand_grid(a = 1:3, b = 1:3, c = 1:3)
      disc <- expand_grid(a = letters[1:3], b = letters[1:3], c = letters[1:3])
      mix <- expand_grid(a = 1:3, b = letters[1:3], c = letters[1:3])

      test <- function(d, layer, scl = "fixed"){
            cont %>%
                  ggplot(aes(a, b, c)) +
                  layer +
                  coord_3d(scales = scl)
      }

      expect_no_error({
            test(cont, geom_point())
            test(disc, geom_point())
            test(mix, geom_point())

            test(cont, geom_point_3d())
            test(disc, geom_point_3d())
            test(mix, geom_point_3d())

            test(cont, stat_voxel_3d())
            test(disc, stat_voxel_3d())
            test(mix, stat_voxel_3d())

            test(cont, stat_col_3d())
            test(disc, stat_col_3d())
            test(mix, stat_col_3d())
      })
})
