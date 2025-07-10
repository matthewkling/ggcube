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

            test(cont, stat_voxel())
            test(disc, stat_voxel())
            test(mix, stat_voxel())

            test(cont, stat_pillar())
            test(disc, stat_pillar())
            test(mix, stat_pillar())
      })
})
