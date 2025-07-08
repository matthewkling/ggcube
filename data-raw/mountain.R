## code to prepare `mountain` dataset

library(raster)
library(tidyverse)

mountain <- volcano %>%
      raster() %>%
      focal(w = matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3), fun = mean) %>%
      aggregate(2, mean) %>%
      rasterToPoints() %>%
      as.data.frame() %>%
      setNames(c("x", "y", "z"))

usethis::use_data(mountain, overwrite = TRUE)
