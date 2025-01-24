## code to prepare `mountain` dataset

library(raster)
mountain <- as.data.frame(rasterToPoints(disaggregate(raster(volcano), 2, method = "bilinear")))
names(mountain) <- c("x", "y", "z")

usethis::use_data(mountain, overwrite = TRUE)
