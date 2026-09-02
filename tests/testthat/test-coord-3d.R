test_that("continuous and discrete scales run without error for various stats", {
      cont <- expand.grid(a = 1:3, b = 1:3, c = 1:3)
      disc <- expand.grid(a = letters[1:3], b = letters[1:3], c = letters[1:3])
      mix <- expand.grid(a = 1:3, b = letters[1:3], c = letters[1:3])

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

test_that("each plot uses its own z scale, regardless of construction order", {

      d <- expand.grid(x = 1:5, y = 1:5)

      make_plot <- function(upper) {
            dd <- d
            dd$z <- seq(0, upper, length.out = nrow(dd))
            ggplot(dd, aes(x, y, z = z)) +
                  geom_point() +
                  scale_z_continuous(limits = c(0, upper), expand = expansion(0)) +
                  coord_3d()
      }

      z_limits <- function(p) {
            ggplot_build(p)$layout$panel_params[[1]]$scale_info$z$limits
      }

      p_small <- make_plot(0.001)
      p_big <- make_plot(1000)

      # p_small's scale_z_continuous() call is no longer the most recent one
      expect_equal(z_limits(p_small), c(0, 0.001))
      expect_equal(z_limits(p_big), c(0, 1000))

      # And rebuilding in the other order gives the same answer
      expect_equal(z_limits(p_big), c(0, 1000))
      expect_equal(z_limits(p_small), c(0, 0.001))
})

test_that("repeated builds don't accumulate z scale range", {

      d <- expand.grid(x = 1:5, y = 1:5)
      d$z <- d$x * d$y

      p <- ggplot(d, aes(x, y, z = z)) +
            geom_point() +
            coord_3d()

      z_limits <- function(p) {
            ggplot_build(p)$layout$panel_params[[1]]$scale_info$z$limits
      }

      expect_equal(z_limits(p), z_limits(p))
})
