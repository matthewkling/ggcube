# 3D guides are used by default for aesthetics that lighting applies to.

# The class of the guide built for an aesthetic
built_guide_class <- function(p, aesthetic = "fill") {
      b <- ggplot_build(p)
      guides <- b$plot$guides
      i <- match(aesthetic, guides$aesthetics)
      if (is.na(i)) return(NA_character_)
      class(guides$guides[[i]])[1]
}

surface_plot <- function(...) {
      ggplot(mountain, aes(x, y, z, ...)) + coord_3d()
}


test_that("lit layers mapping fill get 3D guides by default", {
      expect_equal(built_guide_class(surface_plot(fill = z) + geom_surface_3d()),
                   "GuideColourbar3D")
      expect_equal(built_guide_class(surface_plot(fill = x > .5, group = 1) + geom_surface_3d()),
                   "GuideLegend3D")

      # Fill computed by the stat by default
      expect_equal(built_guide_class(surface_plot() + geom_surface_3d()), "GuideColourbar3D")

      # Explicitly set lighting, before or after coord_3d()
      p <- ggplot(mountain, aes(x, y, z, fill = z)) + geom_surface_3d()
      expect_equal(built_guide_class(p + light("hard") + coord_3d()), "GuideColourbar3D")
      expect_equal(built_guide_class(p + coord_3d() + light("hard")), "GuideColourbar3D")
})

test_that("unlit layers keep ordinary guides", {
      expect_equal(built_guide_class(surface_plot(fill = z) + geom_surface_3d(light = "none")),
                   "GuideColourbar")
      expect_equal(built_guide_class(ggplot(mountain, aes(x, y, z, fill = z)) +
                                           geom_surface_3d() + coord_3d(light = NULL)),
                   "GuideColourbar")
      expect_equal(built_guide_class(surface_plot(fill = z) + geom_surface_3d() + light("none")),
                   "GuideColourbar")

      # Lighting that doesn't target fill
      expect_equal(built_guide_class(surface_plot(fill = z) + geom_surface_3d() +
                                           light(fill = FALSE)),
                   "GuideColourbar")
})

test_that("contours get 3D guides only when lighting is set explicitly", {
      p <- ggplot(mountain, aes(x, y, z, fill = after_stat(z))) + geom_contour_3d()
      expect_equal(built_guide_class(p + coord_3d()), "GuideColourbar")
      expect_equal(built_guide_class(p + coord_3d() + light()), "GuideColourbar3D")
})

test_that("guides of aesthetics mapped only by points aren't shaded", {
      p <- ggplot(mountain, aes(x, y, z)) +
            geom_surface_3d(fill = "grey70") +
            geom_point_3d(aes(colour = z)) +
            coord_3d()
      expect_equal(built_guide_class(p, "colour"), "GuideColourbar")
})

test_that("guides set by the user are left alone", {
      p <- surface_plot(fill = z) + geom_surface_3d()
      expect_equal(built_guide_class(p + guides(fill = "colourbar")), "GuideColourbar")
      expect_equal(built_guide_class(p + guides(fill = guide_colourbar(reverse = TRUE))),
                   "GuideColourbar")
      expect_equal(built_guide_class(p + scale_fill_viridis_c(guide = guide_colourbar())),
                   "GuideColourbar")
      expect_equal(built_guide_class(p + scale_fill_viridis_c(guide = "none")), NA_character_)
})

test_that("building a plot doesn't change the guides of its scales", {
      p <- surface_plot(fill = z) + geom_surface_3d() + scale_fill_viridis_c()
      invisible(ggplot_build(p))
      expect_equal(p$scales$get_scales("fill")$guide, "colourbar")
})

test_that("3D guides keep their options", {
      g <- guide_colorbar_3d(reverse_shade = TRUE, shade_range = c(-1, .2), reverse = TRUE)
      expect_s3_class(g, "GuideColourbar3D")
      expect_true(g$params$reverse_shade)
      expect_equal(g$params$shade_range, c(-1, .2))
      expect_true(g$params$reverse)
      expect_error(guide_legend_3d(shade_range = c(-2, 0)), "shade_range")

      # Drawn with their shading
      p <- surface_plot(fill = z) + geom_surface_3d() +
            guides(fill = guide_colorbar_3d(shade_range = c(-1, 1)))
      expect_no_error(ggplotGrob(p))
})

test_that("merged guides of fill and colour are shaded", {
      # Scales of fill and colour mapped to the same variable share one guide,
      # whose aesthetics are both
      for (p in list(surface_plot(fill = z, color = z) + geom_surface_3d(),
                     surface_plot(fill = x > .5, color = x > .5, group = 1) + geom_surface_3d())) {
            b <- ggplot_build(p)
            expect_length(b$plot$guides$guides, 1)
            expect_true(inherits(b$plot$guides$guides[[1]], c("GuideColourbar3D", "GuideLegend3D")))
            expect_no_error(ggplotGrob(p))
      }
})
