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

# The shading gradient drawn in each key of a plot's legend, by key label
legend_key_gradients <- function(p) {
      gt <- ggplotGrob(p)
      box <- gt$grobs[[grep("guide-box", gt$layout$name)[1]]]
      leg <- box$grobs[[which(box$layout$name == "guides")]]
      lay <- leg$layout
      keys <- which(grepl("^key", lay$name))
      out <- lapply(keys, function(i) {
            label <- which(grepl("^label", lay$name) & lay$t == lay$t[i] & lay$l == lay$l[i] + 1)
            text <- grid::getGrob(leg$grobs[[label]], gPath = "GRID.text", grep = TRUE)$label
            raster <- Filter(function(g) inherits(g, "rastergrob"), leg$grobs[[i]]$children)[[1]]
            list(label = text, colors = toupper(as.vector(raster$raster)))
      })
      stats::setNames(lapply(out, `[[`, "colors"), vapply(out, `[[`, character(1), "label"))
}

test_that("each legend key is shaded from its own color, in any layout", {
      d <- expand.grid(x = 1:4, y = 1:2)
      d$z <- d$x + d$y
      d$k <- factor(letters[1:8])
      base <- scales::hue_pal()(8)
      key_gradient <- function(color) {
            unique(toupper(create_light_gradients(color, light(), shade_range = c(-.5, .5))[1, ]))
      }

      for (ncol in 1:2) {
            p <- ggplot(d, aes(x, y, z, fill = k)) + geom_col_3d() + coord_3d() +
                  guides(fill = guide_legend_3d(ncol = ncol))
            keys <- legend_key_gradients(p)
            expect_setequal(names(keys), letters[1:8])
            for (i in 1:8) {
                  expect_equal(unique(keys[[letters[i]]]), key_gradient(base[i]),
                               info = paste("ncol", ncol, "key", letters[i]))
            }
      }
})
