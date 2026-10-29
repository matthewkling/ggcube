# Tests for shaded color steps and bins guides, and for reversed shading.

# The drawn guide of a plot with one guide
drawn_guide <- function(p) {
      gt <- ggplotGrob(p)
      box <- gt$grobs[[grep("guide-box", gt$layout$name)[1]]]
      box$grobs[[which(box$layout$name == "guides")]]
}

# The bar of a drawn color steps guide or colorbar
drawn_bar <- function(p) {
      guide <- drawn_guide(p)
      guide$grobs[[which(guide$layout$name == "bar")]]
}

# The gradient a guide shows for a color, from shadow to highlight
color_gradient <- function(color) {
      toupper(create_light_gradients(color, light(), shade_range = c(-.5, .5))[1, ])
}

# Keys of a drawn bins guide, as their shading gradients, from the top
drawn_bins_keys <- function(p) {
      guide <- drawn_guide(p)
      keys <- guide$grobs[[which(guide$layout$name == "keys")]]
      cells <- which(grepl("^key \\d+ bg$", keys$layout$name))
      cells <- cells[order(keys$layout$t[cells], keys$layout$l[cells])]
      lapply(cells, function(i) {
            raster <- Filter(function(g) inherits(g, "rastergrob"), keys$grobs[[i]]$children)[[1]]
            unique(toupper(as.vector(raster$raster)))
      })
}

surface <- ggplot(mountain, aes(x, y, z, fill = z)) + geom_surface_3d() + coord_3d()
oklab_L <- function(cols) linear_to_oklab(srgb_to_linear(t(col2rgb(cols)) / 255))[, 1]


test_that("binned scales of lit layers get shaded guides by default", {
      guide_class <- function(p) {
            guides <- ggplot_build(p)$plot$guides
            class(guides$guides[[match("fill", guides$aesthetics)]])[1]
      }
      expect_equal(guide_class(surface + scale_fill_viridis_b()), "GuideColoursteps3D")
      expect_equal(guide_class(surface + scale_fill_viridis_b(guide = "colorsteps")), "GuideColoursteps3D")
      expect_equal(guide_class(surface + scale_fill_viridis_b(guide = "bins")), "GuideBins3D")

      expect_equal(guide_class(surface + scale_fill_viridis_b() + light("none")), "GuideColoursteps")
      expect_equal(guide_class(surface + scale_fill_viridis_b() + guides(fill = "coloursteps")),
                   "GuideColoursteps")
})

test_that("each color step is shaded from its own color, in place", {
      for (even in c(TRUE, FALSE)) {
            scale <- function(guide) scale_fill_viridis_b(guide = guide, breaks = c(60, 65, 80, 100))
            plain <- drawn_bar(surface + scale(guide_colorsteps(even.steps = even)) + light("none"))
            shaded <- drawn_bar(surface + scale(guide_colorsteps_3d(even.steps = even)))
            expect_s3_class(plain, "rect")
            rasters <- shaded$children
            expect_length(rasters, length(plain$gp$fill))
            for (i in seq_along(rasters)) {
                  r <- rasters[[i]]
                  expect_s3_class(r, "rastergrob")
                  expect_equal(dim(r$raster), c(1, 20))
                  expect_equal(toupper(as.vector(r$raster)), color_gradient(plain$gp$fill[i]))
                  expect_equal(as.numeric(r$y), as.numeric(plain$y[i]), info = paste("even", even))
                  expect_equal(as.numeric(r$height), as.numeric(plain$height[i]))
            }
      }
})

test_that("horizontal color steps are shaded from bottom to top", {
      plain <- drawn_bar(surface + scale_fill_viridis_b(guide = guide_colorsteps(direction = "horizontal")) +
                               light("none"))
      shaded <- drawn_bar(surface + scale_fill_viridis_b(guide = guide_colorsteps_3d(direction = "horizontal")))
      for (i in seq_along(shaded$children)) {
            r <- shaded$children[[i]]
            expect_equal(dim(r$raster), c(20, 1))
            # Raster rows run from the top, so the shadow comes last
            expect_equal(toupper(as.vector(r$raster)), rev(color_gradient(plain$gp$fill[i])))
            expect_equal(as.numeric(r$x), as.numeric(plain$x[i]))
      }
})

test_that("reverse_shade reverses the shading of color steps", {
      plain <- drawn_bar(surface + scale_fill_viridis_b(guide = guide_colorsteps()) + light("none"))
      for (horizontal in c(FALSE, TRUE)) {
            dir <- if (horizontal) "horizontal" else "vertical"
            fwd <- drawn_bar(surface + scale_fill_viridis_b(guide = guide_colorsteps_3d(direction = dir)))
            rev <- drawn_bar(surface + scale_fill_viridis_b(
                  guide = guide_colorsteps_3d(direction = dir, reverse_shade = TRUE)))
            for (i in seq_along(fwd$children)) {
                  expect_equal(as.vector(rev$children[[i]]$raster),
                               rev(as.vector(fwd$children[[i]]$raster)), info = dir)
            }
      }
})

test_that("colorbars drawn as rectangles are shaded", {
      bar <- drawn_bar(surface + scale_fill_viridis_c(guide = guide_colorbar_3d(display = "rectangles", nbin = 8)))
      expect_length(bar$children, 8)
      expect_true(all(vapply(bar$children, inherits, logical(1), "rastergrob")))
})

test_that("each bins key is shaded from its own color", {
      plain <- drawn_guide(surface + scale_fill_viridis_b(guide = "bins") + light("none"))
      keys <- plain$grobs[[which(plain$layout$name == "keys")]]
      cells <- which(grepl("^key \\d+ bg$", keys$layout$name))
      cells <- cells[order(keys$layout$t[cells])]
      colors <- vapply(cells, function(i) key_glyph_fill(keys$grobs[[i]]), character(1))

      shaded <- drawn_bins_keys(surface + scale_fill_viridis_b(guide = "bins"))
      expect_length(shaded, length(colors))
      for (i in seq_along(colors)) {
            expect_equal(shaded[[i]], unique(color_gradient(colors[i])))
      }

      # Reversed bins: keys in the other order, still shaded from their own
      # colors, with shadows on the left
      reversed <- drawn_bins_keys(surface + scale_fill_viridis_b(guide = guide_bins_3d(reverse = TRUE)))
      for (i in seq_along(colors)) {
            expect_equal(reversed[[i]], unique(color_gradient(rev(colors)[i])))
      }
})

test_that("3D guides pass `reverse` to ggplot2 rather than reversing shading", {
      for (guide in list(guide_colorbar_3d(reverse = TRUE), guide_legend_3d(reverse = TRUE),
                         guide_colorsteps_3d(reverse = TRUE), guide_bins_3d(reverse = TRUE))) {
            expect_true(guide$params$reverse)
            expect_false(guide$params$reverse_shade)
      }
})

test_that("reverse_shade reverses the shading of horizontal colorbars, not their colors", {
      cbar <- function(...) {
            bar <- drawn_bar(surface + scale_fill_viridis_c(guide = guide_colorbar_3d(direction = "horizontal", ...)))
            as.matrix(bar$raster)
      }
      fwd <- cbar()
      rev <- cbar(reverse_shade = TRUE)
      # Rows are shading steps, columns colors
      expect_gt(ncol(fwd), nrow(fwd))
      expect_equal(rev, fwd[nrow(fwd):1, ])
      # Low colors on the left, shadow at the bottom
      expect_lt(oklab_L(fwd[nrow(fwd), 1]), oklab_L(fwd[1, 1]))
      expect_lt(oklab_L(fwd[1, 1]), oklab_L(fwd[1, ncol(fwd)]))
})

test_that("reverse_shade reverses the shading of each key of horizontal legends", {
      d <- expand.grid(x = 1:3, y = 1)
      d$z <- d$x
      d$k <- factor(letters[1:3])
      base <- scales::hue_pal()(3)
      for (dir in c("vertical", "horizontal")) {
            p <- ggplot(d, aes(x, y, z, fill = k)) + geom_col_3d() + coord_3d() +
                  guides(fill = guide_legend_3d(direction = dir, reverse_shade = TRUE))
            gt <- drawn_guide(p)
            cells <- which(grepl("^key", gt$layout$name))
            cells <- cells[order(gt$layout$t[cells], gt$layout$l[cells])]
            for (j in seq_along(cells)) {
                  raster <- Filter(function(g) inherits(g, "rastergrob"), gt$grobs[[cells[j]]]$children)[[1]]
                  expect_equal(unique(toupper(as.vector(raster$raster))),
                               rev(unique(color_gradient(base[j]))), info = dir)
            }
      }
})
