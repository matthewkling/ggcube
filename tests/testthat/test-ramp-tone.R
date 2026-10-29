# Tests for ramps and tones, which turn faces' light values into colors.

oklab_of <- function(colors) linear_to_oklab(srgb_to_linear(t(col2rgb(colors)) / 255))
hue_of <- function(colors) {
      lab <- oklab_of(colors)
      atan2(lab[, 3], lab[, 2])
}


test_that("ramps leave colors unchanged at their neutral angle", {
      for (ramp in list(ramp_smooth(45), ramp_smooth(90), ramp_smooth(120),
                        ramp_hard(45), ramp_hard(60))) {
            expect_identical(ramp_level(ramp, cospi(ramp$neutral / 180)), 0,
                             info = class(ramp)[1])
      }
})

test_that("ramps reach full highlight facing the light and increase toward it", {
      x <- cospi(seq(180, 0, by = -5) / 180)
      for (ramp in list(ramp_smooth(45), ramp_hard(45), ramp_steps(4, 45))) {
            level <- ramp_level(ramp, x)
            expect_true(all(diff(level) >= 0), info = class(ramp)[1])
            expect_true(all(level >= -1 & level <= 1), info = class(ramp)[1])
      }
      expect_equal(ramp_level(ramp_smooth(45), 1), 1)
      expect_equal(ramp_level(ramp_hard(45), 1), 1)
})

test_that("smooth ramps reach full shadow facing away from the light", {
      expect_equal(ramp_level(ramp_smooth(45), -1), -1)
      expect_gt(ramp_level(ramp_smooth(45), 0), -1)
})

test_that("hard ramps fully shadow faces at 90 degrees or more from the light", {
      expect_equal(ramp_level(ramp_hard(45), c(0, -.3, -1)), c(-1, -1, -1))
      expect_gt(ramp_level(ramp_hard(45), .1), -1)
})

test_that("stepped ramps give n flat levels", {
      level <- ramp_level(ramp_steps(4), cospi(seq(0, 180, by = 1) / 180))
      expect_setequal(unique(level), c(-.75, -.25, .25, .75))
      expect_length(unique(ramp_level(ramp_steps(3), seq(-1, 1, by = .01))), 3)
})

test_that("ramps clamp light values beyond the unit range", {
      # Back-face offsets can push light values outside [-1, 1]
      expect_equal(ramp_level(ramp_smooth(45), c(-1.5, 1.5)), c(-1, 1))
})

test_that("ramp and tone constructors validate their arguments", {
      expect_error(ramp_smooth(0), "neutral")
      expect_error(ramp_smooth(180), "neutral")
      expect_error(ramp_hard(90), "neutral")
      expect_error(ramp_steps(1), "`n`")
      expect_error(ramp_steps(2.5), "`n`")
      expect_error(tone_tint(shadow = "notacolor"), "`shadow`")
      expect_error(tone_tint(highlight = c("red", "blue")), "`highlight`")
})

test_that("lightness and tint tones leave colors exactly unchanged at zero amount", {
      colors <- c("darkorange", "navy", "grey60", "white", "black", "#21908C")
      expected <- rgb(t(col2rgb(colors)), maxColorValue = 255)
      for (tone in list(tone_lightness(), tone_tint("navy", "lightyellow"))) {
            expect_identical(tone_colors(tone, colors, rep(0, length(colors))), expected,
                             info = class(tone)[1])
      }
})

test_that("the lightness tone reaches white and black, and keeps hue in between", {
      colors <- c("royalblue", "darkorange", "forestgreen", "purple", "#F8766D")
      expect_equal(toupper(tone_colors(tone_lightness(), colors, rep(1, 5))), rep("#FFFFFF", 5))
      expect_equal(toupper(tone_colors(tone_lightness(), colors, rep(-1, 5))), rep("#000000", 5))

      # Lightness moves monotonically with amount
      amounts <- seq(-.9, .9, by = .3)
      L <- oklab_of(tone_colors(tone_lightness(), rep("royalblue", length(amounts)), amounts))[, 1]
      expect_true(all(diff(L) > 0))

      # Hue is kept, unlike in CIELAB, where lightened blues drift toward
      # purple
      for (a in c(-.6, -.3, .3, .6)) {
            lit <- tone_colors(tone_lightness(), colors, rep(a, 5))
            expect_lt(max(abs(hue_of(lit) - hue_of(colors))), .06)
      }
})

test_that("the tint tone reaches its endpoint colors", {
      tone <- tone_tint(shadow = "navy", highlight = "lightyellow")
      colors <- c("darkorange", "grey60", "#21908C")
      expect_equal(tone_colors(tone, colors, rep(1, 3)), rep(toupper(rgb(t(col2rgb("lightyellow")), maxColorValue = 255)), 3))
      expect_equal(tone_colors(tone, colors, rep(-1, 3)), rep(toupper(rgb(t(col2rgb("navy")), maxColorValue = 255)), 3))
})

test_that("tones keep alpha given in colors, and pass NA through", {
      lit <- tone_colors(tone_lightness(), c("#FF000080", "#0000FF", NA), c(-.5, -.5, -.5))
      expect_match(lit[1], "^#[0-9A-F]{6}80$")
      expect_match(lit[2], "^#[0-9A-F]{6}$")
      expect_true(is.na(lit[3]))
      expect_identical(tone_colors(tone_lightness(), "red", NA_real_), "red")
})

# A lighting specification with ggcube 0.3's color math: a ramp with a neutral
# angle of 90 degrees (60 for direct lighting), and equal strengths
legacy_light <- function(space = "hsv", ramp = ramp_smooth(90), contrast = 1,
                         shade = "both") {
      structure(list(ramp = ramp, tone = tone_legacy(space), shadow = contrast,
                     highlight = contrast, shade = shade),
                class = "light")
}

test_that("legacy hsv and hsl tones reproduce ggcube 0.3 colors", {
      # Expected values were computed with the blending code of ggcube 0.3
      cases <- list(
            list(legacy_light(), "darkgreen", .5, "#00B200"),
            list(legacy_light(), "darkgreen", -.5, "#003200"),
            list(legacy_light("hsl"), "#3b6ea5", .5, "#96B6D9"),
            list(legacy_light("hsl"), "#3b6ea5", -.75, "#0F1B29"),
            list(legacy_light(ramp = ramp_hard(60)), "steelblue", .25, "#23415A"),
            list(legacy_light(ramp = ramp_hard(60)), "steelblue", .8, "#58A2E1"),
            list(legacy_light("hsl", contrast = 1.5), "gold", .3, "#FFE973"),
            list(legacy_light(contrast = .5), "grey60", -1, "#4D4D4D")
      )
      for (cs in cases) {
            expect_equal(toupper(blend_light_with_colors(cs[[2]], cs[[3]], cs[[1]])), cs[[4]],
                         info = paste(cs[[2]], cs[[3]]))
      }
})

test_that("blend_light lights only the targeted aesthetics of lit rows", {
      spec <- legacy_light(shade = "fill")
      coords <- data.frame(fill = c("darkgreen", "darkgreen"), colour = c("black", "black"),
                           light = c(.5, NA))
      coords$lighting_spec <- I(list(spec, spec))
      out <- blend_light(coords)
      expect_equal(toupper(out$fill), c("#00B200", "DARKGREEN"))
      expect_equal(out$colour, c("black", "black"))

      none <- coords
      none$lighting_spec <- I(rep(list(light("none")), 2))
      expect_identical(blend_light(none)$fill, coords$fill)
})


# light() -----------------------------------------------------------------------

test_that("light() defaults to a smooth ramp, lightness tone, and light from above xmin", {
      l <- light()
      expect_equal(l$ramp, ramp_smooth(45))
      expect_equal(l$tone, tone_lightness())
      expect_equal(c(l$shadow, l$highlight), c(.8, .6))
      expect_equal(l$direction / sqrt(sum(l$direction^2)), direction(270, 60))
})

test_that("light() takes ramps and tones by name or as objects", {
      expect_equal(light("hard"), light(ramp_hard()))
      expect_equal(light("steps"), light(ramp_steps(4)))
      expect_equal(light(ramp = "smooth"), light(ramp = ramp_smooth()))
      expect_equal(light(tone = "tint"), light(tone = tone_tint()))
      expect_equal(light(tone = "normals"), light(tone = tone_normals()))
      expect_equal(light(ramp_steps(3, neutral = 30))$ramp, ramp_steps(3, neutral = 30))
})

test_that("light() validates ramps, tones, and strengths", {
      expect_error(light("bogus"), "none")
      expect_error(light(tone = "bogus"), "`tone`")
      expect_error(light(shadow = 1.5), "`shadow`")
      expect_error(light(highlight = -.1), "`highlight`")
      expect_error(light(shadow = c(.5, .5)), "`shadow`")
})

test_that("light()'s method argument is deprecated in favor of ramp", {
      lifecycle::expect_deprecated(l <- light(method = "direct"))
      expect_equal(l, light("hard"))
      lifecycle::expect_deprecated(l <- light(method = "diffuse"))
      expect_equal(l, light())
      lifecycle::expect_deprecated(l <- light(method = "rgb"))
      expect_equal(l, light(tone = "normals"))
      expect_equal(suppressWarnings(light(method = "none")), light("none"))

      # Old method names given positionally, as ramps
      lifecycle::expect_deprecated(l <- light("direct"))
      expect_equal(l, light("hard"))
      lifecycle::expect_deprecated(l <- light("diffuse"))
      expect_equal(l, light())
      lifecycle::expect_deprecated(l <- light("rgb"))
      expect_equal(l, light(tone = "normals"))
})

test_that("light()'s mode argument is deprecated, and gives legacy tones", {
      lifecycle::expect_deprecated(l <- light(mode = "hsl"))
      expect_equal(l$tone, tone_legacy("hsl"))
      expect_error(suppressWarnings(light(mode = "lab")), "`mode`")
})

test_that("light()'s contrast argument is deprecated, and scales the default strengths", {
      lifecycle::expect_deprecated(l <- light(contrast = .5))
      expect_equal(c(l$shadow, l$highlight), c(.4, .3))
      l <- suppressWarnings(light(contrast = 2))
      expect_equal(c(l$shadow, l$highlight), c(1, 1))
      l <- suppressWarnings(light(contrast = .5, shadow = .9))
      expect_equal(c(l$shadow, l$highlight), c(.9, .3))
})

test_that("lights, ramps, and tones print", {
      expect_output(print(light()), "smooth \\(neutral 45 deg\\)")
      expect_output(print(light()), "lightness")
      expect_output(print(light("none")), "Disabled")
      expect_output(print(ramp_steps(3)), "steps \\(3 bands, neutral 45 deg\\)")
      expect_output(print(tone_tint("navy", "gold")), "tint \\(shadow navy, highlight gold\\)")
})

test_that("3D guide gradients are centered on the scale's colors", {
      base <- c("#440154", "#21908C", "#FDE725")
      grad <- create_light_gradients(base, light(), shade_range = c(-.5, .5), n_steps = 21)
      expect_equal(grad[, 11], toupper(base))

      # Lighter on the highlight side, darker on the shadow side
      L <- function(cols) linear_to_oklab(srgb_to_linear(t(col2rgb(cols)) / 255))[, 1]
      expect_true(all(L(grad[, 21]) > L(grad[, 11])))
      expect_true(all(L(grad[, 1]) < L(grad[, 11])))

      # Normal-based tones have no gradient to show
      flat <- create_light_gradients(base, light(tone = "normals"), n_steps = 5)
      expect_equal(flat[, 1], base)
})
