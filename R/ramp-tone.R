# How light values become colors.
#
# Each lit face has a light value: the cosine of the angle between its normal
# and the direction toward the light, with back faces' values flipped. Colors
# are lit in three steps:
#
# 1. A ramp maps light values to light levels in [-1, 1], where 0 is the
#    neutral level that leaves a color unchanged, -1 full shadow, and 1 full
#    highlight.
# 2. The light's `shadow` and `highlight` strengths scale levels into signed
#    amounts.
# 3. A tone turns base colors and amounts into lit colors.
#
# Ramps and tones are small classed lists, not closures, so that lighting
# specifications compare, print, and serialize as plain data.


# Ramps ------------------------------------------------------------------------

#' Construct a ramp
#'
#' @param kind Ramp type, giving its class `ggcube_ramp_<kind>`.
#' @param ... Fields of the ramp.
#' @return A `ggcube_ramp` object.
#' @keywords internal
#' @noRd
new_ramp <- function(kind, ...) {
      structure(list(...), class = c(paste0("ggcube_ramp_", kind), "ggcube_ramp"))
}

#' Validate a ramp's neutral angle
#'
#' @param neutral Angle in degrees between a face's normal and the direction
#'   toward the light at which colors are unchanged.
#' @param max Largest allowed angle, exclusive.
#' @return `neutral`, invisibly.
#' @keywords internal
#' @noRd
check_neutral <- function(neutral, max = 180) {
      if (!is.numeric(neutral) || length(neutral) != 1 || !is.finite(neutral) ||
          neutral <= 0 || neutral >= max) {
            rlang::abort(paste0("`neutral` must be a single angle greater than 0 and less than ",
                                max, " degrees."), call = NULL)
      }
      invisible(neutral)
}

#' Lighting ramps
#'
#' `r lifecycle::badge("experimental")`
#'
#' A ramp decides how brightness changes as a surface turns away from the
#' light, for use as the `ramp` argument of [light()]. Each ramp keeps colors
#' unchanged on faces at `neutral` degrees to the light, and moves faces turned
#' further toward the light toward full highlight, and faces turned further away
#' toward full shadow. How far toward full highlight and shadow colors go is set
#' by the `highlight` and `shadow` arguments of [light()].
#'
#' * `ramp_smooth()` changes brightness gradually, reaching full highlight on
#'   faces that face the light and full shadow on faces that face directly away
#'   from it. This is the default.
#' * `ramp_hard()` is like `ramp_smooth()` toward the light, but faces at 90
#'   degrees or more from the light are fully shadowed, giving a sharp edge
#'   between lit and unlit sides.
#' * `ramp_steps()` divides faces into flat bands of brightness, for a
#'   cartoon-like look. The bands run evenly from full highlight on the band
#'   nearest the light to full shadow on the band farthest from it, so with an
#'   odd number of bands the middle one shows colors unchanged. Bands are
#'   either `n` equal divisions of the brightness of `ramp_smooth()`, or are
#'   divided at the angles given by `breaks`. Specular highlights (see
#'   [light()]) are flat spots too.
#'
#' These can also be given to [light()] by name, as `"smooth"`, `"hard"`, or
#' `"steps"`, to use their defaults.
#'
#' @param neutral Angle in degrees between a face and the direction toward the
#'   light at which colors are unchanged. Must be greater than 0 and less than
#'   180, or less than 90 for `ramp_hard()`. Default is 45.
#' @param n Number of bands, at least 2. Default is 4.
#' @param breaks Alternative to `n` and `neutral`: angles in degrees between a
#'   face and the direction toward the light at which one band ends and the
#'   next begins, increasing, greater than 0 and less than 180. Faces at a
#'   break fall in the darker band. For example, `breaks = 90` gives a fully
#'   highlighted lit side and a fully shadowed unlit side, and
#'   `breaks = c(60, 90)` adds a band of unchanged color between them.
#' @return A ramp object, for the `ramp` argument of [light()].
#' @seealso [light()], and [tone_lightness()] for tones, which decide what lighter
#'   and darker look like.
#' @examples
#' p <- ggplot(sphere_points, aes(x, y, z)) +
#'   geom_hull_3d(fill = "steelblue", color = "steelblue") +
#'   coord_3d(scales = "fixed")
#'
#' p + light(ramp_smooth())
#'
#' # base color on faces closer to facing the light, for darker overall shading
#' p + light(ramp_smooth(neutral = 30))
#'
#' p + light(ramp_hard())
#'
#' p + light(ramp_steps(n = 3))
#'
#' # bands divided at chosen angles from the light
#' p + light(ramp_steps(breaks = c(60, 90)))
#' @name ramps
NULL

#' @rdname ramps
#' @export
ramp_smooth <- function(neutral = 45) {
      check_neutral(neutral)
      new_ramp("smooth", neutral = neutral)
}

#' @rdname ramps
#' @export
ramp_hard <- function(neutral = 45) {
      check_neutral(neutral, max = 90)
      new_ramp("hard", neutral = neutral)
}

#' @rdname ramps
#' @export
ramp_steps <- function(n = 4, neutral = 45, breaks = NULL) {
      if (!is.null(breaks)) {
            if (!missing(n) || !missing(neutral)) {
                  rlang::abort("Give either `breaks`, or `n` and `neutral`, not both.", call = NULL)
            }
            if (!is.numeric(breaks) || length(breaks) == 0 || any(!is.finite(breaks)) ||
                any(breaks <= 0 | breaks >= 180) || any(diff(breaks) <= 0)) {
                  rlang::abort("`breaks` must be increasing angles greater than 0 and less than 180 degrees.",
                               call = NULL)
            }
            return(new_ramp("steps", n = length(breaks) + 1L, breaks = as.numeric(breaks)))
      }
      if (!is.numeric(n) || length(n) != 1 || !is.finite(n) || n < 2 || n != round(n)) {
            rlang::abort("`n` must be a single whole number of at least 2.", call = NULL)
      }
      check_neutral(neutral)
      new_ramp("steps", n = as.integer(n), neutral = neutral)
}

#' Cosine of an angle in degrees
#'
#' Rounded so that angles with exact cosines, such as 60 degrees, get them
#' exactly.
#'
#' @param angle Angles in degrees.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
cos_deg <- function(angle) {
      round(cospi(angle / 180), 15)
}

#' Light levels of light values
#'
#' @param ramp A `ggcube_ramp` object.
#' @param x Numeric vector of light values: cosines of the angles between faces'
#'   normals and the direction toward the light. Values outside `[-1, 1]` (e.g.
#'   from a nonzero `backface_offset`) are allowed.
#' @return Numeric vector of light levels in `[-1, 1]`.
#' @keywords internal
#' @noRd
ramp_level <- function(ramp, x) {
      n <- ramp[["n"]]
      breaks <- ramp[["breaks"]]

      # Stepped ramps divided at given angles: band 0 is nearest the light.
      # Faces within rounding error of a break fall in the darker band.
      if (!is.null(breaks)) {
            band <- rowSums(outer(x, cos_deg(breaks) + 1e-12, `<=`))
            return(1 - 2 * band / (n - 1))
      }

      c0 <- cos_deg(ramp$neutral)
      toward <- (x - c0) / (1 - c0)
      level <- switch(class(ramp)[1],
                      ggcube_ramp_smooth = ,
                      ggcube_ramp_steps = ifelse(x >= c0, toward, (x - c0) / (1 + c0)),
                      ggcube_ramp_hard = ifelse(x >= c0, toward, (x - c0) / c0),
                      rlang::abort(paste0("Unknown ramp type: ", class(ramp)[1], "."), call = NULL))
      level <- pmax(-1, pmin(1, level))
      # Faces at the neutral angle leave colors exactly unchanged
      level[abs(level) < 1e-12] <- 0
      # Stepped ramps divided evenly: band 0 is farthest from the light
      if (inherits(ramp, "ggcube_ramp_steps")) {
            band <- pmin(floor((level + 1) / 2 * n), n - 1)
            level <- -1 + 2 * band / (n - 1)
      }
      level
}


# Tones ------------------------------------------------------------------------

#' Construct a tone
#'
#' @param kind Tone type, giving its class `ggcube_tone_<kind>`.
#' @param ... Fields of the tone.
#' @return A `ggcube_tone` object.
#' @keywords internal
#' @noRd
new_tone <- function(kind, ...) {
      structure(list(...), class = c(paste0("ggcube_tone_", kind), "ggcube_tone"))
}

#' Lighting tones
#'
#' `r lifecycle::badge("experimental")`
#'
#' A tone decides what lighter and darker look like for a given color, for use
#' as the `tone` argument of [light()]. How far faces move toward a tone's full
#' highlight and shadow is set by the ramp and the `highlight` and `shadow`
#' arguments of [light()].
#'
#' * `tone_lightness()` makes colors lighter and darker versions of the same
#'   hue, reaching white at full highlight and black at full shadow. It changes
#'   lightness in the perceptual OKLab color space, keeping hue, and as much
#'   chroma as the sRGB gamut allows. This is the default.
#' * `tone_tint()` mixes colors toward a chosen highlight color and shadow color,
#'   in OKLab. This gives colored light, such as warm highlights with cool
#'   shadows, like the split toning of photography.
#' * `tone_normals()` ignores base colors, and colors each face by the direction
#'   it faces: face normals are mapped to RGB color space, rotated so that faces
#'   facing the light are white. 3D guides aren't shaded with this tone.
#'
#' These can also be given to [light()] by name, as `"lightness"`, `"tint"`, or
#' `"normals"`, to use their defaults.
#'
#' @param shadow,highlight Colors reached at full shadow and full highlight.
#'   Defaults are black and white.
#' @return A tone object, for the `tone` argument of [light()].
#' @seealso [light()], and [ramp_smooth()] for ramps, which decide how brightness
#'   changes with angle.
#' @examples
#' p <- ggplot(sphere_points, aes(x, y, z)) +
#'   geom_hull_3d(fill = "darkorange", color = "darkorange") +
#'   coord_3d(scales = "fixed")
#'
#' p + light(tone = tone_lightness())
#'
#' # warm light with cool shadows
#' p + light(tone = tone_tint(shadow = "midnightblue", highlight = "lightyellow"))
#'
#' p + light(tone = tone_normals())
#' @name tones
NULL

#' @rdname tones
#' @export
tone_lightness <- function() {
      new_tone("lightness")
}

#' @rdname tones
#' @export
tone_tint <- function(shadow = "black", highlight = "white") {
      for (arg in c("shadow", "highlight")) {
            value <- get(arg)
            ok <- length(value) == 1 && !is.na(value) &&
                  !inherits(try(col2rgb(value), silent = TRUE), "try-error")
            if (!ok) rlang::abort(paste0("`", arg, "` must be a single color."), call = NULL)
      }
      new_tone("tint", shadow = shadow, highlight = highlight)
}

#' @rdname tones
#' @export
tone_normals <- function() {
      new_tone("normals")
}

#' Legacy tone
#'
#' The tones of ggcube 0.3, which move the value of colors in HSV space or
#' their lightness in HSL space toward white or black.
#'
#' @param space `"hsv"` or `"hsl"`.
#' @return A `ggcube_tone` object.
#' @keywords internal
#' @noRd
tone_legacy <- function(space = "hsv") {
      new_tone("legacy", space = space)
}

#' Describe ramps and tones
#'
#' @param x A ramp or tone.
#' @return Character string.
#' @keywords internal
#' @noRd
format_ramp <- function(x) {
      kind <- sub("^ggcube_ramp_", "", class(x)[1])
      details <- c(if (!is.null(x[["n"]])) paste(x[["n"]], "bands"),
                   if (!is.null(x[["breaks"]])) {
                         paste0("breaks at ", paste(x[["breaks"]], collapse = ", "), " deg")
                   } else {
                         paste0("neutral ", x$neutral, " deg")
                   })
      paste0(kind, " (", paste(details, collapse = ", "), ")")
}

#' @noRd
format_tone <- function(x) {
      kind <- sub("^ggcube_tone_", "", class(x)[1])
      switch(kind,
             tint = paste0("tint (shadow ", x$shadow, ", highlight ", x$highlight, ")"),
             legacy = paste0("legacy ", x$space),
             kind)
}

#' @export
print.ggcube_ramp <- function(x, ...) {
      cat("<ramp>", format_ramp(x), "
")
      invisible(x)
}

#' @export
print.ggcube_tone <- function(x, ...) {
      cat("<tone>", format_tone(x), "
")
      invisible(x)
}

#' Apply a tone to colors
#'
#' @param tone A `ggcube_tone` object.
#' @param colors Character vector of base colors.
#' @param amount Numeric vector of signed amounts: positive toward highlight,
#'   negative toward shadow, with magnitude 1 reaching the tone's endpoint.
#' @param normals Matrix with 3 columns of face normals, one row per color, for
#'   normal-based tones.
#' @param direction Light direction for normal-based tones.
#' @return Character vector of lit colors. Colors or amounts that are `NA` are
#'   returned unchanged, and alpha given in colors is kept.
#' @keywords internal
#' @noRd
tone_colors <- function(tone, colors, amount, normals = NULL, direction = NULL) {
      out <- colors

      if (inherits(tone, "ggcube_tone_normals")) {
            if (is.null(normals) || is.null(direction)) return(out)
            ok <- !is.na(amount) & stats::complete.cases(normals)
            if (any(ok)) out[ok] <- compute_rgb_light(normals[ok, , drop = FALSE], direction)
            return(out)
      }

      ok <- !is.na(colors) & !is.na(amount)
      if (!any(ok)) return(out)

      # Tone each distinct color and amount once
      rgba <- col2rgb(colors[ok], alpha = TRUE)
      a <- amount[ok]
      key <- paste(rgba[1, ], rgba[2, ], rgba[3, ], a)
      first <- !duplicated(key)
      rgb_first <- rgba[1:3, first, drop = FALSE]
      a_first <- a[first]
      lit <- switch(class(tone)[1],
                    ggcube_tone_lightness = tone_rgb_lightness(rgb_first, a_first),
                    ggcube_tone_tint = tone_rgb_tint(rgb_first, a_first, tone$shadow, tone$highlight),
                    ggcube_tone_legacy = tone_rgb_legacy(rgb_first, a_first, tone$space),
                    rlang::abort(paste0("Unknown tone type: ", class(tone)[1], "."), call = NULL))
      lit <- lit[match(key, key[first])]

      alpha <- rgba[4, ]
      translucent <- alpha < 255
      lit[translucent] <- paste0(lit[translucent], sprintf("%02X", alpha[translucent]))
      out[ok] <- lit
      out
}

#' Lightness tone of RGB colors
#'
#' @param rgb Integer matrix with 3 rows (red, green, blue, 0-255).
#' @param amount Numeric vector of amounts, one per column of `rgb`.
#' @return Character vector of opaque hex colors.
#' @keywords internal
#' @noRd
tone_rgb_lightness <- function(rgb, amount) {
      amount <- pmax(-1, pmin(1, amount))
      lab <- linear_to_oklab(srgb_to_linear(t(rgb) / 255))
      L <- lab[, 1]
      L2 <- ifelse(amount >= 0, L + amount * (1 - L), L * (1 + amount))
      ab <- lab[, 2:3, drop = FALSE]
      k <- oklab_chroma_in_gamut(L2, ab)
      out <- linear_to_hex(oklab_to_linear(cbind(L2, ab * k)))
      unchanged <- amount == 0
      out[unchanged] <- rgb(rgb[1, unchanged], rgb[2, unchanged], rgb[3, unchanged],
                            maxColorValue = 255)
      out
}

#' Tint tone of RGB colors
#'
#' @param rgb Integer matrix with 3 rows (red, green, blue, 0-255).
#' @param amount Numeric vector of amounts, one per column of `rgb`.
#' @param shadow,highlight Endpoint colors.
#' @return Character vector of opaque hex colors.
#' @keywords internal
#' @noRd
tone_rgb_tint <- function(rgb, amount, shadow, highlight) {
      amount <- pmax(-1, pmin(1, amount))
      lab <- linear_to_oklab(srgb_to_linear(t(rgb) / 255))
      ends <- linear_to_oklab(srgb_to_linear(t(col2rgb(c(shadow, highlight))) / 255))
      target <- ends[ifelse(amount < 0, 1, 2), , drop = FALSE]
      out <- linear_to_hex(oklab_to_linear(lab + abs(amount) * (target - lab)))
      unchanged <- amount == 0
      out[unchanged] <- rgb(rgb[1, unchanged], rgb[2, unchanged], rgb[3, unchanged],
                            maxColorValue = 255)
      out
}

#' Legacy tone of RGB colors
#'
#' @param rgb Integer matrix with 3 rows (red, green, blue, 0-255).
#' @param amount Numeric vector of amounts, one per column of `rgb`. Amounts
#'   beyond 1 in magnitude are allowed, and saturate.
#' @param space `"hsv"` or `"hsl"`.
#' @return Character vector of opaque hex colors.
#' @keywords internal
#' @noRd
tone_rgb_legacy <- function(rgb, amount, space) {
      if (space == "hsl") {
            hsl <- rgb2hsl(rgb / 255)
            l <- hsl[3, ]
            l <- ifelse(amount > 0, l + amount * (1 - l), l * (1 + amount))
            hsl[3, ] <- pmax(0, pmin(1, l))
            out <- hsl2rgb(hsl)
            return(rgb(out[1, ], out[2, ], out[3, ]))
      }
      hsv_m <- rgb2hsv(rgb)
      h <- hsv_m[1, ]
      h[is.nan(h)] <- 0
      v <- hsv_m[3, ]
      v <- ifelse(amount > 0, v + amount * (1 - v), v * (1 + amount))
      hsv(h, hsv_m[2, ], pmax(0, pmin(1, v)))
}


# Blending ---------------------------------------------------------------------

#' Light colors by their light values
#'
#' @param colors Character vector of base colors.
#' @param light_values Numeric vector of light values.
#' @param lighting A lighting specification object.
#' @param normals Matrix with 3 columns of face normals, for normal-based
#'   tones.
#' @param specular Numeric vector of specular strengths, or `NULL`.
#' @return Character vector of lit colors.
#' @keywords internal
#' @noRd
blend_light_with_colors <- function(colors, light_values, lighting, normals = NULL,
                                    specular = NULL) {
      if (is.null(colors) || length(colors) == 0) {
            return(colors)
      }
      level <- ramp_level(lighting$ramp, as.numeric(light_values))
      amount <- ifelse(level >= 0, lighting$highlight * level, lighting$shadow * level)
      # Glints move the rest of the way toward full highlight
      if (!is.null(specular) && !inherits(lighting$tone, "ggcube_tone_normals")) {
            specular[is.na(specular)] <- 0
            amount <- amount + specular * (1 - amount)
      }
      tone_colors(lighting$tone, colors, amount, normals, lighting$tone_direction)
}

#' Light a layer's colors
#'
#' Applies a layer's lighting to its fill and/or border colors, using the
#' lighting specification and light values that the coord attached to its
#' rows. Outlines that polygon layers draw in their fill color by default
#' (marked by `.outline_from_fill`) take the lit fill color.
#'
#' @param coords Transformed layer data.
#' @return `coords`, with lit colors.
#' @keywords internal
#' @noRd
blend_light <- function(coords) {
      if (!all(c("lighting_spec", "light") %in% names(coords))) {
            return(coords)
      }
      lit <- which(!is.na(coords$light))
      if (length(lit) == 0) {
            return(coords)
      }
      spec <- coords$lighting_spec[[lit[1]]]
      if (!inherits(spec, "light") || spec$shade == "neither") {
            return(coords)
      }

      normals <- NULL
      if (inherits(spec$tone, "ggcube_tone_normals") &&
          all(c("normal_x", "normal_y", "normal_z") %in% names(coords))) {
            normals <- as.matrix(coords[c("normal_x", "normal_y", "normal_z")])
      }

      specular <- coords[[".specular"]]
      if (spec$shade %in% c("fill", "both")) {
            coords$fill <- blend_light_with_colors(coords$fill, coords$light, spec, normals, specular)
      }
      if (spec$shade %in% c("colour", "both")) {
            coords$colour <- blend_light_with_colors(coords$colour, coords$light, spec, normals, specular)
      }

      # Outlines drawn in the fill color by default, rather than in a color the
      # user mapped or set, exist only to hide seams between polygons, so they
      # follow the fill whichever colors the light targets. Lit differently
      # from the fill, they would draw the mesh over the shading.
      if (".outline_from_fill" %in% names(coords)) {
            implicit <- coords$.outline_from_fill %in% TRUE
            coords$colour[implicit] <- coords$fill[implicit]
      }
      coords
}


# Color spaces -----------------------------------------------------------------

#' Convert RGB to HSL color space
#'
#' @param rgb_matrix 3xN matrix with RGB values in 0-1 range
#' @return 3xN matrix with HSL values (H in 0-1, S in 0-1, L in 0-1)
#' @keywords internal
#' @noRd
rgb2hsl <- function(rgb_matrix) {
      if (!is.matrix(rgb_matrix) || nrow(rgb_matrix) != 3) {
            stop("rgb_matrix must be a 3xN matrix")
      }

      r <- rgb_matrix[1, ]
      g <- rgb_matrix[2, ]
      b <- rgb_matrix[3, ]

      max_val <- pmax(r, g, b)
      min_val <- pmin(r, g, b)
      diff <- max_val - min_val

      # Lightness
      l <- (max_val + min_val) / 2

      # Initialize hue and saturation
      h <- rep(0, ncol(rgb_matrix))
      s <- rep(0, ncol(rgb_matrix))

      # Calculate saturation
      non_zero_diff <- diff > 0
      s[non_zero_diff] <- ifelse(
            l[non_zero_diff] < 0.5,
            diff[non_zero_diff] / (max_val[non_zero_diff] + min_val[non_zero_diff]),
            diff[non_zero_diff] / (2 - max_val[non_zero_diff] - min_val[non_zero_diff])
      )

      # Calculate hue
      if (any(non_zero_diff)) {
            # Red is max
            red_max <- non_zero_diff & (r == max_val)
            h[red_max] <- ((g[red_max] - b[red_max]) / diff[red_max]) %% 6

            # Green is max
            green_max <- non_zero_diff & (g == max_val)
            h[green_max] <- (b[green_max] - r[green_max]) / diff[green_max] + 2

            # Blue is max
            blue_max <- non_zero_diff & (b == max_val)
            h[blue_max] <- (r[blue_max] - g[blue_max]) / diff[blue_max] + 4

            # Normalize hue to [0, 1]
            h <- h / 6
      }

      rbind(h, s, l)
}

#' Convert HSL to RGB color space
#'
#' @param hsl_matrix 3xN matrix with HSL values (H in 0-1, S in 0-1, L in 0-1)
#' @return 3xN matrix with RGB values in 0-1 range
#' @keywords internal
#' @noRd
hsl2rgb <- function(hsl_matrix) {
      if (!is.matrix(hsl_matrix) || nrow(hsl_matrix) != 3) {
            stop("hsl_matrix must be a 3xN matrix")
      }

      h <- hsl_matrix[1, ]
      s <- hsl_matrix[2, ]
      l <- hsl_matrix[3, ]

      q <- ifelse(l < 0.5, l * (1 + s), l + s - l * s)
      p <- 2 * l - q

      hue_to_rgb <- function(t) {
            t <- ifelse(t < 0, t + 1, t)
            t <- ifelse(t > 1, t - 1, t)
            ifelse(t < 1/6, p + (q - p) * 6 * t,
                   ifelse(t < 1/2, q,
                          ifelse(t < 2/3, p + (q - p) * (2/3 - t) * 6, p)))
      }

      gray <- s == 0
      r <- ifelse(gray, l, hue_to_rgb(h + 1/3))
      g <- ifelse(gray, l, hue_to_rgb(h))
      b <- ifelse(gray, l, hue_to_rgb(h - 1/3))

      rbind(r, g, b)
}

#' Convert between sRGB and linear RGB
#'
#' @param x Numeric values in 0-1.
#' @return Numeric values of the same shape.
#' @keywords internal
#' @noRd
srgb_to_linear <- function(x) {
      ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055)^2.4)
}

#' @noRd
linear_to_srgb <- function(x) {
      ifelse(x <= 0.0031308, 12.92 * x, 1.055 * x^(1 / 2.4) - 0.055)
}

#' Convert between linear RGB and OKLab
#'
#' Björn Ottosson's OKLab, a perceptual color space in which lightness and
#' hue are well separated, so changing lightness keeps hue.
#'
#' @param x Matrix with 3 columns: linear red, green, blue; or OKLab L, a, b.
#' @return Matrix with 3 columns.
#' @keywords internal
#' @noRd
linear_to_oklab <- function(x) {
      m1 <- matrix(c(0.4122214708, 0.5363325363, 0.0514459929,
                     0.2119034982, 0.6806995451, 0.1073969566,
                     0.0883024619, 0.2817188376, 0.6299787005), 3, byrow = TRUE)
      m2 <- matrix(c(0.2104542553, 0.7936177850, -0.0040720468,
                     1.9779984951, -2.4285922050, 0.4505937099,
                     0.0259040371, 0.7827717662, -0.8086757660), 3, byrow = TRUE)
      lms <- x %*% t(m1)
      lms <- sign(lms) * abs(lms)^(1 / 3)
      lms %*% t(m2)
}

#' @noRd
oklab_to_linear <- function(x) {
      m2_inv <- matrix(c(1, 0.3963377774, 0.2158037573,
                         1, -0.1055613458, -0.0638541728,
                         1, -0.0894841775, -1.2914855480), 3, byrow = TRUE)
      m1_inv <- matrix(c(4.0767416621, -3.3077115913, 0.2309699292,
                         -1.2684380046, 2.6097574011, -0.3413193965,
                         -0.0041960863, -0.7034186147, 1.7076147010), 3, byrow = TRUE)
      (x %*% t(m2_inv))^3 %*% t(m1_inv)
}

#' Largest chroma scaling that keeps OKLab colors within the sRGB gamut
#'
#' @param L Numeric vector of OKLab lightness.
#' @param ab Matrix with 2 columns of OKLab a and b.
#' @return Numeric vector of factors in `[0, 1]` by which to scale `ab`.
#' @keywords internal
#' @noRd
oklab_chroma_in_gamut <- function(L, ab) {
      in_gamut <- function(k) {
            lin <- oklab_to_linear(cbind(L, ab * k))
            rowSums(lin < -1e-6 | lin > 1 + 1e-6) == 0
      }
      lo <- rep(0, length(L))
      hi <- rep(1, length(L))
      fits <- in_gamut(hi)
      lo[fits] <- 1
      for (i in seq_len(16)) {
            mid <- (lo + hi) / 2
            ok <- in_gamut(mid)
            lo[ok & !fits] <- mid[ok & !fits]
            hi[!ok & !fits] <- mid[!ok & !fits]
      }
      lo
}

#' Hex colors from linear RGB
#'
#' @param lin Matrix with 3 columns of linear red, green, blue.
#' @return Character vector of opaque hex colors.
#' @keywords internal
#' @noRd
linear_to_hex <- function(lin) {
      srgb <- linear_to_srgb(pmax(0, pmin(1, lin)))
      srgb <- matrix(pmax(0, pmin(1, srgb)), ncol = 3)
      rgb(srgb[, 1], srgb[, 2], srgb[, 3])
}
