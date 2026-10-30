# Lighting ramps

**\[experimental\]**

## Usage

``` r
ramp_smooth(neutral = 45)

ramp_hard(neutral = 45)

ramp_steps(n = 4, neutral = 45, breaks = NULL)
```

## Arguments

- neutral:

  Angle in degrees between a face and the direction toward the light at
  which colors are unchanged. Must be greater than 0 and less than 180,
  or less than 90 for `ramp_hard()`. Default is 45.

- n:

  Number of bands, at least 2. Default is 4.

- breaks:

  Alternative to `n` and `neutral`: angles in degrees between a face and
  the direction toward the light at which one band ends and the next
  begins, increasing, greater than 0 and less than 180. Faces at a break
  fall in the darker band. For example, `breaks = 90` gives a fully
  highlighted lit side and a fully shadowed unlit side, and
  `breaks = c(60, 90)` adds a band of unchanged color between them.

## Value

A ramp object, for the `ramp` argument of
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md).

## Details

A ramp decides how brightness changes as a surface turns away from the
light, for use as the `ramp` argument of
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md).
Each ramp keeps colors unchanged on faces at `neutral` degrees to the
light, and moves faces turned further toward the light toward full
highlight, and faces turned further away toward full shadow. How far
toward full highlight and shadow colors go is set by the `highlight` and
`shadow` arguments of
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md).

- `ramp_smooth()` changes brightness gradually, reaching full highlight
  on faces that face the light and full shadow on faces that face
  directly away from it. This is the default.

- `ramp_hard()` is like `ramp_smooth()` toward the light, but faces at
  90 degrees or more from the light are fully shadowed, giving a sharp
  edge between lit and unlit sides.

- `ramp_steps()` divides faces into flat bands of brightness, for a
  cartoon-like look. The bands run evenly from full highlight on the
  band nearest the light to full shadow on the band farthest from it, so
  with an odd number of bands the middle one shows colors unchanged.
  Bands are either `n` equal divisions of the brightness of
  `ramp_smooth()`, or are divided at the angles given by `breaks`.
  Specular highlights (see
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md))
  are flat spots too.

These can also be given to
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md) by
name, as `"smooth"`, `"hard"`, or `"steps"`, to use their defaults.

## See also

[`light()`](https://matthewkling.github.io/ggcube/reference/light.md),
and
[`tone_lightness()`](https://matthewkling.github.io/ggcube/reference/tones.md)
for tones, which decide what lighter and darker look like.

## Examples

``` r
p <- ggplot(sphere_points, aes(x, y, z)) +
  geom_hull_3d(fill = "steelblue", color = "steelblue") +
  coord_3d(scales = "fixed")

p + light(ramp_smooth())


# base color on faces closer to facing the light, for darker overall shading
p + light(ramp_smooth(neutral = 30))


p + light(ramp_hard())


p + light(ramp_steps(n = 3))


# bands divided at chosen angles from the light
p + light(ramp_steps(breaks = c(60, 90)))
```
