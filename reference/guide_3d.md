# Color guides showing lighting effects

Creates color guides that show shading variation as gradients within
each color. Shows the full range of colors visible when shading is
enabled in 3D plots.

## Usage

``` r
guide_colorbar_3d(reverse_shade = FALSE, shade_range = c(-0.5, 0.5), ...)

guide_legend_3d(reverse_shade = FALSE, shade_range = c(-0.5, 0.5), ...)
```

## Arguments

- reverse_shade:

  Logical. If TRUE, reverses the lighting gradient direction. By
  default, shadows are placed on the left, or on the bottom for
  horizontal colorbars.

- shade_range:

  Length-2 numeric vector in the range -1 to 1, giving the limits of the
  shading gradient. -1 is full shadow, 0 the colors as specified, and 1
  full highlight, as set by the light's `shadow` and `highlight`.
  Default is `c(-.5, .5)`.

- ...:

  Additional arguments passed to
  [`guide_colorbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)
  or
  [`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

## Value

A guide object that displays shading effects

## Details

When lighting applies to a layer that maps `fill` or `colour`, ggcube
uses these guides by default, in place of
[`ggplot2::guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)
and
[`ggplot2::guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).
Call them explicitly to change their options, or use e.g.
`guides(fill = "colourbar")` for an unshaded guide. Guides given to a
scale's `guide` argument as guide objects are left as they are.

When fill and color aesthetics map to the same variable (e.g.,
`aes(fill = z, color = z)`), ggplot2 creates a shared scale with a
single guide. In this case, use `guides(fill = guide_*_3d())` to apply
shading, **not** `guides(color = guide_*_3d())`, even if your layer uses
the color aesthetic. Only use the color guide when color and fill map to
different variables and you want separate guides for each.

## Examples

``` r
# 3D guides are used by default for lit layers
ggplot(mountain, aes(x, y, z, fill = z)) +
   stat_surface_3d() +
   scale_fill_gradientn(colors = c("tomato", "dodgerblue")) +
   coord_3d()


# a horizontal colorbar showing the full range of shading
ggplot(mountain, aes(x, y, z, fill = z)) +
   stat_surface_3d() +
   guides(fill = guide_colorbar_3d(shade_range = c(-1, 1), direction = "horizontal")) +
   scale_fill_gradientn(colors = c("tomato", "dodgerblue")) +
   coord_3d() +
   theme(legend.position = "bottom")


# discrete `legend` guide
ggplot(mountain, aes(x, y, z, fill = x > .5, group = 1)) +
   stat_surface_3d(light = light(direction = c(1, 0, 0))) +
   coord_3d()


# an unshaded guide
ggplot(mountain, aes(x, y, z, fill = z)) +
   stat_surface_3d() +
   guides(fill = "colourbar") +
   coord_3d()
```
