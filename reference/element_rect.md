# Enhanced rectangle element with alpha support

This function extends
[`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
to support transparency via an `alpha` parameter, which is attached to
the returned element as an R attribute (`"ggcube_alpha"`). The attribute
survives ggplot2's theme inheritance machinery, so the alpha value set
here is available to the renderer at draw time. The `alpha` parameter is
consumed only by
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)'s
panel rendering and has no effect on other theme elements.

## Usage

``` r
element_rect(fill = NULL, alpha = NA, ...)
```

## Arguments

- fill:

  Fill colour for the rectangle.

- alpha:

  Transparency level applied to the fill of foreground or background
  panels in
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md),
  ranging from 0 (completely transparent) to 1 (completely opaque). For
  `panel.foreground`, the alpha resolves in order: an explicit value
  here, then alpha inherited from `panel.background`, then a default of
  0.2. For `panel.background`, an explicit value is used directly;
  otherwise the panel is fully opaque. Has no effect on other theme
  elements or in plots without a 3D coordinate system.

- ...:

  Additional arguments passed to
  [`element_rect`](https://ggplot2.tidyverse.org/reference/element.html),
  such as `colour`, `linewidth`, `linetype`, and `inherit.blank`.

## Value

A
[`ggplot2::element_rect`](https://ggplot2.tidyverse.org/reference/element.html)
theme element with an attached `"ggcube_alpha"` attribute.

## See also

[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)
for the original function,
[`coord_3d`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems that utilize foreground panels

## Examples

``` r
# Basic 3D plot with semi-transparent foreground panels
ggplot(mountain, aes(x, y, z)) +
  stat_surface_3d(fill = "darkblue", color = "lightblue", linewidth = .1) +
  coord_3d(panels = c("background", "ymin")) +
  theme(panel.foreground = element_rect(alpha = 0.6))


# Completely transparent foreground panels
ggplot(mtcars, aes(mpg, wt, qsec)) +
  geom_point() +
  coord_3d(panels = "all") +
  theme(panel.border = element_rect(color = "black"),
        panel.foreground = element_rect(fill = "blue", alpha = 0))

```
