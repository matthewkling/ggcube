# Enhanced rectangle element with alpha support

This function extends
[`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
to support transparency via an `alpha` parameter. When both `fill` and
`alpha` are supplied, the alpha level is blended into the fill color
(via [`scales::alpha()`](https://scales.r-lib.org/reference/alpha.html))
before the element is constructed, and a standard
[`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
is returned. The `alpha` parameter is particularly useful for styling
foreground panels in 3D plots to prevent them from obscuring data
layers.

## Usage

``` r
element_rect(fill = NULL, alpha = NULL, ...)
```

## Arguments

- fill:

  Fill colour for the rectangle.

- alpha:

  Transparency level applied to the fill, ranging from 0 (completely
  transparent) to 1 (completely opaque). Has an effect only when `fill`
  is also supplied. Particularly useful for styling foreground panels in
  3D plots to create layered visual effects.

- ...:

  Additional arguments passed to
  [`element_rect`](https://ggplot2.tidyverse.org/reference/element.html),
  such as `color`, `linewidth`, `linetype`, and `inherit.blank`.

## Value

A
[`ggplot2::element_rect`](https://ggplot2.tidyverse.org/reference/element.html)
theme element that can be used in
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
specifications.

## See also

[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)
for the original function,
[`coord_3d`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems that utilise foreground panels

## Examples

``` r
# Basic 3D plot with semi-transparent foreground panels
ggplot(mountain, aes(x, y, z)) +
  stat_surface_3d(fill = "darkblue", color = "lightblue", linewidth = .1) +
  coord_3d(panels = c("background", "ymin")) +
  theme(panel.foreground = element_rect(fill = "white", alpha = 0.6))


# Completely transparent foreground panels
ggplot(mtcars, aes(mpg, wt, qsec)) +
  geom_point() +
  coord_3d(panels = "all") +
  theme(panel.border = element_rect(color = "black"),
        panel.foreground = element_rect(fill = "blue", alpha = 0))

```
