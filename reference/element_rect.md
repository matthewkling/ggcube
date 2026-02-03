# Rectangle theme element with alpha support

This function extends
[`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
to support transparency via an alpha parameter. It maintains full
backward compatibility with the original `element_rect()` function while
enabling transparent panel styling, which is particularly useful for
foreground panels in 3D plots.

## Usage

``` r
element_rect(
  fill = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  color = NULL,
  inherit.blank = FALSE,
  size = lifecycle::deprecated(),
  alpha = NULL
)
```

## Arguments

- fill:

  Fill color for the rectangle. Use `NA` for no fill.

- colour, color:

  Line color for the rectangle border. Use `NA` for no border.

- linewidth:

  Line width for the rectangle border.

- linetype:

  Line type for the rectangle border (e.g., "solid", "dashed").

- inherit.blank:

  Should this element inherit from `element_blank`?

- size:

  **\[deprecated\]** Use `linewidth` instead.

- alpha:

  Transparency level for the rectangle fill, ranging from 0 (completely
  transparent) to 1 (completely opaque). Particularly useful for styling
  foreground panels in 3D plots to create layered visual effects.

## Value

A theme element object that can be used in
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
specifications.

## See also

[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)
for the original function,
[`coord_3d`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems that utilize foreground panels,
[cube_theming](https://matthewkling.github.io/ggcube/reference/cube_theming.md)
for details on panel/gridline/axis label styling.

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
