# Using themes to style 3D panels and axis labels

In ggcube, standard ggplot2 themes generally influence 3D plots as
expected, including adding complete themes like
[`ggplot2::theme_dark()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
and modifying theme elements like
`theme(panel.background = element_rect(fill = "darkblue")`. However,
ggcube also provides additional theme elements that control 3D-specific
styling of panels and labels.

## Text elements

- `axis.text.z`: Styling for z-axis tick labels (inherits from
  `axis.text`)

- `axis.title.z`: Styling for z-axis title (inherits from `axis.title`)

- `axis.text`, `axis.text`: Standard styling with
  [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html).

Use `element_text(margin = margin(...))` to adjust text padding, with
left/right margins affecting axis text and top/bottom margins affecting
axis titles; since placement and justification of these elements varies
dynamically, no distinction is made between left and right margins, or
between top and bottom margins – you can set either, and the maximum of
the two will be used.

## Panel elements

- `panel.foreground`: Styling for cube faces rendered in front of data
  (inherits from `panel.background`). Uses `element_rect(alpha = .2)` by
  default, to prevent foreground panels from obscuring the data.

- `panel.border.foreground`: Styling for cube faces rendered in front of
  data (inherits from `panel.border`)

- `panel.grid.foreground`: Styling for grid lines on foreground faces
  (inherits from `panel.grid`)

- `panel.grid.major.foreground`: Major grid lines on foreground faces
  (inherits from `panel.grid.foreground`)

Background panels use standard `panel.background`, `panel.border`,
`panel.grid`, etc., while foreground panels use the `*.foreground`
variants listed above. Since the foreground elements inherit from the
standard background and grid elements, you can use `panel.background`,
etc. to style both background and foreground faces simultaneously.

## Enhanced elements

- [`element_rect()`](https://matthewkling.github.io/ggcube/reference/element_rect.md)
  extends
  [`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
  by adding an `alpha` parameter for transparency effects. This is
  particularly useful for `panel.foreground` components that sit in
  front of the data.

## Examples

``` r
# example code
p <- ggplot(sphere_points, aes(x, y, z)) +
  geom_hull_3d() +
  coord_3d(panels = "all") +
  theme(panel.background = element_rect(color = "black"),
          panel.border = element_rect(color = "black"),
          panel.foreground = element_rect(alpha = .3),
          panel.grid.foreground = element_line(color = "gray", linewidth = .25),
          axis.text = element_text(color = "darkblue"),
          axis.text.z = element_text(color = "darkred"),
          axis.title = element_text(margin = margin(t = 30)), # add padding
          axis.title.x = element_text(color = "magenta"))
```
