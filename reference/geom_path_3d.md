# 3D paths connecting observations

Connects observations in 3D space in the order they appear in the data.
It converts path data into individual segments for proper depth sorting
while maintaining the appearance of connected paths. Each path is
divided into segments that can be depth-sorted independently.

## Usage

``` r
geom_path_3d(
  mapping = NULL,
  data = NULL,
  stat = StatPath3D,
  position = "identity",
  ...,
  sort_method = "painter",
  scale_depth = TRUE,
  arrow = NULL,
  lineend = "butt",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_path_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomSegment3D,
  position = "identity",
  ...,
  sort_method = "painter",
  scale_depth = TRUE,
  arrow = NULL,
  lineend = "butt",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://matthewkling.github.io/ggcube/reference/aes.md).
  Requires x, y, z coordinates. Grouping aesthetics determine separate
  paths.

- data:

  The data to be displayed in this layer.

- stat:

  The statistical transformation to use on the data. Defaults to
  StatPath3D.

- position:

  Position adjustment, defaults to "identity".

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- sort_method:

  Character indicating algorithm used to determine the order in which
  elements are rendered. This controls depth sorting for all geometry
  types within a layer, including polygons, points, segments, and text.
  Default varies by geometry type.

  - `"painter"`: Elements are sorted by the mean depth (distance from
    viewer after rotation) of their vertices. This is fast, but can give
    incorrect results when primitives overlap in screen space at
    different depths.

  - `"pairwise"`: A more intensive sorting algorithm that compares every
    pair of elements to determine occlusion order. Uses type-specific
    geometric tests: polygon overlap detection for polygon-polygon
    pairs, point-in-polygon tests for polygon-point pairs, line clipping
    for polygon-segment pairs, and line intersection for segment-segment
    pairs. When elements are coplanar, smaller primitives (points,
    segments) render on top of larger ones (polygons). Slower but more
    accurate.

  - `"auto"`: Uses pairwise if the data has fewer than 500 rows, and
    painter otherwise.

- scale_depth:

  Logical indicating whether to apply depth-based scaling to linewidth.
  When `TRUE` (default), path segments closer to the viewer appear
  thicker, and segments farther away appear thinner.

- arrow:

  Specification for arrow heads, created by
  [`arrow()`](https://rdrr.io/r/grid/arrow.html).

- lineend:

  Line end style, one of "round", "butt", "square".

- na.rm:

  If `FALSE`, missing values are removed with a warning.

- show.legend:

  Logical indicating whether this layer should be included in legends.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

- geom:

  The geometric object used to display the data. Defaults to
  GeomSegment3D.

## Value

A `Layer` object that can be added to a ggplot.

## Aesthetics

`geom_path_3d()` understands the following aesthetics:

- **x, y, z**: Coordinates (required)

- `group`: Grouping variable to create separate paths

- `colour`: Line color

- `linewidth`: Line width (gets depth-scaled when `scale_depth = TRUE`)

- `linetype`: Line type

- `alpha`: Transparency

## Computed variables for StatSegment3D

- `x`, `y`, `z`: Start coordinates of each segment

- `xend`, `yend`, `zend`: End coordinates of each segment

- `group`: Hierarchical group identifier preserving original grouping

## Grouping

Multiple paths are created based on grouping aesthetics (group, colour,
etc.). Each group forms a separate path, and segments from different
paths can be interleaved during depth sorting for proper 3D rendering.

## See also

[`geom_segment_3d()`](https://matthewkling.github.io/ggcube/reference/geom_segment_3d.md)
for individual segments,
[`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
for 2D paths,
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems.

## Examples

``` r
library(ggplot2)

x <- seq(0, 20*pi, pi/16)
spiral <- data.frame(
  x = x,
  y = sin(x),
  z = cos(x))

# Basic path
ggplot(spiral, aes(x, y, z)) +
  geom_path_3d() +
  coord_3d()
#> Error in geom_path_3d(): Problem while converting geom to grob.
#> ℹ Error occurred in the 1st layer.
#> Caused by error in `mutate()`:
#> ℹ In argument: `light = ifelse(.backface, light * scl + off, light)`.
#> Caused by error in `rep()`:
#> ! attempt to replicate an object of type 'closure'

# With aesthetic coloring
ggplot(spiral, aes(x, y, z, color = y)) +
  geom_path_3d(linewidth = 1, lineend = "round") +
  coord_3d() +
  scale_color_gradientn(colors = c("red", "purple", "blue"))
#> Error in geom_path_3d(linewidth = 1, lineend = "round"): Problem while converting geom to grob.
#> ℹ Error occurred in the 1st layer.
#> Caused by error in `mutate()`:
#> ℹ In argument: `light = ifelse(.backface, light * scl + off, light)`.
#> Caused by error in `rep()`:
#> ! attempt to replicate an object of type 'closure'

# With grouping
ggplot(spiral, aes(x, y, z, color = x > 30)) +
  geom_path_3d(linewidth = 1, lineend = "round") +
  coord_3d()
#> Error in geom_path_3d(linewidth = 1, lineend = "round"): Problem while converting geom to grob.
#> ℹ Error occurred in the 1st layer.
#> Caused by error in `mutate()`:
#> ℹ In argument: `light = ifelse(.backface, light * scl + off, light)`.
#> Caused by error in `rep()`:
#> ! attempt to replicate an object of type 'closure'
```
