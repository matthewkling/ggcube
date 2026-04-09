# 3D line segments

`geom_segment_3d()` and `stat_segment_3d()` draw line segments in 3D
space with automatic depth-based linewidth scaling and proper depth
sorting. Each segment is defined by start coordinates (x, y, z) and end
coordinates (xend, yend, zend).

## Usage

``` r
geom_segment_3d(
  mapping = NULL,
  data = NULL,
  stat = StatSegment3D,
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

stat_segment_3d(
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
  Requires x, y, z for start coordinates and xend, yend, zend for end
  coordinates.

- data:

  The data to be displayed in this layer.

- stat:

  The statistical transformation to use on the data. Defaults to
  StatSegment3D.

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
  When `TRUE` (default), segments closer to the viewer appear thicker,
  and segments farther away appear thinner.

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

`geom_segment_3d()` understands the following aesthetics:

- **x, y, z**: Start coordinates (required)

- **xend, yend, zend**: End coordinates (required)

- `colour`: Line color

- `linewidth`: Line width (gets depth-scaled when `scale_depth = TRUE`)

- `linetype`: Line type

- `alpha`: Transparency

## See also

[`geom_path_3d()`](https://matthewkling.github.io/ggcube/reference/geom_path_3d.md)
for connected paths,
[`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
for 2D segments,
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems.

## Examples

``` r
# Basic 3D segments
ggplot(sphere_points,
      aes(x, y, z, xend = 0, yend = 0, zend = 0)) +
  geom_segment_3d() +
  coord_3d()
#> Error in geom_segment_3d(): Problem while converting geom to grob.
#> ℹ Error occurred in the 1st layer.
#> Caused by error in `mutate()`:
#> ℹ In argument: `light = ifelse(.backface, light * scl + off, light)`.
#> Caused by error in `rep()`:
#> ! attempt to replicate an object of type 'closure'

# 3D vector field
data <- expand.grid(x = -1:2, y = -1:2, z = -1:2)
data2 <- data + seq(-.5, .5, length.out = length(as.matrix(data)))
data <- cbind(data, setNames(data2, c("x2", "y2", "z2")))
ggplot(data, aes(x, y, z,
      xend = x2, yend = y2, zend = z2, color = x)) +
  geom_segment_3d(arrow = arrow(length = unit(0.1, "inches"),
                  type = "closed", angle = 15),
                  linewidth = .5) +
  coord_3d()
#> Error in geom_segment_3d(arrow = arrow(length = unit(0.1, "inches"), type = "closed",     angle = 15), linewidth = 0.5): Problem while converting geom to grob.
#> ℹ Error occurred in the 1st layer.
#> Caused by error in `mutate()`:
#> ℹ In argument: `light = ifelse(.backface, light * scl + off, light)`.
#> Caused by error in `rep()`:
#> ! attempt to replicate an object of type 'closure'
```
