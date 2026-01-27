# 3D bar chart with automatic counting or binning

Creates 3D bar charts by automatically counting discrete data or binning
continuous data. This is the 3D analogue of
[`ggplot2::geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)
and
[`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).

## Usage

``` r
geom_bar_3d(
  mapping = NULL,
  data = NULL,
  stat = StatBar3D,
  position = "identity",
  ...,
  bins = 10,
  binwidth = NULL,
  drop = TRUE,
  width = 1,
  faces = "all",
  light = NULL,
  cull_backfaces = TRUE,
  sort_method = NULL,
  scale_depth = TRUE,
  force_convex = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_bar_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomPolygon3D,
  position = "identity",
  ...,
  bins = 10,
  binwidth = NULL,
  drop = TRUE,
  width = 1,
  faces = "all",
  light = NULL,
  cull_backfaces = TRUE,
  sort_method = NULL,
  scale_depth = TRUE,
  force_convex = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://matthewkling.github.io/ggcube/reference/aes.md).

- data:

  The data to be displayed in this layer.

- stat:

  The statistical transformation to use. Defaults to `StatBar3D`.

- position:

  Position adjustment, defaults to "identity". To collapse the result
  onto one 2D surface, use
  [`position_on_face()`](https://matthewkling.github.io/ggcube/reference/position_on_face.md).

- ...:

  Other arguments passed on to the the layer function (typically
  GeomPolygon3D), such as aesthetics like `colour`, `fill`, `linewidth`,
  etc.

- bins:

  Number of bins in each dimension. Either a single value (used for both
  x and y) or a vector of length 2 giving `c(bins_x, bins_y)`. Default
  is 10. Ignored for discrete variables.

- binwidth:

  Bin width in each dimension. Either a single value or a vector of
  length 2 giving `c(binwidth_x, binwidth_y)`. If provided, overrides
  `bins`. Ignored for discrete variables.

- drop:

  If `TRUE` (the default), empty bins/combinations are not rendered. If
  `FALSE`, empty bins render as zero-height columns.

- width:

  Column width as a fraction of bin spacing. Either a single value (used
  for both x and y) or a vector of length 2 giving
  `c(width_x, width_y)`. Default is 1.0 (columns touch). Use values less
  than 1 for gaps between columns.

- faces:

  Character vector specifying which faces to render. Options:

  - `"all"` (default): Render all 6 faces

  - `"none"`: Render no faces

  - Vector of face names: `c("zmax", "xmin", "ymax")`, etc.

  Valid face names: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax". Note
  that this setting acts jointly with backface culling, which removes
  faces whose interior faces the viewer – e.g., when
  `cull_backfaces = TRUE` and `faces = "all"` (the default), only front
  faces are rendered.

- light:

  A lighting specification object created by
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
  (see that function for details), or `NULL` to disable shading. Specify
  plot-level lighting in
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  and layer-specific lighting in `geom_*3d()` functions.

- cull_backfaces, sort_method, force_convex, scale_depth:

  Advanced polygon rendering parameters. See
  [polygon_rendering](https://matthewkling.github.io/ggcube/reference/polygon_rendering.md)
  for details.

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Logical indicating whether this layer should be included in legends.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

- geom:

  The geometric object used to display the data. Defaults to
  `GeomPolygon3D`.

## Value

A `Layer` object that can be added to a ggplot.

## Details

The stat automatically detects whether x and y are discrete or
continuous:

- **Both discrete**: Counts occurrences of each (x, y) combination

- **Both continuous**: Performs 2D binning (like a 3D histogram)

- **Mixed**: Bins the continuous axis while preserving discrete groups

For pre-computed bar heights, use
[`geom_col_3d()`](https://matthewkling.github.io/ggcube/reference/geom_col_3d.md)
instead.

## Aesthetics

`stat_bar_3d()` requires the following aesthetics:

- **x**: X coordinate

- **y**: Y coordinate

And optionally understands:

- **weight**: Observation weights for counting

## Computed variables

These variables can be used with
[`after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html)
to map to aesthetics:

- `count`: Number of observations in each bin (default for z)

- `proportion`: Count divided by total count (sums to 1)

- `ncount`: Count scaled to maximum of 1

- `density`: Count divided by (total count × bin area); integrates to 1
  for continuous data

- `ndensity`: Density scaled to maximum of 1

## See also

[`geom_col_3d()`](https://matthewkling.github.io/ggcube/reference/geom_col_3d.md)
for pre-computed heights,
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems,
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
for lighting specifications.

## Examples

``` r
# Discrete x and y: count combinations
d_discrete <- data.frame(
  x = sample(letters[1:4], 200, replace = TRUE),
  y = sample(LETTERS[1:3], 200, replace = TRUE)
)
ggplot(d_discrete, aes(x, y)) +
  geom_bar_3d() +
  coord_3d()


# Continuous x and y: 2D histogram
d_cont <- data.frame(x = rnorm(1000), y = rnorm(1000))
ggplot(d_cont, aes(x, y)) +
  geom_bar_3d() +
  coord_3d()


# Mixed: one discrete, one continuous
d_mixed <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100, 2), rnorm(100, 1, 2), rnorm(100, 0))
)
ggplot(d_mixed, aes(x = group, y = value, fill = group)) +
  geom_bar_3d(bins = 20, width = c(.5, 1)) +
  coord_3d(scales = "fixed", ratio = c(1, 1, .1))


# Use density instead of count for z
ggplot(d_mixed,
      aes(x = group, y = value, z = after_stat(density))) +
  geom_bar_3d(bins = 20, width = c(.5, 1)) +
  coord_3d()


# Show empty bins with drop = FALSE
ggplot(d_cont, aes(x, y)) +
  geom_bar_3d(drop = FALSE) +
  coord_3d()

```
