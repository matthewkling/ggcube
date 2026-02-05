# Contours of a 3D surface

Renders a surface as stacked horizontal contour bands. Each contour band
is a polygon placed at its corresponding z-level.

## Usage

``` r
geom_contour_3d(
  mapping = NULL,
  data = NULL,
  stat = "surface_3d",
  position = "identity",
  ...,
  bins = 20,
  binwidth = NULL,
  breaks = NULL,
  cull_backfaces = FALSE,
  sort_method = "pairwise",
  scale_depth = TRUE,
  force_convex = FALSE,
  light = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_contour_3d(
  mapping = NULL,
  data = NULL,
  geom = "contour_3d",
  position = "identity",
  ...,
  bins = 20,
  binwidth = NULL,
  breaks = NULL,
  cull_backfaces = FALSE,
  sort_method = "pairwise",
  scale_depth = TRUE,
  force_convex = FALSE,
  light = NULL,
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

  Point grid data with x, y, z coordinates.

- stat:

  Statistical transformation. Defaults to
  [`stat_surface_3d()`](https://matthewkling.github.io/ggcube/reference/stat_surface_3d.md).

- position:

  Position adjustment, defaults to "identity".

- ...:

  Other arguments passed to the layer.

- bins:

  Number of contour levels. Default is 20. Ignored if `breaks` or
  `binwidth` is provided.

- binwidth:

  Width of each contour band. Overrides `bins` if provided.

- breaks:

  Numeric vector specifying exact contour break points. Overrides both
  `bins` and `binwidth` if provided.

- cull_backfaces, sort_method, force_convex, scale_depth:

  Advanced polygon rendering parameters. See
  [polygon_rendering](https://matthewkling.github.io/ggcube/reference/polygon_rendering.md)
  for details.

- light:

  A lighting specification object created by
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md),`"none"`
  to disable lighting, or `NULL` to inherit plot-level lighting specs
  from the coord. Specify plot-level lighting in
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  and layer-specific lighting in `geom_*3d()` functions.

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Logical indicating whether this layer should be included in legends.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

## Value

A `Layer` object that can be added to a ggplot.

## Details

This geom takes point grid data (like that produced by
[`stat_surface_3d()`](https://matthewkling.github.io/ggcube/reference/stat_surface_3d.md),
[`stat_function_3d()`](https://matthewkling.github.io/ggcube/reference/stat_function_3d.md),
[`stat_smooth_3d()`](https://matthewkling.github.io/ggcube/reference/geom_smooth_3d.md),
or
[`stat_density_3d()`](https://matthewkling.github.io/ggcube/reference/stat_density_3d.md))
and converts it to filled contour polygons using the `isoband` package.

## Aesthetics

`geom_contour_3d()` requires:

- x, y, z:

  Point coordinates forming a regular grid

And understands these additional aesthetics:

- fill:

  Band fill color. For automatic coloring by elevation, use
  `aes(fill = after_stat(z))`. Default is "grey60".

- colour:

  Band border color (default: "grey30")

- alpha:

  Transparency

- linewidth:

  Border width (default: 0.1)

- linetype:

  Border line type

## Computed variables

Each contour band is placed at its corresponding z-level (the upper
boundary of the band). To color by elevation, use
`aes(fill = after_stat(z))`.

## See also

[`geom_surface_3d()`](https://matthewkling.github.io/ggcube/reference/stat_surface_3d.md)
for continuous surface rendering,
[`geom_ridgeline_3d()`](https://matthewkling.github.io/ggcube/reference/geom_ridgeline_3d.md)
for cross-sectional ridgeline rendering,
[`stat_function_3d()`](https://matthewkling.github.io/ggcube/reference/stat_function_3d.md)
for mathematical function surfaces,
[`stat_smooth_3d()`](https://matthewkling.github.io/ggcube/reference/geom_smooth_3d.md)
for fitted model surfaces,
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems,
[`ggplot2::geom_contour_filled()`](https://ggplot2.tidyverse.org/reference/geom_contour.html)
for the 2D equivalent.

## Examples

``` r
# Basic usage with volcano data
ggplot(mountain, aes(x, y, z)) +
      geom_contour_3d(color = "white", fill = "black") +
      coord_3d(light = "none", ratio = c(1.5, 2, 1))


# Map fill to elevation and customize number of levels
ggplot(mountain, aes(x, y, z, fill = after_stat(z))) +
      geom_contour_3d(bins = 12, color = "white") +
      scale_fill_viridis_c() +
      coord_3d(light = "none", ratio = c(1.5, 2, 1))


# Specify exact breaks
ggplot(mountain, aes(x, y, z, fill = after_stat(z))) +
  geom_contour_3d(breaks = seq(0, 200, by = 5)) +
  scale_fill_viridis_c() +
  coord_3d(light = "none")


# With stat_density_3d
ggplot(faithful, aes(eruptions, waiting)) +
  stat_density_3d(geom = "contour_3d",
    sort_method = "pairwise") +
  coord_3d()


# With stat_function_3d
ggplot() +
  stat_function_3d(
    fun = function(x, y) sin(x) * cos(y),
    xlim = c(-pi, pi), ylim = c(-pi, pi),
    geom = "contour_3d",
    bins = 50, color = "black"
  ) +
  scale_fill_viridis_c(option = "B") +
  coord_3d(light = "none")

```
