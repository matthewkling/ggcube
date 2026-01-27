# 3D columns from grid data

Creates 3D columns (rectangular prisms) from grid data in which x and y
fall on a regular grid. Works with both complete and sparse grid data.
Each data point becomes a rectangular 3D column extending from a base
level to the data value.

## Usage

``` r
geom_col_3d(
  mapping = NULL,
  data = NULL,
  stat = StatCol3D,
  position = "identity",
  ...,
  width = 1,
  faces = "all",
  zmin = NULL,
  light = NULL,
  cull_backfaces = TRUE,
  sort_method = NULL,
  scale_depth = TRUE,
  force_convex = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_col_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomPolygon3D,
  position = "identity",
  ...,
  width = 1,
  faces = "all",
  zmin = NULL,
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

  The statistical transformation to use on the data. Defaults to
  `StatCol3D`.

- position:

  Position adjustment, defaults to "identity". To collapse the result
  onto one 2D surface, use
  [`position_on_face()`](https://matthewkling.github.io/ggcube/reference/position_on_face.md).

- ...:

  Other arguments passed on to the the layer function (typically
  GeomPolygon3D), such as aesthetics like `colour`, `fill`, `linewidth`,
  etc.

- width:

  Numeric value controlling box width as a fraction of grid spacing.
  Default is 1.0 (volumes touch each other). Use 0.8 for small gaps, 1.2
  for overlap. Grid spacing is determined automatically using
  [`resolution()`](https://ggplot2.tidyverse.org/reference/resolution.html).

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

- zmin:

  Base level for all columns. When provided as a parameter, overrides
  any `zmin` aesthetic mapping. If `NULL` (the default), uses the `zmin`
  aesthetic if mapped, otherwise defaults to 0.

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

This is analogous to
[`ggplot2::geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)
for 3D plots. For automatic counting or binning, see
[`geom_bar_3d()`](https://matthewkling.github.io/ggcube/reference/geom_bar_3d.md).

Note that column geometries often require pairwise depth sorting for
correct rendering. This is the default for smaller data sets, but not
for larger data sets due to compute speed; in those cases you may wish
to manually specify `sort_method = "pairwise"` for good results.

## Aesthetics

`stat_col_3d()` requires the following aesthetics:

- **x**: X coordinate (grid position)

- **y**: Y coordinate (grid position)

- **z**: Z coordinate (column top height)

And optionally understands:

- **zmin**: Base level for each column (can be overridden by the `zmin`
  parameter)

## Computed variables

- `normal_x`, `normal_y`, `normal_z`: Face normal components

- `col_id`: Sequential column number

- `face_type`: Face name ("zmax", "xmin", etc.)

## See also

[`geom_bar_3d()`](https://matthewkling.github.io/ggcube/reference/geom_bar_3d.md)
for automatic counting/binning,
[`stat_surface_3d()`](https://matthewkling.github.io/ggcube/reference/geom_surface_3d.md)
for smooth surface rendering,
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems,
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
for lighting specifications, GeomPolygon3D for the default geometry.

## Examples

``` r
# Basic 3D bar chart from regular grid
# (columns extend from z=0 by default)
d <- expand.grid(x = 1:5, y = 1:5)
d$z <- d$x + d$y + rnorm(25, 0, 0.5)
ggplot(d, aes(x, y, z)) +
  geom_col_3d() +
  coord_3d()


# Set uniform base level using `zmin` parameter
ggplot(d, aes(x, y, z)) +
  geom_col_3d(aes(fill = z), color = "white",
              zmin = 5) +
  coord_3d()


# Set variable base levels using `zmin` aesthetic
d$base_level <- runif(nrow(d), -5, 1)
ggplot(d, aes(x, y, z = z, zmin = base_level)) +
  geom_col_3d(color = "black") +
  coord_3d()


# Show only a subset of column faces
ggplot(d, aes(x, y, z)) +
  geom_col_3d(faces = c("zmax", "ymin"),
    cull_backfaces = FALSE,
    fill = "steelblue", color = "black") +
  coord_3d()


# With gaps between columns
ggplot(d, aes(x, y, z)) +
  geom_col_3d(color = "black", width = 0.6) +
  coord_3d()

```
