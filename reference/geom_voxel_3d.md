# 3D voxel visualization from sparse 3D data

Creates 3D voxel visualizations from sparse 3D point data. Each data
point becomes a fixed-size cube centered on its coordinates. Useful for
volumetric data and 3D pixel art.

## Usage

``` r
geom_voxel_3d(
  mapping = NULL,
  data = NULL,
  stat = StatVoxel3D,
  position = "identity",
  ...,
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

stat_voxel_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomPolygon3D,
  position = "identity",
  ...,
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

  The statistical transformation to use on the data. Defaults to
  `StatVoxel3D`.

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

Note that voxel geometries sometimes require pairwise depth sorting for
correct rendering. This is the default for smaller data sets, but not
for larger data sets due to compute speed; in those cases you may wish
to manually specify `sort_method = "pairwise"`.

## Aesthetics

Voxel 3D requires the following aesthetics:

- **x**: X coordinate (voxel center position)

- **y**: Y coordinate (voxel center position)

- **z**: Z coordinate (voxel center position)

## Computed variables

- `normal_x`, `normal_y`, `normal_z`: Face normal components

- `voxel_id`: Sequential voxel number

- `face_type`: Face name ("zmax", "xmin", etc.)

## See also

[`stat_pillar_3d()`](https://matthewkling.github.io/ggcube/reference/geom_pillar_3d.md)
for variable-height columns,
[`stat_surface_3d()`](https://matthewkling.github.io/ggcube/reference/geom_surface_3d.md)
for smooth surfaces,
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems,
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
for lighting specifications, GeomPolygon3D for the default geometry.

## Examples

``` r
# Sparse 3D voxel data
voxel_data <- data.frame(
  x = round(rnorm(100, 0, 2)),
  y = round(rnorm(100, 0, 2)),
  z = round(rnorm(100, 0, 2))
)

p <- ggplot(voxel_data, aes(x, y, z)) + coord_3d()

# Basic 3D voxel plot
p + geom_voxel_3d(fill = "steelblue")


# With aesthetic fill
p + stat_voxel_3d(aes(fill = z)) +
  geom_fill_viridis_c() + guides(fill = guide_colorbar_3d())
#> Error in geom_fill_viridis_c(): could not find function "geom_fill_viridis_c"

# Show only visible faces for performance
p + geom_voxel_3d(faces = c("zmax", "ymin", "xmin"))

```
