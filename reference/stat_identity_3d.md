# 3D-aware identity transformation

This stat performs identity transformation (passes data through
unchanged) while properly handling discrete scales and lighting
specifications for 3D coordinate systems. It also converts group values
to hierarchical format to prevent reordering withing groups during depth
sorting.

## Usage

``` r
stat_identity_3d(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  light = NULL,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://matthewkling.github.io/ggcube/reference/aes.md).

- data:

  The data to be displayed in this layer.

- geom:

  The geometric object to use display the data.

- position:

  Position adjustment, defaults to "identity".

- na.rm:

  If `FALSE`, missing values are removed with a warning.

- show.legend:

  Logical indicating whether this layer should be included in legends.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

- light:

  A lighting specification object created by
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
  (see that function for details), or `NULL` to disable shading. Specify
  plot-level lighting in
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  and layer-specific lighting in `geom_*3d()` functions.

- ...:

  Other arguments passed on to geom layer.

## Value

A `Layer` object that can be added to a ggplot.

## Details

This stat is primarily intended for use with 3D geoms that need discrete
scale or lighting support, following the same pattern as other ggcube
stats.

## Computed variables

- `x_raw`, `y_raw`, `z_raw`: Original values before discrete-to-numeric
  conversion

- `group`: Converted to hierarchical format (e.g., "1\_\_group",
  "2\_\_group") for proper depth sorting.

## See also

[`geom_point_3d()`](https://matthewkling.github.io/ggcube/reference/geom_point_3d.md),
[`geom_polygon_3d()`](https://matthewkling.github.io/ggcube/reference/geom_polygon_3d.md)
which use this stat for discrete scale support.
