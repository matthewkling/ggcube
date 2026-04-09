# Create point rows from raw data for mixed-geometry rendering

Takes raw data points and prepares them for inclusion alongside polygon
surfaces in a mixed-geometry layer. Assigns unique group IDs and sets
`.prim = "point"`. Aesthetic columns are stripped so that annotation
points don't contaminate scale training; styling is applied at render
time in the geom's `draw_panel` via override parameters.

## Usage

``` r
make_point_rows(data, group_prefix = "data_point")
```

## Arguments

- data:

  Data frame with x, y, z and aesthetic columns.

- group_prefix:

  Prefix for point group IDs.

## Value

Data frame with one row per point, tagged with `.prim = "point"`.
