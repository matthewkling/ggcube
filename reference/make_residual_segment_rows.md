# Create segment rows connecting data points to a surface

For each data point at (x, y, z), creates a two-row segment group
connecting the point to the surface at (x, y, fitted). Used for residual
lines in geom_smooth_3d. Aesthetic columns are stripped so annotations
don't contaminate scale training; styling is applied at render time via
override parameters.

## Usage

``` r
make_residual_segment_rows(data, fitted, group_prefix = "residual_line")
```

## Arguments

- data:

  Data frame with x, y, z columns (raw data points).

- fitted:

  Numeric vector of fitted z values at each data point.

- group_prefix:

  Prefix for segment group IDs.

## Value

Data frame with two rows per segment, tagged with `.prim = "segment"`.
