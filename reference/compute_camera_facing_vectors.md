# Compute per-point camera facing vectors

Compute per-point camera facing vectors

## Usage

``` r
compute_camera_facing_vectors(anchors, spec, coord = NULL)
```

## Arguments

- anchors:

  Data frame with x, y, z columns for anchor positions

- spec:

  A camera_facing_spec object

- coord:

  Optional coord_3d object for proper axis scaling

## Value

Matrix with one row per anchor, columns for facing x, y, z
