# Transform 3D points using rotation and optional perspective with viewpoint distance

Transform 3D points using rotation and optional perspective with
viewpoint distance

## Usage

``` r
transform_3d_standard(
  data,
  proj = list(pitch = 0, roll = 0, yaw = 0, persp = TRUE, dist = 2)
)
```

## Arguments

- data:

  Data frame with x, y, z columns (in standard -0.5, 0.5 domain)

- proj:

  A list of projection parameters

## Value

Data frame with transformed coordinates, depth for sorting, and
depth_scale for size scaling
