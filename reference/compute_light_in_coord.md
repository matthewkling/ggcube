# Compute lighting from standardized coordinates in coord_3d

Compute lighting from standardized coordinates in coord_3d

## Usage

``` r
compute_light_in_coord(
  data,
  standardized_coords,
  scale_ranges,
  scales,
  ratio,
  proj = NULL
)
```

## Arguments

- data:

  Original data frame with lighting_spec column

- standardized_coords:

  Data frame with standardized x, y, z coordinates

- scale_ranges:

  List with x, y, z scale ranges

- scales:

  Character, either "free" or "fixed"

- ratio:

  Numeric vector of length 3

- proj:

  List with pitch, roll, yaw, persp, dist (projection parameters)

## Value

data frame with lighting values and normal components added
