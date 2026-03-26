# Transform pre-computed normals from data space to standardized lighting space

Normals transform under non-uniform scaling by the inverse-transpose of
the scaling matrix. For the diagonal scaling used in standardization,
this means scaling each component by range_width / effective_ratio, then
renormalizing.

## Usage

``` r
transform_normals_to_standard(
  normals,
  scale_ranges,
  scales,
  ratio,
  anchor,
  proj = NULL
)
```

## Arguments

- normals:

  Matrix with 3 columns (normal_x, normal_y, normal_z) in data space

- scale_ranges:

  List with x, y, z scale ranges

- scales:

  Character, either "free" or "fixed"

- ratio:

  Numeric vector of length 3

- anchor:

  Character, "scene" or "camera"

- proj:

  Projection parameters (needed for camera anchor rotation)

## Value

Matrix with 3 columns of transformed normals
