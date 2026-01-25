# Calculate per-face light directions for positional lighting

Calculate per-face light directions for positional lighting

## Usage

``` r
calculate_positional_light_directions(face_centers, light_position)
```

## Arguments

- face_centers:

  Matrix with 3 columns (x, y, z face center coordinates)

- light_position:

  Numeric vector of length 3 (x, y, z light position)

## Value

Matrix with 3 columns (normalized light direction vectors)
