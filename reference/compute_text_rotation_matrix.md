# Compute text rotation matrix with intuitive "up" direction

Creates a rotation matrix that orients text to face a given direction,
with angle=0 producing the most natural baseline orientation (horizontal
when possible).

## Usage

``` r
compute_text_rotation_matrix(facing_normal, angle)
```

## Arguments

- facing_normal:

  Unit vector for direction text should face

- angle:

  Additional rotation around facing axis (degrees)

## Value

3x3 rotation matrix
