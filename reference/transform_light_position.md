# Convert light position from data units to visual space units

Convert light position from data units to visual space units

## Usage

``` r
transform_light_position(position, scale_ranges, scales, ratio)
```

## Arguments

- position:

  Numeric vector of length 3 (x, y, z in data coordinates)

- scale_ranges:

  List with x, y, z scale ranges

- scales:

  Character, either "free" or "fixed"

- ratio:

  Numeric vector of length 3

## Value

Position vector in visual space, or NULL if position is NULL
