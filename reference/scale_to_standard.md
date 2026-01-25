# Scale data to standard domain with aspect ratio control

Scale data to standard domain with aspect ratio control

## Usage

``` r
scale_to_standard(values, data_range, scales = "free", ratio = c(1, 1, 1))
```

## Arguments

- values:

  Vector of values to scale (for single axis) OR data frame with x,y,z
  columns (for multi-axis)

- data_range:

  Original range of the data min, max (for single axis) OR list with
  x,y,z scale ranges (for multi-axis)

- scales:

  Aspect ratio behavior ("free" or "fixed") (only used for multi-axis)

- ratio:

  Length-3 numeric vector of axis ratios (only used for multi-axis)

## Value

Scaled values in -0.5, 0.5 domain (single axis) OR data frame with
scaled coordinates (multi-axis)
