# Bin mixed discrete/continuous data

Bin mixed discrete/continuous data

## Usage

``` r
compute_bin_mixed(data, bins, binwidth, drop = TRUE, x_discrete)
```

## Arguments

- data:

  Data frame with x, y, weight columns

- bins:

  Vector of length 2 giving number of bins

- binwidth:

  Vector of length 2 giving bin widths

- drop:

  Whether to drop empty bins

- x_discrete:

  Logical indicating if x is discrete (y is continuous) or vice versa

## Value

Data frame with x, y, count, bin_area columns
