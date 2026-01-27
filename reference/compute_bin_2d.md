# Bin continuous x and y data into 2D grid

Bin continuous x and y data into 2D grid

## Usage

``` r
compute_bin_2d(data, bins, binwidth, drop = TRUE)
```

## Arguments

- data:

  Data frame with x, y, weight columns

- bins:

  Vector of length 2 giving number of bins in x and y

- binwidth:

  Vector of length 2 giving bin widths (overrides bins if provided)

- drop:

  Whether to drop empty bins

## Value

Data frame with x, y, count, bin_area columns
