# Compute gradients at grid points using finite differences

For rectangular grids, computes dz/dx and dz/dy at each grid point using
central differences where possible, forward/backward differences at
edges.

## Usage

``` r
compute_point_gradients(data)
```

## Arguments

- data:

  Data frame with x, y, z and row, column indices

## Value

Data frame with added columns: dzdx, dzdy, slope, aspect
