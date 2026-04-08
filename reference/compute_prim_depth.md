# Compute per-group representative depth for sorting

Uses mean depth of all vertices in each group, which works correctly for
all primitive types (polygon, point, segment, text).

## Usage

``` r
compute_prim_depth(data)
```

## Arguments

- data:

  Data frame with `group`, `depth`, and optionally `.prim`.

## Value

Data frame with `.prim_depth` column added.
