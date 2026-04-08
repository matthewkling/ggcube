# Compute runs of consecutive same-type primitives

Groups are the atomic unit: all rows in a group share a .prim type. We
detect run boundaries by looking at where .prim changes across
consecutive rows (groups are contiguous after depth sorting).

## Usage

``` r
compute_prim_runs(data)
```

## Arguments

- data:

  Data frame with `.prim` and `group` columns.

## Value

Data frame with columns `.prim`, `start`, `end` (row indices).
