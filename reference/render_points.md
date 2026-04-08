# Render point primitives

Each group is a single point. Points are rendered in a single vectorized
pointsGrob call when all share the same shape, or as individual grobs
when shapes differ (required by grid's pointsGrob which takes scalar
pch).

## Usage

``` r
render_points(data)
```

## Arguments

- data:

  Point data with x, y, and aesthetic columns.

## Value

A pointsGrob or grobTree.
