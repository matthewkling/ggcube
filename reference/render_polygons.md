# Render polygon primitives

Handles both simple polygons and polygons with holes (subgroups). All
polygons in the batch are rendered in a single vectorized grob call.

## Usage

``` r
render_polygons(data, rule = "evenodd")
```

## Arguments

- data:

  Polygon data with x, y, group, and aesthetic columns.

- rule:

  Fill rule for polygons with holes.

## Value

A polygonGrob or pathGrob.
