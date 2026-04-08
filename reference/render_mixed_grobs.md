# Render mixed-geometry data into a grob tree

Takes depth-sorted, transformed data with a `.prim` column indicating
primitive type and produces a single grob (or grobTree) for the layer.
Consecutive runs of the same primitive type are batched into single
vectorized grob calls for efficiency.

## Usage

``` r
render_mixed_grobs(data, rule = "evenodd", arrow = NULL, lineend = "butt")
```

## Arguments

- data:

  Data frame with transformed NPC coordinates, depth-sorted. Must
  contain a `.prim` column with values in `"polygon"`, `"point"`,
  `"segment"`, `"text"`.

- rule:

  Polygon fill rule: `"evenodd"` or `"winding"`. Only used for polygons
  with subgroups (holes).

- arrow:

  Arrow specification for segments, created by
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

- lineend:

  Line end style for segments.

## Value

A grid grob.
