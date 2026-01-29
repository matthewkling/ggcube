# Convert point grid to ridgeline polygons

Takes a grid of points and creates ridgeline polygons where each slice
along one axis becomes a filled polygon.

## Usage

``` r
points_to_ridgelines(
  data,
  direction = "x",
  base = NULL,
  group_prefix = "ridgeline__"
)
```

## Arguments

- data:

  Data frame with x, y, z columns.

- direction:

  Direction of ridges: "x" means one ridge per unique x value (ridge
  varies in y), "y" means one ridge per unique y value (ridge varies in
  x).

- base:

  Z-value for the bottom of ridgeline polygons. If NULL, uses min(z).

- group_prefix:

  String prefix for polygon group IDs.

## Value

Data frame with polygon vertices for ridgeline rendering.
