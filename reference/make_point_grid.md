# Generate a grid of unique vertex points

Creates a regular grid of unique vertex positions. Unlike
[`make_tile_grid()`](https://matthewkling.github.io/ggcube/reference/make_tile_grid.md),
this returns one row per unique vertex position rather than polygon
vertices with duplicates.

## Usage

``` r
make_point_grid(
  grid = c("rectangle", "tri1", "tri2", "triangle"),
  n = 40,
  direction = c("x", "y"),
  xlim,
  ylim,
  trim = TRUE
)
```

## Arguments

- n:

  Integer or length-2 integer vector specifying grid resolution.

- xlim, ylim:

  Length-two numeric vectors defining bounding box.

## Value

A data frame with columns `x`, `y`, `row`, `column`.
