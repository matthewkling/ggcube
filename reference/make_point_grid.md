# Generate a grid of unique vertex points

Creates a regular grid of unique vertex positions for use by
surface-generating stats. The grid is tessellated into polygon tiles by
[`points_to_tiles()`](https://matthewkling.github.io/ggcube/reference/points_to_tiles.md)
in the geom layer (or in the stat for
[`stat_smooth_3d()`](https://matthewkling.github.io/ggcube/reference/geom_smooth_3d.md)).

## Usage

``` r
make_point_grid(
  grid = c("rectangle", "right1", "right2", "equilateral"),
  n = 40,
  direction = c("x", "y"),
  xlim,
  ylim,
  trim = TRUE
)
```

## Arguments

- grid, n, direction, trim:

  Parameters determining the geometry, resolution, and orientation of
  the surface grid. See
  [grid_generation](https://matthewkling.github.io/ggcube/reference/grid_generation.md)
  for details.

- xlim, ylim:

  Length-two numeric vectors defining bounding box.

## Value

A data frame with columns `x`, `y`.
