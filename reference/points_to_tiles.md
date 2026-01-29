# Convert point grid to polygon tiles

Takes a grid of points and tessellates into polygon tiles.

## Usage

``` r
points_to_tiles(
  data,
  method = "auto",
  grid_type = "rectangle",
  group_prefix = "surface__tile"
)
```

## Arguments

- data:

  Data frame with x, y, z columns (and optionally row, column).

- method:

  Tessellation method: "grid" for regular grid, "delaunay" for
  triangulation, "auto" to detect.

- grid_type:

  For method="grid", type of tiles: "rectangle", "tri1", "tri2".

- group_prefix:

  String prefix for polygon group IDs.

## Value

Data frame with polygon vertices including `group` and `order` columns.
