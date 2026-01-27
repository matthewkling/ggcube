# Polygon rendering parameters

Polygon rendering parameters

## Arguments

- cull_backfaces:

  Logical indicating whether to remove back-facing polygons from
  rendering. This is primarily for performance optimization but may be
  useful for aesthetic reasons in some situations. Backfaces are
  determined using screen-space winding order after 3D transformation.
  Defaults vary by geometry type: FALSE for open surface-type
  geometries, TRUE for solid objects (hulls, voxels, etc. where
  backfaces are generally hidden unless frontfaces are transparent or
  explicitly disabled).

- sort_method:

  Character indicating algorithm used to determine the order in which
  polygons are rendered.

  - `"painter"`: Polygons are sorted by the mean depth (distance from
    viewer after rotation) of their vertices. This is fast, but can give
    incorrect results in certain cases.

  - `"pairwise"`: A more intensive sorting algorithm that compares every
    pair of polygons in 3D to determine which face should be rendered
    behind the other; slower but more accurate.

  - `"auto"`: The default. Uses pairwise if polygon data has less than
    500 rows and painter otherwise.

- scale_depth:

  Logical indicating whether polygon linewidths should be scaled to make
  closer lines wider and farther lines narrower. Default is TRUE.
  Scaling is based on the mean depth of a polygon.

- force_convex:

  Logical indicating whether to remove polygon vertices that are not
  part of the convex hull. Default value varies by geom. Specifying TRUE
  can help reduce artifacts in surfaces that have polygon tiles that
  wrap over a visible horizon. For prism-type geoms like columns and
  voxels, FALSE is safe because polygons fill always be convex.
