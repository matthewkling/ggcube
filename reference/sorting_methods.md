# Depth sorting method

Depth sorting method

## Arguments

- sort_method:

  Character indicating algorithm used to determine the order in which
  elements are rendered. This controls depth sorting for all geometry
  types within a layer, including polygons, points, segments, and text.
  Default varies by geometry type.

  - `"painter"`: Elements are sorted by the mean depth (distance from
    viewer after rotation) of their vertices. This is fast, but can give
    incorrect results when primitives overlap in screen space at
    different depths.

  - `"pairwise"`: A more intensive sorting algorithm that compares every
    pair of elements to determine occlusion order. Uses type-specific
    geometric tests: polygon overlap detection for polygon-polygon
    pairs, point-in-polygon tests for polygon-point pairs, line clipping
    for polygon-segment pairs, and line intersection for segment-segment
    pairs. When elements are coplanar, smaller primitives (points,
    segments) render on top of larger ones (polygons). Slower but more
    accurate.

  - `"auto"`: Uses pairwise if the data has fewer than 500 rows, and
    painter otherwise.
