# Convert point grid to contour band polygons

Convert point grid to contour band polygons

## Usage

``` r
points_to_contours(data, breaks = NULL, bins = NULL, binwidth = NULL)
```

## Arguments

- data:

  Data frame with x, y, z columns forming a regular grid.

- breaks:

  Numeric vector of break points for contour levels. If NULL, breaks are
  computed automatically using `bins` or `binwidth`.

- bins:

  Number of contour bins. Ignored if `breaks` or `binwidth` is provided.

- binwidth:

  Width of contour bins. Ignored if `breaks` is provided.

- group_prefix:

  String prefix for polygon group IDs.

## Value

Data frame with polygon vertices for contour band rendering, including
`group` and `subgroup` columns for proper hole handling.
