# Split segments at polygon interpenetration points

For each segment, finds all polygons it passes through in 3D (i.e., the
segment is in front of the polygon at one end and behind at the other).
Computes the parameter t where segment depth equals polygon depth, and
splits the segment at all such crossing points.

## Usage

``` r
split_interpenetrating_segments(
  data,
  grp_data,
  grp_idx,
  prim_lookup,
  poly_betas
)
```

## Arguments

- data:

  Data frame with columns x, y, depth (post-transform), group, .prim,
  and all aesthetic columns.

- grp_data:

  List of per-group matrices from pw_render_order.

- grp_idx:

  List of row index vectors per group, keyed by group name.

- prim_lookup:

  Named character vector of primitive types per group.

- poly_betas:

  List of pre-computed plane beta vectors.

## Value

Data frame with interpenetrating segments replaced by sub-segments.
Non-segment rows are returned unchanged.

## Details

Handles three overlap cases:

- Both segment endpoints inside polygon footprint

- One endpoint inside, one outside

- Both endpoints outside but segment crosses through polygon
