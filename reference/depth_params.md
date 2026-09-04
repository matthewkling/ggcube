# Depth scaling parameter

Generic wording for layers whose depth-scaled property is something
other than polygon linewidth. Polygon layers document `scale_depth`
through
[polygon_params](https://matthewkling.github.io/ggcube/reference/polygon_params.md)
instead, which describes the mean-depth behaviour specific to them. Keep
the two in step when the semantics change.

## Arguments

- scale_depth:

  Controls depth-based scaling, drawing closer elements larger or
  thicker and farther ones smaller or thinner. `TRUE` (the default)
  applies full scaling and `FALSE` disables it; a number sets the
  strength directly, where `1` matches `TRUE`, `0` matches `FALSE`,
  values below 1 subdue the effect and values above 1 exaggerate it.
  This affects the layer only; use `coord_3d(scale_depth = )` for panel
  and axis components.
