# Build annotation rows from annotation specs

Takes annotation specifications (single or list) and generates
data-space rows tagged with `.prim` and dot-prefixed style columns,
ready to be bound onto primary data in `setup_data` so that annotation
positions participate in scale training.

## Usage

``` r
build_annotations(annotate, max_group = 0)
```

## Arguments

- annotate:

  A single `annotate_3d` object, or a list of them.

- max_group:

  Numeric. The maximum existing group number, used to generate
  non-colliding group IDs for annotation rows.

## Value

A data frame of annotation rows, or `NULL` if `annotate` is NULL.

## Details

For annotation types that need panel range information (e.g. planes),
positional columns may contain sentinel values (NA) that are resolved
later in `draw_panel` via `resolve_annotations()`.
