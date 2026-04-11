# Build and bind annotation rows in setup_data

One-liner helper for geom `setup_data` methods. Checks for annotation
specs in params, builds annotation rows, handles PANEL assignment and
type coercion, and binds onto the primary data. Returns data unchanged
if no annotations are present.

## Usage

``` r
setup_annotations(data, params)
```

## Arguments

- data:

  Primary data frame from the layer.

- params:

  Layer params list (checked for `$annotate`).

## Value

Data frame with annotation rows appended, or original data if no
annotations.
