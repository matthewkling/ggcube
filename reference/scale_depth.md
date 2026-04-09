# Apply depth scaling to point sizes, strokes, and linewidths

Scales visual properties by the depth_scale factor so that closer
objects appear larger/thicker and farther objects appear
smaller/thinner.

## Usage

``` r
scale_depth(coords, scale_depth)
```

## Arguments

- coords:

  Data frame with aesthetic columns and optionally `depth_scale`.

- scale_depth:

  Logical; if FALSE, no scaling is applied.

## Value

Data frame with scaled aesthetics.
