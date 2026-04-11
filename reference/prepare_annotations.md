# Resolve annotation sentinels in draw_panel

One-liner helper for geom `draw_panel` methods, called before coord
transformation. For annotation types that need panel range information
(e.g. planes needing extent on free axes), replaces sentinel NA values
with actual ranges from `panel_params`.

## Usage

``` r
prepare_annotations(data, panel_params)
```

## Arguments

- data:

  Data frame potentially containing annotation rows with sentinel
  values.

- panel_params:

  Panel parameters from the coord, containing scale ranges.

## Value

Data frame with sentinel values resolved.

## Details

For v1 annotation types (point, text, segment), this is a no-op since
all positions are fully specified at build time.
