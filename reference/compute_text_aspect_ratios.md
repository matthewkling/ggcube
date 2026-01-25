# Compute aspect ratio correction for text

Estimates the aspect ratio correction needed so text doesn't appear
stretched when the coord_3d scales axes differently. Returns a scalar to
multiply the text's y-coordinates by.

## Usage

``` r
compute_text_aspect_ratios(data_ranges, coord)
```

## Arguments

- data_ranges:

  List with x, y, z elements, each a length-2 vector of min, max

- coord:

  A coord_3d object, or NULL

## Value

Scalar aspect ratio correction (1 = no correction needed)
