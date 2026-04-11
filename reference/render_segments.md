# Render segment primitives

Each group is a segment with exactly two rows (start and end points).
Segments are rendered in a single vectorized segmentsGrob call.

## Usage

``` r
render_segments(data, arrow = NULL, lineend = "butt")
```

## Arguments

- data:

  Segment data with x, y, group, and aesthetic columns. Each group must
  have exactly 2 rows with `point_type` "start"/"end", or simply two
  rows in order.

- arrow:

  Arrow specification.

- lineend:

  Line end style.

## Value

A segmentsGrob.
