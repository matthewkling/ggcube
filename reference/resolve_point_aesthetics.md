# Resolve reference element aesthetics and assign .prim types

Walks through the data and applies ref\_\* styling inheritance logic,
writing final visual property values into standard aesthetic columns.
Also maps element_type to .prim for the shared renderer.

## Usage

``` r
resolve_point_aesthetics(
  coords,
  ref_line_colour = NULL,
  ref_line_linewidth = 0.25,
  ref_line_linetype = NULL,
  ref_line_alpha = NULL,
  ref_point_colour = NULL,
  ref_point_fill = NULL,
  ref_point_alpha = NULL,
  ref_point_size = NULL,
  ref_point_stroke = NULL,
  ref_point_shape = NULL
)
```

## Arguments

- coords:

  Transformed coordinate data with element_type column.

- ref_line_colour, ref_line_linewidth, ref_line_linetype,
  ref_line_alpha:

  Overrides for reference line styling.

- ref_point_colour, ref_point_fill, ref_point_alpha, ref_point_size,
  ref_point_stroke, ref_point_shape:

  Overrides for reference point/circle styling.

## Value

Data frame with resolved aesthetics and .prim column.
