# Rotate and position text vertices in 3D space

Takes 2D text vertices (in x-y plane) and transforms them to face the
specified direction at the specified position. With angle=0, the text
baseline will be as horizontal as possible (parallel to x-y plane when
the facing direction allows).

## Usage

``` r
rotate_text_to_facing(
  vertices,
  facing_normal,
  angle,
  anchor_x,
  anchor_y,
  anchor_z,
  size_mm,
  data_span,
  aspect_adjust = 1
)
```

## Arguments

- vertices:

  Data frame with vertex_x, vertex_y columns (text outline vertices in
  points)

- facing_normal:

  Unit vector for the direction the text should face

- angle:

  Rotation angle around the facing axis (degrees)

- anchor_x, anchor_y, anchor_z:

  Position to place the text

- size_mm:

  Text size in mm (same units as geom_text)

- data_span:

  The span of the data in data units (used to compute conversion)

- aspect_adjust:

  Scalar multiplier for text y-coordinates (height). Values \> 1 make
  text taller, values \< 1 make text wider.

## Value

Data frame with x, y, z columns in data coordinates
