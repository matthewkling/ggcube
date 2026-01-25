# Generate reference elements with simplified approach

Generate reference elements with simplified approach

## Usage

``` r
generate_point_elements(
  data,
  raw_points,
  ref_faces,
  ref_points,
  ref_lines,
  ref_circle_radius,
  ref_circle_vertices
)
```

## Arguments

- data:

  Original point data

- raw_points:

  Whether to include raw points

- ref_faces:

  Character vector of face names

- ref_points:

  Type of reference points: FALSE, "circles", or "points"

- ref_lines:

  Whether to include reference lines

- ref_circle_radius:

  Radius for circular reference points

- ref_circle_vertices:

  Number of vertices for circular reference points

## Value

Data frame with reference elements
