# Generate circle vertices in 3D space for a given face

Generate circle vertices in 3D space for a given face

## Usage

``` r
generate_circle_vertices(x_std, y_std, z_std, face, radius, n_vertices)
```

## Arguments

- x_std, y_std, z_std:

  Standardized point coordinates

- face:

  Face name (e.g., "zmin", "xmax")

- radius:

  Circle radius in standardized units

- n_vertices:

  Number of vertices for the circle

## Value

Data frame with x, y, z coordinates for circle vertices
