# Compute triangle normals from vertex coordinates

Compute triangle normals from vertex coordinates

## Usage

``` r
compute_triangle_normals(data, face_data)
```

## Arguments

- data:

  Face vertex data with standardized CCW winding order

- face_data:

  Unique face data (unused, kept for compatibility)

## Value

Matrix with normalized normal vectors (one row per face, 3 columns)
