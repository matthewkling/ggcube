# Compute surface normals from face gradients

Compute surface normals from face gradients

## Usage

``` r
compute_surface_normals(face_data)
```

## Arguments

- face_data:

  Data frame with unique faces containing dzdx and dzdy

## Value

Matrix with normalized normal vectors (one row per face, 3 columns)
