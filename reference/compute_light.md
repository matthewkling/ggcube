# Apply lighting models to surface normals with positional light support

Computes lighting values from surface normals using various lighting
models. Supports both directional lighting (parallel rays) and
positional lighting (point light sources with per-face light
directions).

## Usage

``` r
compute_light(normals, lighting, face_centers = NULL)
```

## Arguments

- normals:

  Matrix with 3 columns (x, y, z normal components), where each row
  represents a face normal vector. Should be unit vectors (normalized).

- face_centers:

  Matrix with 3 columns (x, y, z coordinates) representing the center
  position of each face in data coordinate space. Required for
  positional lighting, optional for directional lighting.

- light:

  A lighting specification object created by
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)

## Value

Vector of lighting values. For most methods, returns numeric values. For
`method = "rgb"`, returns hex color strings with
[`I()`](https://rdrr.io/r/base/AsIs.html) class for identity scaling.
