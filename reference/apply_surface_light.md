# Apply lighting models to surface normals

Apply lighting models to surface normals

## Usage

``` r
apply_surface_light(face_data, normals, face_centers, light)
```

## Arguments

- face_data:

  Data frame with unique faces (group column)

- normals:

  Matrix of surface normals

- face_centers:

  Matrix of face centers

- light:

  Lighting specification object

## Value

Data frame with lighting values and normal components
