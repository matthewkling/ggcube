# Create voxel faces from 3D sparse data

Create voxel faces from 3D sparse data

## Usage

``` r
create_voxels(data, x_spacing, y_spacing, z_spacing, width, selected_faces)
```

## Arguments

- data:

  Data frame with x, y, z columns

- x_spacing:

  Grid spacing in x direction

- y_spacing:

  Grid spacing in y direction

- z_spacing:

  Grid spacing in z direction

- width:

  Width factor (1.0 = full grid spacing)

- selected_faces:

  Character vector of face names to render

## Value

Data frame with voxel face vertices
