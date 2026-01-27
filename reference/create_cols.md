# Create column faces from grid data

Create column faces from grid data

## Usage

``` r
create_cols(data, x_spacing, y_spacing, width, selected_faces)
```

## Arguments

- data:

  Data frame with x, y, z, zmin columns

- x_spacing:

  Grid spacing in x direction

- y_spacing:

  Grid spacing in y direction

- width:

  Width factor (1.0 = full grid spacing)

- selected_faces:

  Character vector of face names to render

## Value

Data frame with column face vertices
