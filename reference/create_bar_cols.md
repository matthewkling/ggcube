# Create column faces from aggregated bar data

Similar to create_cols but accepts separate x/y widths and adds z0
metadata to mark baseline vertices (since after_stat will overwrite z
values)

## Usage

``` r
create_bar_cols(data, x_spacing, y_spacing, width, selected_faces)
```

## Arguments

- data:

  Data frame with x, y, count columns

- x_spacing:

  Grid spacing in x direction

- y_spacing:

  Grid spacing in y direction

- width:

  Vector of length 2 giving width factors for x and y

- selected_faces:

  Character vector of face names to render

## Value

Data frame with column face vertices including z0 column
