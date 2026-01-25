# Generate grid data using actual scale breaks with aspect ratio

Generate grid data using actual scale breaks with aspect ratio

## Usage

``` r
make_scale_grid(visible_faces, scale_info, scales = "free", ratio = c(1, 1, 1))
```

## Arguments

- visible_faces:

  Character vector of visible face names

- scale_info:

  List containing limits and breaks for x, y, z

- scales:

  Aspect ratio behavior ("free" or "fixed")

- ratio:

  Length-3 numeric vector of axis ratios

## Value

Data frame with grid lines in standard domain, including break values
