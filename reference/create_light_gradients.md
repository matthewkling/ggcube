# Create shading gradients for colorbar

Create shading gradients for colorbar

## Usage

``` r
create_light_gradients(
  base_colors,
  lighting_info,
  shade_range = c(-1, 1),
  n_steps = 20
)
```

## Arguments

- base_colors:

  Character vector of base colors

- lighting_info:

  Lighting specification object

- n_steps:

  Number of lighting gradient steps (default 10)

## Value

Matrix of colors with lighting applied
