# Compute normal-to-RGB mapping with light direction rotation

Maps surface normals to RGB colors with rotation so that normals aligned
with light_dir map to white/bright colors.

## Usage

``` r
compute_rgb_light(normals, light_dir_norm)
```

## Arguments

- normals:

  Matrix with 3 columns (x, y, z normal components)

- light_dir_norm:

  Normalized light direction vector

## Value

Character vector of hex color codes
