# Aesthetic mapping with positional z support

This function extends
[`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html) to
support positional mapping to the z aesthetic. It maintains full
backward compatibility with the original `aes()` function while enabling
the convenient `aes(x, y, z)` syntax for 3D plots.

## Usage

``` r
aes(x, y, z, ...)
```

## Arguments

- x:

  Variable to map to x aesthetic (required)

- y:

  Variable to map to y aesthetic (required)

- z:

  Variable to map to z aesthetic (optional)

- ...:

  Other aesthetic mappings (color, size, etc.)

## Value

An aesthetic mapping object, same as
[`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)

## Details

This function is a lightweight wrapper around
[`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
that:

- Maintains full backward compatibility with existing 2D plots

- Enables positional z mapping: `aes(x_var, y_var, z_var)`

- Works with any geom that uses the z aesthetic (contour, raster, 3D
  plots)

- Passes through all other aesthetics unchanged

## See also

[`aes`](https://ggplot2.tidyverse.org/reference/aes.html) for the
original aesthetic mapping function,
[`coord_3d`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for 3D coordinate systems

## Examples

``` r
library(ggplot2)

# 2D plots work like regular ggplot2
ggplot(mtcars, aes(mpg, wt)) + geom_point()


# 3D plots can use positional syntax, or explicitly map to z
# same as: ggplot(mtcars, aes(mpg, wt, z = qsec)) + geom_point() + coord_3d()
ggplot(mtcars, aes(mpg, wt, qsec)) + geom_point() + coord_3d()


# Also works with non-ggcube z-aesthetic geoms
ggplot(mountain, aes(x, y, z)) + geom_contour()

```
