# Set z-axis limits

This is a shorthand for `scale_z_continuous(limits = c(min, max))`. It's
a convenient way to set the z-axis limits without specifying other scale
parameters.

## Usage

``` r
zlim(min, max, ...)
```

## Arguments

- min, max:

  The minimum and maximum values for the z-axis.

- ...:

  Additional arguments passed to
  [`scale_z_continuous()`](https://matthewkling.github.io/ggcube/reference/scale_z_continuous.md).

## Value

A ggplot2 scale object for the z aesthetic with specified limits.

## See also

[`scale_z_continuous`](https://matthewkling.github.io/ggcube/reference/scale_z_continuous.md)
for more control over z-axis scaling,
[`xlim`](https://ggplot2.tidyverse.org/reference/lims.html),
[`ylim`](https://ggplot2.tidyverse.org/reference/lims.html) for x and y
axis limits

Other 3D scale functions:
[`scale_z_continuous()`](https://matthewkling.github.io/ggcube/reference/scale_z_continuous.md),
[`scale_z_discrete()`](https://matthewkling.github.io/ggcube/reference/scale_z_discrete.md)

## Examples

``` r
library(ggplot2)

# Set z-axis limits
ggplot(mtcars, aes(mpg, wt, z = qsec)) +
  geom_point() +
  zlim(15, 20) +
  coord_3d()


# Equivalent to:
ggplot(mtcars, aes(mpg, wt, z = qsec)) +
  geom_point() +
  scale_z_continuous(limits = c(15, 20)) +
  coord_3d()

```
