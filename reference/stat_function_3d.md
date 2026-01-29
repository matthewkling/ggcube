# 3D surface from a function

Evaluates a function f(x,y) = z over a regular grid and renders the
result as a 3D surface or ridgeline plot.

## Usage

``` r
stat_function_3d(
  mapping = NULL,
  data = ensure_nonempty_data,
  geom = "surface_3d",
  position = "identity",
  ...,
  fun = NULL,
  xlim = NULL,
  ylim = NULL,
  n = 40,
  cull_backfaces = FALSE,
  light = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_function_3d(
  mapping = NULL,
  data = ensure_nonempty_data,
  stat = "function_3d",
  position = "identity",
  ...,
  fun = NULL,
  xlim = NULL,
  ylim = NULL,
  n = 40,
  cull_backfaces = FALSE,
  light = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://matthewkling.github.io/ggcube/reference/aes.md).

- data:

  Ignored; this stat generates its own data.

- geom:

  Geom to use for rendering. Defaults to GeomSurface3D for mesh
  surfaces. Use GeomRidgeline3D for ridgeline rendering.

- position:

  Position adjustment, defaults to "identity".

- ...:

  Other arguments passed to the layer.

- fun:

  Function to evaluate. Must accept (x, y) and return numeric z values.

- light:

  A lighting specification object created by
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md),`"none"`
  to disable lighting, or `NULL` to inherit plot-level lighting specs
  from the coord. Specify plot-level lighting in
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  and layer-specific lighting in `geom_*3d()` functions.

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Logical indicating whether this layer should be included in legends.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

## Computed variables

- x, y, z:

  Grid coordinates and function values

- dzdx, dzdy:

  Partial derivatives at each point

- slope:

  Gradient magnitude: sqrt(dzdx^2 + dzdy^2)

- aspect:

  Direction of steepest slope: atan2(dzdy, dzdx)

## See also

[`geom_surface_3d()`](https://matthewkling.github.io/ggcube/reference/stat_surface_3d.md),
[`geom_ridgeline_3d()`](https://matthewkling.github.io/ggcube/reference/geom_ridgeline_3d.md),
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)

## Examples

``` r
# Basic function surface
ggplot() +
  geom_function_3d(fun = function(x, y) sin(x) * cos(y),
                   xlim = c(-pi, pi), ylim = c(-pi, pi)) +
  coord_3d()


# Fill by slope
ggplot() +
  geom_function_3d(fun = function(x, y) x^2 + y^2,
                   xlim = c(-2, 2), ylim = c(-2, 2),
                   aes(fill = after_stat(slope))) +
  scale_fill_viridis_c() +
  coord_3d()


# As ridgelines
ggplot() +
  stat_function_3d(fun = function(x, y) dnorm(x) * dnorm(y) * 10,
                   xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), n = c(15, 30),
                   geom = "ridgeline_3d", base = 0, light = "none",
                   fill = "black", color = "white") +
  coord_3d()
#> Error in stat_function_3d(fun = function(x, y) dnorm(x) * dnorm(y) * 10,     xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), n = c(15, 30),     geom = "ridgeline_3d", base = 0, light = "none", fill = "black",     color = "white"): Problem while setting up geom.
#> ℹ Error occurred in the 1st layer.
#> Caused by error in `loadNamespace()`:
#> ! there is no package called ‘polyclip’
```
