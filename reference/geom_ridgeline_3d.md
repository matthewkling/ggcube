# 3D ridgeline plot from point grid

Renders a point grid as ridgeline polygons (Joy Division style). Each
slice along one axis becomes a filled polygon, creating a
cross-sectional view of the surface.

## Usage

``` r
geom_ridgeline_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity_3d",
  position = "identity",
  ...,
  direction = "x",
  base = NULL,
  cull_backfaces = FALSE,
  sort_method = "pairwise",
  scale_depth = TRUE,
  force_convex = FALSE,
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

  Point grid data with x, y, z coordinates.

- stat:

  Statistical transformation. Defaults to "identity".

- position:

  Position adjustment, defaults to "identity".

- ...:

  Other arguments passed to the layer.

- direction:

  Direction of ridges:

  "x"

  :   One ridge per unique x value; ridge varies in y (default)

  "y"

  :   One ridge per unique y value; ridge varies in x

- base:

  Z-value for ridge polygon bottoms. If NULL, uses min(z).

- cull_backfaces, sort_method, force_convex, scale_depth:

  Advanced polygon rendering parameters. See
  [polygon_rendering](https://matthewkling.github.io/ggcube/reference/polygon_rendering.md)
  for details.

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

## Aesthetics

- x, y, z:

  Point coordinates (required)

- fill:

  Polygon fill color (default: "grey70")

- colour:

  Polygon border color (default: "black")

- alpha:

  Transparency

- linewidth:

  Border width

- linetype:

  Border type

## See also

[`geom_surface_3d()`](https://matthewkling.github.io/ggcube/reference/stat_surface_3d.md),
[`stat_function_3d()`](https://matthewkling.github.io/ggcube/reference/stat_function_3d.md),
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)

## Examples

``` r
# From point grid
grid_data <- expand.grid(x = seq(-3, 3, 0.5), y = seq(-3, 3, 0.1))
grid_data$z <- with(grid_data, dnorm(y) * dnorm(x) * 10)

ggplot(grid_data, aes(x, y, z = z)) +
  geom_ridgeline_3d() +
  coord_3d()


# With fill
ggplot(grid_data, aes(x, y, z = z, fill = x)) +
  geom_ridgeline_3d(colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c() +
  coord_3d()


# Ridges in y direction
ggplot(grid_data, aes(x, y, z = z)) +
  geom_ridgeline_3d(direction = "y", fill = "steelblue") +
  coord_3d()


# With stat_function_3d
ggplot() +
  stat_function_3d(fun = function(x, y) sin(x) * cos(y),
                   xlim = c(-pi, pi), ylim = c(-pi, pi),
                   n = 30,
                   geom = "ridgeline_3d") +
  coord_3d()
#> Error in get(paste0(cap(type), cap(gsub("_3d", "3D", x)))): object 'StatRidgeline3D' not found
```
