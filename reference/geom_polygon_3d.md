# 3D polygon geometry with depth sorting

`geom_polygon_3d()` renders 3D polygons with proper depth sorting for
realistic 3D surface visualization. It's designed to work with surface
data from
[`stat_hull_3d()`](https://matthewkling.github.io/ggcube/reference/geom_hull_3d.md)
and
[`stat_surface_3d()`](https://matthewkling.github.io/ggcube/reference/stat_surface_3d.md),
as well as regular polygon data like maps.

## Usage

``` r
geom_polygon_3d(
  mapping = NULL,
  data = NULL,
  stat = StatIdentity3D,
  position = "identity",
  ...,
  sort_method = "auto",
  scale_depth = TRUE,
  force_convex = FALSE,
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

  The data to be displayed in this layer. Note that if you specify
  `light` or `cull_backfaces`, behavior will depend on the "winding
  order" of polygon vertices, with the counter-clockwise face considered
  the "front".

- stat:

  The statistical transformation to use on the data. Defaults to
  StatIdentity3D.

- position:

  Position adjustment, defaults to "identity". To collapse the result
  onto one 2D surface, use
  [`position_on_face()`](https://matthewkling.github.io/ggcube/reference/position_on_face.md).

- ...:

  Other arguments passed on to the the layer function (typically
  GeomPolygon3D), such as aesthetics like `colour`, `fill`, `linewidth`,
  etc.

- cull_backfaces, sort_method, force_convex, scale_depth:

  Advanced polygon rendering parameters. See
  [polygon_rendering](https://matthewkling.github.io/ggcube/reference/polygon_rendering.md)
  for details.

- na.rm:

  If `FALSE`, missing values are removed.

- show.legend:

  Logical indicating whether this layer should be included in legends.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

## Value

A `Layer` object that can be added to a ggplot.

## Aesthetics

`geom_polygon_3d()` requires:

- **x**: X coordinate

- **y**: Y coordinate

- **z**: Z coordinate (for depth sorting)

- **group**: Polygon grouping variable

And understands these additional aesthetics:

- `fill`: Polygon fill color

- `colour`: Border color

- `linewidth`: Border line width

- `linetype`: Border line type

- `alpha`: Transparency

- `order`: Vertex order within polygons (for proper polygon
  construction)

## Examples

``` r
# Typically used via stats like stat_surface_3d() or stat_hull_3d()
ggplot(sphere_points, aes(x, y, z)) +
  stat_hull_3d(method = "convex", fill = "dodgerblue",
            light = light(fill = TRUE, mode = "hsl")) +
  coord_3d()


# Can be used directly with properly structured data
triangles <- data.frame(x = rep(c(3, 2, 1), 3),
                            y = rep(c(1, 3, 1), 3),
                            z = rep(1:3, each = 3),
                            shape = rep(letters[1:3], each = 3))
ggplot(triangles, aes(x, y, z, fill = shape)) +
  geom_polygon_3d(color = "black") +
  coord_3d()
#> Error in as.vector(x, "character"): cannot coerce type 'environment' to vector of type 'character'

# Use `sort_method` to choose between depth sorting algorithms
d <- data.frame(group = rep(letters[1:3], each = 4),
                x = c(1, 1, 2, 2,   1, 1, 3, 3,   2, 2, 3, 3),
                y = rep(c(1, 2, 2, 1), 3),
                z = rep(c(1, 1.5, 2), each = 4))
p <- ggplot(d, aes(x, y, z, group = group, fill = group)) +
      coord_3d(pitch = 50, roll = 20, yaw = 0,
               scales = "fixed", light = "none") +
      theme_light()

# fast, but rendering order is incorrect in this particular example
p + geom_polygon_3d(color = "black", linewidth = 1, alpha = .75,
      sort_method = "painter")
#> Error in as.vector(x, "character"): cannot coerce type 'environment' to vector of type 'character'

# correct rendering order (but slower for large data sets)
p + geom_polygon_3d(color = "black", linewidth = 1, alpha = .75,
      sort_method = "pairwise")
#> Error in as.vector(x, "character"): cannot coerce type 'environment' to vector of type 'character'
```
