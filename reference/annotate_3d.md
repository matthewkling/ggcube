# Create a 3D annotation specification

Defines fixed-position annotations to be embedded within a 3D layer.
Annotations are depth-sorted together with the primary geometry, unlike
ggplot2's
[`annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html)
which creates a separate layer.

## Usage

``` r
annotate_3d(type, ...)
```

## Arguments

- type:

  Character string specifying the annotation type. One of `"point"`,
  `"text"`, `"segment"`, `"axes"`, or `"plane"`.

- ...:

  Type-specific parameters. See Details.

## Value

An S3 object of class `"annotate_3d"`.

## Details

Parameters can be vectors to create multiple annotations of the same
type in one call, with scalar values recycled as needed (matching
[`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html)
behaviour).

### Point annotations

Requires `x`, `y`, `z`. Optional styling: `colour`/`color`, `fill`,
`size`, `shape`, `alpha`, `stroke`.

### Text annotations

Requires `x`, `y`, `z`, `label`. Optional styling: `colour`/`color`,
`size`, `alpha`, `family`, `fontface`, `hjust`, `vjust`, `angle`,
`lineheight`.

### Segment annotations

Requires `x`, `y`, `z`, `xend`, `yend`, `zend`. Optional styling:
`colour`/`color`, `linewidth`, `linetype`, `alpha`, and `arrow`, an
arrow specification created by
[`grid::arrow()`](https://rdrr.io/r/grid/arrow.html) that applies to
every segment in the call. Closed arrowheads are filled with the segment
colour. When a segment passes through a polygon and is split, its
arrowheads stay at the segment's original ends.

### Axes annotations

Draws a line along each of the x, y, and z axes through an origin given
by `x`, `y`, `z` (default `0`), each spanning the panel's full range
along its axis, with an arrowhead and a label at its high end. Optional:
`labels` (a length-3 vector, default `c("x", "y", "z")`, or `NULL` for
none), `arrow` (default a closed 6pt arrowhead, or `NULL` for none),
line styling `colour`/`color`, `linewidth`, `linetype`, `alpha`, and
label styling `size`, `family`, `fontface`. Styling values can have
length 3 to style each axis separately.

Axes are built from the panel's limits when the plot is drawn, so unlike
other annotations they don't expand the plot's axis ranges.

### Plane annotations

Draws the plane `a * x + b * y + c * z = d`, clipped to the panel.
Requires `a`, `b`, `c` (not all zero); `d` defaults to `0`. All four can
be vectors to draw several planes, matching the rows of a linear system
`A x = d`. Optional styling: `fill` (default `"grey60"`), `alpha`
(default `0.5`), `colour`/`color` for the outline (default none),
`linewidth`, `linetype`.

Like axes, planes are built from the panel's limits and don't expand the
plot's axis ranges. They are lit like the layer's other polygons, from
whichever side faces the viewer, and are never removed by back-face
culling. A plane that misses the panel entirely is skipped with a
warning.

### Coordinates

Annotation coordinates are placed directly in the plot's (transformed)
scale space: with
[`scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
for example, give x positions as log10 values.

## Examples

``` r
p <- ggplot(mountain, aes(x, y, z)) +
  coord_3d(ratio = c(2, 3, 1),
           light = light(mode = "hsl", direction = c(-1, 0, 0)))

# Basic point annotation
p + geom_surface_3d(
  annotate = annotate_3d("point", x = .5, y = .25, z = 100,
                         color = "red", size = 3))


# Vectorized: multiple points in one call
p + geom_surface_3d(
  annotate = annotate_3d("point", x = .5, y = .25, z = seq(50, 100, 5),
                         color = "red", size = 3))


# Multiple annotation types
p + geom_surface_3d(
  annotate = list(
    annotate_3d("point", x = .5, y = .25, z = 100,
                color = "red", size = 3),
    annotate_3d("text", x = .5, y = .25, z = 100,
                color = "red", label = "Look here!",
                vjust = -1, fontface = "bold"),
    annotate_3d("segment", x = .5, y = .25, z = 100,
                xend = .5, yend = .25, zend = 50,
                color = "red", size = 3)))


# Planes of a linear system, meeting at its solution
A <- rbind(c(1, 1, 1), c(1, -1, 0), c(0, 1, -1))
ggplot(data.frame(x = 1/3, y = 1/3, z = 1/3), aes(x, y, z)) +
  geom_point_3d(size = 3, sort_method = "pairwise",
                annotate = annotate_3d("plane", a = A[, 1], b = A[, 2],
                                       c = A[, 3], d = c(1, 0, 0),
                                       fill = c("tomato", "seagreen", "steelblue"))) +
  lims(x = c(-1, 1), y = c(-1, 1)) + zlim(-1, 1) +
  coord_3d()


# Axes through the origin, depth-sorted with a surface
ggplot() +
  geom_function_3d(fun = function(x, y) (x^2 - y^2) / 2,
                   xlim = c(-1, 1), ylim = c(-1, 1),
                   annotate = annotate_3d("axes", linewidth = 0.8)) +
  coord_3d(panels = "none") +
  theme_void()

```
