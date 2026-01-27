# ggcube: 3D plotting extension for ggplot2

ggcube extends ggplot2 into the third dimension, providing 3D coordinate
systems, surface plotting, and volumetric visualization capabilities.

## Details

To use ggcube, load both packages:
[`library(ggplot2)`](https://ggplot2.tidyverse.org)
[`library(ggcube)`](https://github.com/matthewkling/ggcube)

Key functions:

- [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md):
  3D coordinate system with rotation and perspective

- [`aes()`](https://matthewkling.github.io/ggcube/reference/aes.md):
  Enhanced aesthetic mapping with positional z support

- [`stat_surface_3d()`](https://matthewkling.github.io/ggcube/reference/geom_surface_3d.md):
  Surface plotting from grid data

- [`stat_hull_3d()`](https://matthewkling.github.io/ggcube/reference/geom_hull_3d.md):
  3D convex and alpha hulls

- `stat_column_3d()`: 3D pillar/bar charts

- [`light()`](https://matthewkling.github.io/ggcube/reference/light.md):
  Lighting specifications for 3D surfaces

## See also

Useful links:

- <https://github.com/matthewkling/ggcube>

- <https://matthewkling.github.io/ggcube/>

- Report bugs at <https://github.com/matthewkling/ggcube/issues>
