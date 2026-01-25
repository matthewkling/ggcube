# Shared pillar and voxel params

Shared pillar and voxel params

## Arguments

- width:

  Numeric value controlling box width as a fraction of grid spacing.
  Default is 1.0 (volumes touch each other). Use 0.8 for small gaps, 1.2
  for overlap. Grid spacing is determined automatically using
  [`resolution()`](https://ggplot2.tidyverse.org/reference/resolution.html).

- faces:

  Character vector specifying which faces to render. Options:

  - `"all"` (default): Render all 6 faces

  - `"none"`: Render no faces

  - Vector of face names: `c("zmax", "xmin", "ymax")`, etc.

  Valid face names: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax". Note
  that this setting acts jointly with backface culling, which removes
  faces whose interior faces the viewer – e.g., when
  `cull_backfaces = TRUE` and `faces = "all"` (the default), only front
  faces are rendered.
