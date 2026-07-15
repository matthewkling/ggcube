# Grid generation

Parameters defining the geometry, resolution, and orientation of a
regular grid of surface tiles, as used by various ggcube layer functions
including
[`geom_smooth_3d()`](https://matthewkling.github.io/ggcube/reference/geom_smooth_3d.md),
[`geom_function_3d()`](https://matthewkling.github.io/ggcube/reference/stat_function_3d.md),
[`geom_density_3d()`](https://matthewkling.github.io/ggcube/reference/stat_density_3d.md),
and
[`geom_surface_3d()`](https://matthewkling.github.io/ggcube/reference/stat_surface_3d.md).

## Arguments

- `grid`:

  Character specifying tile geometry. Options:

  `"rectangle"`

  :   Rectangular grid (the default).

  `"right1"`

  :   Rectangular grid with each quad split into two right triangles
      along the bottom-left to top-right diagonal.

  `"right2"`

  :   Like `"right1"` but split along the bottom-right to top-left
      diagonal.

  `"equilateral"`

  :   Equilateral triangular lattice (tessellated via Delaunay
      triangulation). Can prevent lighting artifacts where a surface
      curves past parallel with the sight line.

- `n`:

  Either a single integer specifying grid resolution in both dimensions,
  or a vector of length 2 specifying `c(nx, ny)` for separate x and y
  resolutions. Default is `40`. Higher values create smoother surfaces
  but slower rendering.

- `direction`:

  Either `"x"` (the default) or `"y"`, specifying the orientation of
  tile rows. Ignored for rectangular grids.

- `trim`:

  Logical. Only relevant for `grid = "equilateral"`. If `TRUE`
  (default), trims edge vertices so that grid boundaries are straight
  lines. If `FALSE`, preserves the full lattice, resulting in a grid
  with irregular edges.
