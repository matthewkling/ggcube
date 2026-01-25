# Grid generation parameters

Parameters defining the geometry, resolution, and orientation of a
regular grid of tiles, as used by various ggcube layer functions.

## Arguments

- grid:

  Character argument specifying geometry of grid to generate. Options
  include `"tri"` (the default) for triangular grid, `"rect"` for
  rectangular grid, or `"hex"` for hexagonal grid. Triangles produce a
  proper 3D surface that can prevent lighting artifacts in places where
  a surface curves past parallel with the sight line.

- n:

  Either a single integer specifying grid resolution in both dimensions,
  or a vector of length 2 specifying `c(nx, ny)` for separate x and y
  resolutions. Default is `40`. Higher values create smoother surfaces
  but slower rendering.

- direction:

  Either `"x"` (the default) or `"y"`, specifying the orientation of
  tile rows. Ignored for rectangular grids.

- trim:

  Logical. Only relevant for triangular and hexagonal grids. If TRUE
  (default), trims edge tiles to so that grid boundaries are straight
  lines. If FALSE, preserves the full shape of all tiles, resulting in a
  grid with irregular edges.

## Details

Grids are constructed such that tiles are approximately equilateral when
scaled to a square domain, unless `n` gives separate resolution values
for the two dimensions. For triangular and hexagonal grids, this means
that `n` is only approximate.
