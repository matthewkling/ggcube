# Shared documentation for ggcube functions


# polygons ----------------------------------------------------

#' Polygon rendering parameters
#'
#' @param cull_backfaces Logical indicating whether to remove back-facing polygons
#'   from rendering. This is primarily for performance optimization but may be useful
#'   for aesthetic reasons in some situations. Backfaces are determined using
#'   screen-space winding order after 3D transformation. Defaults vary by geometry
#'   type: FALSE for open surface-type geometries, TRUE for solid objects (hulls,
#'   voxels, etc. where backfaces are generally hidden unless frontfaces are transparent
#'   or explicitly disabled).
#' @param sort_method Character indicating algorithm used to determine the order in which
#'   polygons are rendered.
#'   \itemize{
#'     \item \code{"painter"}: Polygons are sorted by the mean depth (distance from viewer after
#'     rotation) of their vertices. This is fast, but can give incorrect results in certain cases.
#'     \item \code{"pairwise"}: A more intensive sorting algorithm that compares every pair of
#'     polygons in 3D to determine which face should be rendered behind the other;
#'     slower but more accurate.
#'     \item \code{"auto"}: The default. Uses pairwise if polygon data has less
#'     than 500 rows and painter otherwise.
#'   }
#' @param scale_depth Logical indicating whether polygon linewidths should be scaled to make closer lines
#'   wider and farther lines narrower. Default is TRUE. Scaling is based on the mean depth of a polygon.
#' @param force_convex Logical indicating whether to remove polygon vertices that are not part of the
#'   convex hull. Default value varies by geom. Specifying TRUE can help reduce artifacts in surfaces
#'   that have polygon tiles that wrap over a visible horizon. For prism-type geoms like columns and
#'   voxels, FALSE is safe because polygons fill always be convex.
#' @name polygon_rendering
NULL


#' Parameters for layers using GeomPolygon3D
#'
#' @param cull_backfaces,sort_method,force_convex,scale_depth Advanced polygon
#'   rendering parameters. See [polygon_rendering] for details.
#' @param na.rm If `FALSE`, missing values are removed.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed on to the the layer function (typically GeomPolygon3D), such as
#'   aesthetics like `colour`, `fill`, `linewidth`, etc.
#' @name polygon_params
#' @keywords internal
NULL

#' Shared column and voxel params
#'
#' @param width Numeric value controlling box width as a fraction of grid spacing.
#'   Default is 1.0 (volumes touch each other). Use 0.8 for small gaps, 1.2 for overlap.
#'   Grid spacing is determined automatically using [resolution()].
#' @param faces Character vector specifying which faces to render. Options:
#'   \itemize{
#'     \item \code{"all"} (default): Render all 6 faces
#'     \item \code{"none"}: Render no faces
#'     \item Vector of face names: \code{c("zmax", "xmin", "ymax")}, etc.
#'   }
#'   Valid face names: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax".
#'   Note that this setting acts jointly with backface culling, which removes faces whose
#'   interior faces the viewer -- e.g., when `cull_backfaces = TRUE` and `faces = "all"`
#'   (the default), only front faces are rendered.
#' @name col_params
#' @keywords internal
NULL

#' Shared ridgeline params
#'
#' @param direction Direction of ridges:
#'   \describe{
#'     \item{"x"}{One ridge per unique x value; ridge varies in y (default)}
#'     \item{"y"}{One ridge per unique y value; ridge varies in x}
#'   }
#' @param base Z-value for ridge polygon bottoms. If NULL, uses min(z).
#' @name ridgeline_params
#' @keywords internal
NULL


# grids ----------------------------------------------------

#' Grid generation parameters
#'
#' Parameters defining the geometry, resolution, and orientation of a regular grid
#' of tiles, as used by various ggcube layer functions.
#'
#' @param grid Character argument specifying geometry of grid to generate.
#'   Options include `"tri"` (the default) for triangular grid, `"rect"` for
#'   rectangular grid, or `"hex"` for hexagonal grid. Triangles produce a proper 3D
#'   surface that can prevent lighting artifacts in places where a surface curves past
#'   parallel with the sight line.
#' @param n Either a single integer specifying grid resolution in both dimensions,
#'   or a vector of length 2 specifying `c(nx, ny)` for separate x and y resolutions.
#'   Default is `40`. Higher values create smoother surfaces but slower rendering.
#' @param direction Either `"x"` (the default) or `"y"`, specifying the orientation
#'   of tile rows. Ignored for rectangular grids.
#' @param trim Logical. Only relevant for triangular and hexagonal grids. If TRUE
#'   (default), trims edge tiles to so that grid boundaries are straight lines. If
#'   FALSE, preserves the full shape of all tiles, resulting in a grid with
#'   irregular edges.
#'
#' @details Grids are constructed such that tiles are approximately equilateral
#'   when scaled to a square domain, unless `n` gives separate resolution values
#'   for the two dimensions. For triangular and hexagonal grids, this means that
#'   `n` is only approximate.
#'
#' @name grid_generation
NULL

#' Grid generation parameters
#'
#' @param grid,n,direction,trim Parameters determining the geometry, resolution, and
#'   orientation of the surface grid. See [grid_generation] for details.
#' @name grid_params
#' @keywords internal
NULL


# computed variables ----------------------------------------------------


#' Computed variables for surfaces
#'
#' @section Computed variables:
#' The following computed variables are available via `after_stat()`:
#' - `x`, `y`, `z`: Grid coordinates and function values
#' - `normal_x`, `normal_y`, `normal_z`: Surface normal components
#' - `slope`: Gradient magnitude from surface calculations
#' - `aspect`: Direction of steepest slope from surface calculations
#' - `dzdx`, `dzdy`: Partial derivatives from surface calculation
#'
#' @name surface_computed_vars
#' @keywords internal
NULL



# others ----------------------------------------------------


#' Position param
#'
#' @param position Position adjustment, defaults to "identity". To collapse the result
#'   onto one 2D surface, use `position_on_face()`.
#' @name position_param
#' @keywords internal
NULL


#' Light param
#'
#' @param light A lighting specification object created by [light()],`"none"` to disable
#'   lighting, or `NULL` to inherit plot-level lighting specs from the coord. Specify
#'   plot-level lighting in `coord_3d()` and layer-specific lighting in `geom_*3d()`
#'   functions.
#' @name light_param
#' @keywords internal
NULL


