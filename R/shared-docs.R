# Shared documentation for ggcube functions


# sorting ----------------------------------------------------

#' Depth sorting method
#'
#' The `sort_method` argument determines the order in which elements are rendered.
#' This controls depth sorting for all geometry types within a layer, including
#' polygons, points, segments, and text. Default varies by geometry type.
#'
#' @section Options:
#' \describe{
#'   \item{\code{"painter"}}{Elements are sorted by the mean depth (distance from
#'     viewer after rotation) of their vertices. This is fast, but can give incorrect
#'     results when primitives overlap in screen space at different depths.}
#'   \item{\code{"pairwise"}}{A more intensive sorting algorithm that compares every
#'     pair of elements to determine occlusion order. Polygons that pass through
#'     one another are first split along their line of intersection, and segments
#'     that pass through polygons are split where they cross them. Wherever a
#'     polygon and another element overlap on screen, their order is then
#'     determined exactly from their 3D geometry where possible, and otherwise
#'     estimated from depths sampled in the region of overlap. Points,
#'     segments, and text lying in the plane of a planar polygon render on top
#'     of it. Results are exact for planar polygons; non-planar polygons
#'     (e.g. tiles of a curved surface on a rectangular grid) are treated as
#'     approximately planar. Slower but more accurate.}
#'   \item{\code{"auto"}}{Uses pairwise if the data has fewer than 500 rows, and
#'     painter otherwise.}
#' }
#'
#' @name sorting_methods
NULL

#' Sorting parameters
#'
#' @param sort_method Depth sorting algorithm. See [sorting_methods] for details.
#' @name sort_params
#' @keywords internal
NULL


#' Annotation parameter
#'
#' @param annotate An [annotate_3d()] specification, or a list of them, to draw
#'   within this layer. Annotations are depth-sorted together with the layer's
#'   own geometry, so they can pass in front of and behind it correctly.
#' @name annotate_params
#' @keywords internal
NULL


#' Depth scaling parameter
#'
#' Generic wording for layers whose depth-scaled property is something other
#' than polygon linewidth. Polygon layers document `scale_depth` through
#' [polygon_params] instead, which describes the mean-depth behaviour specific
#' to them. Keep the two in step when the semantics change.
#'
#' @param scale_depth Controls depth-based scaling, drawing closer elements
#'   larger or thicker and farther ones smaller or thinner. `TRUE` (the default)
#'   applies full scaling and `FALSE` disables it; a number sets the strength
#'   directly, where `1` matches `TRUE`, `0` matches `FALSE`, values below 1
#'   subdue the effect and values above 1 exaggerate it. This affects the layer
#'   only; use `coord_3d(scale_depth = )` for panel and axis components.
#' @name depth_params
#' @keywords internal
NULL


# polygons ----------------------------------------------------

#' Polygon rendering parameters
#'
#' Parameters controlling depth sorting, backface culling, depth scaling, and
#' convexity enforcement for polygon-based 3D layers.
#'
#' @param sort_method Depth sorting algorithm. See [sorting_methods] for details.
#' @param cull_backfaces Logical indicating whether to remove back-facing polygons
#'   from rendering. This is primarily for performance optimization but may be useful
#'   for aesthetic reasons in some situations. Backfaces are determined using
#'   screen-space winding order after 3D transformation. Defaults vary by geometry
#'   type: FALSE for open surface-type geometries, TRUE for solid objects (hulls,
#'   voxels, etc. where backfaces are generally hidden unless frontfaces are transparent
#'   or explicitly disabled).
#' @param scale_depth Controls whether polygon linewidths are scaled to make closer lines
#'   wider and farther lines narrower, based on the mean depth of a polygon. `TRUE`
#'   (the default) applies full scaling and `FALSE` disables it; a number sets the
#'   strength directly, where `1` matches `TRUE`, `0` matches `FALSE`, values below
#'   1 subdue the effect and values above 1 exaggerate it. This affects the layer
#'   only; use `coord_3d(scale_depth = )` for gridlines, cube borders, axis ticks,
#'   and axis text.
#' @param force_convex Logical indicating whether to remove polygon vertices that are not part of the
#'   convex hull. Default value varies by geom. Specifying TRUE can help reduce artifacts in surfaces
#'   that have polygon tiles that wrap over a visible horizon. For prism-type geoms like columns and
#'   voxels, FALSE is safe because polygons fill always be convex.
#' @param na.rm If `FALSE`, missing values are removed.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed on to the layer function (typically GeomPolygon3D), such as
#'   aesthetics like `colour`, `fill`, `linewidth`, `annotate = annotate_3d(...)`, etc.
#' @name polygon_params
#' @keywords internal
NULL


#' Shared column and voxel params
#'
#' @param width Numeric value controlling box width as a fraction of grid spacing.
#'   Default is 1.0 (volumes touch each other). Use 0.8 for small gaps, 1.2 for overlap.
#'   Grid spacing is determined automatically using `ggplot2::resolution()`
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

#' Grid generation
#'
#' Parameters defining the geometry, resolution, and orientation of a regular grid
#' of surface tiles, as used by various ggcube layer functions including
#' [geom_smooth_3d()], [geom_function_3d()], [geom_density_3d()], and
#' [geom_surface_3d()].
#'
#' @section Arguments:
#' \describe{
#'   \item{`grid`}{Character specifying tile geometry. Options:
#'     \describe{
#'       \item{`"rectangle"`}{Rectangular grid (the default).}
#'       \item{`"right1"`}{Rectangular grid with each quad split into two right triangles
#'         along the bottom-left to top-right diagonal.}
#'       \item{`"right2"`}{Like `"right1"` but split along the bottom-right to top-left
#'         diagonal.}
#'       \item{`"equilateral"`}{Equilateral triangular lattice (tessellated via Delaunay
#'         triangulation). Can prevent lighting artifacts where a surface curves past
#'         parallel with the sight line.}
#'     }
#'   }
#'   \item{`n`}{Either a single integer specifying grid resolution in both dimensions,
#'     or a vector of length 2 specifying `c(nx, ny)` for separate x and y resolutions.
#'     Default is `40`. Higher values create smoother surfaces but slower rendering.}
#'   \item{`direction`}{Either `"x"` (the default) or `"y"`, specifying the orientation
#'     of tile rows. Ignored for rectangular grids.}
#'   \item{`trim`}{Logical. Only relevant for `grid = "equilateral"`. If `TRUE`
#'     (default), trims edge vertices so that grid boundaries are straight lines. If
#'     `FALSE`, preserves the full lattice, resulting in a grid with irregular edges.}
#' }
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
#' @param light A lighting specification object created by [light()], `light("none")`
#'   or the string `"none"` to disable lighting, or `NULL` to inherit plot-level
#'   lighting. Set plot-level lighting by adding [light()] to the plot, and
#'   layer-specific lighting via the `light` argument of `geom_*_3d()` functions,
#'   which takes precedence.
#' @name light_param
#' @keywords internal
NULL
