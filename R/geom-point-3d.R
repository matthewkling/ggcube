
# Apply depth scaling to point sizes, strokes, and linewidths if enabled
scale_depth <- function(coords, scale_depth){
      if (scale_depth && "depth_scale" %in% names(coords)) {
            if("size" %in% names(coords)) {
                  coords$size <- coords$size * coords$depth_scale
            }
            if("stroke" %in% names(coords)) {
                  coords$stroke <- coords$stroke * coords$depth_scale
            }
            if("linewidth" %in% names(coords)) {
                  coords$linewidth <- coords$linewidth * coords$depth_scale
            }
      }
      return(coords)
}

GeomPoint3D <- ggproto("GeomPoint3D", GeomPoint,

                       draw_panel = function(data, panel_params, coord, na.rm = FALSE, scale_depth = TRUE,
                                             ref_line_colour = NULL, ref_line_linewidth = 0.25, ref_line_linetype = NULL, ref_line_alpha = NULL,
                                             ref_point_colour = NULL, ref_point_fill = NULL, ref_point_alpha = NULL, ref_point_size = NULL, ref_point_stroke = NULL, ref_point_shape = NULL) {

                             # Transform data
                             validate_coord3d(coord)
                             data$ref_circle_radius <- data$ref_circle_radius / 100
                             coords <- coord$transform(data, panel_params)

                             # Scale points, strokes, linewidths by depth
                             if(!"linewidth" %in% names(coords)) coords$linewidth <- 0.5  # Default linewidth
                             coords <- scale_depth(coords, scale_depth)

                             # Extract base object IDs (everything before "__")
                             coords$object_id <- sub("__.*", "", coords$group)

                             # Split data by object ID
                             objects <- split(coords, coords$object_id)
                             objects <- objects[unique(coords$object_id)] # restore depth ordering

                             # Initialize grobs list
                             grobs <- list()

                             # Loop over complete objects
                             for (object_data in objects) {
                                   element_type <- object_data$element_type[1]

                                   # Detect shape complexity for inheritance
                                   raw_shape <- object_data$shape[1]
                                   is_complex_shape <- is.numeric(raw_shape) && raw_shape %in% 21:25

                                   # Detect alpha mapping (if all alpha values are 1, assume no mapping)
                                   alpha_is_mapped <- !all(object_data$alpha == 1, na.rm = TRUE)

                                   if (element_type == "ref_circle") {
                                         # ref_circles inheritance logic

                                         # Fill
                                         if (!is.null(ref_point_fill)) {
                                               circle_fill <- ref_point_fill
                                         } else if (is_complex_shape) {
                                               circle_fill <- object_data$fill[1]
                                         } else {
                                               circle_fill <- object_data$colour[1]
                                         }

                                         # Border color
                                         if (!is.null(ref_point_colour)) {
                                               circle_colour <- ref_point_colour
                                         } else if (is_complex_shape) {
                                               circle_colour <- object_data$colour[1]
                                         } else {
                                               circle_colour <- NA
                                         }

                                         # Alpha
                                         if (!is.null(ref_point_alpha)) {
                                               circle_alpha <- ref_point_alpha
                                         } else if (alpha_is_mapped) {
                                               circle_alpha <- object_data$alpha[1]
                                         } else {
                                               circle_alpha <- 0.5
                                         }

                                         # Border linewidth (stroke)
                                         avg_depth_scale <- mean(object_data$depth_scale)
                                         if (!is.null(ref_point_stroke)) {
                                               circle_linewidth <- ref_point_stroke * avg_depth_scale
                                         } else if (is_complex_shape) {
                                               # Inherit stroke from raw point data
                                               base_stroke <- object_data$stroke[1] %||% 0.5
                                               circle_linewidth <- base_stroke * avg_depth_scale
                                         } else {
                                               circle_linewidth <- 0  # No border for simple shapes
                                         }

                                         polygon_grob <- grid::polygonGrob(
                                               x = object_data$x,
                                               y = object_data$y,
                                               default.units = "npc",
                                               gp = grid::gpar(
                                                     col = circle_colour,
                                                     fill = circle_fill,
                                                     lwd = circle_linewidth * .pt,
                                                     alpha = circle_alpha
                                               )
                                         )

                                         grobs <- append(grobs, list(polygon_grob))

                                   } else if (element_type == "ref_line") {
                                         # ref_lines inheritance logic

                                         # Color
                                         line_colour <- ref_line_colour %||% object_data$colour[1]

                                         # Linetype
                                         line_linetype <- ref_line_linetype %||% 1

                                         # Alpha
                                         if (!is.null(ref_line_alpha)) {
                                               line_alpha <- ref_line_alpha
                                         } else if (alpha_is_mapped) {
                                               line_alpha <- object_data$alpha[1]
                                         } else {
                                               line_alpha <- 0.5
                                         }

                                         # Linewidth (explicit default, with depth scaling)
                                         avg_depth_scale <- mean(object_data$depth_scale)
                                         line_linewidth <- ref_line_linewidth * avg_depth_scale

                                         # Create segment grob for line (should have exactly 2 points)
                                         if (nrow(object_data) == 2) {
                                               segment_grob <- grid::segmentsGrob(
                                                     x0 = object_data$x[1], y0 = object_data$y[1],
                                                     x1 = object_data$x[2], y1 = object_data$y[2],
                                                     default.units = "npc",
                                                     gp = grid::gpar(
                                                           col = line_colour,
                                                           lwd = line_linewidth * .pt,
                                                           lty = line_linetype,
                                                           alpha = line_alpha
                                                     )
                                               )

                                               grobs <- append(grobs, list(segment_grob))
                                         }

                                   } else if (element_type == "ref_point") {
                                         # ref_points inheritance logic

                                         # Color
                                         point_colour <- ref_point_colour %||% object_data$colour[1]

                                         # Fill (only relevant for complex shapes)
                                         if (is_complex_shape) {
                                               point_fill <- ref_point_fill %||% object_data$fill[1]
                                         } else {
                                               point_fill <- NA  # Not applicable for simple shapes
                                         }

                                         # Alpha
                                         if (!is.null(ref_point_alpha)) {
                                               point_alpha <- ref_point_alpha
                                         } else if (alpha_is_mapped) {
                                               point_alpha <- object_data$alpha[1]
                                         } else {
                                               point_alpha <- 0.5
                                         }

                                         # Shape
                                         point_shape <- ref_point_shape %||% object_data$shape[1]

                                         # Size
                                         if (!is.null(ref_point_size)) {
                                               point_size <- ref_point_size * object_data$depth_scale[1]
                                         } else {
                                               point_size <- object_data$size[1] * (1/3)
                                         }

                                         # Stroke
                                         if (!is.null(ref_point_stroke)) {
                                               point_stroke <- ref_point_stroke
                                         } else {
                                               point_stroke <- object_data$stroke[1]
                                         }

                                         # Size and stroke handling (shape-aware)
                                         if (is_complex_shape) {
                                               # Complex shapes: ggplot2 approach for fontsize compensation
                                               point_fontsize <- (point_size + 0.5 * point_stroke) * .pt
                                               point_final_fill <- point_fill
                                         } else {
                                               # Simple shapes: just use size
                                               point_fontsize <- point_size * .pt
                                               point_final_fill <- NA  # Simple shapes don't use fill
                                         }

                                         point_grob <- grid::pointsGrob(
                                               x = object_data$x[1], y = object_data$y[1],
                                               default.units = "npc",
                                               pch = point_shape,
                                               gp = grid::gpar(
                                                     col = point_colour,
                                                     fill = point_final_fill,
                                                     fontsize = point_fontsize,
                                                     lwd = point_stroke * .pt,
                                                     alpha = point_alpha
                                               )
                                         )

                                         grobs <- append(grobs, list(point_grob))

                                   } else if (element_type == "raw_point") {
                                         # Create point grob for raw point (use shape-aware approach)

                                         # Size and stroke handling (shape-aware)
                                         if (is_complex_shape) {
                                               # Complex shapes: ggplot2 approach for fontsize compensation
                                               raw_fontsize <- (object_data$size[1] + 0.5 * object_data$stroke[1]) * .pt
                                               raw_fill <- if(is.na(object_data$fill[1])) NA else object_data$fill[1]
                                         } else {
                                               # Simple shapes: just use size, no fill
                                               raw_fontsize <- object_data$size[1] * .pt
                                               raw_fill <- NA  # Simple shapes don't use fill
                                         }

                                         point_grob <- grid::pointsGrob(
                                               x = object_data$x[1], y = object_data$y[1],
                                               default.units = "npc",
                                               pch = object_data$shape[1],
                                               gp = grid::gpar(
                                                     col = object_data$colour[1],
                                                     fill = raw_fill,
                                                     fontsize = raw_fontsize,
                                                     lwd = object_data$stroke[1] * .pt,
                                                     alpha = if(is.na(object_data$alpha[1])) 1 else object_data$alpha[1]
                                               )
                                         )

                                         grobs <- append(grobs, list(point_grob))
                                   }
                             }

                             # Combine all grobs (already in depth order)
                             if (length(grobs) == 0) {
                                   return(grid::nullGrob())
                             } else if (length(grobs) == 1) {
                                   return(grobs[[1]])
                             } else {
                                   return(do.call(grid::grobTree, grobs))
                             }
                       }
)

#' 3D scatter plot with depth-based size scaling and reference projections
#'
#' `geom_point_3d()` creates scatter plots in 3D space with automatic depth-based
#' size scaling. Points closer to the viewer appear larger, while points farther
#' away appear smaller, creating realistic perspective effects. Optionally adds
#' reference lines and points projecting to cube faces.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. In addition to
#'   the standard point aesthetics, `geom_point_3d()` requires x, y, and z coordinates.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to
#'   [StatPoint3D] for proper discrete scale handling and reference line support.
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed on to [layer()].
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param scale_depth Logical indicating whether to apply depth-based scaling
#'   to point sizes, point stroke widths, and reference line widths.
#'   When `TRUE` (default), points/lines closer to the viewer appear larger/wider, and
#'   points farther away appear smaller. When `FALSE`, all points/lines have uniform size/width.
#' @param raw_points Logical indicating whether to show the original 3D points.
#'   Default is `TRUE`.
#' @param ref_lines Logical indicating whether to show reference lines projecting
#'   from points to cube faces. Default is `FALSE`.
#' @param ref_points Type of reference points to create. Options:
#'   \itemize{
#'     \item \code{FALSE}: No reference points (default)
#'     \item \code{TRUE or "circles"}: Circular reference points that project properly
#'     \item \code{"points"}: Single-point references (renders faster and supports non-circular shapes,
#'     but point shape is not 3D-transformed)
#'   }
#' @param ref_faces Character vector specifying which cube faces to project to.
#'   Valid face names are: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax".
#'   Default is "zmin" (bottom face). Multiple faces can be specified.
#' @param ref_circle_radius Radius for circular reference points as percentage of standardized coordinate space.
#' @param ref_circle_vertices Number of vertices for circular reference points (higher = smoother).
#' @param ref_line_color,ref_line_colour,ref_line_linewidth,ref_line_linetype,ref_line_alpha
#'   Styling overrides for reference lines. When NULL, inherits from point aesthetics or uses defaults.
#'   Both American (`ref_line_color`) and British (`ref_line_colour`) spellings are accepted.
#' @param ref_point_color,ref_point_colour,ref_point_fill,ref_point_size,ref_point_alpha,ref_point_stroke,ref_point_shape
#'   Styling overrides for reference points and circles. When NULL, inherits from raw point aesthetics
#'   with shape-aware logic (complex shapes 21-25 vs simple shapes 0-20/characters).
#'   Both American (`ref_point_color`) and British (`ref_point_colour`) spellings are accepted.
#'
#' @section Aesthetics:
#' `geom_point_3d()` supports all the same aesthetics as [geom_point()], plus `z`:
#' - **x**: X coordinate (required)
#' - **y**: Y coordinate (required)
#' - **z**: Z coordinate (required)
#' - `alpha`: Transparency
#' - `colour`: Point border color
#' - `fill`: Point fill color (for certain shapes)
#' - `shape`: Point shape
#' - `size`: Point size (gets depth-scaled when `scale_depth = TRUE`)
#' - `stroke`: Border width for shapes with borders
#'
#' @section Depth Scaling:
#' The depth scaling uses an inverse relationship with distance, following the
#' mathematical relationship: `apparent_size = base_size * reference_distance / actual_distance`
#'
#' This creates realistic perspective where:
#' - Objects twice as far appear half as big
#' - Objects twice as close appear twice as big
#' - The center of the plot volume renders at exactly the user-specified size
#'
#' @section Point Rendering:
#' ggcube uses shape-aware rendering for improved stroke behavior:
#' - **Simple shapes (0-20, characters):** Use `size` for fontsize, `stroke` for border width, no fill
#' - **Complex shapes (21-25):** Use ggplot2's approach: `size + 0.5*stroke` for fontsize to prevent gaps
#' - Both size and stroke are depth-scaled when `scale_depth = TRUE`
#' - All shapes preserve stroke depth scaling and parameter control
#'
#' @section Reference Features:
#' Reference lines and points help visualize the 3D relationships by projecting
#' data points onto cube faces:
#' - **Reference lines**: Connect each 3D point to its projection on specified faces
#' - **Reference points**: Show the projected location on the faces
#' - **Reference circles**: Circular projections that appear as realistic 3D shadows
#' - **Depth sorting**: All elements (original points, reference lines, reference points)
#'   are automatically depth-sorted for proper 3D rendering
#'
#' @section Aesthetic Inheritance:
#' Reference elements intelligently inherit styling from raw points:
#' - **Shape-aware**: Complex shapes (21-25) with fill/colour are handled differently from simple shapes (0-20/characters)
#' - **Alpha detection**: If all points have alpha=1, uses 0.5 default for ref elements; otherwise inherits mapped alpha
#' - **Priority**: Explicit ref_* parameters > mapped aesthetics > smart defaults
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic 3D scatter plot with depth scaling
#' ggplot(expand.grid(x = 1:5, y = 1:5, z = 1:5),
#'        aes(x, y, z, fill = z)) +
#'   geom_point_3d(size = 10, shape = 21, color = "white", stroke = .1) +
#'   coord_3d(pitch = 40, roll = 5, yaw = 0, dist = 1.5) +
#'   scale_fill_viridis_c()
#'
#' # Add circular reference points on 2D face panel
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point_3d(size = 3,
#'     ref_points = TRUE, ref_lines = TRUE, ref_faces = "zmin") +
#'   coord_3d()
#'
#' # Aesthetic inheritance - ref elements inherit color and fill
#' ggplot(mpg, aes(displ, hwy, cty, color = cty, fill = cty)) +
#'   geom_point_3d(shape = 21, size = 3,
#'                 ref_points = TRUE, ref_lines = TRUE,
#'                 ref_faces = "zmin") +
#'   coord_3d()
#'
#' # Use point-style references with custom shape
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point_3d(ref_points = "points", ref_lines = TRUE,
#'                 ref_point_shape = 4, ref_point_size = 2,
#'                 ref_line_alpha = 0.5) +
#'   coord_3d()
#'
#' # Show only circular reference projections (no original points)
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point_3d(raw_points = FALSE, ref_points = "circles", ref_lines = TRUE,
#'                 ref_faces = c("zmin", "ymin")) +
#'   coord_3d()
#'
#' # Project to multiple faces with custom circle styling
#' ggplot(mtcars, aes(mpg, wt, qsec, color = factor(cyl))) +
#'   geom_point_3d(ref_points = "circles", ref_lines = TRUE,
#'                 ref_faces = c("zmin", "ymin", "xmax"),
#'                 ref_line_color = "grey50", ref_line_alpha = 0.3,
#'                 ref_point_fill = "white", ref_point_stroke = 0.8,
#'                 ref_circle_radius = 1) +
#'   coord_3d()
#'
#' # Disable depth scaling for uniform sizes
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point_3d(scale_depth = FALSE, size = 3) +
#'   coord_3d()
#'
#' @seealso [geom_point()] for 2D scatter plots, [coord_3d()] for 3D coordinate systems,
#'   [stat_point_3d()] for the underlying statistical transformation.
#' @export
geom_point_3d <- function(mapping = NULL, data = NULL,
                          stat = StatPoint3D,
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          scale_depth = TRUE,
                          raw_points = TRUE,
                          ref_lines = FALSE,
                          ref_points = FALSE,
                          ref_faces = "zmin",
                          ref_circle_radius = 1.5,
                          ref_circle_vertices = 16,
                          ref_line_color = NULL,
                          ref_line_colour = NULL,  # British spelling
                          ref_line_linewidth = 0.25,
                          ref_line_linetype = NULL,
                          ref_line_alpha = NULL,
                          ref_point_color = NULL,
                          ref_point_colour = NULL,  # British spelling
                          ref_point_fill = NULL,
                          ref_point_size = NULL,
                          ref_point_alpha = NULL,
                          ref_point_stroke = NULL,
                          ref_point_shape = NULL) {

      # Handle both American and British spellings
      if (!is.null(ref_line_colour) && is.null(ref_line_color)) {
            ref_line_color <- ref_line_colour
      }
      if (!is.null(ref_point_colour) && is.null(ref_point_color)) {
            ref_point_color <- ref_point_colour
      }

      layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomPoint3D,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  scale_depth = scale_depth,
                  raw_points = raw_points,
                  ref_lines = ref_lines,
                  ref_points = ref_points,
                  ref_faces = ref_faces,
                  ref_circle_radius = ref_circle_radius,
                  ref_circle_vertices = ref_circle_vertices,
                  ref_line_color = ref_line_color,
                  ref_line_linewidth = ref_line_linewidth,
                  ref_line_linetype = ref_line_linetype,
                  ref_line_alpha = ref_line_alpha,
                  ref_point_color = ref_point_color,
                  ref_point_fill = ref_point_fill,
                  ref_point_size = ref_point_size,
                  ref_point_alpha = ref_point_alpha,
                  ref_point_stroke = ref_point_stroke,
                  ref_point_shape = ref_point_shape,
                  ...
            )
      )
}
