GeomPoint3D <- ggproto("GeomPoint3D", GeomPoint,

                       draw_panel = function(data, panel_params, coord, na.rm = FALSE, scale_depth = TRUE,
                                             ref_line_colour = NULL, ref_line_linewidth = NULL, ref_line_linetype = NULL, ref_line_alpha = NULL,
                                             ref_point_colour = NULL, ref_point_fill = NULL, ref_point_alpha = NULL, ref_point_size = NULL, ref_point_stroke = NULL, ref_point_shape = NULL) {

                             # Transform data through coordinate system
                             data$ref_circle_radius <- data$ref_circle_radius / 100
                             coords <- coord$transform(data, panel_params)

                             # Apply depth scaling to point sizes, strokes, and linewidths if enabled
                             if (scale_depth && "depth_scale" %in% names(coords)) {
                                   coords$size <- coords$size * coords$depth_scale
                                   if("stroke" %in% names(coords)) {
                                         coords$stroke <- coords$stroke * coords$depth_scale
                                   }
                                   # Add linewidth column for lines if not present
                                   if(!"linewidth" %in% names(coords)) {
                                         coords$linewidth <- 0.5  # Default linewidth
                                   }
                                   coords$linewidth <- coords$linewidth * coords$depth_scale
                             }

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

                                   if (element_type == "ref_circle") {
                                         # Create polygon grob for circle - inherit aesthetics from raw points
                                         circle_colour <- ref_point_colour %||% object_data$colour[1]
                                         circle_fill <- ref_point_fill %||% object_data$fill[1]
                                         circle_alpha <- ref_point_alpha %||% (if(is.na(object_data$alpha[1])) 1 else object_data$alpha[1])

                                         # Handle stroke/linewidth with depth scaling - inherit from raw point stroke
                                         avg_depth_scale <- mean(object_data$depth_scale)
                                         if (!is.null(ref_point_stroke)) {
                                               circle_linewidth <- ref_point_stroke * avg_depth_scale
                                         } else {
                                               # Inherit stroke from raw point data
                                               base_stroke <- object_data$stroke[1] %||% 0.5
                                               circle_linewidth <- base_stroke * avg_depth_scale
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
                                         # Create segment grob for line (should have exactly 2 points)
                                         if (nrow(object_data) == 2) {
                                               line_colour <- ref_line_colour %||% object_data$colour[1]
                                               line_linetype <- ref_line_linetype %||% 1
                                               line_alpha <- ref_line_alpha %||% (if(is.na(object_data$alpha[1])) 1 else object_data$alpha[1])

                                               # Handle linewidth with depth scaling
                                               avg_depth_scale <- mean(object_data$depth_scale)
                                               if (!is.null(ref_line_linewidth)) {
                                                     line_linewidth <- ref_line_linewidth * avg_depth_scale
                                               } else {
                                                     base_linewidth <- 0.5
                                                     line_linewidth <- base_linewidth * avg_depth_scale
                                               }

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
                                         # Create point grob for reference point
                                         point_colour <- ref_point_colour %||% object_data$colour[1]
                                         point_fill <- ref_point_fill %||% (if(is.na(object_data$fill[1])) NA else object_data$fill[1])
                                         point_alpha <- ref_point_alpha %||% (if(is.na(object_data$alpha[1])) 1 else object_data$alpha[1])
                                         point_shape <- ref_point_shape %||% object_data$shape[1]

                                         # Handle point size with depth scaling
                                         if (!is.null(ref_point_size)) {
                                               point_size <- ref_point_size * object_data$depth_scale[1]
                                         } else {
                                               point_size <- object_data$size[1] * (1/3)
                                         }

                                         # Handle stroke for point borders
                                         point_stroke <- ref_point_stroke %||% object_data$stroke[1]

                                         point_grob <- grid::pointsGrob(
                                               x = object_data$x[1], y = object_data$y[1],
                                               default.units = "npc",
                                               pch = point_shape,
                                               gp = grid::gpar(
                                                     col = point_colour,
                                                     fill = point_fill,
                                                     fontsize = point_size * .pt + .stroke * point_stroke,
                                                     alpha = point_alpha
                                               )
                                         )

                                         grobs <- append(grobs, list(point_grob))

                                   } else if (element_type == "raw_point") {
                                         # Create point grob for raw point (use original aesthetics)
                                         point_grob <- grid::pointsGrob(
                                               x = object_data$x[1], y = object_data$y[1],
                                               default.units = "npc",
                                               pch = object_data$shape[1],
                                               gp = grid::gpar(
                                                     col = object_data$colour[1],
                                                     fill = if(is.na(object_data$fill[1])) NA else object_data$fill[1],
                                                     fontsize = object_data$size[1] * .pt + .stroke * object_data$stroke[1],
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
#' The size scaling is calculated relative to the center of the 3D coordinate space
#' (0, 0, 0). The user-specified size represents the rendered size at this reference
#' point, with closer and farther points scaled proportionally based on their
#' distance from the viewer.
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
#'   Styling overrides for reference lines. When NULL, inherits from point aesthetics.
#'   Both American (`ref_line_color`) and British (`ref_line_colour`) spellings are accepted.
#' @param ref_point_color,ref_point_colour,ref_point_fill,ref_point_size,ref_point_alpha,ref_point_stroke,ref_point_shape
#'   Styling overrides for reference points and circles. When NULL, inherits from raw point aesthetics.
#'   \itemize{
#'     \item For reference points: `ref_point_size` defaults to 1/3 of raw point size, `ref_point_stroke` controls border width, `ref_point_shape` controls point shape
#'     \item For reference circles: `ref_point_stroke` controls border width, other parameters control fill/color (`ref_point_shape` is ignored)
#'   }
#'   Both American (`ref_point_color`) and British (`ref_point_colour`) spellings are accepted.
#'
#' @section Aesthetics:
#' `geom_point_3d()` supports all the same aesthetics as [geom_point()]:
#' - **x**: X coordinate (required)
#' - **y**: Y coordinate (required)
#' - **z**: Z coordinate (required for 3D positioning)
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
#' - The reference point (0, 0, 0) renders at exactly the user-specified size
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
#' @examples
#' library(ggplot2)
#'
#' # Basic 3D scatter plot with depth scaling
#' ggplot(expand_grid(x = 1:5, y = 1:5, z = 1:5),
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
                          ref_point_fill = "black",
                          ref_point_size = NULL,
                          ref_point_alpha = 0.5,
                          ref_point_stroke = NA,
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
