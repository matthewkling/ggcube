GeomPoint3D <- ggproto("GeomPoint3D", GeomPoint,

                       draw_panel = function(data, panel_params, coord, na.rm = FALSE, scale_depth = TRUE,
                                             ref_line_colour = NULL, ref_line_linewidth = NULL, ref_line_linetype = NULL, ref_line_alpha = NULL,
                                             ref_point_colour = NULL, ref_point_fill = NULL, ref_point_alpha = NULL, ref_point_size = NULL) {

                             # Transform data through coordinate system
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

                             # Initialize containers
                             grobs <- list()
                             segment_cache <- list()  # Track partial line segments by group

                             # Process rows in depth-sorted order
                             for (i in 1:nrow(coords)) {
                                   row <- coords[i, ]

                                   if (row$element_type == "ref_line") {
                                         # Handle line segment logic
                                         group_name <- row$group

                                         if (group_name %in% names(segment_cache)) {
                                               # Second point of segment - create the line grob
                                               start_row <- segment_cache[[group_name]]
                                               end_row <- row

                                               # Apply styling overrides or inherit from points

                                               # Apply styling overrides or inherit from points
                                               line_colour <- ref_line_colour %||% start_row$colour
                                               line_linetype <- ref_line_linetype %||% 1
                                               line_alpha <- ref_line_alpha %||% (if(is.na(start_row$alpha)) 1 else start_row$alpha)

                                               # Handle linewidth with depth scaling
                                               avg_depth_scale <- (start_row$depth_scale + end_row$depth_scale) / 2
                                               if (!is.null(ref_line_linewidth)) {
                                                     # Apply averaged depth scaling to the override value
                                                     line_linewidth <- ref_line_linewidth * avg_depth_scale
                                               } else {
                                                     # Use averaged depth scaling on base linewidth
                                                     base_linewidth <- 0.5  # Default base linewidth
                                                     line_linewidth <- base_linewidth * avg_depth_scale
                                               }

                                               # Create segment grob
                                               segment_grob <- grid::segmentsGrob(
                                                     x0 = start_row$x, y0 = start_row$y,
                                                     x1 = end_row$x, y1 = end_row$y,
                                                     default.units = "npc",
                                                     gp = grid::gpar(
                                                           col = line_colour,
                                                           lwd = line_linewidth * .pt,
                                                           lty = line_linetype,
                                                           alpha = line_alpha
                                                     )
                                               )

                                               grobs <- append(grobs, list(segment_grob))

                                               # Remove from cache
                                               segment_cache[[group_name]] <- NULL

                                         } else {
                                               # First point of segment - cache it
                                               segment_cache[[group_name]] <- row
                                         }

                                   } else if (row$element_type == "ref_point") {
                                         # Create reference point grob immediately
                                         point_colour <- ref_point_colour %||% row$colour
                                         point_fill <- ref_point_fill %||% (if(is.na(row$fill)) NA else row$fill)
                                         point_alpha <- ref_point_alpha %||% (if(is.na(row$alpha)) 1 else row$alpha)

                                         # Handle point size with depth scaling
                                         if (!is.null(ref_point_size)) {
                                               # Apply depth scaling to the override value
                                               point_size <- ref_point_size * row$depth_scale
                                         } else {
                                               # Default to 1/3 of the raw point size (already depth-scaled)
                                               point_size <- row$size * (1/3)
                                         }

                                         point_grob <- grid::pointsGrob(
                                               x = row$x, y = row$y,
                                               default.units = "npc",
                                               pch = row$shape,
                                               gp = grid::gpar(
                                                     col = point_colour,
                                                     fill = point_fill,
                                                     fontsize = point_size * .pt + .stroke * row$stroke,
                                                     alpha = point_alpha
                                               )
                                         )

                                         grobs <- append(grobs, list(point_grob))

                                   } else if (row$element_type == "raw_point") {
                                         # Create primary point grob immediately (use original aesthetics)
                                         point_grob <- grid::pointsGrob(
                                               x = row$x, y = row$y,
                                               default.units = "npc",
                                               pch = row$shape,
                                               gp = grid::gpar(
                                                     col = row$colour,
                                                     fill = if(is.na(row$fill)) NA else row$fill,
                                                     fontsize = row$size * .pt + .stroke * row$stroke,
                                                     alpha = if(is.na(row$alpha)) 1 else row$alpha
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
#'   to point sizes (and stroke widths if using a point shape with a stroke property).
#'   When `TRUE` (default), points closer to the viewer appear larger and points
#'   farther away appear smaller. When `FALSE`, all points have uniform size.
#' @param raw_points Logical indicating whether to show the original 3D points.
#'   Default is `TRUE`.
#' @param ref_lines Logical indicating whether to show reference lines projecting
#'   from points to cube faces. Default is `FALSE`.
#' @param ref_points Logical indicating whether to show reference points on cube faces
#'   representing the 2D projections. Default is `FALSE`.
#' @param ref_faces Character vector specifying which cube faces to project to.
#'   Valid face names are: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax".
#'   Default is "zmin" (bottom face). Multiple faces can be specified.
#' @param ref_line_color,ref_line_colour,ref_line_linewidth,ref_line_linetype,ref_line_alpha
#'   Styling overrides for reference lines. When NULL, inherits from point aesthetics.
#'   Both American (`ref_line_color`) and British (`ref_line_colour`) spellings are accepted.
#' @param ref_point_color,ref_point_colour,ref_point_fill,ref_point_size,ref_point_alpha
#'   Styling overrides for reference points. When NULL, inherits from point aesthetics,
#'   except `ref_point_size` which defaults to 1/3 of the original point size.
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
#' # Add reference lines and points (ref points auto-sized to 1/3 of original)
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point_3d(ref_lines = TRUE, ref_points = TRUE,
#'                 ref_line_alpha = 0.5) +
#'   coord_3d()
#'
#' # Show only reference projections (no original points)
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point_3d(raw_points = FALSE, ref_lines = TRUE, ref_points = TRUE,
#'                 ref_faces = c("zmin", "ymin")) +
#'   coord_3d()
#'
#' # Project to multiple faces
#' ggplot(mtcars, aes(mpg, wt, qsec, color = cyl)) +
#'   geom_point_3d(ref_lines = TRUE, ref_points = TRUE,
#'                 ref_faces = c("zmin", "ymin", "xmax"),
#'                 ref_line_color = "grey50", ref_line_alpha = 0.3,
#'                 ref_point_fill = "white", ref_point_size = 1) +
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
                          ref_line_color = NULL,
                          ref_line_colour = NULL,  # British spelling
                          ref_line_linewidth = 0.25,
                          ref_line_linetype = NULL,
                          ref_line_alpha = NULL,
                          ref_point_color = NULL,
                          ref_point_colour = NULL,  # British spelling
                          ref_point_fill = NULL,
                          ref_point_size = NULL,
                          ref_point_alpha = NULL) {

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
                  ref_line_color = ref_line_color,
                  ref_line_linewidth = ref_line_linewidth,
                  ref_line_linetype = ref_line_linetype,
                  ref_line_alpha = ref_line_alpha,
                  ref_point_color = ref_point_color,
                  ref_point_fill = ref_point_fill,
                  ref_point_size = ref_point_size,
                  ref_point_alpha = ref_point_alpha,
                  ...
            )
      )
}
