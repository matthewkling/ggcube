GeomPoint3D <- ggproto("GeomPoint3D", GeomPoint,

                       draw_panel = function(data, panel_params, coord, na.rm = FALSE, scale_depth = TRUE) {

                             # Transform data through coordinate system
                             data$row_id <- 1:nrow(data)
                             coords <- data %>%
                                   coord$transform(panel_params) %>%
                                   arrange(row_id)

                             # Apply depth scaling to point sizes and strokes if enabled
                             if (scale_depth && "depth_scale" %in% names(coords)) {
                                   data$size <- data$size * coords$depth_scale

                                   if("stroke" %in% names(data)) {
                                         data$stroke <- data$stroke * coords$depth_scale
                                   }
                             }

                             # Pass along depth, for sorting
                             data$depth <- coords$depth

                             # Call parent draw_panel method with modified coords
                             GeomPoint$draw_panel(data, panel_params, coord, na.rm = na.rm)
                       }
)

#' 3D scatter plot with depth-based size scaling
#'
#' `geom_point_3d()` creates scatter plots in 3D space with automatic depth-based
#' size scaling. Points closer to the viewer appear larger, while points farther
#' away appear smaller, creating realistic perspective effects.
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
#'   [StatIdentity3D] for proper discrete scale handling and depth scaling.
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed on to [layer()].
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param scale_depth Logical indicating whether to apply depth-based scaling
#'   to point sizes (and stroke widths if using a point shape with a stroke property).
#'   When `TRUE` (default), points closer to the viewer appear larger and points
#'   farther away appear smaller. When `FALSE`, all points have uniform size.
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
#' # Disable depth scaling for uniform point sizes
#' # (this gives the same behavior as using ggplot2::geom_point)
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point_3d(scale_depth = FALSE, size = 3) +
#'   coord_3d(dist = 1.5)
#'
#' # Works with discrete scales
#' ggplot(expand_grid(x = 1:4, y = letters[5:8], z = letters[1:4]), aes(x, y, z)) +
#'   geom_point_3d(size = 5, shape = 21, fill = "lightblue", color = "darkblue") +
#'   coord_3d(dist = 1.5)
#'
#' @seealso [geom_point()] for 2D scatter plots, [coord_3d()] for 3D coordinate systems,
#'   [stat_identity_3d()] for the underlying statistical transformation.
#' @export
geom_point_3d <- function(mapping = NULL, data = NULL,
                          stat = StatIdentity3D,
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          scale_depth = TRUE) {

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
                  ...
            )
      )
}
