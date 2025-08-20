
#' 3D line segments
#'
#' `geom_segment_3d()` and `stat_segment_3d()` drawe line segments in 3D space
#' with automatic depth-based linewidth scaling and proper depth sorting.
#' Each segment is defined by start coordinates (x, y, z) and end
#' coordinates (xend, yend, zend).
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. Requires x, y, z
#'   for start coordinates and xend, yend, zend for end coordinates.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to [StatSegment3D].
#' @param geom The geometric object used to display the data. Defaults to [GeomSegment3D].
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed on to [layer()].
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param scale_depth Logical indicating whether to apply depth-based scaling
#'   to linewidth. When `TRUE` (default), segments closer to the viewer appear
#'   thicker, and segments farther away appear thinner.
#' @param arrow Specification for arrow heads, created by [arrow()].
#' @param lineend Line end style, one of "round", "butt", "square".
#'
#' @section Aesthetics:
#' `geom_segment_3d()` understands the following aesthetics:
#' - **x, y, z**: Start coordinates (required)
#' - **xend, yend, zend**: End coordinates (required)
#' - `colour`: Line color
#' - `linewidth`: Line width (gets depth-scaled when `scale_depth = TRUE`)
#' - `linetype`: Line type
#' - `alpha`: Transparency
#'
#' @examples
#' # Basic 3D segments
#' ggplot(sphere_points,
#'       aes(x, y, z, xend = 0, yend = 0, zend = 0)) +
#'   geom_segment_3d() +
#'   coord_3d()
#'
#' # 3D vector field
#' data <- expand.grid(x = -1:2, y = -1:2, z = -1:2)
#' data2 <- data + seq(-.5, .5, length.out = length(as.matrix(data)))
#' data <- cbind(data, setNames(data2, c("x2", "y2", "z2")))
#' ggplot(data, aes(x, y, z,
#'       xend = x2, yend = y2, zend = z2, color = x)) +
#'   geom_segment_3d(arrow = arrow(length = unit(0.1, "inches"),
#'                   type = "closed", angle = 15),
#'                   linewidth = .5) +
#'   coord_3d()
#'
#' @seealso [geom_path_3d()] for connected paths, [geom_segment()] for 2D segments,
#'   [coord_3d()] for 3D coordinate systems.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_segment_3d
#' @export
geom_segment_3d <- function(mapping = NULL, data = NULL,
                            stat = StatSegment3D, position = "identity",
                            ...,
                            scale_depth = TRUE, arrow = NULL, lineend = "butt",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = stat, geom = GeomSegment3D,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  scale_depth = scale_depth,
                  arrow = arrow,
                  lineend = lineend,
                  ...
            )
      )
}

#' @rdname geom_segment_3d
#' @export
stat_segment_3d <- function(mapping = NULL, data = NULL,
                            geom = GeomSegment3D, position = "identity",
                            ...,
                            scale_depth = TRUE, arrow = NULL, lineend = "butt",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = StatSegment3D, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  scale_depth = scale_depth,
                  arrow = arrow,
                  lineend = lineend,
                  ...
            )
      )
}


StatSegment3D <- ggproto("StatSegment3D", Stat,
                         required_aes = c("x", "y", "z", "xend", "yend", "zend"),

                         compute_group = function(data, scales, na.rm = FALSE) {

                               # Remove missing values if requested
                               if (na.rm) {
                                     complete_cases <- complete.cases(data[c("x", "y", "z", "xend", "yend", "zend")])
                                     data <- data[complete_cases, ]
                               }

                               if (nrow(data) == 0) {
                                     return(data)
                               }

                               # Handle discrete scale conversion for start points
                               if ("z" %in% names(data)) {
                                     data$z_raw <- data$z
                                     if (is.factor(data$z) || is.character(data$z)) {
                                           data$z <- as.numeric(as.factor(data$z))
                                     }
                               }

                               if ("x" %in% names(data)) {
                                     data$x_raw <- data$x
                                     if (is.factor(data$x) || is.character(data$x)) {
                                           data$x <- as.numeric(as.factor(data$x))
                                     }
                               }

                               if ("y" %in% names(data)) {
                                     data$y_raw <- data$y
                                     if (is.factor(data$y) || is.character(data$y)) {
                                           data$y <- as.numeric(as.factor(data$y))
                                     }
                               }

                               # Handle discrete scale conversion for end points
                               if ("zend" %in% names(data)) {
                                     data$zend_raw <- data$zend
                                     if (is.factor(data$zend) || is.character(data$zend)) {
                                           data$zend <- as.numeric(as.factor(data$zend))
                                     }
                               }

                               if ("xend" %in% names(data)) {
                                     data$xend_raw <- data$xend
                                     if (is.factor(data$xend) || is.character(data$xend)) {
                                           data$xend <- as.numeric(as.factor(data$xend))
                                     }
                               }

                               if ("yend" %in% names(data)) {
                                     data$yend_raw <- data$yend
                                     if (is.factor(data$yend) || is.character(data$yend)) {
                                           data$yend <- as.numeric(as.factor(data$yend))
                                     }
                               }

                               # Create hierarchical group IDs if not already present
                               if (!"group" %in% names(data) || !any(grepl("__", data$group))) {
                                     original_group <- data$group[1] %||% "-1"
                                     data$group <- paste0(original_group, "__seg", 1:nrow(data))
                               }

                               return(data)
                         }
)

GeomSegment3D <- ggproto("GeomSegment3D", Geom,
                         required_aes = c("x", "y", "z", "xend", "yend", "zend"),
                         default_aes = aes(
                               colour = "black", linewidth = 0.5, linetype = 1, alpha = 1
                         ),

                         draw_panel = function(data, panel_params, coord, scale_depth = TRUE,
                                               arrow = NULL, lineend = "butt", na.rm = FALSE) {

                               validate_coord3d(coord)

                               if (nrow(data) == 0) {
                                     return(grid::nullGrob())
                               }

                               # Convert wide to long format
                               # Each segment becomes 2 rows with unique sub-group IDs
                               n_segments <- nrow(data)

                               # Create start points
                               start_data <- data.frame(
                                     x = data$x,
                                     y = data$y,
                                     z = data$z,
                                     group = paste0(data$group, "__start"),
                                     segment_id = 1:n_segments,
                                     point_type = "start"
                               )

                               # Create end points
                               end_data <- data.frame(
                                     x = data$xend,
                                     y = data$yend,
                                     z = data$zend,
                                     group = paste0(data$group, "__end"),
                                     segment_id = 1:n_segments,
                                     point_type = "end"
                               )

                               # Preserve other aesthetics for both points
                               other_cols <- setdiff(names(data), c("x", "y", "z", "xend", "yend", "zend", "group"))
                               for (col_name in other_cols) {
                                     start_data[[col_name]] <- data[[col_name]]
                                     end_data[[col_name]] <- data[[col_name]]
                               }

                               # Combine into long format
                               long_data <- rbind(start_data, end_data)

                               # Transform all points together (handles depth sorting)
                               coords <- coord$transform(long_data, panel_params)

                               # Apply depth scaling to linewidth
                               coords <- scale_depth(coords, scale_depth)

                               # Reconstruct segments from transformed data (vectorized)
                               start_coords <- coords[coords$point_type == "start", ]
                               end_coords <- coords[coords$point_type == "end", ]

                               # Order by segment_id to ensure proper pairing
                               start_coords <- start_coords[order(start_coords$segment_id), ]
                               end_coords <- end_coords[order(end_coords$segment_id), ]

                               # Vectorized segment creation
                               segments <- data.frame(
                                     x0 = start_coords$x,
                                     y0 = start_coords$y,
                                     x1 = end_coords$x,
                                     y1 = end_coords$y,
                                     depth = (start_coords$depth + end_coords$depth) / 2,
                                     colour = start_coords$colour,
                                     linewidth = start_coords$linewidth,
                                     linetype = start_coords$linetype,
                                     alpha = start_coords$alpha,
                                     segment_id = start_coords$segment_id
                               )

                               if (nrow(segments) == 0) {
                                     return(grid::nullGrob())
                               }

                               # Sort segments by depth (back to front)
                               segments <- segments[order(-segments$depth), ]

                               # Create segments grob
                               grid::segmentsGrob(
                                     x0 = segments$x0, y0 = segments$y0,
                                     x1 = segments$x1, y1 = segments$y1,
                                     default.units = "npc",
                                     arrow = arrow,
                                     gp = grid::gpar(
                                           col = segments$colour,
                                           lwd = segments$linewidth * .pt,
                                           lty = segments$linetype,
                                           lineend = lineend,
                                           alpha = segments$alpha
                                     )
                               )
                         },

                         draw_key = draw_key_path
)


