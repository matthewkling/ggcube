StatIdentity3D <- ggproto("StatIdentity3D", Stat,
                          required_aes = c("x", "y"),  # Require at least x and y like geom_point

                          compute_panel = function(data, scales, na.rm = FALSE) {

                                # Remove missing values if requested
                                if (na.rm) {
                                      complete_cases <- complete.cases(data)
                                      data <- data[complete_cases, ]
                                }

                                # Handle empty data
                                if (nrow(data) == 0) {
                                      return(data)
                                }

                                # Convert group values to hierarchical format for depth sorting
                                if (!"group" %in% names(data)) {
                                      data$group <- "-1__group"
                                } else {
                                      data$group <- paste0(data$group, "__group")
                                }

                                # Store original z values if z column exists (following stat_voxel pattern)
                                if ("z" %in% names(data)) {
                                      data$z_raw <- data$z

                                      # Convert discrete z to numeric positions
                                      if (is.factor(data$z) || is.character(data$z)) {
                                            data$z <- as.numeric(as.factor(data$z))
                                      }
                                }

                                # Also handle x and y for completeness (some 3D geoms might need this)
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

                                return(data)
                          }
)

#' 3D-aware identity transformation
#'
#' This stat performs identity transformation (passes data through unchanged) while
#' properly handling discrete scales for 3D coordinate systems. It converts factor
#' and character variables to numeric positions and preserves original values in
#' `*_raw` columns for proper scale labeling. It also converts group values to
#' hierarchical format to enable proper depth sorting that preserves vertex order
#' within polygons.
#'
#' This stat is primarily intended for use with 3D geoms that need discrete scale
#' support, following the same pattern as other ggcube stats.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data.
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed on to [layer()].
#'
#' @section Computed variables:
#' - `x_raw`, `y_raw`, `z_raw`: Original values before discrete-to-numeric conversion
#' - `group`: Converted to hierarchical format (e.g., "1__group", "2__group") for proper depth sorting
#'
#' @seealso [geom_point_3d()], [geom_polygon_3d()] which use this stat for discrete scale support.
#' @export
stat_identity_3d <- function(mapping = NULL, data = NULL,
                             geom = "point", position = "identity",
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                             ...) {

      layer(
            stat = StatIdentity3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
      )
}
