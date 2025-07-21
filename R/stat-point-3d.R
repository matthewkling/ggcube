StatPoint3D <- ggproto("StatPoint3D", Stat,
                       required_aes = c("x", "y", "z"),

                       compute_panel = function(data, scales, na.rm = FALSE,
                                                raw_points = TRUE,
                                                ref_lines = FALSE,
                                                ref_points = "circles",
                                                ref_faces = "zmin",
                                                ref_circle_radius = 0.02,
                                                ref_circle_vertices = 16) {

                             # Validate parameters
                             if (!is.logical(raw_points)) raw_points <- TRUE
                             if (!is.logical(ref_lines)) ref_lines <- FALSE

                             # Validate ref_points parameter
                             if (is.logical(ref_points)) {
                                   # Convert legacy logical to new format
                                   ref_points <- if (ref_points) "circles" else FALSE
                             }
                             if (!ref_points %in% c(FALSE, "circles", "points")) {
                                   stop("ref_points must be FALSE, 'circles', or 'points'")
                             }

                             # Remove missing values if requested
                             if (na.rm) {
                                   complete_cases <- complete.cases(data)
                                   data <- data[complete_cases, ]
                             }

                             # Handle empty data
                             if (nrow(data) == 0) {
                                   return(data)
                             }

                             # Handle discrete scale conversion (from original StatIdentity3D)
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

                             if(!any(raw_points, ref_points != FALSE, ref_lines)){
                                   stop("At least one of raw_points, ref_points, or ref_lines must be TRUE.")
                             }

                             # Determine which faces to use for projections
                             valid_faces <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
                             ref_faces <- intersect(ref_faces, valid_faces)

                             result <- generate_point_elements(data, raw_points,
                                                               ref_faces, ref_points, ref_lines,
                                                               ref_circle_radius, ref_circle_vertices)

                             return(result)
                       }
)

#' Generate reference elements with simplified approach
#'
#' @param data Original point data
#' @param raw_points Whether to include raw points
#' @param ref_faces Character vector of face names
#' @param ref_points Type of reference points: FALSE, "circles", or "points"
#' @param ref_lines Whether to include reference lines
#' @param ref_circle_radius Radius for circular reference points
#' @param ref_circle_vertices Number of vertices for circular reference points
#' @return Data frame with reference elements
generate_point_elements <- function(data, raw_points,
                                    ref_faces, ref_points, ref_lines,
                                    ref_circle_radius, ref_circle_vertices) {

      # Build result data frame
      result <- data.frame()
      id <- 1:nrow(data)

      # Add primary points if requested
      if(raw_points) {
            result <- data %>%
                  mutate(element_type = "raw_point",
                         group = paste0("point_raw_", id, "__grouped"),
                         project_to_face = NA_character_,
                         ref_circle_radius = NA_real_,
                         ref_circle_vertices = NA_integer_) %>%
                  bind_rows(result)
      }

      for(face in ref_faces){
            if(ref_points == "circles"){
                  result <- data %>%
                        mutate(element_type = "ref_circle",
                               group = paste0("circle_", face, "_", id, "__grouped"),
                               project_to_face = face,
                               ref_circle_radius = ref_circle_radius,
                               ref_circle_vertices = as.integer(ref_circle_vertices)) %>%
                        bind_rows(result)
            } else if(ref_points == "points") {
                  result <- data %>%
                        mutate(element_type = "ref_point",
                               group = paste0("point_", face, "_", id, "__grouped"),
                               project_to_face = face,
                               ref_circle_radius = NA_real_,
                               ref_circle_vertices = NA_integer_) %>%
                        bind_rows(result)
            }

            if(ref_lines){
                  result <- data %>%
                        mutate(element_type = "ref_line",
                               group = paste0("segment_", face, "_", id, "__grouped"),
                               project_to_face = NA_character_,
                               ref_circle_radius = NA_real_,
                               ref_circle_vertices = NA_integer_) %>%
                        bind_rows(mutate(., project_to_face = face)) %>%
                        bind_rows(result)
            }
      }

      return(result)
}

#' 3D-aware point transformation with reference lines and points
#'
#' This stat performs identity transformation (passes data through unchanged) while
#' properly handling discrete scales for 3D coordinate systems. It can optionally
#' generate reference lines and points projecting to cube faces.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. This stat requires
#'   `x`, `y`, and `z` aesthetics.
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data.
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param raw_points Logical indicating whether to show the primary 3D points.
#' @param ref_lines Logical indicating whether to show reference lines to faces.
#' @param ref_points Type of reference points to create. Options:
#'   \itemize{
#'     \item \code{FALSE}: No reference points
#'     \item \code{"circles"}: Circular reference points that project properly (default)
#'     \item \code{"points"}: Single-point references (legacy behavior)
#'   }
#' @param ref_faces Character vector specifying which faces to project to. Uses same
#'   vocabulary as coord_3d panels argument. Default "zmin" uses bottom face.
#' @param ref_circle_radius Radius for circular reference points in standardized coordinate units.
#' @param ref_circle_vertices Number of vertices for circular reference points (higher = smoother).
#' @param ... Other arguments passed on to [layer()].
#'
#' @section Computed variables:
#' - `x_raw`, `y_raw`, `z_raw`: Original values before discrete-to-numeric conversion
#' - `element_type`: Type of element ("raw_point", "ref_point", "ref_line", "ref_circle")
#' - `segment_id`: ID linking the two endpoints of reference lines
#' - `ref_face`: Which face reference elements project to
#' - `ref_circle_radius`: Radius for circular reference points
#' - `ref_circle_vertices`: Number of vertices for circular reference points
#'
#' @seealso [geom_point_3d()] which uses this stat by default.
#' @export
stat_point_3d <- function(mapping = NULL, data = NULL,
                          geom = "point_3d", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                          raw_points = TRUE,
                          ref_lines = FALSE,
                          ref_points = "circles",
                          ref_faces = "zmin",
                          ref_circle_radius = 0.02,
                          ref_circle_vertices = 16,
                          ...) {

      layer(
            stat = StatPoint3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, raw_points = raw_points,
                          ref_lines = ref_lines, ref_points = ref_points,
                          ref_faces = ref_faces, ref_circle_radius = ref_circle_radius,
                          ref_circle_vertices = ref_circle_vertices, ...)
      )
}
