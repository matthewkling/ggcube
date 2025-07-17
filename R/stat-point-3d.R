StatPoint3D <- ggproto("StatPoint3D", Stat,
                       required_aes = c("x", "y"),  # Require at least x and y like geom_point

                       compute_panel = function(data, scales, na.rm = FALSE,
                                                raw_points = TRUE,
                                                ref_lines = FALSE,
                                                ref_points = FALSE,
                                                ref_faces = "zmin") {

                             # Validate parameters
                             if (!is.logical(raw_points)) raw_points <- TRUE
                             if (!is.logical(ref_lines)) ref_lines <- FALSE
                             if (!is.logical(ref_points)) ref_points <- FALSE

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

                             if(!any(raw_points, ref_points, ref_lines)){
                                   stop("At least one of raw_points, ref_points, or ref_lines must be TRUE.")
                             }

                             # Determine which faces to use for projections
                             valid_faces <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
                             ref_faces <- intersect(ref_faces, valid_faces)


                             result <- generate_point_elements(data, raw_points,
                                                             ref_faces, ref_points, ref_lines)

                             return(result)
                       }
)

#' Generate reference elements with simplified approach
#'
#' @param data Original point data
#' @param faces Character vector of face names
#' @param scale_limits List of scale limits
#' @param include_points Whether to include reference points
#' @param include_lines Whether to include reference lines
#' @return Data frame with reference elements
generate_point_elements <- function(data, raw_points,
                                  ref_faces, ref_points, ref_lines) {

      # Build result data frame
      result <- data.frame()
      id <- 1:nrow(data)

      # Add primary points if requested
      if(raw_points) {
            result <- data %>%
                  mutate(element_type = "raw_point",
                         group = paste0("point_raw_", id),
                         project_to_face = NA_character_) %>%
                  bind_rows(result)
      }

      for(face in ref_faces){
            if(ref_points){
                  result <- data %>%
                        mutate(element_type = "ref_point",
                               group = paste0("point_", face, "_", id),
                               project_to_face = face) %>%
                        bind_rows(result)
            }
            if(ref_lines){
                  result <- data %>%
                        mutate(element_type = "ref_line",
                               group = paste0("segment_", face, "_", id),
                               project_to_face = NA_character_,) %>%
                        bind_rows(mutate(., project_to_face = face)) %>% # only project one end of segment
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
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data.
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param raw_points Logical indicating whether to show the primary 3D points.
#' @param ref_lines Logical indicating whether to show reference lines to faces.
#' @param ref_points Logical indicating whether to show reference points on faces.
#' @param ref_faces Character vector specifying which faces to project to. Uses same
#'   vocabulary as coord_3d panels argument. Default "background" uses visible background faces.
#' @param ... Other arguments passed on to [layer()].
#'
#' @section Computed variables:
#' - `x_raw`, `y_raw`, `z_raw`: Original values before discrete-to-numeric conversion
#' - `element_type`: Type of element ("primary", "ref_point", "ref_line")
#' - `segment_id`: ID linking the two endpoints of reference lines
#' - `ref_face`: Which face reference elements project to
#'
#' @seealso [geom_point_3d()] which uses this stat by default.
#' @export
stat_point_3d <- function(mapping = NULL, data = NULL,
                          geom = "point_3d", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                          raw_points = TRUE,
                          ref_lines = FALSE,
                          ref_points = FALSE,
                          ref_faces = "zmin",
                          ...) {

      layer(
            stat = StatPoint3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, raw_points = raw_points,
                          ref_lines = ref_lines, ref_points = ref_points,
                          ref_faces = ref_faces, ...)
      )
}
