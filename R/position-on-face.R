#' Position for projecting 2D layers onto 3D cube faces
#'
#' `position_on_face()` enables 2D ggplot2 layers to be projected onto faces of the 3D
#' coordinate cube. This position adjustment adds the necessary z-coordinate and projection
#' metadata to allow 2D statistical transformations (like density estimation or contouring)
#' to be displayed on the faces of a 3D plot.
#'
#' @param face Character string specifying which cube face to project onto.
#'   Valid options are: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax".
#' @param axes Character vector of length 2 specifying which 3D dimensions the
#'   2D layer's x and y aesthetics represent. For example, `c("x", "z")` means
#'   the 2D x-axis maps to the 3D x-axis and the 2D y-axis maps to the 3D z-axis.
#'   Default is `c("x", "y")`.
#'
#' @details
#' This position adjustment works by:
#' 1. Renaming the 2D layer's x,y columns to match the specified 3D axes
#' 2. Adding the missing third dimension with NA values
#' 3. Adding projection metadata that coord_3d uses to place the layer on the face
#'
#' The actual projection happens during coordinate transformation in coord_3d,
#' which replaces the NA values with the appropriate face position.
#'
#' @examples
#' # Project an x-y density contour onto the bottom face (z-minimum)
#' ggplot(iris) +
#'   stat_density_2d(aes(Sepal.Length, Sepal.Width),
#'                   position = position_on_face(face = "zmin", axes = c("x", "y"))) +
#'   geom_point_3d(aes(Sepal.Length, Sepal.Width, Petal.Length)) +
#'   coord_3d()
#'
#' ggplot(mtcars) +
#' geom_smooth(aes(mpg, qsec), color = "red", alpha = .5, se = F,
#'     position = position_on_face(face = "ymin", axes = c("x", "z"))) +
#'   stat_density_2d(aes(mpg, wt), color = "blue", alpha = .5,
#'     position = position_on_face(face = "zmin", axes = c("x", "y"))) +
#'   geom_path(aes(wt, qsec), color = "forestgreen", alpha = .5,
#'     position = position_on_face(face = "xmax", axes = c("y", "z"))) +
#'     geom_point_3d(aes(mpg, wt, qsec)) +
#'     coord_3d() +
#'     theme_light()
#'
#' @seealso [coord_3d()] for 3D coordinate systems, [stat_density_2d()] and other
#'   2D statistical transformations that can be projected onto faces.
#' @export
position_on_face <- function(face = "zmin", axes = c("x", "y")) {
      # Validate face parameter
      valid_faces <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
      if (!face %in% valid_faces) {
            stop("Invalid face '", face, "'. Must be one of: ",
                 paste(valid_faces, collapse = ", "))
      }

      # Validate axes parameter
      if (length(axes) != 2) {
            stop("axes must be a character vector of length 2")
      }

      valid_axes <- c("x", "y", "z")
      if (!all(axes %in% valid_axes)) {
            stop("axes must contain only 'x', 'y', or 'z'")
      }

      if (length(unique(axes)) != 2) {
            stop("axes must specify two different dimensions")
      }

      # Check that face and axes are compatible
      face_dim <- substr(face, 1, 1)
      missing_dim <- setdiff(c("x", "y", "z"), axes)

      if (length(missing_dim) != 1) {
            stop("axes must specify exactly two different dimensions from 'x', 'y', 'z'")
      }

      if (face_dim != missing_dim) {
            stop("Face '", face, "' is incompatible with axes = c('", axes[1], "', '", axes[2], "'). ",
                 "The face dimension ('", face_dim, "') must match the missing dimension ('", missing_dim, "').")
      }

      ggproto(NULL, PositionOnFace,
              face = face,
              axes = axes
      )
}

PositionOnFace <- ggproto("PositionOnFace", Position,
                          face = NULL,
                          axes = NULL,

                          compute_layer = function(self, data, params, layout) {
                                # Validate axes parameter first
                                if (is.null(self$axes) || length(self$axes) != 2) {
                                      stop("position_on_face requires axes parameter to be a character vector of length 2")
                                }

                                # If the layer doesn't have both x and y aesthetics, return unchanged
                                if (!all(c("x", "y") %in% names(data))) {
                                      warning("position_on_face requires both x and y aesthetics")
                                      return(data)
                                }

                                # Map the 2D x,y columns to the correct 3D dimensions
                                # Rename columns based on axes specification
                                names(data)[match(c("x", "y"), names(data))] <- self$axes

                                # Add the missing dimension with placeholder value
                                missing_dim <- setdiff(c("x", "y", "z"), self$axes)
                                if (length(missing_dim) == 1) {
                                      # Check if the missing dimension already exists (e.g., from stat_contour)
                                      if (missing_dim %in% names(data)) {
                                            warning("Data already contains a '", missing_dim, "' column (possibly computed by the stat). ",
                                                    "This will be overwritten for face projection. ",
                                                    "If you need the original values, consider using after_stat() before applying this position.")
                                      }
                                      # Use -Inf as placeholder for face-projected values
                                      # (because NA errors, and finite numbers pollute scale training)
                                      data[[missing_dim]] <- -Inf
                                }

                                # Add projection metadata
                                data$project_to_face <- self$face

                                # Add hierarchical grouping to prevent depth sorting within groups
                                # This ensures contour lines and other connected geometries maintain vertex order
                                if ("group" %in% names(data)) {
                                      # Add a prefix to create hierarchical groups
                                      data$group <- paste0("face_", self$face, "__", data$group)
                                }

                                # Return the modified data
                                return(data)
                          }
)
