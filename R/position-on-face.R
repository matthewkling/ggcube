#' Position for projecting 2D and 3D layers onto cube faces
#'
#' Enables layers to be projected onto 2D faces of the 3D coordinate cube.
#' It can be used to flatten 3D ggcube layers onto a single cube face as a way of
#' visualizing them in 2D, or to add certain natively 2D ggplot2 layers like
#' `geom_density_2d()` or `geom_smooth()` to a cube face.
#'
#' @param faces Character string or vector specifying which cube face(s) to project onto.
#'   Valid options are: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax", "3D".
#'   "3D" indicates the raw, non-flattened 3D position.
#'   Multiple faces and "3D" are only supported for 3D layers (when `axes = NULL`).
#' @param axes For 2D layers only: Character vector of length 2 specifying which
#'   3D dimensions the 2D layer's x and y aesthetics represent. For example,
#'   `c("x", "z")` means the 2D x-axis maps to the 3D x-axis and the 2D y-axis
#'   maps to the 3D z-axis. For 3D layers, use `NULL` (default) to preserve
#'   the existing x,y,z mapping.
#'
#' @details
#' This position adjustment supports both 2D and 3D layers:
#'
#' **For 3D layers** (`axes = NULL`):
#' - Data already has x,y,z coordinates in the correct order
#' - Simply adds projection metadata for coord_3d to place the layer on the specified face(s)
#' - The face coordinate will be overridden during coordinate transformation
#' - Multiple faces are supported - the layer will be duplicated on each specified face.
#'          All specified faces inherit aesthetics from the layer function; if you want different
#'          parameters for different faces and on the primary 3D layer, add each as a separate layer call.
#'
#' **For 2D layers** (`axes = c("dim1", "dim2")`):
#' - Renames the layer's x,y columns to match the specified 3D axes
#' - Adds the missing third dimension
#' - Adds projection metadata for coord_3d
#' - Only single faces are supported for 2D layers
#'
#' The actual projection happens during coordinate transformation in coord_3d.
#'
#' **Compatibility Note:** This position adjustment is not compatible with all 2D stats.
#' It works well with `stat_density_2d()` and other
#' stats that don't depend heavily on scale ranges during computation, but may cause
#' errors or rendering issues with `stat_density_2d_filled()` and similar stats that
#' generate polygons based on scale domains, as well as with layers like `stat_bin_2d()`
#' that return position variables other than x and y. For `geom_smooth()`, this position
#' works only if you set `se = FALSE` as shown in the example.
#'
#' @examples
#'
#' # 3D point layer in raw 3D form, and projected onto 2D face
#' ggplot(sphere_points, aes(x, y, z)) +
#'   geom_point_3d(position = position_on_face("zmin"), color = "red") +
#'   geom_point_3d(color = "black") + # add this layer last so it appears on top
#'   coord_3d()
#'
#' # 3D layer projected to multiple faces
#' set.seed(1)
#' d <- data.frame(x = round(rnorm(10)), y = round(rnorm(10)), z = round(rnorm(10)))
#' ggplot(d, aes(x, y, z)) +
#'   stat_voxel_3d(color = "black", fill = "steelblue",
#'     light = light(shade = "fill", direction = c(1, 1, 0), shade_mode = "hsl"),
#'     position = position_on_face(c("3D", "zmin", "xmax", "ymax"))) +
#'   coord_3d()
#'
#' # 3D layer projected differently on individual faces
#' ggplot(sphere_points, aes(x, y, z)) +
#'   stat_hull_3d(position = position_on_face("zmin"), fill = "black") +
#'   geom_point_3d(position = position_on_face("ymax")) +
#'   geom_path(position = position_on_face("xmax")) +
#'   stat_hull_3d(color = "black") +
#'   coord_3d()
#'
#' # 2D density contour on a specific face
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, Petal.Length, color = Species)) +
#'   stat_density_2d(position = position_on_face(faces = "zmin", axes = c("x", "y"))) +
#'   geom_point_3d() +
#'   coord_3d()
#'
#' # Distinct 2D layers projected to different faces
#' ggplot(mtcars) +
#'   geom_smooth(aes(mpg, qsec), color = "red", alpha = .5, se = FALSE,
#'     position = position_on_face(faces = "ymax", axes = c("x", "z"))) +
#'   geom_density_2d(aes(mpg, wt), alpha = .5,
#'     position = position_on_face(faces = "zmin", axes = c("x", "y"))) +
#'   geom_path(aes(wt, qsec), color = "forestgreen", alpha = .5,
#'     position = position_on_face(faces = "xmax", axes = c("y", "z"))) +
#'   geom_point_3d(aes(mpg, wt, qsec)) +
#'     coord_3d() +
#'   theme_light()
#'
#' @seealso [coord_3d()] for 3D coordinate systems, [stat_density_2d()] and other
#'   2D statistical transformations that can be projected onto faces.
#' @return A ggplot position object that can be supplied to the `position` argument
#'   to layer functions.
#' @export
position_on_face <- function(faces = "zmin", axes = NULL) {
      # Validate faces parameter
      valid_faces <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax", "3D")
      if (!all(faces %in% valid_faces)) {
            invalid_faces <- setdiff(faces, valid_faces)
            stop("Invalid face names: ", paste(invalid_faces, collapse = ", "),
                 ". Valid faces are: ", paste(valid_faces, collapse = ", "))
      }

      # Validate axes parameter
      if (!is.null(axes)) {
            if (length(axes) != 2) {
                  stop("axes must be NULL (for 3D layers) or a character vector of length 2 (for 2D layers)")
            }

            valid_axes <- c("x", "y", "z")
            if (!all(axes %in% valid_axes)) {
                  stop("axes must contain only 'x', 'y', or 'z'")
            }

            if (length(unique(axes)) != 2) {
                  stop("axes must specify two different dimensions")
            }

            # Multiple faces not allowed for 2D case
            if (length(faces) > 1) {
                  stop("Multiple faces are only supported for 3D layers (axes = NULL). ",
                       "For 2D layers, specify a single face.")
            }

            # Check that face and axes are compatible (only for 2D case)
            face_dim <- substr(faces[1], 1, 1)  # Use first (and only) face
            missing_dim <- setdiff(c("x", "y", "z"), axes)

            if (length(missing_dim) != 1) {
                  stop("axes must specify exactly two different dimensions from 'x', 'y', 'z'")
            }

            if (face_dim != missing_dim) {
                  stop("Face '", faces[1], "' is incompatible with axes = c('", axes[1], "', '", axes[2], "'). ",
                       "The face dimension ('", face_dim, "') must match the missing dimension ('", missing_dim, "').")
            }
      }

      ggproto(NULL, PositionOnFace,
              faces = faces,
              axes = axes
      )
}

#' Validate stat compatibility with position_on_face
#'
#' This function walks parent frames to find the layer's stat and checks if it's
#' compatible with position_on_face. Some stats have scale training conflicts that
#' cause incorrect rendering when used with position_on_face.
#'
#' @return NULL if compatible, otherwise issues a warning
#' @keywords internal
validate_position_stat <- function() {
      # Known incompatible stats
      incompatible_stats <- c(
            "StatDensity2dFilled",
            "StatBin2d",
            "StatBinhex"
      )

      # Walk parent frames to find the layer
      layer_obj <- NULL
      tryCatch({
            for (i in 1:25) {
                  env <- parent.frame(i)

                  # Look for layer being constructed
                  if (exists("layer", envir = env)) {
                        potential_layer <- get("layer", envir = env)
                        if (inherits(potential_layer, "function") &&
                            "stat" %in% names(formals(potential_layer))) {
                              # This is the layer() function call
                              # Try to get the stat argument
                              call_obj <- sys.call(i)
                              if (!is.null(call_obj) && "stat" %in% names(call_obj)) {
                                    stat_arg <- eval(call_obj$stat, envir = env)
                                    if (inherits(stat_arg, "Stat")) {
                                          layer_obj <- list(stat = stat_arg)
                                          break
                                    }
                              }
                        }
                  }

                  # Also look for already-constructed layer objects
                  for (var_name in ls(envir = env)) {
                        obj <- get(var_name, envir = env)
                        if (inherits(obj, "Layer")) {
                              layer_obj <- obj
                              break
                        }
                  }
                  if (!is.null(layer_obj)) break
            }
      }, error = function(e) {
            # If frame walking fails, skip validation
            return(NULL)
      })

      # Extract stat class name
      if (!is.null(layer_obj) && !is.null(layer_obj$stat)) {
            stat_class <- class(layer_obj$stat)[1]

            if (stat_class %in% incompatible_stats) {
                  # Generate helpful warning message
                  stat_name <- switch(stat_class,
                                      "StatDensity2dFilled" = "stat_density_2d_filled()",
                                      "StatBin2d" = "stat_bin_2d()",
                                      paste0(stat_class, "()"))

                  alternative <- switch(stat_class,
                                        "StatDensity2dFilled" = "stat_density_2d()",
                                        "StatBin2d" = "stat_bin()",
                                        "a compatible stat")

                  warning("position_on_face() is incompatible with ", stat_name, ". ",
                          "Stats that have position aesthetics beyond x and y, or that employ certain scale training routines, are not currently supported. ",
                          "Consider using ", alternative, " instead.",
                          call. = FALSE)
            }
      }

      return(NULL)
}

PositionOnFace <- ggproto("PositionOnFace", Position,
                          faces = NULL,
                          axes = NULL,

                          compute_layer = function(self, data, params, layout) {
                                # Only validate stat compatibility for 2D case (axes is not NULL)
                                if (!is.null(self$axes)) {
                                      validate_position_stat()
                                }

                                # Collect results for all faces
                                all_face_data <- list()

                                for (face in self$faces) {
                                      if (is.null(self$axes)) {
                                            # 3D case: data should already have x, y, z columns
                                            # Just add projection metadata
                                            if (!all(c("x", "y", "z") %in% names(data))) {
                                                  warning("position_on_face with axes = NULL requires x, y, and z aesthetics for 3D layers")
                                                  return(data)
                                            }

                                            face_data <- data
                                            # Add projection metadata
                                            if(face == "3D") face <- NA # transform leaves NA values alone
                                            face_data$project_to_face <- face

                                      } else {
                                            # 2D case
                                            # If the layer doesn't have both x and y aesthetics, return unchanged
                                            if (!all(c("x", "y") %in% names(data))) {
                                                  warning("position_on_face requires both x and y aesthetics")
                                                  return(data)
                                            }

                                            if(face == "3D") stop("the selected stat or `axes` specification is incompatible with `faces = '3D'` .")

                                            face_data <- data
                                            # Map the 2D x,y columns to the correct 3D dimensions
                                            # Rename columns based on axes specification
                                            names(face_data)[match(c("x", "y"), names(face_data))] <- self$axes

                                            # Add the missing dimension with placeholder value
                                            missing_dim <- setdiff(c("x", "y", "z"), self$axes)
                                            if (length(missing_dim) == 1) {
                                                  # Check if the missing dimension already exists (e.g., from stat_contour)
                                                  if (missing_dim %in% names(face_data)) {
                                                        warning("Data already contains a '", missing_dim, "' column (possibly computed by the stat). ",
                                                                "This will be overwritten for face projection. ",
                                                                "If you need the original values, consider using after_stat() before applying this position.")
                                                  }
                                                  # Use -Inf as placeholder for face-projected values
                                                  # (because NA errors, and finite numbers pollute scale training)
                                                  face_data[[missing_dim]] <- -Inf
                                            }

                                            # Add projection metadata
                                            face_data$project_to_face <- face
                                      }

                                      # Add hierarchical grouping to prevent depth sorting within groups
                                      # This ensures contour lines and other connected geometries maintain vertex order
                                      if ("group" %in% names(face_data)) {
                                            face_data$group <- paste0("face_", face, "__", face_data$group)
                                      }

                                      all_face_data[[length(all_face_data) + 1]] <- face_data
                                }

                                # Combine all face data
                                if (length(all_face_data) == 1) {
                                      return(all_face_data[[1]])
                                } else {
                                      return(do.call(rbind, all_face_data))
                                }
                          }
)
