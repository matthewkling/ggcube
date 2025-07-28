StatPillar3D <- ggproto("StatPillar3D", Stat,
                      required_aes = c("x", "y", "z"),

                      compute_panel = function(data, scales, na.rm = FALSE,
                                               width = 1.0, faces = "all",
                                               light = lighting(), zmin = NULL) {

                            # Remove missing values if requested
                            if (na.rm) {
                                  data <- data[complete.cases(data[c("x", "y", "z")]), ]
                            }

                            # Check we have enough data
                            if (nrow(data) < 1) {
                                  stop("stat_pillar_3d requires at least 1 point")
                            }

                            # Handle zmin with proper precedence:
                            # 1. Parameter (if provided) - overrides aesthetic
                            # 2. Aesthetic (if mapped) - used if no parameter
                            # 3. Default (min of data) - if neither provided

                            if (!is.null(zmin)) {
                                  # Parameter provided - use for all pillars (overrides aesthetic)
                                  data$zmin <- zmin
                            } else if (!"zmin" %in% names(data)) {
                                  # No aesthetic mapped and no parameter - use default
                                  data$zmin <- min(data$z, na.rm = TRUE)
                            }
                            # If aesthetic exists and no parameter, keep the aesthetic values (no action needed)

                            # Ensure z is never less than zmin
                            zmin_vals <- pmin(data$zmin, data$z)
                            zmax_vals <- pmax(data$zmin, data$z)
                            data$zmin <- zmin_vals
                            data$z <- zmax_vals

                            # Generate numeric positions before calculating spacing
                            data <- convert_to_numeric(data)

                            # Calculate grid spacing using resolution (works for both regular and sparse grids)
                            x_spacing <- resolution(data$x, zero = FALSE)
                            y_spacing <- resolution(data$y, zero = FALSE)

                            # Fallback for edge cases (single point or identical coordinates)
                            if (is.na(x_spacing) || x_spacing <= 0) {
                                  x_spacing <- 1.0
                            }
                            if (is.na(y_spacing) || y_spacing <= 0) {
                                  y_spacing <- 1.0
                            }

                            # Validate and process faces parameter
                            selected_faces <- select_faces(faces)

                            if (length(selected_faces) == 0) {
                                  # Return empty data frame with required columns for consistency
                                  return(data.frame(
                                        x = numeric(0), y = numeric(0), z = numeric(0),
                                        group = character(0), pillar_id = integer(0), face_type = character(0),
                                        light = numeric(0),
                                        normal_x = numeric(0), normal_y = numeric(0), normal_z = numeric(0)
                                  ))
                            }

                            # Create pillars
                            pillar_faces <- create_pillars(data, x_spacing, y_spacing, width, selected_faces)

                            if (nrow(pillar_faces) == 0) {
                                  # Return empty data frame with required columns for consistency
                                  return(data.frame(
                                        x = numeric(0), y = numeric(0), z = numeric(0),
                                        group = character(0), pillar_id = integer(0), face_type = character(0),
                                        light = numeric(0),
                                        normal_x = numeric(0), normal_y = numeric(0), normal_z = numeric(0)
                                  ))
                            }

                            # Calculate face normals
                            face_normals <- calculate_pillar_face_normals(pillar_faces)

                            # Apply lighting
                            light_vals <- compute_lighting(face_normals, light)

                            # Add lighting and normal components to faces data
                            pillar_faces$light <- light_vals
                            pillar_faces$normal_x <- face_normals[, 1]
                            pillar_faces$normal_y <- face_normals[, 2]
                            pillar_faces$normal_z <- face_normals[, 3]

                            # Add lighting parameters for blend processing
                            pillar_faces$blend_enabled <- light$blend
                            pillar_faces$blend_strength <- light$blend_strength
                            pillar_faces$blend_mode <- light$blend_mode
                            pillar_faces$lighting_method <- light$method

                            return(pillar_faces)
                      }
)

#' Create pillar faces from grid data
#'
#' @param data Data frame with x, y, z, zmin columns
#' @param x_spacing Grid spacing in x direction
#' @param y_spacing Grid spacing in y direction
#' @param width Width factor (1.0 = full grid spacing)
#' @param selected_faces Character vector of face names to render
#' @return Data frame with pillar face vertices
#' @keywords internal
create_pillars <- function(data, x_spacing, y_spacing, width, selected_faces) {

      # Calculate actual pillar dimensions
      pillar_width_x <- x_spacing * width
      pillar_width_y <- y_spacing * width
      half_x <- pillar_width_x / 2
      half_y <- pillar_width_y / 2

      all_faces <- list()

      # Debug: check inputs
      if (nrow(data) == 0) {
            warning("create_pillars: No data points provided")
            return(data.frame())
      }

      if (length(selected_faces) == 0) {
            warning("create_pillars: No faces selected")
            return(data.frame())
      }

      for (i in 1:nrow(data)) {
            point <- data[i, ]
            cx <- point$x  # Center x
            cy <- point$y  # Center y
            z_top <- point$z
            z_bottom <- point$zmin

            # Define the 8 corners of the pillar
            corners <- list(
                  # Bottom face (z = z_bottom) - viewed from above, counter-clockwise
                  c(cx - half_x, cy - half_y, z_bottom),  # 1: left-back
                  c(cx + half_x, cy - half_y, z_bottom),  # 2: right-back
                  c(cx + half_x, cy + half_y, z_bottom),  # 3: right-front
                  c(cx - half_x, cy + half_y, z_bottom),  # 4: left-front
                  # Top face (z = z_top) - viewed from above, counter-clockwise
                  c(cx - half_x, cy - half_y, z_top),     # 5: left-back
                  c(cx + half_x, cy - half_y, z_top),     # 6: right-back
                  c(cx + half_x, cy + half_y, z_top),     # 7: right-front
                  c(cx - half_x, cy + half_y, z_top)      # 8: left-front
            )

            # Define faces using corner indices (ordered to form proper rectangles)
            face_definitions <- list(
                  zmin = c(1, 2, 3, 4),  # Bottom face
                  zmax = c(5, 6, 7, 8),  # Top face
                  xmin = c(1, 4, 8, 5),  # Left face
                  xmax = c(2, 6, 7, 3),  # Right face
                  ymin = c(1, 5, 6, 2),  # Back face
                  ymax = c(4, 3, 7, 8)   # Front face
            )

            # Create requested faces
            for (face_name in selected_faces) {
                  if (face_name %in% names(face_definitions)) {
                        corner_indices <- face_definitions[[face_name]]

                        # Create 2-level hierarchical group ID: pillar_id__face_type
                        hierarchical_group <- paste0("pillar", i, "__", face_name)

                        # Create face vertices with hierarchical grouping
                        face_vertices <- data.frame(
                              x = sapply(corner_indices, function(idx) corners[[idx]][1]),
                              y = sapply(corner_indices, function(idx) corners[[idx]][2]),
                              z = sapply(corner_indices, function(idx) corners[[idx]][3]),
                              z_raw = point$z_raw,
                              group = hierarchical_group,
                              pillar_id = i,
                              face_type = face_name
                        )

                        # Preserve all non-coordinate columns
                        non_coord_cols <- setdiff(names(point), names(face_vertices))
                        for (col_name in non_coord_cols) {
                              face_vertices[[col_name]] <- rep(point[[col_name]], 4)
                        }

                        all_faces[[length(all_faces) + 1]] <- face_vertices
                  }
            }
      }

      if (length(all_faces) == 0) {
            return(data.frame())
      }

      # Combine all faces
      result <- do.call(rbind, all_faces)
      rownames(result) <- NULL

      return(result)
}

#' Calculate normals for pillar faces
#'
#' @param pillar_faces Data frame with pillar face vertices
#' @return Matrix of face normals (one row per face, 3 columns for x,y,z)
#' @keywords internal
calculate_pillar_face_normals <- function(pillar_faces) {

      if (nrow(pillar_faces) == 0) {
            return(matrix(nrow = 0, ncol = 3))
      }

      # Get unique faces using the new group column
      unique_faces <- unique(pillar_faces$group)
      normals <- matrix(0, nrow = length(unique_faces), ncol = 3)

      for (i in seq_along(unique_faces)) {
            face_data <- pillar_faces[pillar_faces$group == unique_faces[i], ]
            face_type <- face_data$face_type[1]

            # Use predefined normals for axis-aligned rectangular faces
            normal <- switch(face_type,
                             zmin = c(0, 0, -1),   # Bottom face (points down)
                             zmax = c(0, 0, 1),    # Top face (points up)
                             xmin = c(-1, 0, 0),   # Left face (points left)
                             xmax = c(1, 0, 0),    # Right face (points right)
                             ymin = c(0, -1, 0),   # Back face (points back)
                             ymax = c(0, 1, 0),    # Front face (points front)
                             c(0, 0, 1)            # Default fallback
            )

            normals[i, ] <- normal
      }

      # Expand normals to match number of vertices (4 per face)
      vertex_normals <- matrix(0, nrow = nrow(pillar_faces), ncol = 3)
      for (i in seq_along(unique_faces)) {
            face_indices <- which(pillar_faces$group == unique_faces[i])
            # Assign the same normal to all vertices of this face
            for (idx in face_indices) {
                  vertex_normals[idx, ] <- normals[i, ]
            }
      }

      return(vertex_normals)
}

#' 3D pillar visualization from grid data
#'
#' Creates 3D pillar visualizations from grid data (regular or sparse).
#' Each data point becomes a rectangular 3D column extending from a base level
#' to the data value. Perfect for 3D bar charts, architectural visualization, and terrain layers.
#' Works with both complete regular grids and sparse point data.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomPolygon3D] for proper 3D depth sorting.
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param width Numeric value controlling pillar width as a fraction of grid spacing.
#'   Default is 1.0 (pillars touch each other). Use 0.8 for small gaps, 1.2 for overlap.
#'   Grid spacing is determined automatically using [resolution()].
#' @param faces Character vector specifying which faces to render. Options:
#'   \itemize{
#'     \item \code{"all"} (default): Render all 6 faces
#'     \item \code{"none"}: Render no faces
#'     \item Vector of face names: \code{c("zmax", "xmin", "ymax")}, etc.
#'   }
#'   Valid face names: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax".
#' @param light A lighting specification object created by \code{lighting()}
#' @param zmin Base level for all pillars. When provided as a parameter, overrides any
#'   \code{zmin} aesthetic mapping. If \code{NULL} (default), uses the \code{zmin} aesthetic
#'   if mapped, otherwise defaults to the minimum \code{z} value in the data.
#' @param ... Other arguments passed on to [layer()].
#'
#' @section Aesthetics:
#' `stat_pillar_3d()` requires the following aesthetics:
#' - **x**: X coordinate (grid position)
#' - **y**: Y coordinate (grid position)
#' - **z**: Z coordinate (pillar top height)
#'
#' And optionally understands:
#' - **zmin**: Base level for each pillar (can be overridden by the \code{zmin} parameter)
#'
#' @section Computed variables:
#' - `light`: Computed lighting value (numeric for most methods, hex color for `normal_rgb`)
#' - `normal_x`, `normal_y`, `normal_z`: Face normal components
#' - `group`: Hierarchical group identifier with format "pillarX__face_type" for proper depth sorting
#' - `pillar_id`: Sequential pillar number
#' - `face_type`: Face name ("zmax", "xmin", etc.)
#'
#' @examples
#' # Basic 3D bar chart from regular grid
#' d <- expand.grid(x = 1:5, y = 1:5)
#' d$z <- d$x + d$y + rnorm(25, 0, 0.5)
#'
#' ggplot(d, aes(x, y, z)) +
#'   stat_pillar_3d(aes(fill = after_stat(light))) +
#'   scale_fill_gradient(low = "darkblue", high = "white") +
#'   coord_3d()
#'
#' # Sparse data (only some points)
#' sparse_data <- data.frame(
#'   x = c(1, 3, 2, 4, 1),
#'   y = c(1, 2, 3, 1, 4),
#'   z = c(2, 5, 3, 4, 6)
#' )
#' ggplot(sparse_data, aes(x, y, z)) +
#'   stat_pillar_3d(aes(fill = z)) +
#'   coord_3d()
#'
#' # Set base level for all pillars using parameter
#' ggplot(sparse_data, aes(x, y, z)) +
#'   stat_pillar_3d(aes(fill = z), zmin = 0) +
#'   coord_3d()
#'
#' # Variable base levels using aesthetic
#' d$base_level <- runif(nrow(d), -1, 1)
#' ggplot(d, aes(x, y, z = z, zmin = base_level)) +
#'   stat_pillar_3d(aes(fill = after_stat(light))) +
#'   coord_3d()
#'
#' # Parameter overrides aesthetic
#' ggplot(d, aes(x, y, z = z, zmin = base_level)) +
#'   stat_pillar_3d(aes(fill = after_stat(light)), zmin = -2) +  # All pillars use zmin = -2
#'   coord_3d()
#'
#' # Show only top and front faces for performance
#' ggplot(sparse_data, aes(x, y, z)) +
#'   stat_pillar_3d(aes(fill = after_stat(light)),
#'               faces = c("zmax", "ymax")) +
#'   coord_3d()
#'
#' # Architectural visualization with gaps between pillars
#' ggplot(sparse_data, aes(x, y, z)) +
#'   stat_pillar_3d(aes(fill = after_stat(normal_z)),
#'               width = 0.9, light = lighting("direct")) +
#'   scale_fill_viridis_c() +
#'   coord_3d()
#'
#' @seealso [stat_surface_3d()] for smooth surface rendering, [coord_3d()] for 3D coordinate systems,
#'   [lighting()] for lighting specifications, [GeomPolygon3D] for the default geometry.
#' @export
stat_pillar_3d <- function(mapping = NULL, data = NULL,
                        geom = GeomPolygon3D,
                        position = "identity",
                        width = 1.0,
                        faces = "all",
                        light = lighting(),
                        zmin = NULL,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                        ...) {

      # Set default group mapping using the new hierarchical group
      default_mapping <- aes(group = after_stat(group))

      if (!is.null(mapping)) {
            mapping_names <- names(mapping)
            if (!"group" %in% mapping_names) {
                  mapping <- modifyList(default_mapping, mapping)
            }
      } else {
            mapping <- default_mapping
      }

      layer(
            stat = StatPillar3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, width = width, faces = faces, light = light, zmin = zmin, ...)
      )
}
