# Validate and process faces parameter
# (Helper used by multiple stats)
select_faces <- function(faces) {
      valid_faces <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
      if (length(faces) == 1 && faces == "all") {
            selected_faces <- valid_faces
      } else if (length(faces) == 1 && faces == "none") {
            selected_faces <- character(0)
      } else {
            invalid_faces <- setdiff(faces, valid_faces)
            if (length(invalid_faces) > 0) {
                  stop("Invalid face names: ", paste(invalid_faces, collapse = ", "),
                       ". Valid faces are: ", paste(valid_faces, collapse = ", "))
            }
            selected_faces <- faces
      }
      return(selected_faces)
}

# Convert categorical position columns to numeric positions
# (Helper used by multiple stats)
convert_to_numeric <- function(data) {
      data$z_raw <- data$z # (stash a copy of original z values first)
      if (is.factor(data$x) || is.character(data$x)) {
            data$x <- as.numeric(as.factor(data$x))
      }
      if (is.factor(data$y) || is.character(data$y)) {
            data$y <- as.numeric(as.factor(data$y))
      }
      if (is.factor(data$z) || is.character(data$z)) {
            data$z <- as.numeric(as.factor(data$z))
      }
      data
}


StatVoxel <- ggproto("StatVoxel", Stat,
                     required_aes = c("x", "y", "z"),

                     compute_panel = function(data, scales, na.rm = FALSE,
                                              width = 1.0, faces = "all",
                                              light = lighting()) {

                           # Remove missing values if requested
                           if (na.rm) {
                                 data <- data[complete.cases(data[c("x", "y", "z")]), ]
                           }

                           # Check we have enough data
                           if (nrow(data) < 1) {
                                 stop("stat_voxel requires at least 1 point")
                           }

                           # Generate numeric positions before calculating spacing
                           data <- convert_to_numeric(data)

                           # Calculate voxel spacing using resolution (works for sparse 3D grids)
                           x_spacing <- resolution(data$x, zero = FALSE)
                           y_spacing <- resolution(data$y, zero = FALSE)
                           z_spacing <- resolution(data$z, zero = FALSE)

                           # Fallback for edge cases (single point or identical coordinates)
                           if (is.na(x_spacing) || x_spacing <= 0) {
                                 x_spacing <- 1.0
                           }
                           if (is.na(y_spacing) || y_spacing <= 0) {
                                 y_spacing <- 1.0
                           }
                           if (is.na(z_spacing) || z_spacing <= 0) {
                                 z_spacing <- 1.0
                           }

                           # Validate and process faces parameter
                           selected_faces <- select_faces(faces)

                           # Return empty data frame with required columns for consistency
                           empty_frame <- data.frame(
                                       x = numeric(0), y = numeric(0), z = numeric(0),
                                       group = character(0), voxel_id = integer(0), face_type = character(0),
                                       light = numeric(0),
                                       normal_x = numeric(0), normal_y = numeric(0), normal_z = numeric(0)
                           )
                           if (length(selected_faces) == 0) return(empty_frame)

                           # Create voxels
                           voxel_faces <- create_voxels(data, x_spacing, y_spacing, z_spacing, width, selected_faces)
                           if (nrow(voxel_faces) == 0) return(empty_frame)

                           # Calculate face normals
                           face_normals <- calculate_voxel_face_normals(voxel_faces)

                           # Calculate face centers for positional lighting
                           face_centers <- calculate_voxel_face_centers(voxel_faces)

                           # Apply lighting
                           light_vals <- compute_lighting(face_normals, light, face_centers)

                           # Add lighting and normal components to faces data
                           voxel_faces$light <- light_vals
                           voxel_faces$normal_x <- face_normals[, 1]
                           voxel_faces$normal_y <- face_normals[, 2]
                           voxel_faces$normal_z <- face_normals[, 3]

                           # Add lighting parameters for blend processing
                           voxel_faces$blend_enabled <- light$blend
                           voxel_faces$blend_strength <- light$blend_strength
                           voxel_faces$blend_mode <- light$blend_mode
                           voxel_faces$lighting_method <- light$method

                           # Note: RGB colors are already wrapped with I() in compute_lighting()

                           return(voxel_faces)
                     }
)

#' Create voxel faces from 3D sparse data
#'
#' @param data Data frame with x, y, z columns
#' @param x_spacing Grid spacing in x direction
#' @param y_spacing Grid spacing in y direction
#' @param z_spacing Grid spacing in z direction
#' @param width Width factor (1.0 = full grid spacing)
#' @param selected_faces Character vector of face names to render
#' @return Data frame with voxel face vertices
create_voxels <- function(data, x_spacing, y_spacing, z_spacing, width, selected_faces) {

      voxel_width_x <- x_spacing * width
      voxel_width_y <- y_spacing * width
      voxel_width_z <- z_spacing * width
      half_x <- voxel_width_x / 2
      half_y <- voxel_width_y / 2
      half_z <- voxel_width_z / 2

      all_faces <- list()

      # Debug: check inputs
      if (nrow(data) == 0) {
            warning("create_voxels: No data points provided")
            return(data.frame())
      }

      if (length(selected_faces) == 0) {
            warning("create_voxels: No faces selected")
            return(data.frame())
      }

      for (i in 1:nrow(data)) {
            point <- data[i, ]
            cx <- point$x  # Center x
            cy <- point$y  # Center y
            cz <- point$z  # Center z

            # Define the 8 corners of the voxel (fixed-size cube centered on point)
            corners <- list(
                  # Bottom face (z = cz - half_z)
                  c(cx - half_x, cy - half_y, cz - half_z),  # 1: left-back-bottom
                  c(cx + half_x, cy - half_y, cz - half_z),  # 2: right-back-bottom
                  c(cx + half_x, cy + half_y, cz - half_z),  # 3: right-front-bottom
                  c(cx - half_x, cy + half_y, cz - half_z),  # 4: left-front-bottom
                  # Top face (z = cz + half_z)
                  c(cx - half_x, cy - half_y, cz + half_z),  # 5: left-back-top
                  c(cx + half_x, cy - half_y, cz + half_z),  # 6: right-back-top
                  c(cx + half_x, cy + half_y, cz + half_z),  # 7: right-front-top
                  c(cx - half_x, cy + half_y, cz + half_z)   # 8: left-front-top
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

                        # Create 2-level hierarchical group ID: voxel_id__face_type
                        hierarchical_group <- paste0("voxel", i, "__", face_name)

                        # Create face vertices with hierarchical grouping
                        face_vertices <- data.frame(
                              x = sapply(corner_indices, function(idx) corners[[idx]][1]),
                              y = sapply(corner_indices, function(idx) corners[[idx]][2]),
                              z = sapply(corner_indices, function(idx) corners[[idx]][3]),
                              z_raw = point$z_raw,
                              group = hierarchical_group,
                              voxel_id = i,
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

#' Calculate normals for voxel faces
#'
#' @param voxel_faces Data frame with voxel face vertices
#' @return Matrix of face normals (one row per face, 3 columns for x,y,z)
calculate_voxel_face_normals <- function(voxel_faces) {

      if (nrow(voxel_faces) == 0) {
            return(matrix(nrow = 0, ncol = 3))
      }

      # Get unique faces using the new group column
      unique_faces <- unique(voxel_faces$group)
      normals <- matrix(0, nrow = length(unique_faces), ncol = 3)

      for (i in seq_along(unique_faces)) {
            face_data <- voxel_faces[voxel_faces$group == unique_faces[i], ]
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
      vertex_normals <- matrix(0, nrow = nrow(voxel_faces), ncol = 3)
      for (i in seq_along(unique_faces)) {
            face_indices <- which(voxel_faces$group == unique_faces[i])
            # Assign the same normal to all vertices of this face
            for (idx in face_indices) {
                  vertex_normals[idx, ] <- normals[i, ]
            }
      }

      return(vertex_normals)
}

#' Calculate face centers for voxel faces
#'
#' @param voxel_faces Data frame with voxel face vertices
#' @return Matrix of face centers (one row per face, 3 columns for x,y,z)
calculate_voxel_face_centers <- function(voxel_faces) {

      if (nrow(voxel_faces) == 0) {
            return(matrix(nrow = 0, ncol = 3))
      }

      # Get unique faces using the new group column
      unique_faces <- unique(voxel_faces$group)
      face_centers <- matrix(0, nrow = length(unique_faces), ncol = 3)

      for (i in seq_along(unique_faces)) {
            face_data <- voxel_faces[voxel_faces$group == unique_faces[i], ]

            # Calculate geometric center of the rectangular face (mean of 4 vertices)
            face_centers[i, 1] <- mean(face_data$x)  # Center x
            face_centers[i, 2] <- mean(face_data$y)  # Center y
            face_centers[i, 3] <- mean(face_data$z)  # Center z
      }

      # Expand face centers to match number of vertices (4 per face)
      vertex_face_centers <- matrix(0, nrow = nrow(voxel_faces), ncol = 3)
      for (i in seq_along(unique_faces)) {
            face_indices <- which(voxel_faces$group == unique_faces[i])
            # Assign the same face center to all vertices of this face
            for (idx in face_indices) {
                  vertex_face_centers[idx, ] <- face_centers[i, ]
            }
      }

      return(vertex_face_centers)
}

#' 3D voxel visualization from sparse 3D data
#'
#' Creates 3D voxel visualizations from sparse 3D point data.
#' Each data point becomes a fixed-size cube centered on its coordinates.
#' Useful for volumetric data and 3D pixel art.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomPolygon3D] for proper 3D depth sorting.
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param width Numeric value controlling voxel size as a fraction of grid spacing.
#'   Default is 1.0 (voxels touch each other). Use 0.8 for small gaps, 1.2 for overlap.
#'   Grid spacing is determined automatically using [resolution()] for each dimension.
#' @param faces Character vector specifying which faces to render. Options:
#'   \itemize{
#'     \item \code{"all"} (default): Render all 6 faces
#'     \item \code{"none"}: Render no faces
#'     \item Vector of face names: \code{c("zmax", "xmin", "ymax")}, etc.
#'   }
#'   Valid face names: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax".
#' @param light A lighting specification object created by \code{lighting()}
#' @param ... Other arguments passed on to [layer()].
#'
#' @section Aesthetics:
#' `stat_voxel()` requires the following aesthetics:
#' - **x**: X coordinate (voxel center position)
#' - **y**: Y coordinate (voxel center position)
#' - **z**: Z coordinate (voxel center position)
#'
#' And understands these additional aesthetics:
#' - **fill**: Voxel fill color
#' - **colour**: Voxel border color
#' - **alpha**: Voxel transparency
#'
#' @section Computed variables:
#' - `light`: Computed lighting value (numeric for most methods, hex color for `normal_rgb`)
#' - `normal_x`, `normal_y`, `normal_z`: Face normal components
#' - `group`: Hierarchical group identifier with format "voxel_XXXX__face_type" for proper depth sorting
#' - `voxel_id`: Sequential voxel number
#' - `face_type`: Face name ("zmax", "xmin", etc.)
#'
#' @examples
#' # Sparse 3D voxel data
#' voxel_data <- data.frame(
#'   x = c(1, 2, 3, 2, 1, 3, 4),
#'   y = c(1, 1, 2, 3, 2, 1, 2),
#'   z = c(1, 2, 1, 1, 3, 3, 2)
#' )
#'
#' p <- ggplot(voxel_data, aes(x, y, z)) + coord_3d()
#'
#' # Basic 3D voxel plot
#' p + stat_voxel(aes(fill = z), color = "black") +
#'   scale_fill_viridis_c()
#'
#' # Directional lighting (like sunlight)
#' p + stat_voxel(aes(fill = after_stat(light)),
#'              light = lighting(direction = c(1, 0, .5))) +
#'   scale_fill_gradient(low = "darkgreen", high = "lightgreen")
#'
#' # Show only visible faces for performance
#' p + stat_voxel(faces = c("zmax", "ymax", "xmin"), color = "black")
#'
#' @seealso [stat_pillar()] for variable-height columns, [stat_surface()] for smooth surfaces,
#'   [coord_3d()] for 3D coordinate systems, [lighting()] for lighting specifications,
#'   [GeomPolygon3D] for the default geometry.
#' @export
stat_voxel <- function(mapping = NULL, data = NULL,
                       geom = GeomPolygon3D,
                       position = "identity",
                       width = 1.0,
                       faces = "all",
                       light = lighting(),
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
            stat = StatVoxel, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, width = width, faces = faces, light = light, ...)
      )
}
