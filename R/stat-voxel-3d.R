

StatVoxel3D <- ggproto("StatVoxel3D", Stat,
                     required_aes = c("x", "y", "z"),

                     compute_panel = function(data, scales, na.rm = FALSE,
                                              width = 1.0, faces = "all",
                                              light = NULL) {

                           # Remove missing values if requested
                           if (na.rm) {
                                 data <- data[complete.cases(data[c("x", "y", "z")]), ]
                           }

                           # Check we have enough data
                           if (nrow(data) < 1) {
                                 stop("stat_voxel_3d requires at least 1 point")
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

                           # Create voxels
                           voxel_faces <- create_voxels(data, x_spacing, y_spacing, z_spacing, width, selected_faces)

                           return(attach_light(voxel_faces, light))
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
#' @keywords internal
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
#' @param light A lighting specification object created by \code{light()}, or NULL to disable shading.
#' @param ... Other arguments passed on to the geom (typically `geom_polygon_3d()`), such as
#'   `sort_method` and `scale_depth` as well as aesthetics like `colour`, `fill`, `linewidth`, etc.
#'
#' @section Aesthetics:
#' `stat_voxel_3d()` requires the following aesthetics:
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
#'   x = round(rnorm(100, 0, 2)),
#'   y = round(rnorm(100, 0, 2)),
#'   z = round(rnorm(100, 0, 2))
#' )
#'
#' p <- ggplot(voxel_data, aes(x, y, z)) + coord_3d()
#'
#' # Basic 3D voxel plot
#' p + stat_voxel_3d(fill = "steelblue")
#'
#' # With aesthetic fill
#' p + stat_voxel_3d(aes(fill = z)) +
#'   scale_fill_viridis_c() + guides(fill = guide_colorbar_3d())
#'
#' # Show only visible faces for performance
#' p + stat_voxel_3d(faces = c("zmax", "ymin", "xmin"))
#'
#' @seealso [stat_pillar_3d()] for variable-height columns, [stat_surface_3d()] for smooth surfaces,
#'   [coord_3d()] for 3D coordinate systems, [light()] for lighting specifications,
#'   [GeomPolygon3D] for the default geometry.
#' @export
stat_voxel_3d <- function(mapping = NULL, data = NULL,
                       geom = GeomPolygon3D,
                       position = "identity",
                       width = 1.0,
                       faces = "all",
                       light = ggcube::light(),
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
            stat = StatVoxel3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, width = width, faces = faces, light = light, ...)
      )
}
