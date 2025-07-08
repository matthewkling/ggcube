StatSurface <- ggproto("StatSurface", Stat,
                       required_aes = c("x", "y", "z"),

                       compute_group = function(data, scales, na.rm = FALSE,
                                                light = lighting()) {

                             # Remove missing values if requested
                             if (na.rm) {
                                   data <- data[complete.cases(data[c("x", "y", "z")]), ]
                             }

                             # Check we have enough data
                             if (nrow(data) < 4) {
                                   stop("stat_surface requires at least 4 points")
                             }

                             # Detect grid structure
                             grid_info <- detect_grid_structure(data)

                             if (!grid_info$is_regular || !grid_info$is_complete) {
                                   stop("Data must be on a regular, complete grid. Each x,y combination should appear exactly once.")
                             }

                             # Create quadrilateral faces from grid cells
                             faces <- create_grid_quads(data)

                             # Get unique faces for normal/lighting computation
                             face_data <- faces %>%
                                   group_by(group) %>%
                                   slice(1) %>%  # One row per face
                                   ungroup()

                             # Compute surface normals from gradients
                             normals <- matrix(nrow = nrow(face_data), ncol = 3)
                             normals[, 1] <- -face_data$dzdx  # Normal x component
                             normals[, 2] <- -face_data$dzdy  # Normal y component
                             normals[, 3] <- 1                # Normal z component

                             # Normalize the normal vectors
                             normal_lengths <- sqrt(rowSums(normals^2))
                             normals <- normals / normal_lengths

                             # Calculate face centers for positional lighting
                             face_centers <- matrix(nrow = nrow(face_data), ncol = 3)
                             for (i in seq_len(nrow(face_data))) {
                                   face_group <- face_data$group[i]
                                   face_vertices <- faces[faces$group == face_group, ]

                                   # Calculate geometric center of the quadrilateral face
                                   face_centers[i, 1] <- mean(face_vertices$x)  # Center x
                                   face_centers[i, 2] <- mean(face_vertices$y)  # Center y
                                   face_centers[i, 3] <- mean(face_vertices$z)  # Center z
                             }

                             # Apply lighting models to the normals
                             light_vals <- compute_lighting(normals, light, face_centers)

                             # Expand lighting values to match vertices (4 per face)
                             light_expanded <- rep(light_vals, each = 4)

                             # Re-apply identity scaling for RGB colors after rep()
                             if (light$method == "normal_rgb") {
                                   light_expanded <- I(light_expanded)
                             }

                             # Add lighting and normal components to faces data
                             faces$light <- light_expanded
                             faces$normal_x <- rep(normals[, 1], each = 4)
                             faces$normal_y <- rep(normals[, 2], each = 4)
                             faces$normal_z <- rep(normals[, 3], each = 4)

                             # Add lighting parameters for blend processing
                             faces$blend_enabled <- light$blend
                             faces$blend_strength <- light$blend_strength
                             faces$blend_mode <- light$blend_mode
                             faces$lighting_method <- light$method

                             # Ensure faces are sorted by group then order for proper rendering
                             if ("order" %in% names(faces)) {
                                   faces <- faces[order(faces$group, faces$order), ]
                             }

                             # Add computed variables (keep your group naming)
                             faces$face_id <- faces$group

                             return(faces)
                       }
)

#' 3D surface from regular grid data
#'
#' Creates 3D surfaces from regularly gridded data (like elevation maps).
#' Assumes data is on a regular x,y grid and creates quadrilateral faces.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomPolygon3D] for proper 3D depth sorting.
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param light A lighting specification object created by \code{lighting()}
#' @param ... Other arguments passed on to [layer()].
#'
#' @section Aesthetics:
#' `stat_surface()` requires the following aesthetics:
#' - **x**: X coordinate
#' - **y**: Y coordinate
#' - **z**: Z coordinate (elevation/height)
#'
#' @section Computed variables:
#' - `light`: Computed lighting value (numeric for most methods, hex color for `normal_rgb`)
#' - `normal_x`, `normal_y`, `normal_z`: Surface normal components
#' - `slope`: Gradient magnitude from original surface calculations
#' - `aspect`: Direction of steepest slope from original surface calculations
#' - `dzdx`, `dzdy`: Partial derivatives from original surface calculations
#' - `face_id`: Quad group identifier
#'
#' @examples
#' # Generate and visualize a basic surface
#' d <- mutate(expand_grid(x = -20:20, y = -20:20),
#'       z = sqrt(x^2 + y^2) / 1.5,
#'       z = cos(z) - z)
#' ggplot(d, aes(x, y, z)) +
#'   stat_surface(aes(fill = after_stat(normal_x))) +
#'   scale_fill_viridis_c() +
#'   coord_3d(pitch = 0, roll = 130, yaw = 30)
#'
#' @export
stat_surface <- function(mapping = NULL, data = NULL,
                         geom = GeomPolygon3D,
                         position = "identity",
                         light = lighting(),
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {

      # Set default group mapping like stat_surface does
      default_mapping <- aes(group = after_stat(face_id))

      if (!is.null(mapping)) {
            mapping_names <- names(mapping)
            if (!"group" %in% mapping_names) {
                  mapping <- modifyList(default_mapping, mapping)
            }
      } else {
            mapping <- default_mapping
      }

      layer(
            stat = StatSurface, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, light = light, ...)
      )
}

# Helper function to detect if data is on regular grid
detect_grid_structure <- function(data) {

      # Get unique values
      x_unique <- sort(unique(data$x))
      y_unique <- sort(unique(data$y))

      # Check for regular spacing
      x_regular <- TRUE
      y_regular <- TRUE

      if (length(x_unique) > 1) {
            x_diffs <- diff(x_unique)
            x_regular <- all(abs(x_diffs - x_diffs[1]) < 1e-10)
      }

      if (length(y_unique) > 1) {
            y_diffs <- diff(y_unique)
            y_regular <- all(abs(y_diffs - y_diffs[1]) < 1e-10)
      }

      # Check if we have complete grid (every x,y combination)
      expected_points <- length(x_unique) * length(y_unique)
      has_complete_grid <- nrow(data) == expected_points

      # Check for duplicates
      has_duplicates <- any(duplicated(data[c("x", "y")]))

      list(
            is_regular = x_regular && y_regular,
            is_complete = has_complete_grid && !has_duplicates,
            x_vals = x_unique,
            y_vals = y_unique,
            n_x = length(x_unique),
            n_y = length(y_unique)
      )
}

#' Create quadrilateral faces from grid data
#'
#' @param data Regular grid data frame
#' @return Data frame with quad faces and computed gradients
create_grid_quads <- function(data) {
      data <- data %>%
            ungroup() %>%
            mutate(group = 1:nrow(.))

      dy <- data %>%
            group_by(x) %>%
            mutate(y = lag(y),
                   z = lag(z)) %>%
            ungroup()

      dx <- data %>%
            group_by(y) %>%
            mutate(x = lag(x),
                   z = lag(z)) %>%
            ungroup()

      dxy <- data.frame(x = dx$x,
                        y = dy$y) %>%
            left_join(data, by = join_by(x, y)) %>%
            mutate(group = dx$group)

      d <- bind_rows(data, dx, dxy, dy) %>%
            na.omit() %>%
            group_by(group) %>%
            filter(n() == 4) %>%
            arrange(x, y) %>%
            mutate(order = c(1, 2, 4, 3)) %>%
            arrange(x) %>%
            mutate(dzdx = (mean(z[3:4]) - mean(z[1:2])) / (mean(x[3:4]) - mean(x[1:2]))) %>%
            arrange(y) %>%
            mutate(dzdy = (mean(z[3:4]) - mean(z[1:2])) / (mean(y[3:4]) - mean(y[1:2])),
                   slope = sqrt(dzdy^2 + dzdx^2),
                   aspect = atan2(dzdy, dzdx)) %>%
            ungroup() %>%
            arrange(group, order) %>%
            as.data.frame()

      return(d)
}
