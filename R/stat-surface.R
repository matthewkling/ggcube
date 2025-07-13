StatSurface <- ggproto("StatSurface", Stat,
                       required_aes = c("x", "y", "z"),

                       compute_group = function(data, scales, na.rm = FALSE, light = lighting()) {

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

                             face_data <- select(face_data, group) %>%
                                   mutate(light = light_vals,
                                          normal_x = normals[, 1],
                                          normal_y = normals[, 2],
                                          normal_z = normals[, 3])

                             # merge face-level vars back into vertex-level data set
                             faces <- left_join(faces, face_data,
                                                by = join_by(group))
                             if (light$method == "normal_rgb") {
                                   faces$light <- I(faces$light)
                             }

                             # Add lighting parameters for blend processing
                             faces$blend_enabled <- light$blend
                             faces$blend_strength <- light$blend_strength
                             faces$blend_mode <- light$blend_mode
                             faces$lighting_method <- light$method

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
#'
#' @examples
#' # Generate and visualize a basic surface
#' d <- dplyr::mutate(tidyr::expand_grid(x = -20:20, y = -20:20),
#'       z = sqrt(x^2 + y^2) / 1.5,
#'       z = cos(z) - z)
#'
#' p <- ggplot(d, aes(x, y, z)) + coord_3d()
#'
#' # basic surface
#' p + stat_surface(fill = "dodgerblue", color = "darkblue", linewidth = .2)
#'
#' # with 3d lighting
#' p + stat_surface(fill = "darkgreen", color = "darkgreen", linewidth = .2,
#'       light = lighting(blend = "both"))
#'
#' # mesh wireframe, without fill, with aes line color
#' p + stat_surface(aes(color = z), fill = NA) +
#'   scale_color_viridis_c()
#'
#' ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
#'   stat_surface(light = lighting(method = "diffuse", direction = c(1, 0, .5),
#'                            blend = "both", blend_mode = "hsv", blend_strength = .9),
#'                linewidth = .2) +
#'   coord_3d(roll = 125, pitch = 0, yaw = 150,
#'            ratio = c(1, 1.5, .5)) +
#'   theme_light() +
#'   scale_fill_gradientn(colors = c("darkgreen", "rosybrown4", "gray60")) +
#'   scale_color_gradientn(colors = c("darkgreen", "rosybrown4", "gray60"))
#'
#' @export
stat_surface <- function(mapping = NULL, data = NULL,
                         geom = GeomPolygon3D,
                         position = "identity",
                         light = lighting(),
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {

      # Set default group mapping like stat_surface does
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
            mutate(quad_id = 1:nrow(.))

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
            mutate(quad_id = dx$quad_id)

      d <- bind_rows(data, dx, dxy, dy) %>%
            na.omit() %>%
            group_by(quad_id) %>%
            filter(n() == 4) %>%
            arrange(x, y) %>%
            mutate(vertex_order = c(1, 2, 4, 3)) %>%
            arrange(x) %>%
            mutate(dzdx = (mean(z[3:4]) - mean(z[1:2])) / (mean(x[3:4]) - mean(x[1:2]))) %>%
            arrange(y) %>%
            mutate(dzdy = (mean(z[3:4]) - mean(z[1:2])) / (mean(y[3:4]) - mean(y[1:2])),
                   slope = sqrt(dzdy^2 + dzdx^2),
                   aspect = atan2(dzdy, dzdx)) %>%
            ungroup() %>%
            arrange(quad_id, vertex_order) %>%
            mutate(group = paste0("surface__quad", quad_id, "::", group)) %>%
            as.data.frame()

      return(d)
}
