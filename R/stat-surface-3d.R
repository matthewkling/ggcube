StatSurface3D <- ggproto("StatSurface3D", Stat,
                       required_aes = c("x", "y", "z"),

                       compute_group = function(data, scales, na.rm = FALSE, light = lighting()) {

                             # Remove missing values if requested
                             if (na.rm) {
                                   data <- data[complete.cases(data[c("x", "y", "z")]), ]
                             }

                             # Check we have enough data
                             if (nrow(data) < 4) {
                                   stop("stat_surface_3d requires at least 4 points")
                             }

                             # Detect grid structure
                             grid_info <- detect_grid_structure(data)
                             if (!grid_info$is_regular || !grid_info$is_complete) {
                                   stop("Data must be on a regular, complete grid. Each x,y combination should appear exactly once.")
                             }

                             # Process surface using common pipeline
                             return(process_surface_grid(data, light))
                       }
)

#' Process regular grid data into 3D surface with lighting
#'
#' Common pipeline for converting regular grid data into quadrilateral faces
#' with surface normals, lighting, and blend parameters. Used by stat_surface_3d,
#' stat_function_3d, and stat_smooth_3d.
#'
#' @param grid_data Data frame with x, y, z columns on a regular grid
#' @param light Lighting specification object
#' @return Data frame with face vertices, normals, and lighting
#' @keywords internal
process_surface_grid <- function(grid_data, light = lighting()) {

      # Add grouping variable if not present (required for face processing)
      if (!"group" %in% names(grid_data)) {
            grid_data$group <- 1
      }

      # Create quadrilateral faces from grid cells
      faces <- create_grid_quads(grid_data)

      # Get unique faces for normal/lighting computation
      face_data <- faces %>%
            group_by(group) %>%
            slice(1) %>%  # One row per face
            ungroup()

      # Compute surface normals and face centers
      normals <- compute_surface_normals(face_data)
      face_centers <- calculate_surface_face_centers(faces, face_data)

      # Apply lighting models
      face_data_with_lighting <- apply_surface_lighting(face_data, normals, face_centers, light)

      # Merge face-level vars back into vertex-level data set
      faces <- left_join(faces, face_data_with_lighting, by = join_by(group))

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

#' Compute surface normals from face gradients
#'
#' @param face_data Data frame with unique faces containing dzdx and dzdy
#' @return Matrix with normalized normal vectors (one row per face, 3 columns)
#' @keywords internal
compute_surface_normals <- function(face_data) {
      # Compute surface normals from gradients
      normals <- matrix(nrow = nrow(face_data), ncol = 3)
      normals[, 1] <- -face_data$dzdx  # Normal x component
      normals[, 2] <- -face_data$dzdy  # Normal y component
      normals[, 3] <- 1                # Normal z component

      # Normalize the normal vectors
      normal_lengths <- sqrt(rowSums(normals^2))
      normals <- normals / normal_lengths

      return(normals)
}

#' Calculate face centers for positional lighting
#'
#' @param faces Data frame with all face vertices
#' @param face_data Data frame with unique faces
#' @return Matrix with face centers (one row per face, 3 columns)
#' @keywords internal
calculate_surface_face_centers <- function(faces, face_data) {
      face_centers <- matrix(nrow = nrow(face_data), ncol = 3)

      for (i in seq_len(nrow(face_data))) {
            face_group <- face_data$group[i]
            face_vertices <- faces[faces$group == face_group, ]

            # Calculate geometric center of the quadrilateral face
            face_centers[i, 1] <- mean(face_vertices$x)  # Center x
            face_centers[i, 2] <- mean(face_vertices$y)  # Center y
            face_centers[i, 3] <- mean(face_vertices$z)  # Center z
      }

      return(face_centers)
}

#' Apply lighting models to surface normals
#'
#' @param face_data Data frame with unique faces (group column)
#' @param normals Matrix of surface normals
#' @param face_centers Matrix of face centers
#' @param light Lighting specification object
#' @return Data frame with lighting values and normal components
#' @keywords internal
apply_surface_lighting <- function(face_data, normals, face_centers, light) {
      # Apply lighting models to the normals
      light_vals <- compute_lighting(normals, light, face_centers)

      face_data_with_lighting <- select(face_data, group) %>%
            mutate(light = light_vals,
                   normal_x = normals[, 1],
                   normal_y = normals[, 2],
                   normal_z = normals[, 3])

      return(face_data_with_lighting)
}

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
#' `stat_surface_3d()` requires the following aesthetics:
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
#' p + stat_surface_3d(fill = "dodgerblue", color = "darkblue", linewidth = .2)
#'
#' # with 3d lighting
#' p + stat_surface_3d(fill = "darkgreen", color = "darkgreen", linewidth = .2,
#'       light = lighting(blend = "both"))
#'
#' # mesh wireframe, without fill, with aes line color
#' p + stat_surface_3d(aes(color = z), fill = NA) +
#'   scale_color_viridis_c()
#'
#' # use `group` to plot data for multiple surfaces
#' # (depth rendering works fine unless the surfaces intersect)
#' d <- expand.grid(x = -5:5, y = -5:5)
#' d$z <- d$x^2 - d$y^2
#' d$g <- "a"
#' d2 <- d
#' d2$z <- d$z + 10
#' d2$g <- "b"
#' ggplot(rbind(d, d2),
#'        aes(x, y, z, group = g, fill = g)) +
#'   coord_3d() +
#'   stat_surface_3d(color = "black", alpha = .5)
#'
#' ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
#'   stat_surface_3d(light = lighting(method = "diffuse", direction = c(1, 0, .5),
#'                            blend = "both", blend_mode = "hsv", blend_strength = .9),
#'                linewidth = .2) +
#'   coord_3d(ratio = c(1, 1.5, .5)) +
#'   theme_light() +
#'   scale_fill_gradientn(colors = c("darkgreen", "rosybrown4", "gray60")) +
#'   scale_color_gradientn(colors = c("darkgreen", "rosybrown4", "gray60"))
#'
#' @export
stat_surface_3d <- function(mapping = NULL, data = NULL,
                         geom = GeomPolygon3D,
                         position = "identity",
                         light = lighting(),
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {

      layer(
            stat = StatSurface3D, data = data, mapping = mapping, geom = geom,
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
#' @keywords internal
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
