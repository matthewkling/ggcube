#' Lighting specification for 3D surface rendering
#'
#' Creates a lighting specification object for use with 3D polygon layers.
#' Lighting modifies the brightness of fill and/or base color aesthetics based on surface
#' orientation (i.e., it implements form shadows but not cast shadows).
#' Various options are available to control light qualities and light source location.
#' The result can be passed `coord_3d()` to for application to the whole plot, or to individual
#' `geom_*_3d()` layer functions (which take precedence over plot-level lighting).
#'
#' Note that light-like effects can also be achieved in some stats by mapping color
#' aesthestics to computed variables such as `after_stat(dzdx)`; see [geom_surface_3d()]
#' for examples.
#'
#' @param method Character string specifying lighting model:
#'   \itemize{
#'     \item \code{"diffuse"}: The default. Atmospheric lighting with soft shadows (only surfaces pointing
#'          directly away from light source are fully dark; base color occurs on surfaces perpendicular to light)
#'     \item \code{"direct"}: Direct lighting with hard shadows (all surfaces angled beyond 90 degrees from
#'          light source are fully dark; base color occurs on surfaces angled 45 degrees toward light)
#'     \item \code{"rgb"}: Map surface orientation to three dimensional color space. Rather than the light model
#'          darkening or brightening base colors as in the other models, this model generates a wholly new set of
#'           colors. If this option is selected, parameters like `mode` and `contrast` are ignored, but
#'           `fill`, `color`, and `direction` still apply.
#'   }
#' @param mode Character string specifying color lighting mode:
#'   \itemize{
#'     \item \code{"hsv"}: The default. Modifies _value_ component of _HSV_ color
#'          (fades to bright colors at high end, black at low end).
#'     \item \code{"hsl"}: Modifies _lightness_ component of _HSL_ color
#'          (fades to white at high end, black at low end).
#'   }
#'   These two options give identical results for grayscale colors.
#' @param fill Logical indicating whether to apply lighting to fill colors. Default is TRUE.
#' @param color Logical indicating whether to apply lighting to border/line colors. Default is TRUE.
#' @param contrast Numeric value greater than zero controlling the intensity of lighting effects.
#'   1.0 (the default) gives full black-to-white range. Values less than 1 give subtler effects, while
#'   values greater than 1 give more dramatic effects.
#' @param direction Numeric vector of length 3 specifying direction in 3D space (x, y, z) that
#'   light comes from for directional lighting. Depending on `anchor`, direction is relative to
#'   either the plot axes or the viewing pane. The default is \code{c(-.5, 0, 1)}, giving
#'   diagonal lighting from the upper right edge with default rotation and anchor. At least
#'   one value must be non-zero. Values are automatically normalized, so magnitude doesn't
#'   matter, only sign and relative magnitude. Direction is specified in visual space (relative
#'   to the rendered cube's bounding box), not data space. This argument is ignored if
#'   \code{position} is provided.
#' @param anchor Character string specifying the reference frame for light direction:
#'   \itemize{
#'     \item \code{"scene"} (default): Light `direction` is fixed relative to the data/scene.
#'       Light rotates with the plot, so regardless of rotation, `direction = c(1, 0, 0)`
#'       lights surfaces facing the "xmax" cube face, for example.
#'     \item \code{"camera"}: Light `direction` is fixed relative to the camera/viewer.
#'       Light direction stays in place regardless of rotation, so `direction = c(1, 0, 0)`
#'       lights surfaces facing the right side of the plot, for example.
#'   }
#'   When all rotation parameters are zero, these options give the same result.
#' @param position Numeric vector of length 3 specifying light source position in
#'   data coordinate space for positional lighting. When specified, each face gets
#'   its own light direction calculated from the light position to the face center.
#'   Mutually exclusive with \code{direction}. Default is NULL (use directional lighting).
#' @param distance_falloff Logical indicating whether to apply distance-based
#'   intensity falloff for positional lighting using inverse square law
#'   (intensity ∝ 1/distance²). Only used when \code{position} is specified.
#'   Default is FALSE.
#' @param backface_scale,backface_offset Numeric values that determine how "frontface" light values get
#'   modified (scaled and then offset) to derive "backface" light values. A backface is the side of a
#'   polygon that faces the underside of a surface or the inside of a volume. Frontface light values are
#'   typically in the range `[-1, 1]` (unless `contrast` is boosted). The default scale of -1 gives
#'   backfaces highly contrasting lighting to frontfaces. To light backfaces the same as frontfaces,
#'   set scale to 1. To uniformly darken (brighten) all backfaces, use a negative (positive) offset.
#'
#' @return A \code{lighting} object that can be passed to polygon-based 3D layers or to `coord_3d()`.
#' @examples
#' # base plot used in examples
#' p <- ggplot(sphere_points, aes(x, y, z)) +
#'   geom_hull_3d(fill = "#9e2602", color = "#5e1600")
#'
#' # Light qualities ----------------------------------------------------------
#'
#' # default `"diffuse"` lighting
#' p + coord_3d()
#'
#' # use `"direct"` lighting to apply full shade to unlit surfaces
#' p + coord_3d(light = light(method = "direct"))
#'
#' # use `"hsl"` mode to fade highlights to white
#' p + coord_3d(light = light(mode = "hsl"))
#'
#' # adjust lighting intensity with `contrast`
#' p + coord_3d(light = light(mode = "hsl", contrast = 1.5))
#'
#' # use `"rgb"` lighting to map face orientation to 3D color space
#' p + coord_3d(light = light(method = "rgb"))
#'
#'
#' # Lighting targets ---------------------------------------------------------
#'
#' # use `fill` and `color` to select which aesthetics get modified by light
#' p + coord_3d(light = light(fill = TRUE, color = FALSE))
#'
#' # apply lighting to aesthetically mapped colors, with shaded guide to match
#' p + geom_hull_3d(aes(fill = x, color = x)) +
#'       scale_fill_viridis_c() +
#'       scale_color_viridis_c() +
#'       guides(fill = guide_colorbar_3d()) +
#'       coord_3d(light = light(mode = "hsl", contrast = .9))
#'
#' # disable lighting entirely
#' # (equivalent to specifying `light(fill = FALSE, color = FALSE`))
#' p + coord_3d(light = "none")
#'
#' # if provided, layer-level lighting overrides plot-level (coord_3d) lighting
#' p + coord_3d(light = light("direct"), # plot-level: affects original layer
#'       scales = "fixed") +
#'   geom_hull_3d(aes(x = x + 2.5), fill = "#9e2602", color = "#5e1600",
#'       light = light("direct", mode = "hsl", direction = c(0, -1, 0)))
#'
#'
#' # Light sources ------------------------------------------------------------
#'
#' # directional light from the xmin-ymin-zmin direction
#' # (`direction` is relative to rotated axes with default `anchor = "scene"`)
#' p + coord_3d(light = light(direction = c(-1, -1, -1)))
#'
#' # directional light from top-right corner of figure
#' # (`anchor = "camera"` makes `direction` fixed relative to the plot)
#' p + coord_3d(light = light(direction = c(1, 1, 0), anchor = "camera"))
#'
#' # positional light source within plot
#' ggplot(mountain, aes(x, y, z)) +
#'   stat_surface_3d(fill = "red", color = "red") +
#'   coord_3d(
#'     light = light(position = c(.5, .7, 95), distance_falloff = TRUE,
#'                   mode = "hsl", contrast = .9),
#'     ratio = c(1.5, 2, 1))
#'
#'
#' # Backface lighting --------------------------------------------------------
#'
#' # backfaces get "opposite" lighting by default (`backface_scale = -1`)
#' p <- ggplot() +
#'   geom_function_3d(fun = function(x, y) x^2 + y^2,
#'     xlim = c(-3, 3), ylim = c(-3, 3),
#'     fill = "steelblue", color = "steelblue")
#' p + coord_3d(pitch = 0, roll = -70, yaw = 0,
#'              light = light(mode = "hsl"))
#'
#' # use `backface_scale = 1` to light backfaces as if they're frontfaces
#' p + coord_3d(pitch = 0, roll = -70, yaw = 0,
#'              light = light(backface_scale = 1, mode = "hsl"))
#'
#' # use `backface_offset` to uniformly darken (or lighten) backfaces
#' p + coord_3d(pitch = 0, roll = -70, yaw = 0,
#'              light = light(backface_scale = 1, mode = "hsl",
#'                            backface_offset = -.5))
#'
#' @seealso \code{\link{scale_colorbar_shade}}
#' @export
light <- function(method = "diffuse",
                  direction = c(-.5, 0, 1),
                  anchor = "scene",
                  position = NULL,
                  distance_falloff = FALSE,
                  fill = TRUE,
                  color = TRUE,
                  mode = "hsv",
                  contrast = 1.0,
                  backface_scale = -1,
                  backface_offset = 0) {

      # Validate method
      valid_methods <- c("direct", "diffuse", "rgb")
      if (!method %in% valid_methods) {
            stop("method must be one of: ", paste(valid_methods, collapse = ", "))
      }

      # Validate direction
      if (!is.numeric(direction) || length(direction) != 3) {
            stop("direction must be a numeric vector of length 3")
      }

      if (all(direction == 0)) {
            stop("direction must contain at least one nonzero value")
      }

      # Validate position if provided
      if (!is.null(position)) {
            if (!is.numeric(position) || length(position) != 3) {
                  stop("position must be a numeric vector of length 3")
            }
      }

      # Validate distance_falloff
      if (!is.logical(distance_falloff) || length(distance_falloff) != 1) {
            stop("distance_falloff must be a single logical value")
      }

      # Validate fill and color
      if (!is.logical(fill) || length(fill) != 1) {
            stop("fill must be a single logical value")
      }
      if (!is.logical(color) || length(color) != 1) {
            stop("color must be a single logical value")
      }

      # Validate contrast
      if (!is.numeric(contrast) || length(contrast) != 1 || contrast < 0) {
            stop("contrast must be a single non-negative numeric value")
      }

      # Validate mode
      valid_modes <- c("hsv", "hsl")
      if (!mode %in% valid_modes) {
            stop("mode must be one of: ", paste(valid_modes, collapse = ", "))
      }

      # Validate anchor
      valid_anchors <- c("scene", "camera")
      if (!anchor %in% valid_anchors) {
            stop("anchor must be one of: ", paste(valid_anchors, collapse = ", "))
      }

      # Convert new API to old internal representation for backward compatibility
      if (fill && color) {
            shade <- "both"
      } else if (fill) {
            shade <- "fill"
      } else if (color) {
            shade <- "colour"
      } else {
            shade <- "neither"
      }

      # Create lighting specification object using original internal structure
      structure(
            list(
                  method = method,
                  direction = direction,
                  position = position,
                  distance_falloff = distance_falloff,
                  shade = shade,
                  shade_strength = contrast,
                  shade_mode = mode,
                  anchor = anchor,
                  backface_scale = backface_scale,
                  backface_offset = backface_offset
            ),
            class = "light"
      )
}

#' @export
print.light <- function(x, ...) {
      cat("Lighting specification:\n")
      cat("  Method:", x$method, "\n")

      if (!is.null(x$position)) {
            cat("  Position: [", paste(x$position, collapse = ", "), "] (positional lighting)\n")
            if (x$distance_falloff) {
                  cat("  Distance falloff: enabled\n")
            } else {
                  cat("  Distance falloff: disabled\n")
            }
      } else {
            cat("  Direction: [", paste(x$direction, collapse = ", "), "] (directional lighting)\n")
      }

      cat("  anchor:", x$anchor, "\n")

      # Show what gets shaded using new API terms
      shading_targets <- c()
      if (x$shade %in% c("fill", "both")) shading_targets <- c(shading_targets, "fill")
      if (x$shade %in% c("colour", "both")) shading_targets <- c(shading_targets, "color")

      if (length(shading_targets) > 0) {
            cat("  Shading:", paste(shading_targets, collapse = " + "),
                "(contrast =", x$shade_strength, ", mode =", x$shade_mode, ")\n")
      } else {
            cat("  Shading: disabled\n")
      }

      invisible(x)
}


#' Apply lighting models to surface normals with positional light support
#'
#' Computes lighting values from surface normals using various lighting models.
#' Supports both directional lighting (parallel rays) and positional lighting
#' (point light sources with per-face light directions).
#'
#' @param normals Matrix with 3 columns (x, y, z normal components), where each
#'   row represents a face normal vector. Should be unit vectors (normalized).
#' @param light A lighting specification object created by \code{light()}
#' @param face_centers Matrix with 3 columns (x, y, z coordinates) representing
#'   the center position of each face in data coordinate space. Required for
#'   positional lighting, optional for directional lighting.
#' @return Vector of lighting values. For most methods, returns numeric values.
#'   For \code{method = "rgb"}, returns hex color strings with \code{I()}
#'   class for identity scaling.
#' @keywords internal
compute_light <- function(normals, lighting, face_centers = NULL) {

      # Validate inputs
      if (!is.matrix(normals) || ncol(normals) != 3) {
            stop("normals must be a matrix with 3 columns (x, y, z)")
      }

      params <- lighting

      # Validate resolved parameters
      valid_light <- c("direct", "diffuse", "rgb", "normal_x", "normal_y", "normal_z")
      if (!params$method %in% valid_light) {
            stop("lighting method must be one of: ", paste(valid_light, collapse = ", "))
      }

      # Check if positional lighting is requested
      use_positional <- !is.null(params$position)

      if (use_positional) {
            # Positional lighting: need face centers
            if (is.null(face_centers)) {
                  stop("face_centers required for positional lighting")
            }
            if (!is.matrix(face_centers) || ncol(face_centers) != 3 || nrow(face_centers) != nrow(normals)) {
                  stop("face_centers must be a matrix with 3 columns and same number of rows as normals")
            }

            # Calculate per-face light directions
            light_directions <- calculate_positional_light_directions(face_centers, params$position)

            # Calculate per-face dot products
            dot_products <- rowSums(normals * light_directions)

            # Apply distance falloff if requested
            if (params$distance_falloff) {
                  distances <- sqrt(rowSums((face_centers - matrix(rep(params$position, nrow(face_centers)),
                                                                   nrow = nrow(face_centers), byrow = TRUE))^2))
                  # Prevent division by zero and extreme values
                  distances <- pmax(distances, 0.1)
                  falloff_factor <- 1 / (distances^2)
                  # Normalize falloff to prevent extreme values
                  falloff_factor <- falloff_factor / max(falloff_factor)

                  # For diffuse lighting, interpolate between -1 (dark) and full lighting value
                  if (params$method == "diffuse") {
                        dot_products <- -1 + falloff_factor * (dot_products + 1)
                  } else {
                        # For other methods, scale toward appropriate dark value
                        if (params$method == "direct") {
                              # Direct: interpolate between 0 (dark) and full value
                              dot_products <- dot_products * falloff_factor
                        } else {
                              # General case: scale the lighting values
                              dot_products <- dot_products * falloff_factor
                        }
                  }
            }

      } else {
            # Directional lighting: uniform direction for all faces
            if (!is.numeric(params$direction) || length(params$direction) != 3) {
                  stop("light direction must be a numeric vector of length 3")
            }

            # Normalize light direction
            light_dir_norm <- normalize_light_direction(params$direction)

            # Compute dot products between normals and light direction
            dot_products <- compute_light_dot_products(normals, light_dir_norm)
      }

      # Apply lighting model (same for both directional and positional)
      light <- switch(params$method,
                      direct = pmax(0, dot_products),             # Direct lighting with hard shadows
                      diffuse = dot_products,                     # Atmospheric lighting: full -1 to +1 range
                      rgb = {                              # Map normals to RGB hex colors
                            if (use_positional) {
                                  # For positional lighting, use average light direction for RGB mapping
                                  avg_light_dir <- colMeans(light_directions)
                                  compute_rgb_light(normals, avg_light_dir)
                            } else {
                                  light_dir_norm <- normalize_light_direction(params$direction)
                                  compute_rgb_light(normals, light_dir_norm)
                            }
                      },
                      normal_x = (normals[,1] + 1) / 2,           # X-normal as color (0-1 range)
                      normal_y = (normals[,2] + 1) / 2,           # Y-normal as color (0-1 range)
                      normal_z = (normals[,3] + 1) / 2,           # Z-normal as color (0-1 range)
                      stop("Unknown lighting method")
      )

      # Apply quantization if requested
      if (!is.null(params$quanta) && !params$method %in% c("rgb", "normal_x", "normal_y", "normal_z")) {
            light <- apply_quantization(light, params$method, params$quanta)
      }

      # Auto-wrap RGB colors with I() for identity scaling
      if (params$method == "rgb") {
            light <- I(light)
      }

      return(light)
}

#' Calculate per-face light directions for positional lighting
#'
#' @param face_centers Matrix with 3 columns (x, y, z face center coordinates)
#' @param light_position Numeric vector of length 3 (x, y, z light position)
#' @return Matrix with 3 columns (normalized light direction vectors)
#' @keywords internal
calculate_positional_light_directions <- function(face_centers, light_position) {
      # Calculate light directions from each face center TO the light position
      light_vectors <- matrix(rep(light_position, nrow(face_centers)),
                              nrow = nrow(face_centers), byrow = TRUE) - face_centers

      # Normalize each light direction vector
      light_lengths <- sqrt(rowSums(light_vectors^2))
      # Prevent division by zero
      light_lengths <- pmax(light_lengths, 1e-10)

      # Return normalized light directions
      light_vectors / light_lengths
}

#' Normalize light direction vector
#'
#' @param light_dir Numeric vector of length 3
#' @return Normalized light direction vector
#' @keywords internal
normalize_light_direction <- function(light_dir) {
      light_dir / sqrt(sum(light_dir^2))
}

#' Compute dot products between normals and light direction
#'
#' @param normals Matrix with 3 columns (x, y, z normal components)
#' @param light_dir_norm Normalized light direction vector
#' @return Vector of dot products
#' @keywords internal
compute_light_dot_products <- function(normals, light_dir_norm) {
      rowSums(normals * matrix(rep(light_dir_norm, nrow(normals)),
                               nrow = nrow(normals), byrow = TRUE))
}

#' Compute normal-to-RGB mapping with light direction rotation
#'
#' Maps surface normals to RGB colors with rotation so that normals aligned
#' with light_dir map to white/bright colors.
#'
#' @param normals Matrix with 3 columns (x, y, z normal components)
#' @param light_dir_norm Normalized light direction vector
#' @return Character vector of hex color codes
#' @keywords internal
compute_rgb_light <- function(normals, light_dir_norm) {

      # Target direction for white color in RGB space
      white_dir <- c(1, 1, 1) / sqrt(3)

      # Create rotation matrix using Rodrigues' formula
      # We want to rotate FROM white_dir TO light_dir_norm
      # So we can apply the inverse rotation to normals

      # Cross product for rotation axis (white_dir × light_dir_norm)
      v <- c(
            white_dir[2] * light_dir_norm[3] - white_dir[3] * light_dir_norm[2],
            white_dir[3] * light_dir_norm[1] - white_dir[1] * light_dir_norm[3],
            white_dir[1] * light_dir_norm[2] - white_dir[2] * light_dir_norm[1]
      )

      s <- sqrt(sum(v^2))  # sine of angle
      c <- sum(white_dir * light_dir_norm)  # cosine of angle

      if (s < 1e-10) {
            # Vectors are already aligned or opposite
            if (c > 0) {
                  rot_matrix <- diag(3)  # Identity matrix
            } else {
                  # 180 degree rotation - use any perpendicular axis
                  rot_matrix <- -diag(3)
            }
      } else {
            # Rodrigues' rotation formula
            # R = I + [v]× + [v]×^2 * (1-c)/s^2
            v_cross <- matrix(c(
                  0, -v[3], v[2],
                  v[3], 0, -v[1],
                  -v[2], v[1], 0
            ), nrow = 3, byrow = TRUE)

            rot_matrix <- diag(3) + v_cross + v_cross %*% v_cross * ((1 - c) / s^2)
      }

      # Apply rotation to normals
      rotated_normals <- normals %*% rot_matrix

      # Scale normals from [-1,1] to [0,1] for RGB
      r <- (rotated_normals[,1] + 1) / 2
      g <- (rotated_normals[,2] + 1) / 2
      b <- (rotated_normals[,3] + 1) / 2

      # Convert to hex colors
      rgb(r, g, b)
}


#' Compute lighting from standardized coordinates in coord_3d
#'
#' @param data Original data frame with lighting_spec column
#' @param standardized_coords Data frame with standardized x, y, z coordinates
#' @param scale_ranges List with x, y, z scale ranges
#' @param scales Character, either "free" or "fixed"
#' @param ratio Numeric vector of length 3
#' @param proj List with pitch, roll, yaw, persp, dist (projection parameters)
#' @return data frame with lighting values and normal components added
#' @keywords internal
compute_light_in_coord <- function(data, standardized_coords, scale_ranges, scales, ratio, proj = NULL) {

      # Extract lighting specification from first row (all rows have same spec)
      light <- data$lighting_spec[[1]]

      # Transform light position from data space to visual space, if applicable
      light$position <- transform_light_position(light$position, scale_ranges, scales, ratio)

      # For camera-frame lighting, rotate the coordinates into camera space
      if (light$anchor == "camera" && !is.null(proj)) {
            standardized_coords <- standardized_coords %>%
                  mutate(z = -z) %>%  # z-flip first, like transform_3d_standard
                  as.matrix() %>%
                  rotate_3d(proj$pitch, proj$roll, proj$yaw) %>%
                  as.data.frame() %>%
                  setNames(names(standardized_coords))

            # Flip light direction
            light$direction[1:2] <- -light$direction[1:2]
      }

      # Replace original coordinates with standardized ones for lighting computation
      data$x <- standardized_coords$x
      data$y <- standardized_coords$y
      data$z <- standardized_coords$z

      # Get unique faces for normal/lighting computation
      faces <- data %>%
            group_by(group) %>%
            slice(1) %>%  # One row per face
            ungroup()

      # Compute normals based on data type
      if (grepl("sh3d__hull", faces$group[1])) {
            # Hull/triangle data: compute normals from vertex coordinates
            normals <- compute_triangle_normals(data, faces)
      } else if ("face_type" %in% names(faces)) {
            # Voxel/column data: axis-aligned normals
            normals <- compute_axis_aligned_normals(faces)
      } else {
            # Surface-like data: normals from fitted plane gradients
            gradients <- compute_surface_gradients_from_vertices(data)
            normals <- compute_surface_normals(gradients)
      }

      # Compute face centers from standardized coordinates
      face_centers <- calculate_face_centers(data)

      # Apply lighting models
      faces <- apply_surface_light(faces, normals, face_centers, light)

      # Merge face-level lighting vars back into vertex-level data set
      data <- left_join(data, faces, by = join_by(group))

      if (light$method == "rgb") {
            data$light <- I(data$light)
      }

      # Add lighting parameters for shade processing in geom
      data$shade_enabled <- light$shade
      data$shade_strength <- light$shade_strength
      data$shade_mode <- light$shade_mode
      data$lighting_method <- light$method

      return(data)
}

compute_surface_gradients_from_vertices <- function(data) {
      # Compute both gradients in one pass using least squares normal equations
      gradients <- data %>%
            group_by(group) %>%
            summarise(
                  n_pts = n(),
                  # Sums needed for least squares: z = a*x + b*y + c
                  sum_x = sum(x), sum_y = sum(y), sum_z = sum(z),
                  sum_xx = sum(x*x), sum_yy = sum(y*y),
                  sum_xy = sum(x*y), sum_xz = sum(x*z), sum_yz = sum(y*z),
                  .groups = "drop"
            ) %>%
            mutate(
                  # Solve normal equations for dz/dx and dz/dy
                  # Using simplified form assuming roughly centered data
                  denom_x = n_pts * sum_xx - sum_x^2,
                  denom_y = n_pts * sum_yy - sum_y^2,

                  # Gradients (with protection against division by zero)
                  dzdx = ifelse(abs(denom_x) > 1e-10,
                                (n_pts * sum_xz - sum_x * sum_z) / denom_x,
                                0),
                  dzdy = ifelse(abs(denom_y) > 1e-10,
                                (n_pts * sum_yz - sum_y * sum_z) / denom_y,
                                0)
            ) %>%
            select(group, dzdx, dzdy)

      return(gradients)
}


#' Compute surface normals from gradients
#'
#' @param gradients Data frame with group, dzdx, dzdy columns
#' @return Matrix with 3 columns (x, y, z normal components)
#' @keywords internal
compute_surface_normals <- function(gradients) {
      # Compute raw normals from gradients
      # For a surface z = f(x,y), the normal is (-dz/dx, -dz/dy, 1) (unnormalized)
      raw_normals <- cbind(
            -gradients$dzdx,
            -gradients$dzdy,
            rep(1, nrow(gradients))
      )

      # Normalize each normal vector
      lengths <- sqrt(rowSums(raw_normals^2))
      normals <- raw_normals / lengths

      return(normals)
}

#' Compute triangle normals from vertex coordinates
#'
#' @param data Full vertex data with x, y, z coordinates
#' @param faces Data frame with one row per face (group column required)
#' @return Matrix with 3 columns (x, y, z normal components)
#' @keywords internal
compute_triangle_normals <- function(data, faces) {
      # For each face, get three vertices and compute cross product
      face_normals <- data %>%
            group_by(group) %>%
            slice(1:3) %>%
            summarise(
                  # Get first three vertices
                  x1 = x[1], y1 = y[1], z1 = z[1],
                  x2 = x[2], y2 = y[2], z2 = z[2],
                  x3 = x[3], y3 = y[3], z3 = z[3],
                  .groups = "drop"
            ) %>%
            mutate(
                  # Edge vectors
                  e1x = x2 - x1, e1y = y2 - y1, e1z = z2 - z1,
                  e2x = x3 - x1, e2y = y3 - y1, e2z = z3 - z1,
                  # Cross product (e1 × e2)
                  normal_x = e1y * e2z - e1z * e2y,
                  normal_y = e1z * e2x - e1x * e2z,
                  normal_z = e1x * e2y - e1y * e2x,
                  # Normalize
                  length = sqrt(normal_x^2 + normal_y^2 + normal_z^2),
                  normal_x = ifelse(length > 0, normal_x / length, 0),
                  normal_y = ifelse(length > 0, normal_y / length, 0),
                  normal_z = ifelse(length > 0, normal_z / length, 1)
            )

      # Return as matrix format expected by lighting functions
      normals <- as.matrix(face_normals[, c("normal_x", "normal_y", "normal_z")])

      return(normals)
}

#' Compute axis-aligned normals for voxel/column faces
#'
#' @param faces Data frame with face_type column indicating face orientation
#' @return Matrix with 3 columns (x, y, z normal components)
#' @keywords internal
compute_axis_aligned_normals <- function(faces) {
      # Map face types to normal directions
      face_normals <- faces %>%
            mutate(
                  normal_x = case_when(
                        face_type == "xmin" ~ -1,
                        face_type == "xmax" ~ 1,
                        TRUE ~ 0
                  ),
                  normal_y = case_when(
                        face_type == "ymin" ~ -1,
                        face_type == "ymax" ~ 1,
                        TRUE ~ 0
                  ),
                  normal_z = case_when(
                        face_type == "zmin" ~ -1,
                        face_type == "zmax" ~ 1,
                        TRUE ~ 0
                  ),
                  # Handle NA face_type (shouldn't happen but be safe)
                  normal_x = ifelse(is.na(normal_x), 0, normal_x),
                  normal_y = ifelse(is.na(normal_y), 0, normal_y),
                  normal_z = ifelse(is.na(normal_z), 1, normal_z)
            )

      # Return as matrix format expected by lighting functions
      normals <- as.matrix(face_normals[, c("normal_x", "normal_y", "normal_z")])

      return(normals)
}

#' Calculate face centers from vertex data
#'
#' @param data Data frame with x, y, z coordinates and group column
#' @return Matrix with 3 columns (x, y, z face center coordinates)
#' @keywords internal
calculate_face_centers <- function(data) {
      centers <- data %>%
            group_by(group) %>%
            summarise(
                  center_x = mean(x, na.rm = TRUE),
                  center_y = mean(y, na.rm = TRUE),
                  center_z = mean(z, na.rm = TRUE),
                  .groups = "drop"
            )

      as.matrix(centers[, c("center_x", "center_y", "center_z")])
}

#' Apply lighting models to surface normals
#'
#' @param face_data Data frame with unique faces (group column)
#' @param normals Matrix of surface normals
#' @param face_centers Matrix of face centers
#' @param light Lighting specification object
#' @return Data frame with lighting values and normal components
#' @keywords internal
apply_surface_light <- function(face_data, normals, face_centers, light) {
      # Apply lighting models to the normals
      light_vals <- compute_light(normals, light, face_centers)

      face_data_with_light <- select(face_data, group) %>%
            mutate(light = light_vals,
                   normal_x = normals[, 1],
                   normal_y = normals[, 2],
                   normal_z = normals[, 3])

      return(face_data_with_light)
}


#' Convert RGB to HSL color space
#'
#' @param rgb_matrix 3xN matrix with RGB values in [0,1] range
#' @return 3xN matrix with HSL values (H in [0,1], S in [0,1], L in [0,1])
#' @keywords internal
rgb2hsl <- function(rgb_matrix) {
      if (!is.matrix(rgb_matrix) || nrow(rgb_matrix) != 3) {
            stop("rgb_matrix must be a 3xN matrix")
      }

      r <- rgb_matrix[1, ]
      g <- rgb_matrix[2, ]
      b <- rgb_matrix[3, ]

      max_val <- pmax(r, g, b)
      min_val <- pmin(r, g, b)
      diff <- max_val - min_val

      # Lightness
      l <- (max_val + min_val) / 2

      # Initialize hue and saturation
      h <- rep(0, ncol(rgb_matrix))
      s <- rep(0, ncol(rgb_matrix))

      # Calculate saturation
      non_zero_diff <- diff > 0
      s[non_zero_diff] <- ifelse(
            l[non_zero_diff] < 0.5,
            diff[non_zero_diff] / (max_val[non_zero_diff] + min_val[non_zero_diff]),
            diff[non_zero_diff] / (2 - max_val[non_zero_diff] - min_val[non_zero_diff])
      )

      # Calculate hue
      if (any(non_zero_diff)) {
            # Red is max
            red_max <- non_zero_diff & (r == max_val)
            h[red_max] <- ((g[red_max] - b[red_max]) / diff[red_max]) %% 6

            # Green is max
            green_max <- non_zero_diff & (g == max_val)
            h[green_max] <- (b[green_max] - r[green_max]) / diff[green_max] + 2

            # Blue is max
            blue_max <- non_zero_diff & (b == max_val)
            h[blue_max] <- (r[blue_max] - g[blue_max]) / diff[blue_max] + 4

            # Normalize hue to [0, 1]
            h <- h / 6
      }

      rbind(h, s, l)
}

#' Convert HSL to RGB color space
#'
#' @param hsl_matrix 3xN matrix with HSL values (H in [0,1], S in [0,1], L in [0,1])
#' @return 3xN matrix with RGB values in [0,1] range
#' @keywords internal
hsl2rgb <- function(hsl_matrix) {
      if (!is.matrix(hsl_matrix) || nrow(hsl_matrix) != 3) {
            stop("hsl_matrix must be a 3xN matrix")
      }

      h <- hsl_matrix[1, ]
      s <- hsl_matrix[2, ]
      l <- hsl_matrix[3, ]

      n <- ncol(hsl_matrix)
      r <- rep(0, n)
      g <- rep(0, n)
      b <- rep(0, n)

      for (i in seq_len(n)) {
            if (s[i] == 0) {
                  # Achromatic (gray)
                  r[i] <- g[i] <- b[i] <- l[i]
            } else {
                  q <- ifelse(l[i] < 0.5, l[i] * (1 + s[i]), l[i] + s[i] - l[i] * s[i])
                  p <- 2 * l[i] - q

                  hue_to_rgb <- function(p, q, t) {
                        if (t < 0) t <- t + 1
                        if (t > 1) t <- t - 1
                        if (t < 1/6) return(p + (q - p) * 6 * t)
                        if (t < 1/2) return(q)
                        if (t < 2/3) return(p + (q - p) * (2/3 - t) * 6)
                        return(p)
                  }

                  r[i] <- hue_to_rgb(p, q, h[i] + 1/3)
                  g[i] <- hue_to_rgb(p, q, h[i])
                  b[i] <- hue_to_rgb(p, q, h[i] - 1/3)
            }
      }

      rbind(r, g, b)
}


#' Apply shading to colors based on lighting values
#'
#' @param colors Vector of color values (hex or named colors)
#' @param light_values Numeric vector of lighting values
#' @param lighting Lighting specification with shade_mode and shade_strength
#' @return Vector of modified colors
#' @keywords internal
blend_light_with_colors <- function(colors, light_values, lighting) {
      # Handle NA or NULL colors
      if (is.null(colors) || length(colors) == 0) {
            return(colors)
      }

      # RGB method produces colors directly - return them as-is
      if (lighting$method == "rgb") {
            return(I(light_values))
      }

      # Initialize result
      result_colors <- colors

      # Handle only valid colors (not NA)
      valid_mask <- !is.na(colors) & !is.na(light_values)

      if (!any(valid_mask)) {
            return(result_colors)
      }

      valid_base <- colors[valid_mask]
      valid_light <- light_values[valid_mask]

      # Normalize light values to [0, 1] if needed
      if (lighting$method == "diffuse") {
            valid_light <- (valid_light + 1) / 2
      }
      valid_light <- pmax(0, pmin(1, valid_light))

      # Convert to RGB matrix
      base_rgb <- col2rgb(valid_base)

      if (lighting$shade_mode == "hsl") {
            # HSL blending mode
            base_rgb_norm <- base_rgb / 255  # Normalize to [0,1] for HSL
            base_hsl <- rgb2hsl(base_rgb_norm)

            # Modify only lightness (L component)
            new_hsl <- base_hsl

            for (i in seq_along(valid_light)) {
                  h_val <- base_hsl[1, i]
                  s_val <- base_hsl[2, i]
                  l_val <- base_hsl[3, i]
                  light_val <- valid_light[i]

                  if (light_val > 0.5) {
                        # Brighten toward white (L = 1)
                        blend_factor <- (light_val - 0.5) * 2 * lighting$shade_strength
                        new_l <- l_val + blend_factor * (1 - l_val)
                  } else {
                        # Darken toward black (L = 0)
                        blend_factor <- (0.5 - light_val) * 2 * lighting$shade_strength
                        new_l <- l_val * (1 - blend_factor)
                  }

                  new_hsl[1, i] <- h_val  # Preserve hue
                  new_hsl[2, i] <- s_val  # Preserve saturation
                  new_hsl[3, i] <- pmax(0, pmin(1, new_l))  # Clamp lightness
            }

            # Convert back to RGB
            new_rgb <- hsl2rgb(new_hsl)
            new_colors <- rgb(new_rgb[1, ], new_rgb[2, ], new_rgb[3, ])

      } else {
            # HSV blending mode
            base_hsv <- rgb2hsv(base_rgb)

            # Modify only brightness (V component)
            new_hsv <- base_hsv

            for (i in seq_along(valid_light)) {
                  h_val <- base_hsv[1, i]
                  s_val <- base_hsv[2, i]
                  v_val <- base_hsv[3, i]
                  light_val <- valid_light[i]

                  # Handle NaN hue (gray colors)
                  if (is.nan(h_val)) h_val <- 0

                  if (light_val > 0.5) {
                        # Brighten
                        blend_factor <- (light_val - 0.5) * 2 * lighting$shade_strength
                        new_v <- v_val + blend_factor * (1 - v_val)
                  } else {
                        # Darken
                        blend_factor <- (0.5 - light_val) * 2 * lighting$shade_strength
                        new_v <- v_val * (1 - blend_factor)
                  }

                  new_hsv[1, i] <- h_val
                  new_hsv[2, i] <- s_val
                  new_hsv[3, i] <- pmax(0, pmin(1, new_v))
            }

            # Convert back to colors
            new_colors <- hsv(new_hsv[1, ], new_hsv[2, ], new_hsv[3, ])
      }

      result_colors[valid_mask] <- new_colors

      return(result_colors)
}


blend_light <- function(coords) {

      # Extract lighting parameters from special columns
      if ("shade_enabled" %in% names(coords) && coords$shade_enabled[1] != "neither") {
            # Reconstruct lighting object from columns
            light <- list(
                  shade = coords$shade_enabled[1],
                  shade_strength = coords$shade_strength[1],
                  shade_mode = coords$shade_mode[1],
                  method = coords$lighting_method[1]
            )

            # Apply shading if enabled
            if (light$shade != "neither" && "light" %in% names(coords)) {
                  # Blend fill colors if requested
                  if (light$shade %in% c("fill", "both")) {
                        coords$fill <- blend_light_with_colors(coords$fill, coords$light, light)
                  }

                  # Blend colour/border colors if requested
                  if (light$shade %in% c("colour", "both")) {
                        coords$colour <- blend_light_with_colors(coords$colour, coords$light, light)
                  }
            }

            # Clean up lighting parameter columns
            coords <- coords[, !names(coords) %in% c("shade_enabled", "shade_strength", "shade_mode", "lighting_method")]
      }

      return(coords)
}

#' Convert light position from data units to visual space units
#'
#' @param position Numeric vector of length 3 (x, y, z in data coordinates)
#' @param scale_ranges List with x, y, z scale ranges
#' @param scales Character, either "free" or "fixed"
#' @param ratio Numeric vector of length 3
#' @return Position vector in visual space, or NULL if position is NULL
#' @keywords internal
transform_light_position <- function(position, scale_ranges, scales, ratio) {
      if(is.null(position)) return(NULL)
      light_data <- data.frame(x = position[1], y = position[2], z = position[3])
      standardized <- scale_to_standard(light_data, scale_ranges, scales, ratio)
      return(c(standardized$x, standardized$y, standardized$z))
}

#' Store lighting specification to data frame for downstream use
#'
#' @param data Data frame to attach lighting spec to
#' @param light Lighting specification object, "none", or NULL
#' @return Data frame with lighting_spec column added (if light is not NULL)
#' @keywords internal
attach_light <- function(data, light){
      if(!is.null(light)){
            if(!inherits(light, "light") && light == "none") light <- light(fill = FALSE, color = FALSE)
            if(! "lighting_spec" %in% names(data)) data$lighting_spec <- I(list(light))
      }

      data
}
