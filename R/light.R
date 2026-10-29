#' Lighting specification for 3D surface rendering
#'
#' Creates a lighting specification object for use with 3D polygon layers.
#' Lighting modifies the brightness of fill and/or base color aesthetics based on surface
#' orientation (i.e., it implements form shadows but not cast shadows).
#' Various options are available to control light qualities and light source location.
#' The result can be added directly to a plot with `+ light()` to set lighting for
#' the whole plot. It can also be passed to individual `geom_*_3d()` layer functions,
#' which takes precedence over plot-level lighting.
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
#'     \item \code{"none"}: Disable lighting entirely. All other parameters are ignored.
#'          Equivalent to \code{light(fill = FALSE, color = FALSE)}, and to passing the
#'          bare string \code{"none"} to a \code{light} argument.
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
#'   to the rendered cube's bounding box), not data space. Use [direction()] to build this
#'   vector from an azimuth and elevation instead of components; the default corresponds to
#'   \code{direction(270, 63.43)}. This argument is ignored if \code{position} is provided.
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
#'   (intensity proportional to 1/distance²). Only used when \code{position} is specified.
#'   Default is FALSE.
#' @param backface_scale,backface_offset Numeric values that determine how "frontface" light values get
#'   modified (scaled and then offset) to derive "backface" light values. A backface is the side of a
#'   polygon that faces the underside of a surface or the inside of a volume. Frontface light values are
#'   typically in the range `[-1, 1]` (unless `contrast` is boosted). The default scale of -1 gives
#'   backfaces highly contrasting lighting to frontfaces. To light backfaces the same as frontfaces,
#'   set scale to 1. To uniformly darken (brighten) all backfaces, use a negative (positive) offset.
#'
#' @return A \code{lighting} object that can be added to a plot or passed to polygon-based 3D layers
#'   or to `coord_3d()`.
#' @examples
#' # base plot used in examples
#' p <- ggplot(sphere_points, aes(x, y, z)) +
#'   geom_hull_3d(fill = "#9e2602", color = "#5e1600") +
#'   coord_3d(scales = "fixed")
#'
#' # Light qualities ----------------------------------------------------------
#'
#' # default `"diffuse"` lighting
#' p
#'
#' \donttest{
#'
#' # use `"direct"` lighting to apply full shade to unlit surfaces
#' p + light(method = "direct")
#'
#' # use `"hsl"` mode to fade highlights to white
#' p + light(mode = "hsl")
#'
#' # adjust lighting intensity with `contrast`
#' p + light(mode = "hsl", contrast = 1.5)
#'
#' # use `"rgb"` lighting to map face orientation to 3D color space
#' p + light(method = "rgb")
#'
#'
#' # Lighting targets ---------------------------------------------------------
#'
#' # use `fill` and `color` to select which aesthetics get modified by light
#' p + light(fill = TRUE, color = FALSE)
#'
#' # apply lighting to aesthetically mapped colors, with shaded guide to match
#' p + geom_hull_3d(aes(fill = x, color = x)) +
#'       scale_fill_viridis_c() +
#'       scale_color_viridis_c() +
#'       guides(fill = guide_colorbar_3d()) +
#'       light(mode = "hsl", contrast = .9)
#'
#' # disable lighting entirely
#' p + light("none")
#'
#' # if provided, layer-level lighting overrides plot-level lighting
#' p + light("direct") + # plot-level: affects original layer
#'   geom_hull_3d(aes(x = x + 2.5), fill = "#9e2602", color = "#5e1600",
#'       light = light("direct", mode = "hsl", direction = c(0, -1, 0)))
#'
#'
#' # Light sources ------------------------------------------------------------
#'
#' # directional light from the xmin-ymin-zmin direction
#' # (`direction` is relative to rotated axes with default `anchor = "scene"`)
#' p + light(direction = c(-1, -1, -1))
#'
#' # directional light from top-right corner of figure
#' # (`anchor = "camera"` makes `direction` fixed relative to the plot)
#' p + light(direction = c(1, 1, 0), anchor = "camera")
#'
#' # positional light source within plot
#' ggplot(mountain, aes(x, y, z)) +
#'   stat_surface_3d(fill = "red", color = "red") +
#'   light(position = c(.5, .7, 95), distance_falloff = TRUE,
#'         mode = "hsl", contrast = .9) +
#'   coord_3d(ratio = c(1.5, 2, 1))
#'
#'
#' # Backface lighting --------------------------------------------------------
#'
#' # backfaces get "opposite" lighting by default (`backface_scale = -1`)
#' p <- ggplot() +
#'   geom_function_3d(fun = function(x, y) x^2 + y^2,
#'     xlim = c(-3, 3), ylim = c(-3, 3),
#'     fill = "steelblue", color = "steelblue") +
#'     coord_3d(pitch = 0, roll = -70, yaw = 0)
#' p + light(mode = "hsl")
#'
#' # use `backface_scale = 1` to light backfaces as if they're frontfaces
#' p + light(backface_scale = 1, mode = "hsl")
#'
#' # use `backface_offset` to uniformly darken (or lighten) backfaces
#' p + light(backface_scale = 1, mode = "hsl", backface_offset = -.5)
#'
#' }
#'
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

      # `method = "none"` disables lighting. Every other parameter describes how to
      # light a surface, so all of them are irrelevant here: return the disabled
      # spec directly without validating them. This makes `light("none")`
      # equivalent to the bare string `"none"` accepted by the `light` argument of
      # layer, coord, and plot-level functions.
      if (identical(method, "none")) {
            return(light(fill = FALSE, color = FALSE))
      }

      # Validate method
      valid_methods <- c("direct", "diffuse", "rgb")
      if (!method %in% valid_methods) {
            stop("method must be one of: ",
                 paste(c(valid_methods, "none"), collapse = ", "))
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

      # How light values become colors: a ramp maps each face's light value to
      # a light level in [-1, 1], `shadow` and `highlight` scale the level into
      # an amount, and a tone turns base colors and amounts into lit colors
      ramp <- switch(method,
                     diffuse = ramp_smooth(neutral = 90),
                     direct = ramp_hard(neutral = 60),
                     rgb = ramp_smooth(neutral = 90))
      tone <- if (method == "rgb") tone_normals() else tone_legacy(mode)

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
                  backface_offset = backface_offset,
                  ramp = ramp,
                  tone = tone,
                  shadow = contrast,
                  highlight = contrast
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
            angles <- direction_angles(x$direction)
            cat("    azimuth:", wrap_angle(angles$azimuth, "bearing", digits = 1),
                "deg, elevation:", round(angles$elevation, 1), "deg\n")
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


#' Light values of faces from their normals
#'
#' Computes each face's light value: the cosine of the angle between its
#' normal and the direction toward the light. Supports both directional
#' lighting (parallel rays) and positional lighting (point light sources with
#' per-face light directions). How light values become colors is decided later,
#' when colors are blended, by the light's ramp and tone.
#'
#' @param normals Matrix with 3 columns (x, y, z normal components), where each
#'   row represents a face normal vector. Should be unit vectors (normalized).
#' @param lighting A lighting specification object created by \code{light()}
#' @param face_centers Matrix with 3 columns (x, y, z coordinates) representing
#'   the center position of each face in data coordinate space. Required for
#'   positional lighting, optional for directional lighting.
#' @return Numeric vector of light values, one per face.
#' @keywords internal
#' @noRd
compute_light <- function(normals, lighting, face_centers = NULL) {

      # Validate inputs
      if (!is.matrix(normals) || ncol(normals) != 3) {
            stop("normals must be a matrix with 3 columns (x, y, z)")
      }

      params <- lighting

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

      # Values are left unclamped, and ramped into light levels when blended
      # with colors, so that back faces can first have their sign flipped:
      # clamping before the flip would leave every back face dark, including
      # those facing the light.
      dot_products
}

#' Light direction used by normal-based tones
#'
#' Normal-based tones such as [tone_normals()] color each face by its normal,
#' rotated so that normals facing the light map to white. They need one light
#' direction per layer: the light's direction, or for positional lighting, the
#' mean of the faces' directions toward the light.
#'
#' @param lighting A lighting specification object, with `position` (if any)
#'   already in visual space.
#' @param face_centers Matrix with 3 columns of face centers in visual space.
#' @return Numeric vector of length 3.
#' @keywords internal
#' @noRd
tone_light_direction <- function(lighting, face_centers) {
      if (is.null(lighting$position)) {
            normalize_light_direction(lighting$direction)
      } else {
            colMeans(calculate_positional_light_directions(face_centers, lighting$position))
      }
}

#' Calculate per-face light directions for positional lighting
#'
#' @param face_centers Matrix with 3 columns (x, y, z face center coordinates)
#' @param light_position Numeric vector of length 3 (x, y, z light position)
#' @return Matrix with 3 columns (normalized light direction vectors)
#' @keywords internal
#' @noRd
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
#' @noRd
normalize_light_direction <- function(light_dir) {
      light_dir / sqrt(sum(light_dir^2))
}

#' Compute dot products between normals and light direction
#'
#' @param normals Matrix with 3 columns (x, y, z normal components)
#' @param light_dir_norm Normalized light direction vector
#' @return Vector of dot products
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
compute_light_in_coord <- function(data, standardized_coords, scale_ranges, scales, ratio, proj = NULL) {

      # Extract lighting specification from first row (all rows have same spec)
      light <- data$lighting_spec[[1]]

      # Transform light position from data space to visual space, if applicable
      light$position <- transform_light_position(light$position, scale_ranges, scales, ratio)

      # Camera-frame directional lighting keeps lighting geometry in scene space and
      # rotates the finished face normals into camera space (see rotate_normals_to_camera,
      # applied per branch below). Positional lighting stays entirely in scene space so
      # that face centers and the light position share one coordinate frame.
      camera_directional <- light$anchor == "camera" && !is.null(proj) && is.null(light$position)

      # Filter to polygon groups only — points and segments don't have
      # meaningful surface normals and should not receive lighting
      has_prim <- ".prim" %in% names(data)

      # Two-sided polygons (planes) are sheets, lit from whichever side faces
      # the viewer
      if (".two_sided" %in% names(data)) {
            two_sided <- data$.two_sided %in% TRUE
            if (has_prim) two_sided <- two_sided & (is.na(data$.prim) | data$.prim == "polygon")
            if (any(two_sided)) {
                  lit <- light_two_sided_polygons(
                        data[two_sided, , drop = FALSE],
                        standardized_coords[two_sided, , drop = FALSE],
                        light, camera_directional, proj)
                  if (all(two_sided)) return(lit)
                  rest <- compute_light_in_coord(data[!two_sided, , drop = FALSE],
                                                 standardized_coords[!two_sided, , drop = FALSE],
                                                 scale_ranges, scales, ratio, proj)
                  out <- bind_rows(rest, lit)
                  return(out[order(c(which(!two_sided), which(two_sided))), , drop = FALSE])
            }
      }

      if (has_prim) {
            poly_mask <- is.na(data$.prim) | data$.prim == "polygon"
      } else {
            # Count vertices per group to identify polygons (3+ vertices)
            grp_sizes <- ave(seq_len(nrow(data)), data$group, FUN = length)
            poly_mask <- grp_sizes >= 3
      }

      if (!any(poly_mask)) {
            # No polygon data to light — return unchanged
            return(data)
      }

      poly_data <- data[poly_mask, , drop = FALSE]
      poly_coords <- standardized_coords[poly_mask, , drop = FALSE]

      # Replace original coordinates with standardized ones for lighting computation
      poly_data$x <- poly_coords$x
      poly_data$y <- poly_coords$y
      poly_data$z <- poly_coords$z

      # Get unique faces for normal/lighting computation
      faces <- poly_data %>%
            group_by(group) %>%
            slice(1) %>%  # One row per face
            ungroup()

      # Compute normals based on data type
      has_precomputed_normals <- all(c("normal_x", "normal_y", "normal_z") %in% names(faces))

      if (has_precomputed_normals) {
            # Pre-existing normals (e.g. from text stat): transform to standardized space
            normals <- transform_normals_to_standard(
                  as.matrix(faces[, c("normal_x", "normal_y", "normal_z")]),
                  scale_ranges, scales, ratio, light$anchor, proj
            )
      } else if (grepl("sh3d__hull", faces$group[1])) {
            # Hull/triangle data: compute normals from vertex coordinates
            normals <- compute_triangle_normals(poly_data, faces)
            if (camera_directional) normals <- rotate_normals_to_camera(normals, proj)
      } else if ("face_type" %in% names(faces)) {
            # Voxel/column data: axis-aligned normals (scene space)
            normals <- compute_axis_aligned_normals(faces)
            if (camera_directional) normals <- rotate_normals_to_camera(normals, proj)
      } else if (grepl("__group$", faces$group[1])) {
            # Polygons given directly (stat_identity_3d): the normal of each
            # polygon's own plane, facing the side from which its vertices
            # run counter-clockwise, consistent with back-face detection
            normals <- compute_polygon_normals(poly_data, faces$group)
            if (camera_directional) normals <- rotate_normals_to_camera(normals, proj)
      } else {
            # Surface-like data: normals from fitted plane gradients. The gradient fit
            # and (-dzdx, -dzdy, 1) normal are computed in scene space, where the surface
            # is single-valued and the +z orientation is a consistent "top" normal.
            gradients <- compute_surface_gradients_from_vertices(poly_data)
            normals <- compute_surface_normals(gradients)
            if (camera_directional) normals <- rotate_normals_to_camera(normals, proj)
      }

      # Compute face centers from standardized coordinates
      face_centers <- calculate_face_centers(poly_data)

      # Apply lighting models
      faces <- apply_surface_light(faces, normals, face_centers, light)

      # Remove pre-existing normal columns from data before join to prevent suffixes
      poly_data <- poly_data[, !names(poly_data) %in% c("normal_x", "normal_y", "normal_z")]

      # Restore original coordinates before join (standardized were temporary)
      poly_data$x <- data$x[poly_mask]
      poly_data$y <- data$y[poly_mask]
      poly_data$z <- data$z[poly_mask]

      # Merge face-level lighting vars back into vertex-level polygon data
      poly_data <- left_join(poly_data, faces, by = join_by(group))

      # Record on the layer's lighting specification the light direction used
      # by normal-based tones, for blending colors at draw time
      poly_data$lighting_spec <- spec_with_tone_direction(data$lighting_spec[[1]], light,
                                                          face_centers, nrow(poly_data))

      # Recombine with non-polygon rows (which get no lighting columns)
      if (all(poly_mask)) {
            return(poly_data)
      }

      non_poly_data <- data[!poly_mask, , drop = FALSE]
      bind_rows(poly_data, non_poly_data)[order(c(which(poly_mask), which(!poly_mask))), ]
}


#' Transform pre-computed normals from data space to standardized lighting space
#'
#' Normals transform under non-uniform scaling by the inverse-transpose of
#' the scaling matrix. For the diagonal scaling used in standardization,
#' this means scaling each component by range_width / effective_ratio,
#' then renormalizing.
#'
#' @param normals Matrix with 3 columns (normal_x, normal_y, normal_z) in data space
#' @param scale_ranges List with x, y, z scale ranges
#' @param scales Character, either "free" or "fixed"
#' @param ratio Numeric vector of length 3
#' @param anchor Character, "scene" or "camera"
#' @param proj Projection parameters (needed for camera anchor rotation)
#' @return Matrix with 3 columns of transformed normals
#' @keywords internal
#' @noRd
transform_normals_to_standard <- function(normals, scale_ranges, scales, ratio, anchor, proj = NULL) {

      # Compute effective ratios (same logic as scale_to_standard multi-axis path)
      effective_ratios <- compute_effective_ratios(scale_ranges, scales, ratio)

      # Compute per-axis scale factors: standardization scales by effective_ratio / range_width
      # Normal transform uses inverse-transpose, which for diagonal = range_width / effective_ratio
      x_width <- diff(scale_ranges$x)
      y_width <- diff(scale_ranges$y)
      z_width <- diff(scale_ranges$z)
      if (x_width == 0 || is.na(x_width)) x_width <- 1
      if (y_width == 0 || is.na(y_width)) y_width <- 1
      if (z_width == 0 || is.na(z_width)) z_width <- 1

      inv_scale <- c(x_width / effective_ratios[1],
                     y_width / effective_ratios[2],
                     z_width / effective_ratios[3])

      # Apply inverse-transpose scaling
      normals[, 1] <- normals[, 1] * inv_scale[1]
      normals[, 2] <- normals[, 2] * inv_scale[2]
      normals[, 3] <- normals[, 3] * inv_scale[3]

      # Renormalize
      lengths <- sqrt(rowSums(normals^2))
      lengths <- pmax(lengths, 1e-10)
      normals <- normals / lengths

      # For camera-anchor lighting, apply the same transforms as the coordinates:
      # z-flip then rotation
      if (anchor == "camera" && !is.null(proj)) {
            normals[, 3] <- -normals[, 3]  # z-flip
            normals <- rotate_3d(normals, proj$pitch, proj$roll, proj$yaw)
      }

      return(normals)
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
                  # Full bivariate plane fit: z = a*x + b*y + c
                  # Normal equations for centered data
                  Sxx = n_pts * sum_xx - sum_x^2,
                  Syy = n_pts * sum_yy - sum_y^2,
                  Sxy = n_pts * sum_xy - sum_x * sum_y,
                  Sxz = n_pts * sum_xz - sum_x * sum_z,
                  Syz = n_pts * sum_yz - sum_y * sum_z,
                  denom = Sxx * Syy - Sxy^2,
                  dzdx = ifelse(abs(denom) > 1e-10,
                                (Syy * Sxz - Sxy * Syz) / denom,
                                ifelse(abs(Sxx) > 1e-10, Sxz / Sxx, 0)),
                  dzdy = ifelse(abs(denom) > 1e-10,
                                (Sxx * Syz - Sxy * Sxz) / denom,
                                ifelse(abs(Syy) > 1e-10, Syz / Syy, 0))
            ) %>%
            select(group, dzdx, dzdy)

      return(gradients)
}


#' Light two-sided polygons
#'
#' Two-sided polygons such as planes are flat sheets, so they are lit from
#' whichever side faces the viewer: each face's normal comes from its own
#' vertices (Newell's method, in scene space) and is flipped toward the
#' viewer before the lighting model is applied.
#'
#' @param data Two-sided polygon rows.
#' @param standardized_coords Their standardized (scene-space) coordinates.
#' @param light Resolved lighting specification.
#' @param camera_directional Whether lighting is camera-anchored directional.
#' @param proj Projection parameters.
#' @return `data` with lighting columns added.
#' @keywords internal
#' @noRd
light_two_sided_polygons <- function(data, standardized_coords, light,
                                     camera_directional, proj) {
      poly_data <- data
      poly_data$x <- standardized_coords$x
      poly_data$y <- standardized_coords$y
      poly_data$z <- standardized_coords$z

      faces <- poly_data %>% group_by(group) %>% slice(1) %>% ungroup()
      face_centers <- calculate_face_centers(poly_data)
      eye <- eye_in_cube_space(proj)

      normals <- t(vapply(faces$group, function(g) {
            pts <- as.matrix(poly_data[poly_data$group == g, c("x", "y", "z")])
            pl <- polygon_plane(pts)
            if (is.null(pl)) return(c(0, 0, 1))
            n <- pl$normal
            if (!is.null(eye)) {
                  toward <- if (eye$persp) eye$v - colMeans(pts) else eye$v
                  if (sum(n * toward) < 0) n <- -n
            }
            n
      }, numeric(3)))
      if (camera_directional) normals <- rotate_normals_to_camera(normals, proj)

      faces <- apply_surface_light(faces, normals, face_centers, light)

      out <- data[, !names(data) %in% c("normal_x", "normal_y", "normal_z"), drop = FALSE]
      out <- left_join(out, faces, by = join_by(group))
      out$lighting_spec <- spec_with_tone_direction(data$lighting_spec[[1]], light,
                                                    face_centers, nrow(out))
      out
}


#' Lighting specification column carrying the tone light direction
#'
#' @param spec The layer's lighting specification, as attached to its data.
#' @param light The specification as resolved for lighting, with `position`
#'   (if any) in visual space.
#' @param face_centers Matrix with 3 columns of face centers in visual space.
#' @param n Number of rows.
#' @return An `AsIs` list column of `n` copies of `spec`, with
#'   `tone_direction` set.
#' @keywords internal
#' @noRd
spec_with_tone_direction <- function(spec, light, face_centers, n) {
      spec$tone_direction <- tone_light_direction(light, face_centers)
      I(rep(list(spec), n))
}


#' Plane normals of polygons from their vertices
#'
#' Newell's method, summed over each polygon's rings (subgroups), gives the
#' normal of the polygon's best-fit plane, pointing toward the side from
#' which its vertices run counter-clockwise.
#'
#' @param poly_data Polygon vertex rows with scene-space x, y, z, in ring
#'   order within each group (and subgroup).
#' @param groups Groups of the faces whose normals are wanted.
#' @return Matrix with 3 columns of unit normals, one row per group (`c(0, 0,
#'   1)` for degenerate polygons).
#' @keywords internal
#' @noRd
compute_polygon_normals <- function(poly_data, groups) {
      ring <- if ("subgroup" %in% names(poly_data)) {
            paste(poly_data$group, poly_data$subgroup, sep = "\r")
      } else {
            poly_data$group
      }
      idx <- seq_len(nrow(poly_data))
      nxt <- stats::ave(idx, ring, FUN = function(i) c(i[-1], i[1]))
      x <- poly_data$x; y <- poly_data$y; z <- poly_data$z
      terms <- cbind((y - y[nxt]) * (z + z[nxt]),
                     (z - z[nxt]) * (x + x[nxt]),
                     (x - x[nxt]) * (y + y[nxt]))
      sums <- rowsum(terms, as.character(poly_data$group))[as.character(groups), , drop = FALSE]
      len <- sqrt(rowSums(sums^2))
      normals <- sums / len
      normals[!is.finite(len) | len < 1e-12, ] <- rep(c(0, 0, 1), each = sum(!is.finite(len) | len < 1e-12))
      unname(normals)
}


#' Rotate scene-space face normals into camera space
#'
#' For camera-anchored directional lighting, face normals are computed in scene
#' space (where surface orientation is well-defined) and then transformed into
#' camera space with the same operations applied to coordinates during projection:
#' a z-flip followed by the projection rotation. This mirrors the camera handling
#' in transform_normals_to_standard() so that all lit geoms share one convention
#' for entering camera space.
#'
#' @param normals Matrix with 3 columns (x, y, z normal components) in scene space
#' @param proj List with pitch, roll, yaw (projection parameters)
#' @return Matrix with 3 columns of camera-space normals
#' @keywords internal
#' @noRd
rotate_normals_to_camera <- function(normals, proj) {
      normals[, 3] <- -normals[, 3]  # z-flip, matching transform_3d_standard
      rotate_3d(normals, proj$pitch, proj$roll, proj$yaw)
}

#' Compute surface normals from gradients
#'
#' @param gradients Data frame with group, dzdx, dzdy columns
#' @return Matrix with 3 columns (x, y, z normal components)
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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


#' Convert light position from data units to visual space units
#'
#' @param position Numeric vector of length 3 (x, y, z in data coordinates)
#' @param scale_ranges List with x, y, z scale ranges
#' @param scales Character, either "free" or "fixed"
#' @param ratio Numeric vector of length 3
#' @return Position vector in visual space, or NULL if position is NULL
#' @keywords internal
#' @noRd
transform_light_position <- function(position, scale_ranges, scales, ratio) {
      if(is.null(position)) return(NULL)
      light_data <- data.frame(x = position[1], y = position[2], z = position[3])
      standardized <- scale_to_standard(light_data, scale_ranges, scales, ratio)
      return(c(standardized$x, standardized$y, standardized$z))
}

#' Resolve a lighting specification to its final value
#'
#' `waiver()` signals that no lighting was specified anywhere, in which case the
#' package default applies. `NULL` is a deliberate request for no lighting and is
#' preserved as-is.
#'
#' @param light A `light` object, `"none"`, `NULL`, or `waiver()`.
#' @return A `light` object, `"none"`, or `NULL`.
#' @keywords internal
#' @noRd
resolve_light <- function(light) {
      if (inherits(light, "waiver")) light() else light
}

#' Store lighting specification to data frame for downstream use
#'
#' @param data Data frame to attach lighting spec to
#' @param light Lighting specification object, "none", NULL, or waiver()
#' @return Data frame with lighting_spec column added (if light is not NULL)
#' @keywords internal
#' @noRd
attach_light <- function(data, light){
      light <- resolve_light(light)
      if(!is.null(light)){
            if(!inherits(light, "light") && identical(light, "none")) light <- light("none")
            if(! "lighting_spec" %in% names(data)) data$lighting_spec <- I(list(light))
      }

      data
}


# Plot-level lighting via `+ light()` -------------------------------------------

#' Take ownership of a plot's coord before mutating it
#'
#' `Coord` ggproto objects have reference semantics and are *not* deep-copied by
#' ggplot2's `plot_clone()`, so one coord object can be shared by several plots.
#' Mutating it in place would leak into every plot that shares it. This clones
#' once, marks the clone, and mutates in place on subsequent calls.
#'
#' The clone must be bound to a *new* name. `ggproto(NULL, parent)` captures
#' `parent` lazily, so `co <- ggproto(NULL, co)` makes the clone its own parent
#' and any inherited field lookup then recurses until the C stack overflows.
#'
#' @param plot A ggplot object whose `coordinates` is a `Coord3D`.
#' @return The plot, with a coord that is safe to mutate.
#' @keywords internal
#' @noRd
own_coord_3d <- function(plot) {
      co <- plot$coordinates
      if (!isTRUE(co$.ggcube_owned)) {
            new_co <- ggproto(NULL, co)
            new_co$.ggcube_owned <- TRUE
            plot$coordinates <- new_co
      }
      plot
}

#' Signal that lighting was specified in two places
#'
#' @return Throws an error.
#' @keywords internal
#' @noRd
abort_double_light <- function() {
      rlang::abort(c(
            "Lighting was specified twice.",
            "x" = "Found lighting in both `coord_3d(light = )` and `+ light()`.",
            "i" = "Supply it in one place only; `+ light()` is the recommended form."
      ), call = NULL)
}

#' Find a plot-level light stashed by `+ light()`
#'
#' `+ light()` added before `coord_3d()` has no coord to write to, so it stashes
#' the spec on the plot instead. `Coord3D$setup_panel_params()` receives only
#' scales and params, so the stash is recovered by reaching the plot itself.
#'
#' @return A `light` object, or `NULL` if none was stashed.
#' @keywords internal
#' @noRd
find_pending_light <- function() {
      p <- find_build_plot()
      if (is.null(p)) return(NULL)
      attr(p, "ggcube_pending_light")
}

#' Report that plot-level lighting is being replaced
#'
#' Mirrors ggplot2's behaviour when a coord or scale replaces an existing one:
#' a component that is discarded wholesale is worth reporting, since
#' `+ light(direction = ) + light(mode = )` looks like it accumulates settings
#' but in fact keeps only the second.
#'
#' @return Called for its side effect.
#' @keywords internal
#' @noRd
inform_light_replaced <- function() {
      rlang::inform(c(
            "Lighting is already present.",
            i = "Adding new lighting, which will replace the existing specification."
      ))
}

#' @export
ggplot_add.light <- function(object, plot, ...) {
      if (inherits(plot$coordinates, "Coord3D")) {
            if (isTRUE(plot$coordinates$light_explicit)) abort_double_light()
            # A light already written to the coord by an earlier `+ light()` is
            # about to be discarded. An unresolved `waiver()` is not a
            # replacement: it means no lighting has been specified yet.
            if (!inherits(plot$coordinates$light, "waiver")) inform_light_replaced()
            plot <- own_coord_3d(plot)
            plot$coordinates$light <- object
      } else {
            # No coord yet. Stash on the plot; Coord3D$setup_panel_params()
            # picks it up at build time, so `+ light()` is order-independent.
            if (!is.null(attr(plot, "ggcube_pending_light"))) inform_light_replaced()
            attr(plot, "ggcube_pending_light") <- object
      }
      plot
}
