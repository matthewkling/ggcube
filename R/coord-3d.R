#' 3D coordinate system
#'
#' \code{coord_3d} is a 3D coordinate system that creates a 2D view of 3D data.
#' This is the essential core component of any plot made with `ggcube`.
#' It supports rotation, perspective projection, and options for controlling plot
#' aspect ratios, panel selection, and axis label placement.
#'
#' @param roll,pitch,yaw Rotation around x, y, and z axes, respectively, in degrees.
#'    Positive values rotate the near face of the plot "downward", "rightward", and clockwise, respectively.
#' @param persp Logical indicating whether to apply perspective projection. When \code{TRUE} (the default),
#'   objects farther from the viewer appear smaller. When \code{FALSE}, produces an orthographic projection in which
#'   lines that are parallel in 3D space render as parallel in the plot.
#' @param dist Distance from viewer to center of the data cube. Only used when \code{persp = TRUE}.
#'   Larger values create less perspective distortion. Default is 2.
#'   Values less than 1 are allowed but can be problematic for rendering.
#' @param expand Logical indicating whether to expand axis ranges beyond the data range,
#'   similar to standard ggplot2 behavior. If \code{TRUE} (the default), expansion behavior
#'   can be controlled using standard axis scaling functions, e.g.
#'   \code{... + scale_x_continuous(expand = expansion(.5))}.
#' @param clip Character string indicating clipping behavior. Use \code{"off"} (the default, recommended
#'   for some 3D plots) to allow drawing outside the plot panel.
#' @param panels Character vector specifying which panels to render, including one or more of the following:
#'   \itemize{
#'     \item \code{"background"}, \code{"foreground"}: faces laying behind or in front of
#'       the cube's interior volume, respectively. These panels vary depending on plot rotation.
#'     \item \code{"xmin"}, \code{"ymax"}, etc.: names of specific cube faces.
#'     \item \code{"all"}, \code{"none"}: display the full cube or remove all faces.
#'   }
#'   The default is \code{"background"}. Note that panel styling is handled
#'   via the normal \code{theme()} approach; if you select panels that sit in front of
#'   the data, you can control their transparency with `element_rect(alpha = ...)`.
#' @param xlabels,ylabels,zlabels Character strings or length-2 character vectors specifying
#'   axis label (text and title) placement. Labels are placed inline with grid lines for
#'   the selected panel face. For each axis, there are four potential panels where labels
#'   could be placed, and two potential edges for each panel. Labels can only be placed on
#'   visible faces (see `panels` argument). Each parameter accepts:
#'   \itemize{
#'     \item \code{"auto"} (default): Automatic edge selection based on an algorithm
#'       that prioritizes edges that are visible on the periphery of the plot and considers
#'       several attributes of face geometry for better readability.
#'     \item \code{c("face1", "face2")}: Manual edge specification using two adjacent
#'       face names (e.g., \code{c("xmin", "ymin")} selects the edge shared by the
#'       xmin and ymin faces). The \strong{first face} in the vector determines which
#'       face the axis labels will be aligned with, while the second face
#'       identifies which edge of this face gets labelled. Available face names are:
#'       "xmin", "xmax", "ymin", "ymax", "zmin", "zmax".
#'   }
#' @param rotate_labels Logical indicating whether axis labels (text and titles) should automatically
#'   rotate to align with the projected axis directions. When \code{FALSE}, uses theme
#'   text and title angle settings.
#' @param scales Character string specifying aspect ratio behavior:
#'   \itemize{
#'     \item \code{"free"} (default): Each axis scales independently to fill cube space,
#'       then \code{ratio} applies to standardized coordinates. This gives maximum
#'       visual range for each dimension.
#'     \item \code{"fixed"}: Maintains proportional relationships in raw data values,
#'       as scaled by \code{ratio}. Similar to \code{coord_fixed()} but for 3D
#'       (visual ratios match the labeled axis ranges).
#'   }
#' @param ratio Numeric vector of length 3 specifying relative axis lengths as
#'   \code{c(x, y, z)}. Defaults to \code{c(1, 1, 1)} for equal proportions.
#'   \itemize{
#'     \item With \code{scales = "free"}: Ratios apply to scaled cube coordinates
#'     \item With \code{scales = "fixed"}: Ratios apply to original data coordinates
#'   }
#'
#' @section 3D Theming:
#' 3D plots support additional theme elements beyond standard ggplot2 themes:
#'
#' **Text elements:**
#' - `axis.text.z`: Styling for z-axis tick labels (inherits from `axis.text`)
#' - `axis.title.z`: Styling for z-axis title (inherits from `axis.title`)
#' - `axis.text`, `axis.text`: Standard styling with `element_text()`. Note: use
#'    `element_text(margin = margin(...))` to adjust text padding, with left/right
#'    margins affecting axis text and top/bottom margins affecting axis titles;
#'    since placement and justification of these elements varies dynamically,
#'    no distinction is made between left and right margins, or between top and
#'    bottom margins -- you can set either, and the maximum of the two will be used.
#'
#' **Panel elements:**
#' - `panel.foreground`: Styling for cube faces rendered in front of data (inherits from `panel.background`)
#' - `panel.border.foreground`: Styling for cube faces rendered in front of data (inherits from `panel.border`)
#' - `panel.grid.foreground`: Styling for grid lines on foreground faces (inherits from `panel.grid`)
#' - `panel.grid.major.foreground`: Major grid lines on foreground faces (inherits from `panel.grid.foreground`)
#'
#' **Enhanced elements:**
#' - `element_rect()` supports an `alpha` parameter for transparency effects, particularly useful for `panel.foreground`
#'
#' The `panels` parameter controls which cube faces are rendered, while `theme()` controls their visual styling.
#' Background panels use standard `panel.background`, `panel.border`, `panel.grid`, etc., while foreground panels
#' use the `*.foreground` variants listed above. Since the foreground elements inherit from the standard background
#' and grid elements, you can use `panel.background`, etc. to style both background and foreground faces simultaneously.
#'
#' @examples
#' # base plot used in all examples
#' p <- ggplot() +
#'   geom_function_3d(
#'     aes(fill = after_stat(z), color = after_stat(z)),
#'     fun = function(x, y) sin(x) * cos(y),
#'     xlim = c(-pi, pi), ylim = c(-pi, pi),
#'     n = 50, light = light(contrast = 1.5)) +
#'   scale_fill_gradientn(colors = c("#344a91", "#753491", "#913434", "#915b34")) +
#'   scale_color_gradientn(colors = c("#344a91", "#753491", "#913434", "#915b34")) +
#'   theme(legend.position = "none")
#'
#' # 3D plot with default coord settings
#' p + coord_3d()
#'
#' # Use `pitch`, `roll`, `yaw` to control plot rotation ----------------------
#'
#' # zero rotation gives view from x-y face
#' p + coord_3d(pitch = 0, roll = 0, yaw = 0)
#'
#' # pitch rotates plot around y axis
#' p + coord_3d(pitch = 30, roll = 0, yaw = 0)
#'
#' # roll rotates plot around x axis
#' p + coord_3d(pitch = 0, roll = 30, yaw = 0)
#'
#' # yaw rotates plot around z axis
#' p + coord_3d(pitch = 0, roll = 0, yaw = 30)
#'
#' # combine them to achieve arbitrary rotations
#' p + coord_3d(pitch = 20, roll = 40, yaw = 60)
#'
#' # Use `persp` and `dist` to control perspective effects --------------------
#'
#' # strong perspective effect as if seen from very close
#' p + coord_3d(dist = 1)
#'
#' # weaker perspective effects as if seen from far away
#' p + coord_3d(dist = 3)
#'
#' # orthographic projection, effectively dist = Inf
#' p + coord_3d(persp = FALSE)
#'
#' # Use `scales` and `ratio` to control aspect ratio -------------------------
#'
#' # The default "free" scales shown above give cube with maximum visual range.
#' # Use "fixed" scales to make figure match data scales, like coord_fixed.
#' p + coord_3d(scales = "fixed")
#'
#' # Custom aspect ratios: make y twice as long visually
#' p + coord_3d(scales = "free", ratio = c(1, 2, 1))
#'
#' # Custom aspect ratios: fix scales but make y twice long
#' p + coord_3d(scales = "fixed", ratio = c(1, 2, 1))
#'
#' # Use `panels` to select which cube faces to render ------------------------
#'
#' p + coord_3d(panels = c("zmin", "xmax"))
#'
#' # and use `theme()` elements to control panel and text styling
#' p + coord_3d(panels = "all") +
#'     theme(panel.background = element_rect(color = "black"),
#'           panel.border = element_rect(color = "black"),
#'           panel.foreground = element_rect(alpha = .3),
#'           panel.grid.foreground = element_line(color = "gray", linewidth = .25),
#'           axis.text = element_text(color = "darkblue"),
#'           axis.text.z = element_text(color = "darkred"),
#'           axis.title = element_text(margin = margin(t = 30)),
#'           axis.title.x = element_text(color = "magenta"))
#'
#' # Use label params to control axis text placement and rotation -------------
#' p + coord_3d(xlabels = c("ymax", "zmax"),
#'              zlabels = c("xmax", "ymin"))
#' p + coord_3d(rotate_labels = FALSE)
#'
#' @export
coord_3d <- function(pitch = 0, roll = -60, yaw = -30,
                     persp = TRUE, dist = 2,
                     expand = TRUE, clip = "off",
                     panels = "background",
                     xlabels = "auto", ylabels = "auto", zlabels = "auto",
                     rotate_labels = TRUE,
                     scales = "free",
                     ratio = c(1, 1, 1)) {

      # Validate parameters
      if (!scales %in% c("free", "fixed")) {
            stop("scales must be 'free' or 'fixed'")
      }

      if (!is.numeric(ratio) || length(ratio) != 3 || any(ratio <= 0)) {
            stop("ratio must be a positive numeric vector of length 3")
      }

      if(persp && dist < 1) {
            warning("Although `dist` values less than 1 are allowed, they often produce nonsensical plots.")
      }

      list(
            ggproto(NULL, Coord3D,
                    pitch = pitch, roll = roll, yaw = yaw,
                    persp = persp, dist = dist,
                    expand = expand, clip = clip,
                    panels = panels,
                    rotate_labels = rotate_labels,
                    scales = scales,
                    ratio = ratio,
                    xlabels = xlabels, ylabels = ylabels, zlabels = zlabels
            ),
            theme(plot.margin = margin(20, 20, 20, 20, "pt"))
      )
}

#' Detect if a scale transformation flips direction
#'
#' @param scale_obj A ggplot2 scale object
#' @return Logical indicating if the scale transform flips direction
#' @keywords internal
detect_scale_direction_flip <- function(scale_obj) {
      if (is.null(scale_obj)) return(FALSE)

      transform <- scale_obj$trans
      if (is.null(transform)) return(FALSE)

      test_output <- transform$transform(c(1, 2))

      # If we can't compare the outputs, assume no flip
      if (length(test_output) != 2 || any(is.na(test_output))) {
            return(FALSE)
      }

      return(test_output[1] > test_output[2])
}

#' Translate face names using stored flip information
#'
#' @param faces Character vector of face names
#' @param flips List with x, y, z flip indicators
#' @return Character vector of translated face names
#' @keywords internal
translate_face_names_from_flips <- function(faces, flips) {
      if (length(faces) == 0) return(faces)
      for(flip in names(flips)){
            if(flips[[flip]]){
                  i <- which(faces == paste0(flip, "min"))
                  j <- which(faces == paste0(flip, "max"))
                  faces[i] <- paste0(flip, "max")
                  faces[j] <- paste0(flip, "min")
            }
      }
      return(faces)
}

#' Translate face names to account for scale direction flips
#'
#' @param face_names Character vector of face names
#' @param scale_x,scale_y,scale_z Scale objects for each axis
#' @return Character vector of translated face names
#' @keywords internal
translate_face_names_from_flips <- function(face_names, scale_flips) {
      if (length(face_names) == 0) return(face_names)

      # Only translate actual face names, leave other values unchanged
      valid_faces <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
      if (!any(face_names %in% valid_faces)) {
            return(face_names)  # No face names to translate
      }

      # Get flip indicators
      x_flipped <- scale_flips$x
      y_flipped <- scale_flips$y
      z_flipped <- scale_flips$z

      # Create translation mapping
      translated <- face_names

      for (i in seq_along(translated)) {
            face <- translated[i]

            if (x_flipped && face == "xmin") {
                  translated[i] <- "xmax"
            } else if (x_flipped && face == "xmax") {
                  translated[i] <- "xmin"
            } else if (y_flipped && face == "ymin") {
                  translated[i] <- "ymax"
            } else if (y_flipped && face == "ymax") {
                  translated[i] <- "ymin"
            } else if (z_flipped && face == "zmin") {
                  translated[i] <- "zmax"
            } else if (z_flipped && face == "zmax") {
                  translated[i] <- "zmin"
            }
      }

      return(translated)
}
translate_face_names <- function(faces, scale_x, scale_y, scale_z) {
      if (length(faces) == 0) return(faces)

      # Detect direction flips for each axis
      flips <- list(
            x = detect_scale_direction_flip(scale_x),
            y = detect_scale_direction_flip(scale_y),
            z = detect_scale_direction_flip(scale_z)
      )

      # Use the same logic as the flips version
      return(translate_face_names_from_flips(faces, flips))
}

Coord3D <- ggproto("Coord3D", CoordCartesian,
                   # Parameters
                   pitch = 0,
                   roll = 120,
                   yaw = 30,
                   persp = FALSE,
                   dist = 2,
                   expand = TRUE,
                   clip = "off",
                   panels = "background",
                   rotate_labels = TRUE,
                   scales = "free",
                   ratio = c(1, 1, 1),

                   plot_bounds = c(0, 1, 0, 1),  # [xmin, xmax, ymin, ymax]

                   setup_panel_params = function(self, scale_x, scale_y, params = list()) {

                         # Check if theme is void-like and override panels if so
                         theme_obj <- NULL
                         tryCatch({
                               for (i in 1:25) {
                                     env <- parent.frame(i)
                                     if (exists("theme", envir = env)) {
                                           potential_theme <- get("theme", envir = env)
                                           if (is.list(potential_theme)) {
                                                 theme_obj <- potential_theme
                                                 break
                                           }
                                     }
                               }
                         }, error = function(e) {
                               # Ignore errors - theme_obj will remain NULL
                         })

                         # Override panels to "none" if theme_void-like
                         original_panels <- self$panels
                         if (!is.null(theme_obj) && is_theme_void_like(theme_obj)) {
                               self$panels <- "none"
                         }

                         # Get standard panel params from parent
                         panel_params <- ggproto_parent(CoordCartesian, self)$setup_panel_params(scale_x, scale_y, params)

                         # Train and recover z scale
                         train_z_scale()
                         scale_z <- .z_scale_cache$scale
                         if (is.null(scale_z)) { # Create default z scale if none exists (e.g., when using stat_function_3d)
                               scale_z <- scale_z_continuous()
                               # scale_z$train(c(-10, 10))
                               .z_scale_cache$scale <- scale_z
                         }

                         # Translate face names to account for scale direction flips
                         self$panels <- translate_face_names(self$panels, scale_x, scale_y, scale_z)
                         self$xlabels <- translate_face_names(self$xlabels, scale_x, scale_y, scale_z)
                         self$ylabels <- translate_face_names(self$ylabels, scale_x, scale_y, scale_z)
                         self$zlabels <- translate_face_names(self$zlabels, scale_x, scale_y, scale_z)

                         # Store scale flip information for later use in transform()
                         panel_params$scale_flips <- list(
                               x = detect_scale_direction_flip(scale_x),
                               y = detect_scale_direction_flip(scale_y),
                               z = detect_scale_direction_flip(scale_z)
                         )

                         # Scale info (including axis names)
                         panel_params$scales <- self$scales
                         panel_params$scale_info <- list(
                               x = get_scale_info(scale_x, expand = self$expand, axis_name = "x"),
                               y = get_scale_info(scale_y, expand = self$expand, axis_name = "y"),
                               z = get_scale_info(scale_z, expand = self$expand, axis_name = "z")
                         )

                         # Blank out native scale names (prevents them from showing in standard ggplot2 rendering)
                         scale_x$name <- ""
                         scale_y$name <- ""

                         # Projection info
                         panel_params$proj <- list(pitch = self$pitch, roll = self$roll, yaw = self$yaw,
                                                   persp = self$persp, dist = self$dist)

                         # Aspect ratio info
                         panel_params$ratio <- self$ratio
                         effective_ratios <- compute_effective_ratios(
                               list(x = panel_params$scale_info$x$limits,
                                    y = panel_params$scale_info$y$limits,
                                    z = panel_params$scale_info$z$limits),
                               panel_params$scales,
                               panel_params$ratio
                         )

                         # Visible faces (using translated panel names)
                         visible_faces_fgbg <- select_visible_faces(self$panels, panel_params$proj, effective_ratios)
                         visible_faces <- do.call("c", visible_faces_fgbg)
                         panel_params$visible_faces <- visible_faces
                         panel_params$visible_faces_fg <- visible_faces_fgbg$fg
                         panel_params$visible_faces_bg <- visible_faces_fgbg$bg

                         # Calculate plot bounds using SCALE BREAKS and TITLE POSITIONS
                         if (length(visible_faces) > 0) {
                               all_faces <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
                               full_grid <- make_scale_grid(all_faces, panel_params$scale_info,
                                                            panel_params$scales, panel_params$ratio)

                               if (!is.null(full_grid)) {
                                     # Transform the FULL grid to get base bounds
                                     full_grid_transformed <- transform_3d_standard(full_grid, panel_params$proj)

                                     # Calculate bounds including potential title positions
                                     all_bounds_x <- full_grid_transformed$x
                                     all_bounds_y <- full_grid_transformed$y

                                     if(F){
                                           # Add title positions to bounds calculation
                                           for (axis in c("x", "y", "z")) {

                                                 # Use same edge selection logic as the rendering
                                                 axis_selection <- select_axis_edge_and_face(axis, visible_faces, panel_params$proj, effective_ratios)

                                                 if (!is.null(axis_selection)) {
                                                       # Use the selected edge center for title position in bounds calculation
                                                       edge_center_x <- (axis_selection$edge_p1_2d$x + axis_selection$edge_p2_2d$x) / 2
                                                       edge_center_y <- (axis_selection$edge_p1_2d$y + axis_selection$edge_p2_2d$y) / 2

                                                       # Add some offset for the title (approximate)
                                                       title_offset <- 0.15 * effective_ratios[match(axis, c("x", "y", "z"))]

                                                       # Calculate perpendicular offset direction (same logic as in rendering)
                                                       edge_dx <- axis_selection$edge_p2_2d$x - axis_selection$edge_p1_2d$x
                                                       edge_dy <- axis_selection$edge_p2_2d$y - axis_selection$edge_p1_2d$y
                                                       edge_length <- sqrt(edge_dx^2 + edge_dy^2)

                                                       if (edge_length > 0) {
                                                             # Get cube center for offset direction
                                                             cube_center_3d <- data.frame(x = 0, y = 0, z = 0)
                                                             cube_center_2d <- transform_3d_standard(cube_center_3d, panel_params$proj)

                                                             # Calculate perpendicular directions
                                                             perp1_dx <- -edge_dy / edge_length
                                                             perp1_dy <- edge_dx / edge_length
                                                             perp2_dx <- edge_dy / edge_length
                                                             perp2_dy <- -edge_dx / edge_length

                                                             # Choose direction away from cube center
                                                             to_edge_dx <- edge_center_x - cube_center_2d$x
                                                             to_edge_dy <- edge_center_y - cube_center_2d$y

                                                             dot1 <- perp1_dx * to_edge_dx + perp1_dy * to_edge_dy
                                                             dot2 <- perp2_dx * to_edge_dx + perp2_dy * to_edge_dy

                                                             if (abs(dot1) > abs(dot2)) {
                                                                   offset_dx <- sign(dot1) * perp1_dx * title_offset
                                                                   offset_dy <- sign(dot1) * perp1_dy * title_offset
                                                             } else {
                                                                   offset_dx <- sign(dot2) * perp2_dx * title_offset
                                                                   offset_dy <- sign(dot2) * perp2_dy * title_offset
                                                             }

                                                             # Add title position to bounds
                                                             title_x <- edge_center_x + offset_dx
                                                             title_y <- edge_center_y + offset_dy
                                                             all_bounds_x <- c(all_bounds_x, title_x)
                                                             all_bounds_y <- c(all_bounds_y, title_y)
                                                       }
                                                 }
                                           }
                                     }

                                     bounds_info <- calculate_plot_bounds(all_bounds_x, all_bounds_y)
                                     panel_params$plot_bounds <- bounds_info$bounds
                                     self$bounds_aspect <- bounds_info$aspect
                               } else {
                                     panel_params$plot_bounds <- c(-1, 1, -1, 1)
                               }

                               # Generate grid for selected faces using real scale breaks
                               if (length(visible_faces) > 0) {
                                     selected_grid <- make_scale_grid(visible_faces, panel_params$scale_info,
                                                                      panel_params$scales, panel_params$ratio)

                                     if (!is.null(selected_grid)) {
                                           selected_grid_transformed <- transform_3d_standard(selected_grid, panel_params$proj)
                                           panel_params$grid_transformed <- selected_grid_transformed
                                           panel_params$grid_transformed$face <- selected_grid$face
                                           panel_params$grid_transformed$group <- selected_grid$group
                                           panel_params$grid_transformed$z_proj <- selected_grid_transformed$z
                                           panel_params$grid_transformed$break_value <- selected_grid$break_value
                                           panel_params$grid_transformed$break_pos <- selected_grid$break_pos
                                           panel_params$grid_transformed$break_axis <- selected_grid$break_axis
                                           panel_params$grid_transformed$start_boundaries <- selected_grid$start_boundaries
                                           panel_params$grid_transformed$end_boundaries <- selected_grid$end_boundaries
                                     } else {
                                           panel_params$grid_transformed <- NULL
                                     }
                               } else {
                                     # No visible faces - no grid to render
                                     panel_params$grid_transformed <- NULL
                               }
                         } else {
                               # No visible faces - use minimal bounds (just cube corners)
                               aspect_cube <- data.frame(
                                     x = c(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5, -0.5, 0.5) * effective_ratios[1],
                                     y = c(-0.5, -0.5, 0.5, 0.5, -0.5, -0.5, 0.5, 0.5) * effective_ratios[2],
                                     z = c(-0.5, -0.5, -0.5, -0.5, 0.5, 0.5, 0.5, 0.5) * effective_ratios[3]
                               )
                               cube_transformed <- transform_3d_standard(aspect_cube, panel_params$proj)

                               bounds_info <- calculate_plot_bounds(cube_transformed$x, cube_transformed$y)
                               panel_params$plot_bounds <- bounds_info$bounds
                               self$bounds_aspect <- bounds_info$aspect
                               panel_params$grid_transformed <- NULL
                         }

                         return(panel_params)
                   },

                   # Force 1:1 aspect ratio
                   aspect = function(self, ranges) {
                         if (!is.null(self$bounds_aspect)) {
                               return(self$bounds_aspect)
                         }
                         return(1)
                   },

                   # Handle back transformation
                   backtransform_range = function(self, panel_params) {
                         return(list(x = c(0, 1), y = c(0, 1)))
                   },

                   render_bg = function(self, panel_params, theme) {
                         # Store theme element states in panel_params for render_cube to use
                         panel_params$theme_elements <- list(
                               show_background_panels = !inherits(calc_element("panel.background", theme), "element_blank"),
                               show_grid = !inherits(calc_element("panel.grid", theme), "element_blank") &&
                                     !inherits(calc_element("panel.grid.major", theme), "element_blank"),
                               show_axis_text = !inherits(calc_element("axis.text", theme), "element_blank"),
                               show_axis_title = !inherits(calc_element("axis.title", theme), "element_blank")
                         )

                         render_cube(self, panel_params, theme, layer = "background")
                   },

                   transform = function(self, data, panel_params) {

                         # Translate project_to_face names if they exist
                         if ("project_to_face" %in% names(data) && !is.null(panel_params$scale_flips)) {
                               data$project_to_face <- translate_face_names_from_flips(data$project_to_face, panel_params$scale_flips)
                         }

                         # Scale data to standard domain with aspect ratio
                         scale_ranges <- list(x = panel_params$scale_info$x$limits,
                                              y = panel_params$scale_info$y$limits,
                                              z = panel_params$scale_info$z$limits)
                         result <- scale_to_standard(data[c("x", "y", "z")], scale_ranges,
                                                     panel_params$scales, panel_params$ratio)

                         # lighting computation
                         if ("lighting_spec" %in% names(data)) {
                               data <- compute_light_in_coord(data, result, scale_ranges,
                                                              panel_params$scales, panel_params$ratio)
                         }

                         # Project data onto cube face, if applicable
                         result <- project_to_face(data, result, panel_params$proj)

                         # Expand ref_circle points to circular polygons, if applicable
                         data <- points_to_circles(data, result)

                         # Apply 3D transformation (returns x, y, z, depth, depth_scale)
                         result <- transform_3d_standard(data, panel_params$proj)

                         # Combine transformed coordinates and additional variables
                         result <- bind_cols(select(result, x, y, z, depth, depth_scale),
                                             select(data, -x, -y, -z))

                         # Apply final coordinate transformation to fit plot bounds
                         result <- scale_to_npc_coordinates(result, plot_bounds = panel_params$plot_bounds)

                         # Backface culling and light modification
                         result <- process_backfaces(result)

                         # Hierarchical depth sorting
                         result <- sort_by_depth(result)

                         # convert group to integer to prevent downstream rendering errors
                         result$group <- as.integer(factor(result$group))

                         return(result)
                   },

                   render_fg = function(self, panel_params, theme) {
                         # Store theme element states for foreground rendering
                         panel_params$theme_elements$show_foreground_panels = !inherits(calc_element("panel.foreground", theme), "element_blank")

                         render_cube(self, panel_params, theme, layer = "foreground")
                   },

                   # Disable standard grids
                   render_grid = function(self, panel_params, theme) {
                         grid::nullGrob()
                   },

                   # Handle z-ordering
                   map_data = function(self, data, panel_params) {
                         data <- self$transform(data, panel_params)

                         # Sort by z-depth
                         if ("z_proj" %in% names(data)) {
                               if ("group" %in% names(data)) {
                                     data <- data[order(data$group, -data$z_proj), ]
                               } else {
                                     data <- data[order(-data$z_proj), ]
                               }
                         }

                         return(data)
                   },

                   # Suppress irrelevant warnings about axis guides
                   modify_scales = function(self, scales_x, scales_y) {
                         # Set guides to none to prevent position guide warnings
                         for (scale in scales_x) {
                               if (!is.null(scale)) {
                                     scale$guide <- "none"
                               }
                         }
                         for (scale in scales_y) {
                               if (!is.null(scale)) {
                                     scale$guide <- "none"
                               }
                         }
                         return(list(x = scales_x, y = scales_y))
                   }
)

# Helper function to compute plot bounds and aspect ratio
calculate_plot_bounds <- function(all_bounds_x, all_bounds_y){
      x_bounds <- range(all_bounds_x, na.rm = TRUE)
      y_bounds <- range(all_bounds_y, na.rm = TRUE)

      # Add padding
      x_padding <- diff(x_bounds) * 0.05
      y_padding <- diff(y_bounds) * 0.05
      x_bounds <- c(x_bounds[1] - x_padding, x_bounds[2] + x_padding)
      y_bounds <- c(y_bounds[1] - y_padding, y_bounds[2] + y_padding)

      # Calculate aspect ratio
      bounds_aspect <- diff(y_bounds) / diff(x_bounds)

      # Return both bounds and aspect ratio
      list(
            bounds = c(x_bounds[1], x_bounds[2], y_bounds[1], y_bounds[2]),
            aspect = bounds_aspect
      )
}


#' Extract variable names from aesthetic mappings
#'
#' @param plot_obj A ggplot object
#' @return A list with x, y, z character vectors of variable names
#' @keywords internal
extract_aesthetic_vars <- function(plot_obj) {
      x_vars <- character(0)
      y_vars <- character(0)
      z_vars <- character(0)

      # Helper function to safely extract variables from a mapping
      extract_vars <- function(mapping) {
            vars <- list(x = character(0), y = character(0), z = character(0))

            tryCatch({
                  if (!is.null(mapping$x)) {
                        vars$x <- all.vars(mapping$x)
                  }
                  if (!is.null(mapping$y)) {
                        vars$y <- all.vars(mapping$y)
                  }
                  if (!is.null(mapping$z)) {
                        vars$z <- all.vars(mapping$z)
                  }
            }, error = function(e) {
                  # If all.vars() fails, ignore this mapping
            })

            return(vars)
      }

      # Plot-level mappings
      if (!is.null(plot_obj$mapping)) {
            plot_vars <- extract_vars(plot_obj$mapping)
            x_vars <- c(x_vars, plot_vars$x)
            y_vars <- c(y_vars, plot_vars$y)
            z_vars <- c(z_vars, plot_vars$z)
      }

      # Layer-level mappings
      if (!is.null(plot_obj$layers)) {
            for (layer in plot_obj$layers) {
                  if (!is.null(layer$mapping)) {
                        layer_vars <- extract_vars(layer$mapping)
                        x_vars <- c(x_vars, layer_vars$x)
                        y_vars <- c(y_vars, layer_vars$y)
                        z_vars <- c(z_vars, layer_vars$z)
                  }
            }
      }

      return(list(
            x = unique(x_vars),
            y = unique(y_vars),
            z = unique(z_vars)
      ))
}


#' Get axis name for a single scale with proper fallback hierarchy
#'
#' @param scale_obj Scale object (can be NULL for z-axis)
#' @param axis_name Axis name ("x", "y", or "z")
#' @return Single axis name string
#' @keywords internal
get_scale_names <- function(scale_obj, axis_name) {

      # CAPTURE SCALE NAME BEFORE BLANKING IT OUT
      scale_name <- if (!is.null(scale_obj)) scale_obj$name %||% waiver() else waiver()

      # TRY TO FIND PLOT OBJECT AND EXTRACT BOTH LABELS AND AESTHETIC VARS
      plot_obj <- NULL
      plot_labels <- NULL
      aesthetic_vars <- NULL

      tryCatch({
            for (i in 1:25) {
                  env <- parent.frame(i)
                  if (exists("plot", envir = env)) {
                        potential_plot <- get("plot", envir = env)
                        if (inherits(potential_plot, "ggplot")) {
                              plot_obj <- potential_plot
                              plot_labels <- potential_plot$labels
                              aesthetic_vars <- extract_aesthetic_vars(potential_plot)
                              break
                        }
                  }
            }
      }, error = function(e) {
            # Ignore errors - will use defaults
      })

      # RESOLVE FINAL NAME WITH FALLBACK HIERARCHY:
      # 1. Explicit scale name (from scale constructors like scale_x_continuous(name = "..."))
      # 2. Plot labels (automatic from aes() expressions OR user-set from labs())
      # 3. Simple aesthetic variable names (fallback if plot labels missing)
      # 4. Default name (axis_name)

      final_name <- axis_name  # default

      if (!inherits(scale_name, "waiver") && !is.null(scale_name) && scale_name != "") {
            # Priority 1: Explicit scale name
            final_name <- scale_name
      } else if (!is.null(plot_labels) && !is.null(plot_labels[[axis_name]]) && !inherits(plot_labels[[axis_name]], "waiver")) {
            # Priority 2: Plot labels (from aes expressions or labs())
            final_name <- plot_labels[[axis_name]]
      } else if (!is.null(aesthetic_vars) && length(aesthetic_vars[[axis_name]]) > 0) {
            # Priority 3: Aesthetic variable names
            final_name <- aesthetic_vars[[axis_name]][1]
      }

      return(final_name)
}

train_z_scale <- function(){

      # Walk parent frames to find layer data
      data <- NULL
      tryCatch({
            for (i in 1:25) {
                  env <- parent.frame(i)
                  if (exists("data", envir = env)) {
                        potential_data <- get("data", envir = env)
                        # Check if this looks like layer data (list of data frames)
                        if (is.list(potential_data) && length(potential_data) > 0 &&
                            is.data.frame(potential_data[[1]])) {
                              data <- potential_data
                              break
                        }
                  }
            }
      }, error = function(e) {})

      # If found data, train z scale
      if (!is.null(data) && !is.null(.z_scale_cache$scale)) {
            for (layer_data in data) {
                  if ("z" %in% names(layer_data) && nrow(layer_data) > 0) {

                        # Train continuous scales
                        if(inherits(.z_scale_cache$scale, "ScaleContinuousPosition")) {
                              .z_scale_cache$scale$train(layer_data$z)
                        }

                        # Train discrete scales
                        if(inherits(.z_scale_cache$scale, "ScaleDiscretePosition")) {

                              if("z_raw" %in% names(layer_data)) { # Data comes from a ggcube stat
                                    .z_scale_cache$scale$range_c$train(layer_data$z)
                                    .z_scale_cache$scale$train(layer_data$z_raw)
                              } else { # Data not from ggcube stat (no z_raw field)
                                    .z_scale_cache$scale$range_c$train(as.integer(factor(layer_data$z)))
                                    .z_scale_cache$scale$train(layer_data$z)
                              }

                        }
                  }
            }
      }
}

#' Check if theme is void-like (has multiple key elements set to element_blank)
#'
#' @param theme_obj Theme object or theme list
#' @return Logical indicating if theme appears to be void-like
#' @keywords internal
is_theme_void_like <- function(theme_obj) {
      if (is.null(theme_obj)) return(FALSE)

      # Create a temporary theme to test elements
      temp_theme <- theme_obj

      # Count how many key theme elements are element_blank
      blank_count <- 0
      key_elements <- c("panel.background", "panel.grid", "panel.grid.major",
                        "axis.text", "axis.title")

      for (element_name in key_elements) {
            tryCatch({
                  element_val <- calc_element(element_name, temp_theme)
                  if (inherits(element_val, "element_blank")) {
                        blank_count <- blank_count + 1
                  }
            }, error = function(e) {
                  # If we can't evaluate the element, skip it
            })
      }

      # If 3 or more key elements are blank, consider it void-like
      return(blank_count >= 3)
}

get_scale_info <- function(scale_obj, expand = TRUE, axis_name = NULL) {

      expansion <- ggplot2:::default_expansion(scale_obj, expand = expand)
      limits <- scale_obj$get_limits()

      # Pass NULL for coord_limits since coord_3d doesn't support them yet
      expanded_range <- ggplot2:::expand_limits_scale(scale_obj, expansion, limits, coord_limits = NULL)

      # Get all breaks and labels
      all_breaks <- scale_obj$get_breaks()
      all_labels <- scale_obj$get_labels()

      # Filter breaks to be within expanded limits
      if (is.numeric(all_breaks)) {
            # For continuous scales, filter numerically
            valid_mask <- !is.na(all_breaks) &
                  all_breaks >= expanded_range[1] &
                  all_breaks <= expanded_range[2]
            valid_breaks <- all_breaks[valid_mask]

            # Filter corresponding labels
            if (length(all_labels) == length(all_breaks)) {
                  valid_labels <- all_labels[valid_mask]
            } else {
                  valid_labels <- all_labels  # Let ggplot2 handle label mismatch
            }
      } else {
            # For discrete scales, keep all breaks/labels as-is
            # (discrete scales shouldn't have out-of-bounds issues)
            valid_breaks <- all_breaks
            valid_labels <- all_labels
      }

      # Get the scale name if axis_name is provided
      scale_name <- if (!is.null(axis_name)) get_scale_names(scale_obj, axis_name) else NULL

      result <- list(limits = expanded_range,
                     breaks = valid_breaks,
                     labels = valid_labels)

      # Add name if provided
      if (!is.null(scale_name)) {
            result$name <- scale_name
      }

      return(result)
}

# Project data onto cube face, if applicable
project_to_face <- function(data, data_std, proj){
      if(! "project_to_face" %in% names(data) || all(is.na(data$project_to_face))) return(data_std)
      data_std %>%
            mutate(face = data$project_to_face,
                   depth_3d = transform_3d_standard(data_std, proj)$depth, # save pre-flattening depth for sorting
                   axis = substr(face, 1, 1),
                   value = ifelse(substr(face, 2, 4) == "min", -.5, .5),
                   x = ifelse(is.na(face) | axis != "x", x, value),
                   y = ifelse(is.na(face) | axis != "y", y, value),
                   z = ifelse(is.na(face) | axis != "z", z, value)) %>%
            select(x, y, z, depth_3d)
}


#' Convert ref_circle points to circular polygons
#'
#' @param data Original data frame
#' @param data_std Standardized data frame
#' @return Data frame with circular polygons replacing ref_circle points
#' @keywords internal
points_to_circles <- function(data, data_std) {

      # Keep std coords and all other vars
      d <- bind_cols(select(data, -x, -y, -z),
                     select(data_std, x, y, z))
      if("depth_3d" %in% names(data_std)) d$depth_3d <- data_std$depth_3d

      # Check if we have any ref_circle elements
      if (!"element_type" %in% names(d) || !any(d$element_type == "ref_circle")) {
            return(d)
      }

      result <- filter(d, element_type == "ref_circle") %>%
            rowwise() %>%
            reframe(generate_circle_vertices(x, y, z, project_to_face,
                                             ref_circle_radius, ref_circle_vertices),
                    vertex_order = 1:ref_circle_vertices,
                    group = group) %>%
            full_join(select(filter(d, element_type == "ref_circle"), -x, -y, -z),
                      by = join_by(group)) %>%
            arrange(group, vertex_order) %>%
            bind_rows(filter(d, element_type != "ref_circle"))

      return(result)
}

#' Generate circle vertices in 3D space for a given face
#'
#' @param x_std,y_std,z_std Standardized point coordinates
#' @param face Face name (e.g., "zmin", "xmax")
#' @param radius Circle radius in standardized units
#' @param n_vertices Number of vertices for the circle
#' @return Data frame with x, y, z coordinates for circle vertices
#' @keywords internal
generate_circle_vertices <- function(x_std, y_std, z_std, face, radius, n_vertices) {
      # Generate angles for circle vertices
      angles <- seq(0, 2 * pi, length.out = n_vertices + 1)[-(n_vertices + 1)]

      # Get the face axis and value
      face_axis <- substr(face, 1, 1)
      face_value <- ifelse(substr(face, 2, 4) == "min", -0.5, 0.5)

      # Generate circle vertices based on the face
      if (face_axis == "z") { # Circle in x-y plane
            x_coords <- x_std + radius * cos(angles)
            y_coords <- y_std + radius * sin(angles)
            z_coords <- rep(face_value, n_vertices)
      } else if (face_axis == "x") { # Circle in y-z plane
            x_coords <- rep(face_value, n_vertices)
            y_coords <- y_std + radius * cos(angles)
            z_coords <- z_std + radius * sin(angles)
      } else if (face_axis == "y") { # Circle in x-z plane
            x_coords <- x_std + radius * cos(angles)
            y_coords <- rep(face_value, n_vertices)
            z_coords <- z_std + radius * sin(angles)
      } else {
            stop("Invalid face: ", face)
      }

      return(data.frame(x = x_coords, y = y_coords, z = z_coords))
}


#' Scale transformed coordinates to final npc coordinates
#'
#' @param result Data frame with transformed x, y coordinates
#' @param plot_bounds Plot bounds vector [xmin, xmax, ymin, ymax]
#' @return Data frame with scaled coordinates
#' @keywords internal
scale_to_npc_coordinates <- function(result, plot_bounds) {
      result$x <- (result$x - plot_bounds[1]) / (plot_bounds[2] - plot_bounds[1])
      result$y <- (result$y - plot_bounds[3]) / (plot_bounds[4] - plot_bounds[3])
      return(result)
}

validate_coord3d <- function(coord){
      stopifnot("Did you forget to add `coord_3d()` to your plot?" = inherits(coord, "Coord3D"))
}

process_backfaces <- function(data) {

      # Compute back_face mask
      data <- data %>%
            group_by(group) %>%
            mutate(
                  n_vertices = n(),

                  # Simple test (old method)
                  back_face_simple = ((x[2] - x[1]) * (y[3] - y[1]) - (x[3] - x[1]) * (y[2] - y[1])) < 0,

                  # Complex test (new method)
                  back_face = case_when(
                        n_vertices != 4 ~ back_face_simple,
                        n_vertices == 4 ~ {
                              # Test all 4 triangulations
                              tri_123 <- (x[2] - x[1]) * (y[3] - y[1]) - (x[3] - x[1]) * (y[2] - y[1]) < 0
                              tri_134 <- (x[3] - x[1]) * (y[4] - y[1]) - (x[4] - x[1]) * (y[3] - y[1]) < 0
                              tri_124 <- (x[2] - x[1]) * (y[4] - y[1]) - (x[4] - x[1]) * (y[2] - y[1]) < 0
                              tri_234 <- (x[3] - x[2]) * (y[4] - y[2]) - (x[4] - x[2]) * (y[3] - y[2]) < 0

                              # # Print debug info for first few groups
                              # if (cur_group_id() <= 3) {
                              #       cat("Group", cur_group_id(), "triangulations:", tri_123, tri_134, tri_124, tri_234, "\n")
                              # }

                              # Both triangulations must be consistently negative
                              (tri_123 & tri_134) & (tri_124 & tri_234)
                        }
                  )
            )


      # Apply culling if requested
      cull_backfaces <- data$cull_backfaces[1] %||% FALSE
      if(cull_backfaces) {
            data <- filter(data, !back_face)
      }

      # Apply lighting effects if present
      if("lighting_spec" %in% names(data)) {
            scl <- data$lighting_spec[[1]]$backface_scale %||% 1
            off <- data$lighting_spec[[1]]$backface_offset %||% 0
            if(scl != 1 || off != 0) {
                  data <- mutate(data, light = ifelse(back_face, light * scl + off, light))
            }
      }

      return(select(data, -back_face))
}


