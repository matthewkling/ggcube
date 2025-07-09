#' 3D coordinate system
#'
#' \code{coord_3d} creates a 3D coordinate system that projects 3D data onto a 2D plot.
#' It supports rotation, perspective projection, and automatic axis labeling with
#' intelligent edge selection for optimal readability.
#'
#' @param pitch Rotation around x-axis in degrees. Positive values rotate "up" towards viewer.
#' @param roll Rotation around y-axis in degrees. Positive values rotate "right" edge towards viewer.
#' @param yaw Rotation around z-axis in degrees. Positive values rotate counterclockwise when viewed from above.
#' @param persp Logical indicating whether to apply perspective projection. When \code{TRUE},
#'   objects farther from the viewer appear smaller.
#' @param dist Distance from viewer to center of the data cube when \code{persp = TRUE}.
#'   Larger values create less perspective distortion.
#' @param expand Logical indicating whether to expand axis ranges beyond the data range,
#'   similar to standard ggplot2 behavior.
#' @param clip Character string indicating clipping behavior. Use \code{"off"} to allow
#'   drawing outside the plot panel (recommended for 3D plots).
#' @param faces Character string specifying which cube faces to render. Options include
#'   \code{"all"}, \code{"background"}, \code{"foreground"}, or specific face names like
#'   \code{"xmin"}, \code{"ymax"}, etc.
#' @param xlabels,ylabels,zlabels Character strings or length-2 character vectors specifying
#'   axis label (text and title) placement. Each parameter accepts:
#'   \itemize{
#'     \item \code{"auto"} (default): Automatic edge selection based on an algorithm
#'       that prioritizes edges that are visible on the edge of the plot and considers
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
#'     \item \code{"fixed"}: Maintains proportional relationship to the displayed
#'       coordinate system (including any expansion/padding), then \code{ratio}
#'       applies to these scale-space proportions. Similar to \code{coord_fixed()}
#'       but for 3D - visual ratios match the labeled axis ranges.
#'   }
#' @param ratio Numeric vector of length 3 specifying relative axis lengths as
#'   \code{c(x, y, z)}. Defaults to \code{c(1, 1, 1)} for equal proportions.
#'   \itemize{
#'     \item With \code{scales = "free"}: Ratios apply to cube coordinates
#'     \item With \code{scales = "fixed"}: Ratios apply to scale-space coordinates
#'   }
#'
#' @examples
#' library(ggplot2)
#'
#' # Default free scales (current behavior - maximum visual range)
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   coord_3d()
#'
#' # Fixed scales - visual proportions match coordinate system (like coord_fixed)
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   coord_3d(scales = "fixed")
#'
#' # Custom cube ratios (make z twice as tall visually)
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   coord_3d(scales = "free", ratio = c(1, 1, 2))
#'
#' # Custom scale ratios (z gets twice the visual space relative to its scale range)
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   coord_3d(scales = "fixed", ratio = c(1, 1, 2))
#'
#' @export
coord_3d <- function(pitch = -30, roll = 30, yaw = 0,
                     persp = TRUE, dist = 3,
                     expand = TRUE, clip = "off",
                     faces = "background",
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

      list(
            ggproto(NULL, Coord3D,
                    pitch = pitch, roll = roll, yaw = yaw,
                    persp = persp, dist = dist,
                    expand = expand, clip = clip,
                    faces = faces,
                    rotate_labels = rotate_labels,
                    scales = scales,
                    ratio = ratio,
                    xlabels = xlabels, ylabels = ylabels, zlabels = zlabels
            ),
            theme(plot.margin = margin(20, 20, 20, 20, "pt"))
      )
}

#' Extract variable names from aesthetic mappings
#'
#' @param plot_obj A ggplot object
#' @return A list with x, y, z character vectors of variable names
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

#' Extract original data ranges from plot object using aesthetic mappings
#'
#' @param plot_obj A ggplot object
#' @return A list with x, y, z ranges (or NULL if not found)
extract_original_ranges <- function(plot_obj) {
      if (is.null(plot_obj$data)) {
            return(list(x = NULL, y = NULL, z = NULL))
      }

      aesthetic_vars <- extract_aesthetic_vars(plot_obj)

      # Extract ranges, handling multiple variables per aesthetic and categorical data
      get_range_for_vars <- function(var_names, data) {
            if (length(var_names) == 0) return(NULL)

            # Filter to variables that actually exist in the data
            existing_vars <- var_names[var_names %in% names(data)]
            if (length(existing_vars) == 0) return(NULL)

            # Get all values from these variables
            all_values <- unlist(data[existing_vars], use.names = FALSE)
            if (length(all_values) == 0 || all(is.na(all_values))) return(NULL)

            # Handle categorical/factor variables
            if (is.factor(all_values) || is.character(all_values)) {
                  # Convert to factor if character
                  if (is.character(all_values)) {
                        all_values <- factor(all_values)
                  }

                  # Return range based on factor level positions (1, 2, 3, ...)
                  n_levels <- length(levels(all_values))
                  if (n_levels > 0) {
                        # Return a special marker indicating this was categorical
                        return(structure(c(1, n_levels), categorical = TRUE))
                  } else {
                        return(NULL)
                  }
            } else {
                  # Numeric variables - use standard range
                  return(range(all_values, na.rm = TRUE))
            }
      }

      return(list(
            x = get_range_for_vars(aesthetic_vars$x, plot_obj$data),
            y = get_range_for_vars(aesthetic_vars$y, plot_obj$data),
            z = get_range_for_vars(aesthetic_vars$z, plot_obj$data)
      ))
}


Coord3D <- ggproto("Coord3D", CoordCartesian,
                   # Parameters
                   pitch = 0,
                   roll = 0,
                   yaw = 0,
                   persp = FALSE,
                   dist = 3,
                   grid = TRUE,
                   expand = TRUE,
                   clip = "off",
                   faces = "background",
                   rotate_labels = TRUE,
                   scales = "free",
                   ratio = c(1, 1, 1),

                   plot_bounds = c(0, 1, 0, 1),  # [xmin, xmax, ymin, ymax]

                   setup_panel_params = function(self, scale_x, scale_y, params = list()) {

                         # Extract original data ranges for 3D coordinate scaling
                         # This is necessary because ggplot2 normalizes z-values to [0,1] but we need
                         # the original ranges for proper 3D scaling and expansion calculations.
                         # We also extract x/y ranges to ensure consistent handling across all axes.
                         original_ranges <- list(x = NULL, y = NULL, z = NULL)
                         tryCatch({
                               for (i in 1:25) {
                                     env <- parent.frame(i)
                                     if (exists("plot", envir = env)) {
                                           plot_obj <- get("plot", envir = env)
                                           if (inherits(plot_obj, "ggplot")) {
                                                 if (!is.null(plot_obj$data)) {
                                                       original_ranges <- extract_original_ranges(plot_obj)
                                                       break
                                                 }
                                           }
                                     }
                               }
                         }, error = function(e) {
                               # Ignore errors - we'll use defaults if this fails
                         })

                         original_x_range <- original_ranges$x
                         original_y_range <- original_ranges$y
                         original_z_range <- original_ranges$z

                         # Apply expansion to x and y axes
                         x_info <- apply_axis_expansion(original_x_range, scale_x, "x")
                         y_info <- apply_axis_expansion(original_y_range, scale_y, "y")

                         # Handle z-axis (both continuous and discrete)
                         z_info <- list(limits = c(0, 1), breaks = scales::extended_breaks()(c(0, 1)))  # Default
                         z_auto_detect <- FALSE

                         # Check if we have cached z scale info
                         if (exists("type", envir = .z_scale_cache)) {

                               # Handle discrete Z scales
                               if (.z_scale_cache$type == "discrete") {
                                     z_info <- handle_discrete_z_scale(original_z_range)
                                     z_auto_detect <- TRUE  # Discrete scales auto-convert data
                               } else {
                                     # Handle continuous Z scales (existing logic)
                                     z_info <- handle_continuous_z_scale(original_z_range)
                                     z_auto_detect <- .z_scale_cache$type == "continuous_auto"
                               }

                         } else if (exists("limits", envir = .z_scale_cache)) {
                               # Legacy path - assume continuous for backward compatibility
                               z_info <- handle_continuous_z_scale(original_z_range)
                               z_auto_detect <- TRUE
                         } else {
                               # No cached z scale info at all - auto-detect scale type and apply expansion
                               if (!is.null(original_z_range)) {
                                     # Auto-detect: check if original data was categorical
                                     if (is_categorical_range(original_z_range)) {
                                           # Auto-apply discrete Z scale
                                           .z_scale_cache$type <- "discrete"
                                           .z_scale_cache$limits <- NULL  # Let it auto-detect from data
                                           .z_scale_cache$breaks <- waiver()
                                           .z_scale_cache$labels <- waiver()
                                           .z_scale_cache$expand <- waiver()
                                           .z_scale_cache$drop <- TRUE

                                           z_info <- handle_discrete_z_scale(original_z_range)
                                           z_auto_detect <- TRUE
                                     } else {
                                           # Apply continuous expansion logic
                                           expand_factor <- 0.05
                                           range_width <- diff(original_z_range)

                                           if (range_width > 0) {
                                                 expanded_range <- c(original_z_range[1] - range_width * expand_factor,
                                                                     original_z_range[2] + range_width * expand_factor)
                                           } else {
                                                 expanded_range <- original_z_range[1] + c(-0.1, 0.1)
                                           }

                                           z_breaks <- scales::extended_breaks(n = 5)(expanded_range)
                                           final_limits <- range(z_breaks)
                                           z_info <- list(
                                                 limits = final_limits,
                                                 breaks = z_breaks,
                                                 original_range = original_z_range
                                           )
                                           z_auto_detect <- TRUE
                                     }
                               } else {
                                     z_info <- list(limits = c(0, 1), breaks = scales::extended_breaks()(c(0, 1)))
                                     z_auto_detect <- TRUE
                               }
                         }

                         # Get axis names with proper labs() support
                         x_name <- scale_x$name %||% "x"
                         y_name <- scale_y$name %||% "y"
                         z_name <- "z"

                         # Check for labs() overrides
                         plot_labels <- NULL

                         # Method 1: Direct from params (most reliable during build)
                         if (!is.null(params$plot) && !is.null(params$plot$labels)) {
                               plot_labels <- params$plot$labels
                         }

                         # Method 2: Try to get from parent environment if Method 1 fails
                         if (is.null(plot_labels)) {
                               tryCatch({
                                     # Look up the call stack for ggplot build context
                                     for (i in 1:10) {
                                           env <- parent.frame(i)
                                           if (exists("plot", envir = env)) {
                                                 potential_plot <- get("plot", envir = env)
                                                 if (inherits(potential_plot, "ggplot") && !is.null(potential_plot$labels)) {
                                                       plot_labels <- potential_plot$labels
                                                       break
                                                 }
                                           }
                                     }
                               }, error = function(e) {})
                         }

                         # Apply labs() overrides if found
                         if (!is.null(plot_labels)) {
                               if (!is.null(plot_labels$x) && !inherits(plot_labels$x, "waiver")) {
                                     x_name <- plot_labels$x
                               }
                               if (!is.null(plot_labels$y) && !inherits(plot_labels$y, "waiver")) {
                                     y_name <- plot_labels$y
                               }
                               if (!is.null(plot_labels$z) && !inherits(plot_labels$z, "waiver")) {
                                     z_name <- plot_labels$z
                               }
                         }

                         # Also check z scale cache for scale_z_continuous() names
                         if (exists("name", envir = .z_scale_cache) && !is.null(.z_scale_cache$name)) {
                               if (!inherits(.z_scale_cache$name, "waiver") && z_name == "z") {
                                     z_name <- .z_scale_cache$name
                               }
                         }

                         # UPDATE scale limits to use the expanded ranges
                         # This ensures the coordinate system uses the expanded ranges
                         scale_x$limits <- x_info$limits
                         scale_y$limits <- y_info$limits

                         # Get standard panel params from parent (this will now use our updated limits)
                         panel_params <- ggproto_parent(CoordCartesian, self)$setup_panel_params(scale_x, scale_y, params)

                         # Store the auto-detect flag for later use in transform()
                         panel_params$z_auto_detect <- z_auto_detect

                         # Store scale information including proper names
                         panel_params$scale_info <- list(
                               x = list(limits = x_info$limits, breaks = x_info$breaks, labels = x_info$labels, name = x_name),
                               y = list(limits = y_info$limits, breaks = y_info$breaks, labels = y_info$labels, name = y_name),
                               z = z_info
                         )

                         # Add z name to z_info if it doesn't have one
                         panel_params$scale_info$z$name <- z_name

                         # Now blank out the scale names
                         scale_x$name <- ""
                         scale_y$name <- ""

                         # Store aspect ratio information
                         panel_params$scales <- self$scales
                         panel_params$ratio <- self$ratio

                         panel_params$proj <- list(pitch = self$pitch, roll = self$roll, yaw = self$yaw,
                                                   persp = self$persp, dist = self$dist)

                         # Calculate effective ratios for face detection
                         effective_ratios <- compute_effective_ratios(
                               list(x = panel_params$scale_info$x$limits,
                                    y = panel_params$scale_info$y$limits,
                                    z = panel_params$scale_info$z$limits),
                               panel_params$scales,
                               panel_params$ratio
                         )

                         # Calculate visible faces using aspect-adjusted cube
                         visible_faces_fgbg <- select_visible_faces(self$faces, panel_params$proj, effective_ratios)
                         visible_faces <- do.call("c", visible_faces_fgbg)
                         panel_params$visible_faces <- visible_faces
                         panel_params$visible_faces_fg <- visible_faces_fgbg$fg
                         panel_params$visible_faces_bg <- visible_faces_fgbg$bg

                         # Calculate effective ratios for bounds calculation
                         effective_ratios <- compute_effective_ratios(
                               list(x = panel_params$scale_info$x$limits,
                                    y = panel_params$scale_info$y$limits,
                                    z = panel_params$scale_info$z$limits),
                               panel_params$scales,
                               panel_params$ratio
                         )

                         # Calculate plot bounds using SCALE BREAKS and TITLE POSITIONS
                         if (self$grid) {
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
                                                 axis_selection <- select_axis_edge_and_face(axis, visible_faces, panel_params$proj)

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

                                     panel_params$plot_bounds <- calculate_plot_bounds(all_bounds_x, all_bounds_y)
                               } else {
                                     panel_params$plot_bounds <- c(-1, 1, -1, 1)
                               }

                               # Generate grid for selected faces using real scale breaks
                               selected_grid <- make_scale_grid(visible_faces, panel_params$scale_info,
                                                                panel_params$scales, panel_params$ratio)

                               if (!is.null(selected_grid)) {
                                     selected_grid_transformed <- transform_3d_standard(selected_grid, panel_params$proj)
                                     panel_params$grid_transformed <- selected_grid_transformed
                                     panel_params$grid_transformed$face <- selected_grid$face
                                     panel_params$grid_transformed$group <- selected_grid$group
                                     panel_params$grid_transformed$z_proj <- selected_grid_transformed$z
                                     panel_params$grid_transformed$break_value <- selected_grid$break_value
                                     panel_params$grid_transformed$break_axis <- selected_grid$break_axis
                                     panel_params$grid_transformed$start_boundaries <- selected_grid$start_boundaries
                                     panel_params$grid_transformed$end_boundaries <- selected_grid$end_boundaries
                               } else {
                                     panel_params$grid_transformed <- NULL
                               }
                         } else {
                               # No grid - use aspect-adjusted cube bounds
                               aspect_cube <- data.frame(
                                     x = c(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5, -0.5, 0.5) * effective_ratios[1],
                                     y = c(-0.5, -0.5, 0.5, 0.5, -0.5, -0.5, 0.5, 0.5) * effective_ratios[2],
                                     z = c(-0.5, -0.5, -0.5, -0.5, 0.5, 0.5, 0.5, 0.5) * effective_ratios[3]
                               )
                               cube_transformed <- transform_3d_standard(aspect_cube, panel_params$proj)

                               panel_params$plot_bounds <- calculate_plot_bounds(all_bounds_x, all_bounds_y)
                               panel_params$grid_transformed <- NULL
                         }

                         return(panel_params)
                   },

                   # Force 1:1 aspect ratio
                   aspect = function(self, ranges) {
                         cat("aspect called\n")
                         if (exists("bounds_aspect")) {
                               return(bounds_aspect)
                         }
                         return(1)
                   },

                   # Handle back transformation
                   backtransform_range = function(self, panel_params) {
                         return(list(x = c(0, 1), y = c(0, 1)))
                   },

                   render_bg = function(self, panel_params, theme) {
                         cat("render_bg called\n")
                         render_cube(self, panel_params, theme, layer = "background")
                   },

                   transform = function(self, data, panel_params) {

                         # Add z column if missing
                         if (!"z" %in% names(data)) {
                               data$z <- 0
                         }

                         # Handle z-value scaling for both continuous and discrete
                         if (panel_params$z_auto_detect %||% FALSE) {
                               # Check if we have discrete Z scale
                               if (!is.null(panel_params$scale_info$z$levels)) {
                                     # Discrete Z scale - convert categorical values to numeric positions
                                     z_levels <- panel_params$scale_info$z$levels

                                     # Convert factors/characters to numeric positions
                                     if (is.factor(data$z)) {
                                           # Factor data - map to positions based on levels
                                           data$z <- as.numeric(data$z)
                                     } else if (is.character(data$z)) {
                                           # Character data - convert to factor then numeric
                                           data$z <- as.numeric(factor(data$z, levels = z_levels))
                                     }
                                     # If already numeric, assume it's in correct format

                               } else {
                                     # Continuous Z scale (existing logic)
                                     original_range <- panel_params$scale_info$z$original_range %||% NULL
                                     expanded_limits <- panel_params$scale_info$z$limits

                                     if (!is.null(original_range) && length(original_range) == 2) {
                                           original_range_width <- original_range[2] - original_range[1]
                                           data$z <- data$z * original_range_width + original_range[1]
                                     } else {
                                           z_limits <- expanded_limits
                                           if (!is.null(z_limits) && length(z_limits) == 2) {
                                                 scale_range <- z_limits[2] - z_limits[1]
                                                 data$z <- data$z * scale_range + z_limits[1]
                                           }
                                     }
                               }
                         } else {
                               # Explicit Z scale - check for discrete vs continuous
                               if (!is.null(panel_params$scale_info$z$levels)) {
                                     # Discrete Z scale with explicit scale
                                     z_levels <- panel_params$scale_info$z$levels

                                     # Convert categorical data to numeric positions
                                     if (is.factor(data$z)) {
                                           data$z <- as.numeric(data$z)
                                     } else if (is.character(data$z)) {
                                           data$z <- as.numeric(factor(data$z, levels = z_levels))
                                     }

                               } else {
                                     # Continuous Z scale (existing logic)
                                     if ("z" %in% names(data)) {
                                           z_limits <- panel_params$scale_info$z$limits
                                           if (!is.null(z_limits) && length(z_limits) == 2) {
                                                 scale_range <- z_limits[2] - z_limits[1]
                                                 data$z <- data$z * scale_range + z_limits[1]
                                           }
                                     }
                               }
                         }

                         # Scale data to standard domain with aspect ratio
                         scale_ranges <- list(
                               x = panel_params$scale_info$x$limits,
                               y = panel_params$scale_info$y$limits,
                               z = panel_params$scale_info$z$limits
                         )

                         data_std <- scale_to_standard(
                               data[c("x", "y", "z")],
                               scale_ranges,
                               panel_params$scales,
                               panel_params$ratio
                         )

                         # Apply 3D transformation (returns x, y, z, depth)
                         transformed <- transform_3d_standard(data_std, panel_params$proj)

                         # Store transformed coordinates
                         data$x <- transformed$x
                         data$y <- transformed$y
                         data$z <- transformed$z      # Keep for face visibility calculations
                         data$depth <- transformed$depth  # Use for depth sorting

                         # Apply final coordinate transformation to fit plot bounds [0, 1]
                         result <- data
                         result$x <- (data$x - panel_params$plot_bounds[1]) / (panel_params$plot_bounds[2] - panel_params$plot_bounds[1])
                         result$y <- (data$y - panel_params$plot_bounds[3]) / (panel_params$plot_bounds[4] - panel_params$plot_bounds[3])

                         # Order by depth (farther objects first for back-to-front rendering)
                         # but preserve vertex order within groups for proper polygon construction
                         if ("group" %in% names(result)) {
                               # Calculate representative depth for each group
                               group_depths <- aggregate(result$depth, by = list(group = result$group), FUN = mean)
                               names(group_depths) <- c("group", "group_depth")
                               result <- merge(result, group_depths, by = "group")

                               # Sort by group depth, then by group, then preserve order within group
                               if ("order" %in% names(result)) {
                                     result <- result[order(-result$group_depth, result$group, result$order), ]
                               } else {
                                     result <- result[order(-result$group_depth, result$group), ]
                               }

                               # Clean up temporary column
                               result$group_depth <- NULL
                         } else {
                               result <- result[order(-result$depth), ]
                         }

                         return(result)
                   },

                   render_fg = function(self, panel_params, theme) {
                         cat("render_fg called\n")
                         # Return empty grob to disable standard panel border
                         # This prevents theme_bw(), theme_light(), etc. from drawing rectangular borders
                         # grid::nullGrob() # original disabling version
                         render_cube(self, panel_params, theme, layer = "foreground")
                   },

                   # Disable standard grids
                   render_grid = function(self, panel_params, theme) {
                         cat("render_grid called\n")
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


# Helper function to apply ggplot2-style expansion and generate breaks
apply_axis_expansion <- function(original_range, scale_obj, axis_name) {

      # Detect if this is a discrete scale
      is_discrete_scale <- inherits(scale_obj, "ScaleDiscrete") ||
            inherits(scale_obj, "ScaleDiscretePosition") ||
            any(grepl("discrete", class(scale_obj), ignore.case = TRUE))

      if (is_discrete_scale) {
            # Handle discrete scales
            return(apply_discrete_axis_expansion(original_range, scale_obj, axis_name))
      } else {
            # Handle continuous scales (existing logic)
            return(apply_continuous_axis_expansion(original_range, scale_obj, axis_name))
      }
}

# Helper function for discrete scale expansion
apply_discrete_axis_expansion <- function(original_range, scale_obj, axis_name) {

      # Get breaks and labels from the scale object
      breaks_raw <- scale_obj$get_breaks()
      labels_raw <- scale_obj$get_labels() %||% as.character(breaks_raw)

      if (is.null(breaks_raw) || length(breaks_raw) == 0) {
            # If no breaks, try to use original range
            if (!is.null(original_range) && length(original_range) == 2) {
                  breaks_raw <- seq(original_range[1], original_range[2])
                  labels_raw <- as.character(breaks_raw)
            } else {
                  # Fallback
                  breaks_raw <- c(1, 2)
                  labels_raw <- c("1", "2")
            }
      }

      # Check if limits were explicitly set on the scale
      scale_limits <- scale_obj$limits
      if (!is.null(scale_limits) && length(scale_limits) >= 2 && !all(is.na(scale_limits))) {
            # User set explicit limits - use them directly
            limits <- range(match(scale_limits, breaks_raw), na.rm = TRUE)
            if (any(is.na(limits))) {
                  # Fallback if matching fails
                  limits <- c(1, length(breaks_raw))
            }
      } else {
            # Apply discrete expansion (typically ±0.6 units)
            expand_amount <- 0.6  # Standard discrete expansion

            # Get expansion from scale if available
            if (!inherits(scale_obj$expand, "waiver")) {
                  expand_amount <- scale_obj$expand[1] %||% 0.6
            }

            # Calculate limits with expansion
            break_range <- range(seq_along(breaks_raw))
            limits <- c(break_range[1] - expand_amount, break_range[2] + expand_amount)
      }

      return(list(
            limits = limits,
            breaks = seq_along(breaks_raw),  # Numeric positions
            labels = labels_raw,
            original_range = original_range
      ))
}

# Helper function for continuous scale expansion (extracted from existing logic)
apply_continuous_axis_expansion <- function(original_range, scale_obj, axis_name) {

      if (is.null(original_range)) {
            # Fallback to scale limits if no original range
            limits <- scale_obj$dimension()
            breaks_raw <- scale_obj$get_breaks()
            labels_raw <- scale_obj$get_labels() %||% as.character(breaks_raw)

            # Filter to limits
            keep_indices <- which(breaks_raw >= limits[1] & breaks_raw <= limits[2])

            return(list(
                  limits = limits,
                  breaks = breaks_raw[keep_indices],
                  labels = labels_raw[keep_indices]
            ))
      }

      # Check if limits were explicitly set on the scale
      scale_limits <- scale_obj$limits
      if (!is.null(scale_limits) && length(scale_limits) == 2 && all(!is.na(scale_limits))) {
            # User set explicit limits - use them directly
            limits <- scale_limits
            breaks_raw <- scale_obj$get_breaks()
            labels_raw <- scale_obj$get_labels() %||% as.character(breaks_raw)

            # Filter to limits
            keep_indices <- which(breaks_raw >= limits[1] & breaks_raw <= limits[2])

            return(list(
                  limits = limits,
                  breaks = breaks_raw[keep_indices],
                  labels = labels_raw[keep_indices]
            ))
      }

      # Check for explicit breaks (even without explicit limits)
      breaks_raw <- scale_obj$get_breaks()
      labels_raw <- scale_obj$get_labels() %||% as.character(breaks_raw)

      # Check if breaks look like they were explicitly set (not auto-generated)
      # This is a bit heuristic, but we check if breaks are substantially different from auto-generated ones
      auto_breaks <- scales::extended_breaks(n = 5)(original_range)
      breaks_look_explicit <- !isTRUE(all.equal(sort(breaks_raw), sort(auto_breaks), tolerance = 1e-10))

      if (breaks_look_explicit) {

            # Use explicit breaks and create limits around them
            breaks_range <- range(breaks_raw)
            breaks_padding <- diff(breaks_range) * 0.1  # 10% padding
            if (breaks_padding == 0) breaks_padding <- 0.1  # Handle single break case

            final_limits <- c(breaks_range[1] - breaks_padding, breaks_range[2] + breaks_padding)

            return(list(
                  limits = final_limits,
                  breaks = breaks_raw,
                  labels = labels_raw,
                  original_range = original_range
            ))
      }

      # Get expansion from scale, with fallback to ggplot2 default
      if (inherits(scale_obj$expand, "waiver")) {
            expand_factor <- 0.05  # Default 5% expansion
      } else {
            expand_factor <- scale_obj$expand[1]  # Use user-specified expansion
      }

      if (expand_factor == 0) {
            # No expansion
            expanded_range <- original_range
      } else {
            range_width <- diff(original_range)
            if (range_width > 0) {
                  expanded_min <- original_range[1] - range_width * expand_factor
                  expanded_max <- original_range[2] + range_width * expand_factor
                  expanded_range <- c(expanded_min, expanded_max)
            } else {
                  expanded_range <- original_range[1] + c(-0.1, 0.1)
            }
      }

      # Generate nice breaks for the expanded range
      breaks <- scales::extended_breaks(n = 5)(expanded_range)

      # Final limits encompass all breaks (like ggplot2)
      final_limits <- range(c(breaks, expanded_range))

      # Generate labels (use scale's label function if available)
      labels_func <- scale_obj$labels
      if (is.function(labels_func)) {
            labels <- labels_func(breaks)
      } else if (!is.null(labels_func) && !inherits(labels_func, "waiver")) {
            labels <- as.character(labels_func)
            # Ensure length matches breaks
            if (length(labels) != length(breaks)) {
                  labels <- rep_len(labels, length(breaks))
            }
      } else {
            labels <- as.character(breaks)
      }

      return(list(
            limits = final_limits,
            breaks = breaks,
            labels = labels,
            original_range = original_range
      ))
}

# Helper function to compute plot bounds
calculate_plot_bounds <- function(all_bounds_x, all_bounds_y){
      x_bounds <- range(all_bounds_x, na.rm = TRUE)
      y_bounds <- range(all_bounds_y, na.rm = TRUE)

      # Add padding
      x_padding <- diff(x_bounds) * 0.05
      y_padding <- diff(y_bounds) * 0.05
      x_bounds <- c(x_bounds[1] - x_padding, x_bounds[2] + x_padding)
      y_bounds <- c(y_bounds[1] - y_padding, y_bounds[2] + y_padding)

      # Store the natural aspect ratio for later use
      bounds_aspect <<- diff(y_bounds) / diff(x_bounds)

      # Use proportional bounds
      c(x_bounds[1], x_bounds[2], y_bounds[1], y_bounds[2])
}

# Helper function to detect if a range came from categorical data
is_categorical_range <- function(range_vals) {
      if (is.null(range_vals) || length(range_vals) != 2) {
            return(FALSE)
      }

      # Check for the categorical marker attribute
      if (!is.null(attr(range_vals, "categorical"))) {
            return(attr(range_vals, "categorical"))
      }

      # Fallback: check if the range represents integer positions from categorical conversion
      # (values like c(1, 3) or c(1, 5) from factor levels)
      is_integer_like <- all(range_vals == round(range_vals))
      starts_at_one <- range_vals[1] == 1
      reasonable_range <- diff(range_vals) <= 100  # Sanity check

      return(is_integer_like && starts_at_one && reasonable_range)
}

# Helper function to handle discrete Z scales
handle_discrete_z_scale <- function(original_z_range) {

      # Get parameters from cache
      z_limits <- .z_scale_cache$limits %||% NULL
      z_breaks <- .z_scale_cache$breaks %||% waiver()
      z_labels_param <- .z_scale_cache$labels %||% waiver()
      z_expand <- .z_scale_cache$expand %||% waiver()

      # Determine the levels/categories
      if (!is.null(z_limits)) {
            # Explicit limits provided
            levels_list <- z_limits
      } else if (!is.null(original_z_range) && is_categorical_range(original_z_range)) {
            # Auto-detect from categorical range (c(1, n_levels))
            n_levels <- original_z_range[2]

            # Try to get actual factor levels from the original data
            # This is a bit hacky, but we need to extract the actual level names
            tryCatch({
                  # Look up the call stack to find the original data
                  for (i in 1:15) {
                        env <- parent.frame(i)
                        if (exists("plot", envir = env)) {
                              plot_obj <- get("plot", envir = env)
                              if (inherits(plot_obj, "ggplot") && !is.null(plot_obj$data)) {
                                    aesthetic_vars <- extract_aesthetic_vars(plot_obj)
                                    z_vars <- aesthetic_vars$z

                                    if (length(z_vars) > 0) {
                                          existing_z_vars <- z_vars[z_vars %in% names(plot_obj$data)]
                                          if (length(existing_z_vars) > 0) {
                                                z_values <- unlist(plot_obj$data[existing_z_vars], use.names = FALSE)
                                                if (is.factor(z_values)) {
                                                      levels_list <- levels(z_values)
                                                      break
                                                } else if (is.character(z_values)) {
                                                      levels_list <- levels(factor(z_values))
                                                      break
                                                }
                                          }
                                    }
                              }
                        }
                  }

                  # If we couldn't find actual levels, use numeric defaults
                  if (!exists("levels_list")) {
                        levels_list <- as.character(seq_len(n_levels))
                  }
            }, error = function(e) {
                  # Fallback to numeric levels
                  levels_list <- as.character(seq_len(n_levels))
            })
      } else {
            # Fallback
            levels_list <- c("1", "2")
      }

      n_levels <- length(levels_list)

      # Generate breaks (integer positions)
      if (inherits(z_breaks, "waiver") || is.null(z_breaks)) {
            breaks_positions <- seq_len(n_levels)
      } else if (is.character(z_breaks)) {
            # Map break names to positions
            breaks_positions <- match(z_breaks, levels_list)
            breaks_positions <- breaks_positions[!is.na(breaks_positions)]
            if (length(breaks_positions) == 0) breaks_positions <- seq_len(n_levels)
      } else {
            # Assume numeric positions
            breaks_positions <- z_breaks
      }

      # Generate labels
      if (inherits(z_labels_param, "waiver") || is.null(z_labels_param)) {
            labels_final <- levels_list[breaks_positions]
      } else if (is.function(z_labels_param)) {
            labels_final <- z_labels_param(levels_list[breaks_positions])
      } else {
            labels_final <- as.character(z_labels_param)
            if (length(labels_final) != length(breaks_positions)) {
                  labels_final <- rep_len(labels_final, length(breaks_positions))
            }
      }

      # Apply discrete expansion
      expand_amount <- 0.6  # Standard discrete expansion
      if (!inherits(z_expand, "waiver") && !is.null(z_expand)) {
            expand_amount <- z_expand[1] %||% 0.6
      }

      # Calculate limits with expansion
      final_limits <- c(1 - expand_amount, n_levels + expand_amount)

      return(list(
            limits = final_limits,
            breaks = breaks_positions,
            labels = labels_final,
            original_range = original_z_range,
            levels = levels_list  # Store for data conversion
      ))
}

# Helper function to handle continuous Z scales (extracted existing logic)
handle_continuous_z_scale <- function(original_z_range) {

      z_limits <- .z_scale_cache$limits
      z_breaks <- .z_scale_cache$breaks

      # Check if we have actual limits (not NULL)
      if (!is.null(z_limits) && length(z_limits) == 2) {
            # We have explicit limits from scale_z_continuous(limits = c(...))

            # Handle breaks
            if (is.function(z_breaks)) {
                  z_breaks <- z_breaks(z_limits)
            } else if (inherits(z_breaks, "waiver") || is.null(z_breaks)) {
                  n_breaks_val <- .z_scale_cache$n.breaks %||% 5
                  z_breaks <- scales::extended_breaks(n = n_breaks_val)(z_limits)
            }

            # Filter breaks to be within limits
            filtered_breaks <- z_breaks[z_breaks >= z_limits[1] & z_breaks <= z_limits[2]]

            # Process labels
            z_labels_param <- .z_scale_cache$labels %||% waiver()

            # Apply label transformation
            if (is.null(z_labels_param) || inherits(z_labels_param, "waiver")) {
                  # Use default labels (the break values themselves)
                  z_labels <- as.character(filtered_breaks)
            } else if (is.function(z_labels_param)) {
                  # Apply label function to breaks
                  z_labels <- z_labels_param(filtered_breaks)
            } else {
                  # Use provided labels directly
                  z_labels <- as.character(z_labels_param)
                  # Ensure length matches breaks
                  if (length(z_labels) != length(filtered_breaks)) {
                        warning("Length of labels (", length(z_labels), ") does not match length of breaks (", length(z_breaks), ")")
                        z_labels <- rep_len(z_labels, length(filtered_breaks))
                  }
            }

            return(list(limits = z_limits, breaks = filtered_breaks, labels = z_labels))

      } else {
            # No explicit limits - check if we have explicit breaks
            z_labels_param <- .z_scale_cache$labels %||% waiver()

            # Check for explicit breaks (not waiver, not NULL, not function)
            has_explicit_breaks <- !is.null(z_breaks) &&
                  !inherits(z_breaks, "waiver") &&
                  !is.function(z_breaks) &&
                  is.numeric(z_breaks)

            if (has_explicit_breaks) {

                  # Use explicit breaks and create limits around them
                  z_range <- range(z_breaks)
                  z_padding <- diff(z_range) * 0.1  # 10% padding
                  if (z_padding == 0) z_padding <- 0.1  # Handle single break case

                  final_limits <- c(z_range[1] - z_padding, z_range[2] + z_padding)

                  # Process labels for explicit breaks
                  if (is.null(z_labels_param) || inherits(z_labels_param, "waiver")) {
                        z_labels <- as.character(z_breaks)
                  } else if (is.function(z_labels_param)) {
                        z_labels <- z_labels_param(z_breaks)
                  } else {
                        z_labels <- as.character(z_labels_param)
                        # Ensure length matches breaks
                        if (length(z_labels) != length(z_breaks)) {
                              warning("Length of labels (", length(z_labels), ") does not match length of breaks (", length(z_breaks), ")")
                              z_labels <- rep_len(z_labels, length(z_breaks))
                        }
                  }

                  return(list(limits = final_limits, breaks = z_breaks, labels = z_labels))

            } else {
                  # scale_z_continuous() called without limits or explicit breaks - auto-detect with expansion
                  if (!is.null(original_z_range)) {

                        # Get expansion from z scale if available
                        if (exists("expand", envir = .z_scale_cache) && !inherits(.z_scale_cache$expand, "waiver")) {
                              expand_factor <- .z_scale_cache$expand[1]
                        } else {
                              expand_factor <- 0.05  # Default
                        }

                        if (expand_factor == 0) {
                              # No expansion
                              expanded_range <- original_z_range
                        } else {
                              # Apply expansion
                              range_width <- diff(original_z_range)
                              if (range_width > 0) {
                                    expanded_range <- c(original_z_range[1] - range_width * expand_factor,
                                                        original_z_range[2] + range_width * expand_factor)
                              } else {
                                    expanded_range <- original_z_range[1] + c(-0.1, 0.1)
                              }
                        }

                        # Generate nice breaks for the expanded range
                        z_breaks <- scales::extended_breaks(n = 5)(expanded_range)

                        # Final limits encompass all breaks (like ggplot2)
                        final_limits <- range(c(z_breaks, expanded_range))

                        # Process labels for ALL breaks
                        if (is.null(z_labels_param) || inherits(z_labels_param, "waiver")) {
                              z_labels <- as.character(z_breaks)
                        } else if (is.function(z_labels_param)) {
                              # Apply label function to all breaks
                              z_labels <- z_labels_param(z_breaks)
                        } else {
                              # Use provided labels directly
                              z_labels <- as.character(z_labels_param)
                              if (length(z_labels) != length(z_breaks)) {
                                    warning("Length of labels (", length(z_labels), ") does not match length of breaks (", length(z_breaks), ")")
                                    z_labels <- rep_len(z_labels, length(z_breaks))
                              }
                        }

                        return(list(
                              limits = final_limits,           # Expanded range for coordinate system
                              breaks = z_breaks,
                              labels = z_labels,
                              original_range = original_z_range  # Store original data range for transform
                        ))

                  } else {
                        return(list(limits = c(0, 1), breaks = scales::extended_breaks()(c(0, 1))))
                  }
            }
      }
}
