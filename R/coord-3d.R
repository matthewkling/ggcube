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
#' @param auto_text_orientation Logical indicating whether axis text should automatically
#'   rotate to align with the projected axis directions. When \code{FALSE}, uses theme
#'   text angle settings.
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
#' @param xtext,ytext,ztext Character strings or length-2 character vectors specifying
#'   axis label placement. Use \code{"auto"} for automatic selection, or specify
#'   \code{c("face1", "face2")} to place labels on the edge shared by those faces.
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
                     auto_text_orientation = TRUE,
                     scales = "free",
                     ratio = c(1, 1, 1),
                     xtext = "auto", ytext = "auto", ztext = "auto") {

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
                    auto_text_orientation = auto_text_orientation,
                    scales = scales,
                    ratio = ratio,
                    xtext = xtext, ytext = ytext, ztext = ztext
            ),
            theme(plot.margin = margin(20, 20, 20, 20, "pt"))
      )
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
                   auto_text_orientation = TRUE,
                   scales = "free",
                   ratio = c(1, 1, 1),

                   plot_bounds = c(0, 1, 0, 1),  # [xmin, xmax, ymin, ymax]

                   setup_panel_params = function(self, scale_x, scale_y, params = list()) {

                         # Capture original x, y, z ranges before ggplot2 normalizes them
                         original_x_range <- NULL
                         original_y_range <- NULL
                         original_z_range <- NULL

                         ## fixme -- ugly and inefficent
                         tryCatch({
                               # Look for the original plot data in parent environments
                               for (i in 1:10) {
                                     env <- parent.frame(i)
                                     if (exists("plot", envir = env)) {
                                           plot_obj <- get("plot", envir = env)
                                           if (inherits(plot_obj, "ggplot")) {
                                                 if (!is.null(plot_obj$data)) {
                                                       if ("x" %in% names(plot_obj$data)) {
                                                             original_x_range <- range(plot_obj$data$x, na.rm = TRUE)
                                                       }
                                                       if ("y" %in% names(plot_obj$data)) {
                                                             original_y_range <- range(plot_obj$data$y, na.rm = TRUE)
                                                       }
                                                       if ("z" %in% names(plot_obj$data)) {
                                                             original_z_range <- range(plot_obj$data$z, na.rm = TRUE)
                                                       }
                                                       break
                                                 }
                                           }
                                     }
                               }
                         }, error = function(e) {
                               # Ignore errors - we'll use defaults if this fails
                         })

                         # Helper function to apply ggplot2-style expansion and generate breaks
                         apply_axis_expansion <- function(original_range, scale_obj, axis_name) {

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

                         # Apply expansion to x and y axes
                         x_info <- apply_axis_expansion(original_x_range, scale_x, "x")
                         y_info <- apply_axis_expansion(original_y_range, scale_y, "y")

                         # Handle z-axis (keep existing logic but use helper function)
                         z_info <- list(limits = c(0, 1), breaks = scales::extended_breaks()(c(0, 1)))  # Default
                         z_auto_detect <- FALSE

                         if (exists("limits", envir = .z_scale_cache)) {
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
                                     z_labels_param <- NULL
                                     if (exists("labels", envir = .z_scale_cache)) {
                                           z_labels_param <- .z_scale_cache$labels
                                     }

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

                                     z_info <- list(limits = z_limits, breaks = filtered_breaks, labels = z_labels)

                               } else {
                                     # No explicit limits - check if we have explicit breaks
                                     z_labels_param <- NULL
                                     if (exists("labels", envir = .z_scale_cache)) {
                                           z_labels_param <- .z_scale_cache$labels
                                     }

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

                                           z_info <- list(limits = final_limits, breaks = z_breaks, labels = z_labels)

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

                                                 z_info <- list(
                                                       limits = final_limits,           # Expanded range for coordinate system
                                                       breaks = z_breaks,
                                                       labels = z_labels,
                                                       original_range = original_z_range  # Store original data range for transform
                                                 )

                                           } else {
                                                 z_info <- list(limits = c(0, 1), breaks = scales::extended_breaks()(c(0, 1)))
                                           }
                                           z_auto_detect <- TRUE
                                     }
                               }
                         } else {
                               # No cached z scale info at all - auto-detect with expansion
                               if (!is.null(original_z_range)) {
                                     # Apply same expansion logic
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
                                           limits = final_limits,           # Expanded range for coordinate system
                                           breaks = z_breaks,
                                           original_range = original_z_range  # Store original data range for transform
                                     )
                               } else {
                                     z_info <- list(limits = c(0, 1), breaks = scales::extended_breaks()(c(0, 1)))
                               }
                               z_auto_detect <- TRUE
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
                         render_cube(self, panel_params, theme, layer = "background")
                   },

                   transform = function(self, data, panel_params) {
                         # Add z column if missing
                         if (!"z" %in% names(data)) {
                               data$z <- 0
                         }

                         # Handle z-value scaling
                         if (panel_params$z_auto_detect %||% FALSE) {
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
                         } else {
                               if ("z" %in% names(data)) {
                                     z_limits <- panel_params$scale_info$z$limits
                                     if (!is.null(z_limits) && length(z_limits) == 2) {
                                           scale_range <- z_limits[2] - z_limits[1]
                                           data$z <- data$z * scale_range + z_limits[1]
                                     }
                               }
                         }

                         # Use scale limits for consistent scaling (includes expansion)
                         scale_ranges <- list(
                               x = panel_params$scale_info$x$limits,
                               y = panel_params$scale_info$y$limits,
                               z = panel_params$scale_info$z$limits
                         )

                         # Scale data to standard domain with aspect ratio
                         data_std <- scale_to_standard(
                               data[c("x", "y", "z")],
                               scale_ranges,
                               panel_params$scales,
                               panel_params$ratio
                         )

                         # Apply 3D transformation in standard domain
                         transformed <- transform_3d_standard(data_std, panel_params$proj)

                         # Store transformed coordinates
                         data$x <- transformed$x
                         data$y <- transformed$y
                         data$z_proj <- transformed$z  # For depth sorting

                         # Apply final coordinate transformation to fit plot bounds [0, 1]
                         result <- data
                         result$x <- (data$x - panel_params$plot_bounds[1]) / (panel_params$plot_bounds[2] - panel_params$plot_bounds[1])
                         result$y <- (data$y - panel_params$plot_bounds[3]) / (panel_params$plot_bounds[4] - panel_params$plot_bounds[3])

                         # Order by depth
                         if ("z_proj" %in% names(result)) {
                               if ("group" %in% names(result)) {
                                     result <- result[order(result$group, -result$z_proj), ]
                               } else {
                                     result <- result[order(-result$z_proj), ]
                               }
                         }

                         return(result)
                   },

                   render_fg = function(self, panel_params, theme) {
                         # Return empty grob to disable standard panel border
                         # This prevents theme_bw(), theme_light(), etc. from drawing rectangular borders
                         # grid::nullGrob() # original disabling version
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

