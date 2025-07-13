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
#' # Use `scales` and `ratio` to modify aspect ratio
#' p <- ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point()
#' p + coord_3d() # Default free scales (maximum visual range)
#' p + coord_3d(scales = "fixed") # Fixed scales (proportions match data scales, like coord_fixed)
#' p + coord_3d(scales = "free", ratio = c(1, 2, 1)) # Custom cube ratios (make z twice as tall visually)
#' p + coord_3d(scales = "fixed", ratio = c(1, 2, 1)) # Custom scale ratios (y gets twice the visual space relative to its scale range)
#'
#'
#' @export
coord_3d <- function(pitch = 0, roll = 120, yaw = 30,
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

#' Extract variable names from aesthetic mappings (NOT CURRENTLY USED)
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

                         # Get standard panel params from parent
                         panel_params <- ggproto_parent(CoordCartesian, self)$setup_panel_params(scale_x, scale_y, params)

                         # Workaround to train and recover the z scale object
                         train_z_scale()
                         scale_z <- .z_scale_cache$scale

                         # Scale info
                         panel_params$scales <- self$scales
                         panel_params$scale_info <- list(
                               x = get_scale_info(scale_x, expand = self$expand),
                               y = get_scale_info(scale_y, expand = self$expand),
                               z = get_scale_info(scale_z, expand = self$expand)
                         )

                         # Blank out native scale names
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

                         # Visible faces
                         visible_faces_fgbg <- select_visible_faces(self$faces, panel_params$proj, effective_ratios)
                         visible_faces <- do.call("c", visible_faces_fgbg)
                         panel_params$visible_faces <- visible_faces
                         panel_params$visible_faces_fg <- visible_faces_fgbg$fg
                         panel_params$visible_faces_bg <- visible_faces_fgbg$bg

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
                                     panel_params$grid_transformed$break_pos <- selected_grid$break_pos
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

                         # Apply 3D transformation (returns x, y, z, depth, depth_scale)
                         transformed <- transform_3d_standard(data_std, panel_params$proj)

                         # Store transformed coordinates
                         data$x <- transformed$x
                         data$y <- transformed$y
                         data$z <- transformed$z      # Keep for face visibility calculations
                         data$depth <- transformed$depth  # Use for depth sorting
                         data$depth_scale <- transformed$depth_scale  # Use for size scaling

                         # Apply final coordinate transformation to fit plot bounds [0, 1]
                         result <- data
                         result$x <- (data$x - panel_params$plot_bounds[1]) / (panel_params$plot_bounds[2] - panel_params$plot_bounds[1])
                         result$y <- (data$y - panel_params$plot_bounds[3]) / (panel_params$plot_bounds[4] - panel_params$plot_bounds[3])

                         # Depth sorting with hierarchical support
                         if ("group" %in% names(result) && grepl("__", result$group[1], fixed = TRUE)) {
                               # Hierarchical depth sorting
                               # Split groups into hierarchy levels
                               group_parts <- strsplit(result$group, "__", fixed = TRUE)
                               n_levels <- max(lengths(group_parts))

                               # Create temporary data frame for sorting calculations
                               sort_df <- data.frame(
                                     row_id = 1:nrow(result),
                                     depth = result$depth,
                                     stringsAsFactors = FALSE
                               )

                               # Add hierarchy level columns
                               for (i in 1:n_levels) {
                                     level_col <- paste0("level_", i)
                                     sort_df[[level_col]] <- sapply(group_parts, function(x) {
                                           if(length(x) >= i) x[i] else NA_character_
                                     })
                               }

                               # Calculate summary depth for each hierarchy level
                               for (i in 1:n_levels) {
                                     level_col <- paste0("level_", i)
                                     depth_col <- paste0("depth_", i)

                                     fun <- switch(i, "1" = max, "2" = mean, mean)

                                     # Calculate depth for each unique value at this level
                                     level_values <- sort_df[[level_col]]
                                     unique_values <- unique(level_values[!is.na(level_values)])
                                     sort_df[[depth_col]] <- NA_real_
                                     for (level_val in unique_values) {
                                           mask <- !is.na(level_values) & level_values == level_val
                                           sort_df[[depth_col]][mask] <- fun(sort_df$depth[mask])
                                     }
                               }

                               # Build sorting arguments (deeper = farther = sort first)
                               depth_cols <- paste0("depth_", 1:n_levels)
                               sort_args <- list()
                               for (col in depth_cols) {
                                     if (col %in% names(sort_df)) {
                                           sort_args[[length(sort_args) + 1]] <- -sort_df[[col]]  # Negative for back-to-front
                                     }
                               }

                               # Sort hierarchically, preserving row order within groups
                               if (length(sort_args) > 0) {
                                     sort_order <- do.call(order, sort_args)
                                     result <- result[sort_order, ]
                               }

                         } else {
                               # Individual vertex sorting (ignore groups)
                               result <- result[order(-result$depth), ]
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

get_scale_info <- function(scale_obj, expand = TRUE) {
      expansion <- ggplot2:::default_expansion(scale_obj, expand = expand)
      limits <- scale_obj$get_limits()

      # Pass NULL for coord_limits since coord_3d doesn't support them yet
      expanded_range <- ggplot2:::expand_limits_scale(scale_obj, expansion, limits, coord_limits = NULL)

      list(limits = expanded_range,
           breaks = scale_obj$get_breaks(),
           labels = scale_obj$get_labels())
}
