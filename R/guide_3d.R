#' Color guides showing lighting effects
#'
#' Creates color guides that show shading variation as gradients
#' within each color. Shows the full range of colors visible when
#' shading is enabled in 3D plots.
#'
#' When fill and color aesthetics map to the same variable (e.g.,
#' \code{aes(fill = z, color = z)}), ggplot2 creates a shared scale with a
#' single guide. In this case, use \code{guides(fill = guide_*_3d())} to
#' apply shading, **not** \code{guides(color = guide_*_3d())}, even if your layer
#' uses the color aesthetic. Only use the color guide when color and fill
#' map to different variables and you want separate guides for each.
#'
#' @param ... Arguments passed to \code{guide_colorbar()} or \code{guide_legend()}
#' @param reverse_shade Logical. If TRUE, reverses the lighting gradient direction. By default,
#'   shadows are placed on the left, or on the bottom for horizontal colorbars.
#' @param shade_limits Length-2 numeric vector in the range -1 to 1, giving the limits
#'   of the shading gradient. -1 is full shade, and 1 is full highlight. Default is `c(-.5, 5)`.
#' @param ... Additional arguments passed to \code{guide_colorbar()} or \code{guide_legend()}.
#' @return A guide object that displays shading effects
#' @examples
#' # continuous `colorbar` guide
#' ggplot(mountain, aes(x, y, z, fill = z)) +
#'    stat_surface_3d(light = light(mode = "hsl", direction = c(1, 0, 0))) +
#'    guides(fill = guide_colorbar_3d()) +
#'    scale_fill_gradientn(colors = c("tomato", "dodgerblue")) +
#'    coord_3d()
#'
#' # discrete `legend` guide
#' ggplot(mountain, aes(x, y, z, fill = x > .5, group = 1)) +
#'    stat_surface_3d(light = light(mode = "hsl", direction = c(1, 0, 0))) +
#'    guides(fill = guide_legend_3d()) +
#'    coord_3d()
#' @name guide_3d
#' @export
guide_colorbar_3d <- function(reverse_shade = FALSE, shade_range = c(-.5, .5), ...) {
      # Create normal guide
      guide <- guide_colorbar(...)

      # Store the original draw method
      original_draw <- guide$draw

      # Override the draw method with custom version
      guide$draw <- function(theme, position = NULL, direction = NULL, params = guide$params) {

            # Get original grob
            original_grob <- original_draw(theme, position, direction, params)

            # Get lighting info
            lighting_info <- extract_light_from_plot()
            if (is.null(lighting_info) || lighting_info$shade == "neither") {
                  return(original_grob)
            }

            # Create lighting gradients
            base_colors <- params$decor[["colour"]]
            grad <- create_light_gradients(base_colors, lighting_info, shade_range)
            if(reverse_shade){
                  if(is.null(params$direction) || params$direction == "vertical"){
                        grad <- grad[, ncol(grad):1]
                  }else{
                        grad <- grad[nrow(grad):1, ]
                  }
            }

            # Replace colors in grob
            shaded_grob <- replace_colorbar_colors(original_grob, grad)
            return(shaded_grob)
      }

      guide
}

#' @rdname guide_3d
#' @export
guide_legend_3d <- function(reverse_shade = FALSE, shade_range = c(-.5, .5), ...) {
      # Create normal guide
      guide <- guide_legend(...)

      # Store the original draw method
      original_draw <- guide$draw

      # Override the draw method with custom version
      guide$draw <- function(theme, position = NULL, direction = NULL, params = guide$params) {

            # Get original grob
            original_grob <- original_draw(theme, position, direction, params)

            # Get lighting info
            lighting_info <- extract_light_from_plot()
            if (is.null(lighting_info) || lighting_info$shade == "neither") {
                  return(original_grob)
            }

            # Create lighting gradients
            base_colors <- params$key[[params$aesthetic]]
            grad <- create_light_gradients(base_colors, lighting_info, shade_range)
            if(reverse_shade){
                  if(is.null(params$direction) || params$direction == "vertical"){
                        grad <- grad[, ncol(grad):1]
                  }else{
                        grad <- grad[nrow(grad):1, ]
                  }
            }

            # Replace colors in grob
            shaded_grob <- replace_legend_colors(original_grob, grad)
            return(shaded_grob)
      }

      guide
}



#' Extract lighting information from plot layers
#'
#' @return List with lighting specification or NULL if none found
#' @keywords internal
extract_light_from_plot <- function() {

      # Walk up the call stack to find the plot object
      for (i in 1:20) {
            tryCatch({
                  env <- parent.frame(i)

                  if (exists("plot", envir = env)) {
                        plot_obj <- get("plot", envir = env)
                        if (inherits(plot_obj, "ggplot")) {

                              # Look for layers with lighting
                              lighting_layers <- list()
                              for (j in seq_along(plot_obj$layers)) {
                                    layer <- plot_obj$layers[[j]]

                                    if ("light" %in% names(layer$stat_params)) {
                                          lighting_layers[[length(lighting_layers) + 1]] <- layer$stat_params$light
                                    }
                              }

                              # Handle results
                              if (length(lighting_layers) == 0) {
                                    return(NULL)
                              } else if (length(lighting_layers) == 1) {
                                    return(lighting_layers[[1]])
                              } else {
                                    # Multiple lighting specs - warn and use first
                                    warning("Multiple layers with different lighting found. Using first layer's lighting.")
                                    return(lighting_layers[[1]])
                              }
                        }
                  }
            }, error = function(e) {
                  # Skip frames that cause errors
            })
      }

      return(NULL)
}

#' Create shading gradients for colorbar
#'
#' @param base_colors Character vector of base colors
#' @param lighting_info Lighting specification object
#' @param n_steps Number of lighting gradient steps (default 10)
#' @return Matrix of colors with lighting applied
#' @keywords internal
create_light_gradients <- function(base_colors,
                                      lighting_info,
                                      shade_range = c(-1, 1),
                                      n_steps = 20) {

      # Reverse order for proper gradient direction
      base_colors <- rev(base_colors)

      # Generate lighting values across range
      if(any(abs(shade_range) > 1)) stop("`shade_range` can't include values outside the range -1 to 1.")
      light <- seq(shade_range[1], shade_range[2], length.out = n_steps)

      # Transform to give appearance of convex surface
      light <- sin(light * pi/2)

      # Adjust range in accordance with lighting method
      if (lighting_info$method != "diffuse") {
            light <- scales::rescale(light, to = c(0, 1), from = c(-1, 1))
      }

      # Create matrix: rows = base colors, columns = lighting steps
      grad <- matrix(NA_character_, nrow = length(base_colors), ncol = n_steps)

      # Apply lighting to base colors across
      for (i in seq_along(base_colors)) {
            gradient_colors <- blend_light_with_colors(
                  rep(base_colors[i], n_steps),
                  light,
                  lighting_info
            )
            grad[i, ] <- gradient_colors
      }

      return(grad)
}

#' Replace colorbar colors with shading gradients
#'
#' @param grob Original colorbar grob
#' @param grad Matrix of gradient colors
#' @return Modified grob with gradients
#' @keywords internal
replace_colorbar_colors <- function(grob, grad) {

      # Handle gtable (grid table) structure
      if (inherits(grob, "gtable")) {

            # Look through the grobs stored in the gtable
            for (i in seq_along(grob$grobs)) {
                  subgrob <- grob$grobs[[i]]

                  # Look for rastergrob which holds the colorbar image
                  if (inherits(subgrob, "rastergrob")) {

                        original_raster <- subgrob$raster

                        # Handle orientation
                        horizontal <- diff(dim(original_raster)) > 0
                        if(horizontal){
                              original_raster <- t(original_raster)
                              # grad <- t(grad)
                        }

                        # Create a new raster with lighting gradients
                        n_colors <- nrow(grad)
                        n_light_steps <- ncol(grad)

                        # Create wider raster to show lighting gradient
                        new_width <- n_light_steps
                        new_height <- nrow(original_raster)

                        # Create the 2D lighting raster
                        new_raster <- array(NA_character_, dim = c(new_height, new_width))

                        for (row in 1:new_height) {
                              # Map row to color (top to bottom = first to last color)
                              color_idx <- ceiling(row / new_height * n_colors)
                              color_idx <- max(1, min(color_idx, n_colors))

                              for (col in 1:new_width) {
                                    # Map column to lighting step (left to right = shadow to highlight)
                                    lighting_idx <- ceiling(col / new_width * n_light_steps)
                                    lighting_idx <- max(1, min(lighting_idx, n_light_steps))

                                    new_raster[row, col] <- grad[color_idx, lighting_idx]
                              }
                        }

                        # Correct orientation
                        if(horizontal){
                              new_raster <- t(new_raster) # transpose axes
                              new_raster <- new_raster[nrow(new_raster):1, ] # move shadow to bottom
                              new_raster <- new_raster[, ncol(new_raster):1] # flip color gradient
                        }

                        # Convert to raster object and replace
                        new_raster <- as.raster(new_raster)
                        grob$grobs[[i]]$raster <- new_raster

                        break  # Only modify the first rastergrob found
                  }
            }
      }

      return(grob)
}

replace_legend_colors <- function(grob, grad) {
      if (inherits(grob, "gtable")) {

            # Find color key grobs (the actual colored rectangles, not backgrounds)
            key_indices <- grep("key-.*-.*-\\d+$", grob$layout$name)

            for (i in seq_along(key_indices)) {
                  idx <- key_indices[i]
                  rect_grob <- grob$grobs[[idx]]

                  # Get gradient for this legend item (row i of gradient_matrix)
                  gradient_colors <- grad[i, ]

                  # Create raster grob with same position/size as original rect
                  raster_grob <- create_legend_gradient_raster(rect_grob, gradient_colors)

                  # Replace rect with raster
                  grob$grobs[[idx]] <- raster_grob
            }
      }

      return(grob)
}

create_legend_gradient_raster <- function(rect_grob, gradient_colors) {

      # Extract position and size from rect grob
      x <- rect_grob$x
      y <- rect_grob$y
      width <- rect_grob$width
      height <- rect_grob$height
      just <- rect_grob$just %||% c(0.5, 0.5)

      # Create horizontal gradient raster (left to right = shadow to highlight)
      n_steps <- length(gradient_colors)
      raster_matrix <- matrix(gradient_colors, nrow = 1, ncol = n_steps)
      raster_obj <- as.raster(raster_matrix)

      # Create raster grob with same position/size as original rect
      grid::rasterGrob(
            image = raster_obj,
            x = x,
            y = y,
            width = width,
            height = height,
            just = just
      )
}
