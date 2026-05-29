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
#' @param reverse_shade Logical. If TRUE, reverses the lighting gradient direction. By default,
#'   shadows are placed on the left, or on the bottom for horizontal colorbars.
#' @param shade_range Length-2 numeric vector in the range -1 to 1, giving the limits
#'   of the shading gradient. -1 is full shade, and 1 is full highlight. Default is `c(-.5, .5)`.
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
#' @noRd
extract_light_from_plot <- function() {
      # Walk up the call stack to find the plot object
      for (i in 1:20) {
            tryCatch({
                  env <- parent.frame(i)

                  if (exists("plot", envir = env)) {
                        plot_obj <- get("plot", envir = env)
                        if (inherits(plot_obj, "ggplot")) {

                              # Look for layers with lighting (existing behavior)
                              lighting_layers <- list()
                              for (j in seq_along(plot_obj$layers)) {
                                    layer <- plot_obj$layers[[j]]

                                    if ("light" %in% names(layer$stat_params)) {
                                          lighting_layers[[length(lighting_layers) + 1]] <- layer$stat_params$light
                                    }
                              }

                              # If layer lighting found, use it (maintains precedence)
                              if (length(lighting_layers) > 0) {
                                    if (length(lighting_layers) == 1) {
                                          return(lighting_layers[[1]])
                                    } else {
                                          warning("Multiple layers with different lighting found. Using first layer's lighting.")
                                          return(lighting_layers[[1]])
                                    }
                              }

                              # No layer lighting found - check coord for lighting
                              if (!is.null(plot_obj$coordinates) && "light" %in% names(plot_obj$coordinates)) {
                                    coord_light <- plot_obj$coordinates$light
                                    if (!is.null(coord_light)) {
                                          return(coord_light)
                                    }
                              }

                              # No lighting found anywhere
                              return(NULL)
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
#' @noRd
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
#' @noRd
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

#' Replace legend key colors with shading gradients
#'
#' Locates the colored key cells structurally rather than by matching a
#' specific gtable layout-name scheme, so it works across ggplot2 versions
#' whose internal naming and nesting differ. In ggplot2 >= 4.0 the legend is
#' wrapped in an outer "guide-box" gtable and each key cell is a gTree
#' containing both a background rect and the layer's drawn glyph rect; in
#' earlier versions the legend gtable is passed directly and key cells are
#' bare rects.
#'
#' @param grob Original legend grob
#' @param grad Matrix of gradient colors
#' @return Modified grob with gradients
#' @keywords internal
#' @noRd
replace_legend_colors <- function(grob, grad) {
      if (!inherits(grob, "gtable")) {
            return(grob)
      }

      # In ggplot2 >= 4.0 the legend gtable is nested inside an outer
      # "guide-box" wrapper. Descend into any child gtables that themselves
      # contain key cells, operate on them, and write them back.
      key_cells_here <- find_legend_key_cells(grob)
      if (length(key_cells_here) == 0) {
            modified <- FALSE
            for (i in seq_along(grob$grobs)) {
                  child <- grob$grobs[[i]]
                  if (inherits(child, "gtable") &&
                      length(find_legend_key_cells(child)) > 0) {
                        grob$grobs[[i]] <- replace_legend_colors(child, grad)
                        modified <- TRUE
                  }
            }
            return(grob)
      }

      # Order key cells by grid position (top-to-bottom, then left-to-right)
      # so cell i corresponds to row i of the gradient matrix, matching the
      # base-colour order regardless of layout naming.
      ord <- order(grob$layout$t[key_cells_here], grob$layout$l[key_cells_here])
      key_cells <- key_cells_here[ord]

      n <- min(length(key_cells), nrow(grad))
      for (i in seq_len(n)) {
            idx <- key_cells[i]
            grob$grobs[[idx]] <- replace_key_glyph(grob$grobs[[idx]], grad[i, ])
      }

      return(grob)
}

#' Identify the colored key cells within a legend gtable
#'
#' @param grob A gtable
#' @return Integer indices into grob$grobs / grob$layout for key cells
#' @keywords internal
#' @noRd
find_legend_key_cells <- function(grob) {
      if (!inherits(grob, "gtable") || is.null(grob$layout$name)) {
            return(integer(0))
      }
      nm <- grob$layout$name
      # ggplot2 >= 4.0: "key-1-1-bg"; ggplot2 3.5.x: "key-1-1-1" etc.
      grep("^key-\\d+-\\d+(-bg|-\\d+)?$", nm)
}

#' Replace the layer glyph in a single legend key cell with a gradient raster
#'
#' Handles both a bare rect cell (older ggplot2) and a gTree cell containing a
#' background rect plus the layer's drawn glyph (ggplot2 >= 4.0). For the gTree
#' case the layer glyph is identified via childrenOrder, where the background
#' carries an empty name and the layer glyph is named after its layer.
#'
#' @param cell A grob (rect or gTree) for one legend key
#' @param gradient_colors Character vector of colors for this key
#' @return Modified grob
#' @keywords internal
#' @noRd
replace_key_glyph <- function(cell, gradient_colors) {

      # Bare rect cell: replace directly.
      if (inherits(cell, "rect")) {
            return(legend_key_raster_grob(cell, gradient_colors))
      }

      # gTree cell: replace the layer glyph rect among the children.
      if (inherits(cell, "gTree") && length(cell$children) > 0) {
            child_names <- names(cell$children)
            order_names <- names(cell$childrenOrder)

            # The layer glyph is the child whose childrenOrder name is non-empty
            # (the key background's name is ""). Fall back to the last rect child.
            glyph_name <- NULL
            if (!is.null(order_names)) {
                  named <- cell$childrenOrder[nzchar(order_names)]
                  if (length(named) > 0) {
                        glyph_name <- unname(named[length(named)])
                  }
            }

            rect_children <- child_names[vapply(
                  cell$children, function(g) inherits(g, "rect"), logical(1)
            )]

            if (is.null(glyph_name) || !(glyph_name %in% child_names) ||
                !inherits(cell$children[[glyph_name]], "rect")) {
                  if (length(rect_children) == 0) return(cell)
                  glyph_name <- rect_children[length(rect_children)]
            }

            ref_rect <- cell$children[[glyph_name]]
            cell$children[[glyph_name]] <- legend_key_raster_grob(ref_rect, gradient_colors)
            return(cell)
      }

      return(cell)
}

#' Build a gradient raster grob matching a reference rect's geometry
#'
#' @param rect_grob Reference rect grob whose position/size/just are reused
#' @param gradient_colors Character vector of colors (shadow to highlight)
#' @return A rasterGrob
#' @keywords internal
#' @noRd
legend_key_raster_grob <- function(rect_grob, gradient_colors) {

      x <- rect_grob$x %||% grid::unit(0.5, "npc")
      y <- rect_grob$y %||% grid::unit(0.5, "npc")
      width <- rect_grob$width %||% grid::unit(1, "npc")
      height <- rect_grob$height %||% grid::unit(1, "npc")
      just <- rect_grob$just %||% c(0.5, 0.5)

      # Horizontal gradient raster (left to right = shadow to highlight)
      n_steps <- length(gradient_colors)
      raster_matrix <- matrix(gradient_colors, nrow = 1, ncol = n_steps)
      raster_obj <- as.raster(raster_matrix)

      grid::rasterGrob(
            image = raster_obj,
            x = x,
            y = y,
            width = width,
            height = height,
            just = just
      )
}
