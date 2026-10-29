#' Color guides showing lighting effects
#'
#' Creates color guides that show shading variation as gradients
#' within each color. Shows the full range of colors visible when
#' shading is enabled in 3D plots.
#'
#' When lighting applies to a layer that maps `fill` or `colour`, ggcube uses
#' these guides by default, in place of [ggplot2::guide_colourbar()] and
#' [ggplot2::guide_legend()]. Call them explicitly to change their options, or
#' use e.g. `guides(fill = "colourbar")` for an unshaded guide. Guides given to
#' a scale's `guide` argument as guide objects are left as they are.
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
#'   of the shading gradient. -1 is full shadow, 0 the colors as specified, and 1 full
#'   highlight, as set by the light's `shadow` and `highlight`. Default is `c(-.5, .5)`.
#' @param ... Additional arguments passed to \code{guide_colorbar()} or \code{guide_legend()}.
#' @return A guide object that displays shading effects
#' @examples
#' # 3D guides are used by default for lit layers
#' ggplot(mountain, aes(x, y, z, fill = z)) +
#'    stat_surface_3d() +
#'    scale_fill_gradientn(colors = c("tomato", "dodgerblue")) +
#'    coord_3d()
#'
#' # a horizontal colorbar showing the full range of shading
#' ggplot(mountain, aes(x, y, z, fill = z)) +
#'    stat_surface_3d() +
#'    guides(fill = guide_colorbar_3d(shade_range = c(-1, 1), direction = "horizontal")) +
#'    scale_fill_gradientn(colors = c("tomato", "dodgerblue")) +
#'    coord_3d() +
#'    theme(legend.position = "bottom")
#'
#' # discrete `legend` guide
#' ggplot(mountain, aes(x, y, z, fill = x > .5, group = 1)) +
#'    stat_surface_3d(light = light(direction = c(1, 0, 0))) +
#'    coord_3d()
#'
#' # an unshaded guide
#' ggplot(mountain, aes(x, y, z, fill = z)) +
#'    stat_surface_3d() +
#'    guides(fill = "colourbar") +
#'    coord_3d()
#' @name guide_3d
#' @export
guide_colorbar_3d <- function(reverse_shade = FALSE, shade_range = c(-.5, .5), ...) {
      check_shade_range(shade_range)
      shade_guide(guide_colorbar(...), GuideColourbar3D, reverse_shade, shade_range)
}

#' @rdname guide_3d
#' @export
guide_legend_3d <- function(reverse_shade = FALSE, shade_range = c(-.5, .5), ...) {
      check_shade_range(shade_range)
      shade_guide(guide_legend(...), GuideLegend3D, reverse_shade, shade_range)
}

#' Validate a 3D guide's shading range
#'
#' @param shade_range The `shade_range` argument.
#' @keywords internal
#' @noRd
check_shade_range <- function(shade_range) {
      if (!is.numeric(shade_range) || length(shade_range) != 2 || anyNA(shade_range) ||
          any(abs(shade_range) > 1)) {
            rlang::abort("`shade_range` must be two numbers in the range -1 to 1.", call = NULL)
      }
      invisible()
}

#' Make a shaded version of a ggplot2 guide
#'
#' The ggplot2 constructor processes the guide's own arguments; the result is
#' rebuilt as an instance of the shaded subclass, with the shading parameters
#' added.
#'
#' @param guide A guide made by [ggplot2::guide_colourbar()] or
#'   [ggplot2::guide_legend()].
#' @param super The shaded guide class.
#' @param reverse_shade,shade_range Shading parameters.
#' @return A guide object.
#' @keywords internal
#' @noRd
shade_guide <- function(guide, super, reverse_shade, shade_range) {
      ggproto(NULL, super,
              params = c(guide$params, list(reverse_shade = reverse_shade, shade_range = shade_range)),
              available_aes = guide$available_aes)
}

#' Shade a drawn guide's colors
#'
#' @param grob The guide drawn by its ggplot2 parent class.
#' @param base_colors The guide's colors.
#' @param params The guide's parameters.
#' @param replace Function replacing a drawn guide's colors with a gradient
#'   matrix.
#' @return The shaded guide, or `grob` unchanged if no lighting applies to
#'   the guide's aesthetic.
#' @keywords internal
#' @noRd
shade_drawn_guide <- function(grob, base_colors, params, replace) {
      lighting <- guide_lighting(params$aesthetic)
      if (is.null(lighting)) return(grob)

      grad <- create_light_gradients(base_colors, lighting, params$shade_range)
      if (isTRUE(params$reverse_shade)) {
            if (is.null(params$direction) || params$direction == "vertical") {
                  grad <- grad[, ncol(grad):1, drop = FALSE]
            } else {
                  grad <- grad[nrow(grad):1, , drop = FALSE]
            }
      }
      replace(grob, grad)
}

GuideColourbar3D <- ggproto("GuideColourbar3D", GuideColourbar,
      params = c(GuideColourbar$params, list(reverse_shade = FALSE, shade_range = c(-.5, .5))),
      draw = function(self, theme, position = NULL, direction = NULL, params = self$params) {
            grob <- ggproto_parent(GuideColourbar, self)$draw(theme, position, direction, params)
            # The colorbar's raster runs from the top, so from the last color
            shade_drawn_guide(grob, rev(params$decor[["colour"]]), params, replace_colorbar_colors)
      }
)

GuideLegend3D <- ggproto("GuideLegend3D", GuideLegend,
      params = c(GuideLegend$params, list(reverse_shade = FALSE, shade_range = c(-.5, .5))),
      draw = function(self, theme, position = NULL, direction = NULL, params = self$params) {
            grob <- ggproto_parent(GuideLegend, self)$draw(theme, position, direction, params)
            aesthetic <- intersect(c("fill", "colour"), params$aesthetic)[1]
            if (is.na(aesthetic)) return(grob)
            base_colors <- params$key[[aesthetic]]
            shade_drawn_guide(grob, base_colors, params, function(grob, grad) {
                  replace_legend_colors(grob, grad, base_colors)
            })
      }
)


# Which lighting applies to an aesthetic -----------------------------------------

#' Lighting that applies to an aesthetic
#'
#' A guide for an aesthetic is shaded if the aesthetic is mapped by a lit
#' polygon layer whose lighting targets it: `fill`, or `colour` for border
#' colors. Points and lines aren't lit, so their guides aren't shaded.
#'
#' @param plot A ggplot object.
#' @param aesthetic `"fill"` or `"colour"`.
#' @param plot_light The plot-level light as resolved for the build, or
#'   `waiver()` to resolve it from `plot`.
#' @return The applicable `light` object, from the first such layer, or
#'   `NULL`.
#' @keywords internal
#' @noRd
aesthetic_light <- function(plot, aesthetic, plot_light = waiver()) {
      if (!isTRUE(aesthetic %in% c("fill", "colour"))) return(NULL)
      if (inherits(plot_light, "waiver")) plot_light <- plot_level_light(plot)
      targets <- if (aesthetic == "fill") c("fill", "both") else c("colour", "both")

      for (layer in plot$layers) {
            geom <- layer$geom
            if (!inherits(geom, c("GeomPolygon3D", "GeomPlane3D"))) next
            if (!layer_maps_aesthetic(layer, aesthetic)) next

            layer_light <- layer$stat_params$light %||% layer$geom_params$light
            light <- if (is.null(layer_light)) plot_light else layer_light
            if (identical(light, "none") || !inherits(light, "light")) next
            if (is.null(layer_light) && is_default_light(light) && isFALSE(geom$lit_by_default)) next
            if (light$shade %in% targets) return(light)
      }
      NULL
}

#' The plot-level light of a plot
#'
#' @param plot A ggplot object.
#' @return The resolved plot-level light, or `NULL` if lighting is disabled.
#' @keywords internal
#' @noRd
plot_level_light <- function(plot) {
      coord <- plot$coordinates
      if (is.null(coord) || !"light" %in% names(coord)) return(NULL)
      light <- coord$light
      # A waiver means lighting was never set on the coord: either it came
      # from `+ light()` added before `coord_3d()` and is still stashed, or
      # nothing was specified and the default applies
      if (inherits(light, "waiver")) {
            pending <- attr(plot, "ggcube_pending_light")
            if (!is.null(pending)) light <- pending
      }
      resolve_light(light)
}

#' Does a layer map an aesthetic?
#'
#' True if the aesthetic is mapped by the layer or its plot, or computed by
#' default by the layer's stat (e.g. `fill = after_stat(z)`), and not set to
#' a fixed value.
#'
#' @param layer A ggplot2 layer.
#' @param aesthetic Aesthetic name.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
layer_maps_aesthetic <- function(layer, aesthetic) {
      if (aesthetic %in% names(layer$aes_params)) return(FALSE)
      mapping <- layer$computed_mapping %||% layer$mapping
      if (aesthetic %in% names(mapping)) return(TRUE)
      default <- layer$stat$default_aes[[aesthetic]]
      !is.null(default) && is_calculated_aes(default)
}

#' Is a default aesthetic computed?
#'
#' @param x A default aesthetic value.
#' @return `TRUE` for `after_stat()` or other calculated aesthetics.
#' @keywords internal
#' @noRd
is_calculated_aes <- function(x) {
      expr <- if (rlang::is_quosure(x)) rlang::quo_get_expr(x) else x
      is.call(expr)
}

#' Lighting that applies to a guide being drawn
#'
#' Guides are drawn after the plot is built, so the plot is found by walking
#' up the call stack.
#'
#' @param aesthetic The guide's aesthetics: more than one when ggplot2 has
#'   merged the guides of several scales, e.g. of `fill` and `colour` mapped to
#'   the same variable.
#' @return A `light` object, or `NULL`.
#' @keywords internal
#' @noRd
guide_lighting <- function(aesthetic) {
      plot <- find_build_plot()
      if (is.null(plot)) return(NULL)
      for (aes in intersect(c("fill", "colour"), aesthetic)) {
            light <- aesthetic_light(plot, aes)
            if (!is.null(light)) return(light)
      }
      NULL
}

#' Use 3D guides for lit aesthetics
#'
#' Called while a plot is built. Scales of `fill` and `colour` that still have
#' the default guide, `"colourbar"` or `"legend"`, get the 3D version if
#' lighting applies to their aesthetic. Guides set with `guides()` take
#' precedence over a scale's guide, so they still apply.
#'
#' @param plot The plot being built, whose scales are its own copies.
#' @param plot_light The plot-level light as resolved for the build.
#' @return Called for its side effect on the scales of `plot`.
#' @keywords internal
#' @noRd
use_3d_guides <- function(plot, plot_light) {
      for (scale in plot$scales$scales) {
            aesthetic <- intersect(c("fill", "colour"), scale$aesthetics)[1]
            if (is.na(aesthetic)) next
            guide <- scale$guide
            if (!is.character(guide) || length(guide) != 1) next
            shaded <- switch(guide,
                             colourbar = , colorbar = guide_colorbar_3d,
                             legend = guide_legend_3d,
                             NULL)
            if (is.null(shaded)) next
            if (is.null(aesthetic_light(plot, aesthetic, plot_light))) next
            scale$guide <- shaded()
      }
      invisible()
}

#' Create shading gradients for 3D guides
#'
#' @param base_colors Character vector of base colors
#' @param lighting_info Lighting specification object
#' @param n_steps Number of lighting gradient steps (default 10)
#' @return Matrix of colors with lighting applied, with one row per base color,
#'   in the order given, and one column per lighting step, from shadow to
#'   highlight
#' @keywords internal
#' @noRd
create_light_gradients <- function(base_colors,
                                   lighting_info,
                                   shade_range = c(-1, 1),
                                   n_steps = 20) {

      # Generate lighting values across range
      if(any(abs(shade_range) > 1)) stop("`shade_range` can't include values outside the range -1 to 1.")
      light <- seq(shade_range[1], shade_range[2], length.out = n_steps)

      # Light levels across the range, transformed to give the appearance of a
      # convex surface, and scaled by the light's strengths into amounts
      level <- sin(light * pi/2)
      amount <- ifelse(level >= 0, lighting_info$highlight * level, lighting_info$shadow * level)

      # Rows are base colors, columns are lighting steps
      grad <- tone_colors(lighting_info$tone,
                          rep(base_colors, times = n_steps),
                          rep(amount, each = length(base_colors)))
      matrix(grad, nrow = length(base_colors), ncol = n_steps)
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
replace_legend_colors <- function(grob, grad, base_colors = NULL) {
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
                        grob$grobs[[i]] <- replace_legend_colors(child, grad, base_colors)
                        modified <- TRUE
                  }
            }
            return(grob)
      }

      # Each key cell gets the gradient of the key whose color its glyph is
      # drawn in. Legends with several columns are filled column by column, so
      # grid position alone doesn't give a cell's key. Cells whose glyph color
      # matches no key fall back to grid position, top-to-bottom, then
      # left-to-right.
      ord <- order(grob$layout$t[key_cells_here], grob$layout$l[key_cells_here])
      key_cells <- key_cells_here[ord]
      rows <- seq_along(key_cells)
      if (!is.null(base_colors)) {
            glyph_fill <- vapply(key_cells, function(idx) {
                  key_glyph_fill(grob$grobs[[idx]])
            }, character(1))
            matched <- match(opaque_color_key(glyph_fill), opaque_color_key(base_colors))
            rows[!is.na(matched)] <- matched[!is.na(matched)]
      }

      for (i in seq_along(key_cells)) {
            if (rows[i] > nrow(grad)) next
            idx <- key_cells[i]
            grob$grobs[[idx]] <- replace_key_glyph(grob$grobs[[idx]], grad[rows[i], ])
      }

      return(grob)
}

#' Color key that ignores alpha
#'
#' @param colors Character vector of colors.
#' @return Character vector of opaque hex colors, with `NA` for missing or
#'   invalid colors.
#' @keywords internal
#' @noRd
opaque_color_key <- function(colors) {
      out <- rep(NA_character_, length(colors))
      ok <- !is.na(colors)
      rgb_m <- tryCatch(col2rgb(colors[ok]), error = function(e) NULL)
      if (!is.null(rgb_m)) out[ok] <- rgb(rgb_m[1, ], rgb_m[2, ], rgb_m[3, ], maxColorValue = 255)
      out
}

#' Fill color of a legend key cell's glyph
#'
#' @param cell A grob (rect or gTree) for one legend key.
#' @return The fill of the glyph that [replace_key_glyph()] would replace, or
#'   `NA`.
#' @keywords internal
#' @noRd
key_glyph_fill <- function(cell) {
      if (inherits(cell, "rect")) return(cell$gp$fill %||% NA_character_)
      name <- key_glyph_name(cell)
      if (is.null(name)) return(NA_character_)
      fill <- cell$children[[name]]$gp$fill
      if (is.null(fill) || length(fill) == 0) NA_character_ else as.character(fill[1])
}

#' Name of the layer glyph rect within a legend key gTree
#'
#' @param cell A gTree legend key cell.
#' @return The name of the child to replace, or `NULL`.
#' @keywords internal
#' @noRd
key_glyph_name <- function(cell) {
      if (!inherits(cell, "gTree") || length(cell$children) == 0) return(NULL)
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
            if (length(rect_children) == 0) return(NULL)
            glyph_name <- rect_children[length(rect_children)]
      }
      glyph_name
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
            glyph_name <- key_glyph_name(cell)
            if (is.null(glyph_name)) return(cell)

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
