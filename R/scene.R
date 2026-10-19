# Scene assembly: depth sorting across layers at draw time
#
# ggcube geoms project their data but don't sort or draw it. Instead each
# returns a deferred layer grob (class `ggcube_layer`) holding its projected
# rows, a function that applies the layer's styling to sorted rows, and the
# layer's render settings. `coord_3d()`'s `draw_panel()` merges each run of
# consecutive ggcube layers in a panel into one scene grob (`ggcube_scene`),
# which, when drawn, splits primitives of all its layers that pass through
# one another, styles each layer's rows, depth-sorts all primitives together
# (using the drawn sizes of markers and lines), and renders them in that
# order, each with its own layer's settings. Layers from other packages pass through
# unchanged, and separate runs of ggcube layers.


# Deferred layer grobs ----------------------------------------------------------------

#' Deferred grob for a ggcube layer
#'
#' @param rows Projected, unsorted rows from `coord$transform(sort = FALSE)`.
#' @param panel_params Panel parameters (for `proj` and `plot_bounds`).
#' @param finish Function taking sorted rows of this layer and returning them
#'   styled for rendering (row order preserved).
#' @param render Named list of arguments for `render_mixed_grobs()` (`rule`,
#'   `arrow`, `arrow.fill`, `lineend`).
#' @param on_top Whether the layer is drawn on top of the rest of its scene
#'   rather than sorted with it.
#' @return A `ggcube_layer` gTree, or a null grob for empty layers.
#' @keywords internal
#' @noRd
layer_scene_grob <- function(rows, panel_params, finish = identity, render = list(),
                             on_top = FALSE) {
      if (is.null(rows) || nrow(rows) == 0) return(grid::nullGrob())
      check_on_top(on_top)
      grid::gTree(rows = rows, finish = finish, render = render, on_top = on_top,
                  proj = panel_params$proj, plot_bounds = panel_params$plot_bounds,
                  cache = new.env(parent = emptyenv()), cl = "ggcube_layer")
}

#' Check a layer's `on_top` argument
#' @keywords internal
#' @noRd
check_on_top <- function(on_top, call = rlang::caller_env()) {
      if (!is.logical(on_top) || length(on_top) != 1 || is.na(on_top)) {
            rlang::abort("`on_top` must be `TRUE` or `FALSE`.", call = call)
      }
      invisible()
}

#' @export
makeContent.ggcube_layer <- function(x) {
      grid::setChildren(x, scene_children(list(x), x$cache))
}


# Scene grobs ------------------------------------------------------------------------

#' Merge runs of consecutive ggcube layers into scene grobs
#'
#' Grobs that draw nothing (null or zero grobs) don't interrupt a run.
#'
#' @param grobs List of a panel's grobs, in drawing order.
#' @return The list with each run of `ggcube_layer` grobs replaced by one
#'   `ggcube_scene` grob.
#' @keywords internal
#' @noRd
group_scene_layers <- function(grobs) {
      is_layer <- vapply(grobs, inherits, logical(1), "ggcube_layer")
      if (!any(is_layer)) return(grobs)
      is_empty <- vapply(grobs, function(g) inherits(g, c("null", "zeroGrob")), logical(1))

      out <- list()
      run <- list()
      flush <- function() {
            if (length(run) > 0) {
                  out[[length(out) + 1]] <<- scene_grob(run)
                  run <<- list()
            }
      }
      for (k in seq_along(grobs)) {
            if (is_layer[k]) {
                  run[[length(run) + 1]] <- grobs[[k]]
            } else if (is_empty[k] && length(run) > 0) {
                  next
            } else {
                  flush()
                  out[[length(out) + 1]] <- grobs[[k]]
            }
      }
      flush()
      out
}

scene_grob <- function(layers) {
      if (length(layers) == 1) return(layers[[1]])
      grid::gTree(layers = layers, cache = new.env(parent = emptyenv()), cl = "ggcube_scene")
}

#' @export
makeContent.ggcube_scene <- function(x) {
      grid::setChildren(x, scene_children(x$layers, x$cache))
}


# Sorting and rendering a scene ------------------------------------------------------

# Columns that depth sorting reads or writes; all other columns of a row are
# carried through sorting by row reference
.sort_columns <- c("x", "y", "z", "depth", "depth_scale", "depth_3d", "group", ".prim",
                   ".x3d", ".y3d", ".z3d", "point_type", "segment_id",
                   ".subgroup", "subgroup")

#' Sort, style, and render the layers of a scene
#'
#' Called at draw time, in the panel's viewport, whose size sets how large
#' point markers and lines are relative to the panel. The result is cached
#' for that size.
#'
#' @param layers List of `ggcube_layer` grobs, in layer order.
#' @param cache Environment in which to keep the result between draws.
#' @return A gList of child grobs.
#' @keywords internal
#' @noRd
scene_children <- function(layers, cache = NULL) {
      panel_pt <- c(grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE),
                    grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE))
      if (!is.null(cache) && identical(cache$panel_pt, panel_pt)) return(cache$children)

      rows <- scene_rows(layers, panel_pt)
      capture_scene(rows, layers)

      grobs <- list()
      if (nrow(rows) > 0) {
            run <- cumsum(c(TRUE, rows$.layer[-1] != rows$.layer[-nrow(rows)]))
            for (r in unique(run)) {
                  sel <- run == r
                  layer <- layers[[rows$.layer[sel][1]]]
                  # The layer's own rows, as it styled them
                  own <- attr(rows, "layer_rows")[[as.character(rows$.layer[sel][1])]]
                  own <- own[match(rows$.key[sel], own$.key), , drop = FALSE]
                  grobs[[length(grobs) + 1]] <- do.call(render_mixed_grobs, c(list(own), layer$render))
            }
      }
      grobs <- grobs[!vapply(grobs, inherits, logical(1), "null")]
      children <- do.call(grid::gList, grobs)
      if (!is.null(cache)) {
            cache$panel_pt <- panel_pt
            cache$children <- children
      }
      children
}

#' Sorted, styled rows of a scene, in drawing order
#'
#' Primitives of all layers are split where they pass through one another,
#' each layer's rows are styled by its `finish` function, and all primitives
#' are then depth-sorted together, using the drawn sizes of point markers and
#' lines. Layers drawn `on_top` are split and sorted among themselves, and
#' drawn after the rest.
#'
#' @param layers List of `ggcube_layer` grobs.
#' @param panel_pt Panel width and height in points, or NULL if unknown.
#' @return Data frame of rows in drawing order, with `.layer` giving each
#'   row's layer and `.key` identifying it. Columns whose types differ between
#'   layers are converted to character, so its attribute `layer_rows` keeps
#'   each layer's styled rows as the layer made them, for rendering.
#' @keywords internal
#' @noRd
scene_rows <- function(layers, panel_pt = NULL) {
      proj <- layers[[1]]$proj
      plot_bounds <- layers[[1]]$plot_bounds
      multi <- length(layers) > 1

      # Sort-relevant columns of every layer, with a reference back to the
      # full row. Group names are made unique across layers.
      frames <- lapply(seq_along(layers), function(i) {
            r <- layers[[i]]$rows
            f <- r[intersect(.sort_columns, names(r))]
            f$.row <- seq_len(nrow(r))
            f$.layer <- i
            if (multi) {
                  f$group <- paste0("L", i, "::", f$group)
                  if (".prim" %in% names(f)) f$.prim <- as.character(f$.prim)
            }
            f
      })
      merged <- if (multi) bind_layer_rows(frames) else frames[[1]]
      on_top <- vapply(layers, function(l) isTRUE(l$on_top), logical(1))
      top <- on_top[merged$.layer]
      split <- if (any(top) && !all(top)) {
            dplyr::bind_rows(split_for_sorting(merged[!top, , drop = FALSE], proj, plot_bounds),
                             split_for_sorting(merged[top, , drop = FALSE], proj, plot_bounds))
      } else {
            split_for_sorting(merged, proj, plot_bounds)
      }
      split$.key <- seq_len(nrow(split))

      # Restore each layer's full rows, with the columns splitting changed,
      # and style them
      changed <- setdiff(names(split), c(".row", ".layer", ".key"))
      styled <- lapply(seq_along(layers), function(i) {
            s <- split[split$.layer == i, , drop = FALSE]
            if (nrow(s) == 0) return(NULL)
            full <- layers[[i]]$rows[s$.row, , drop = FALSE]
            for (col in changed) full[[col]] <- s[[col]]
            if (multi) full$group <- sub("^L[0-9]+::", "", full$group)
            full$group <- as.integer(factor(full$group))
            full$.layer <- i
            full$.key <- s$.key
            rownames(full) <- NULL
            layers[[i]]$finish(full)
      })
      styled <- styled[!vapply(styled, is.null, logical(1))]
      if (length(styled) == 0) return(data.frame())
      names(styled) <- vapply(styled, function(d) as.character(d$.layer[1]), character(1))

      # Drawn extents of point markers and lines, then the order
      ext <- dplyr::bind_rows(lapply(styled, function(d) {
            e <- drawn_extents(d)
            data.frame(.key = d$.key, radius = e$radius, halfwidth = e$halfwidth)
      }))
      at <- match(split$.key, ext$.key)
      split$.radius_pt <- ext$radius[at]
      split$.halfwidth_pt <- ext$halfwidth[at]
      top <- on_top[split$.layer]
      ordered <- if (any(top) && !all(top)) {
            dplyr::bind_rows(order_by_depth(split[!top, , drop = FALSE], proj, plot_bounds, panel_pt),
                             order_by_depth(split[top, , drop = FALSE], proj, plot_bounds, panel_pt))
      } else {
            order_by_depth(split, proj, plot_bounds, panel_pt)
      }

      merged <- if (length(styled) > 1) bind_layer_rows(styled) else styled[[1]]
      out <- merged[stats::na.omit(match(ordered$.key, merged$.key)), , drop = FALSE]
      rownames(out) <- NULL
      attr(out, "layer_rows") <- styled
      out
}

#' Bind rows of several layers
#'
#' Columns whose types differ between layers (e.g. a line type given as a
#' number in one layer and a name in another) are converted to character.
#'
#' @param frames List of data frames.
#' @return One data frame.
#' @keywords internal
#' @noRd
bind_layer_rows <- function(frames) {
      frames <- lapply(frames, function(f) {
            if (inherits(f, "tbl_df")) f <- as.data.frame(f)
            f
      })
      kind <- function(v) {
            if (is.numeric(v) && !inherits(v, "AsIs")) "number"
            else if (is.logical(v) && all(is.na(v))) "missing"
            else paste(class(v), collapse = "/")
      }
      cols <- unique(unlist(lapply(frames, names)))
      for (col in cols) {
            kinds <- unique(unlist(lapply(frames, function(f) if (col %in% names(f)) kind(f[[col]]))))
            kinds <- setdiff(kinds, "missing")
            is_list <- any(vapply(frames, function(f) col %in% names(f) && is.list(f[[col]]), logical(1)))
            if (length(kinds) > 1 && !is_list) {
                  frames <- lapply(frames, function(f) {
                        if (col %in% names(f)) {
                              v <- f[[col]]
                              f[[col]] <- if (is.factor(v)) as.character(v) else as.character(unclass(v))
                        }
                        f
                  })
            }
      }
      dplyr::bind_rows(frames)
}

#' Drawn extents of styled rows, in points
#'
#' Marker radii follow grid's plotting symbols (a circle's radius is 0.375
#' times the font size), plus half the outline for filled shapes; line
#' half-widths follow the line width.
#'
#' @param rows Styled rows (as passed to `render_mixed_grobs()`).
#' @return List with `radius` (points; 0 for non-points) and `halfwidth`
#'   (segments; 0 otherwise), per row.
#' @keywords internal
#' @noRd
drawn_extents <- function(rows) {
      n <- nrow(rows)
      prim <- if (".prim" %in% names(rows)) rows$.prim else rep(NA_character_, n)
      num <- function(col, default) {
            v <- if (col %in% names(rows)) suppressWarnings(as.numeric(rows[[col]])) else rep(default, n)
            ifelse(is.na(v), default, v)
      }
      size <- num("size", 1.5)
      stroke <- num("stroke", 0.5)
      lw <- num("linewidth", 0.5)
      shape <- if ("shape" %in% names(rows)) rows$shape else rep(19, n)
      filled <- is.numeric(shape) & shape %in% 21:25
      lwd_pt <- 72.27 / 96                                      # grid lwd unit in points
      fontsize <- ifelse(filled, (size + 0.5 * stroke) * .pt, size * .pt)
      radius <- 0.375 * fontsize + ifelse(filled, stroke * .pt * lwd_pt / 2, 0)
      list(radius = ifelse(prim %in% "point", radius, 0),
           halfwidth = ifelse(prim %in% "segment", lw * .pt * lwd_pt / 2, 0))
}


# Validation hook ----------------------------------------------------------------------

.scene_capture <- new.env(parent = emptyenv())

capture_scene <- function(rows, layers) {
      if (!isTRUE(.scene_capture$active)) return(invisible())
      .scene_capture$scenes[[length(.scene_capture$scenes) + 1]] <- list(
            rows = rows, proj = layers[[1]]$proj, plot_bounds = layers[[1]]$plot_bounds)
      invisible()
}

#' Drawn primitives of a plot, for the regression harness's ordering validator
#'
#' Draws the plot off-screen at the given size, capturing each scene's rows in
#' drawing order with their cube-space coordinates.
#'
#' @param p A ggplot.
#' @param width,height,res Device size in inches and resolution.
#' @return List with `panels` (each with `rows`, `proj`, `plot_bounds`),
#'   `panel_pt` (the first panel's size in points), and `sizes_final = TRUE`
#'   (marker and line sizes are as drawn).
#' @keywords internal
#' @noRd
validation_scene <- function(p, width = 6, height = 5, res = 100) {
      .scene_capture$active <- TRUE
      .scene_capture$scenes <- list()
      on.exit({ .scene_capture$active <- FALSE; .scene_capture$scenes <- NULL })

      f <- tempfile(fileext = ".png")
      grDevices::png(f, width = width, height = height, units = "in", res = res)
      on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
      grid::grid.newpage()
      grid::grid.draw(ggplotGrob(p))
      vps <- grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name
      pv <- grep("^panel\\.", vps, value = TRUE)
      panel_pt <- c(width, height) * 72.27
      if (length(pv)) {
            grid::seekViewport(pv[1])
            panel_pt <- c(grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE),
                          grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE))
      }
      list(panels = .scene_capture$scenes, panel_pt = panel_pt, sizes_final = TRUE)
}
