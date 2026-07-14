#' Create an interactive drag-to-rotate ggcube
#'
#' Creates a ggcube HTML object that you can rotate interactively by dragging.
#' Pre-renders a set of ggcube plots across a 1D or 2D grid of rotation angles,
#' and packages them as a self-contained HTML file that can be viewed in the
#' RStudio viewer or a web browser. When viewing the HTML, you can navigate
#' through the views by dragging the image or using keyboard arrows.
#'
#' @section Specifying rotation:
#' Each of `pitch`, `roll`, and `yaw` is given either as a
#' single value (held fixed) or as a length-2 range `c(from, to)` that
#' is a variable, draggable axis. The number of ranges determines the
#' interaction:
#' \itemize{
#'   \item One range gives a 1D flipbook: horizontal drag (or left/right
#'     keyboard arrows) vary that one parameter across its range.
#'   \item Two ranges give a 2D flipbook: horizontal drag (or left/right
#'     keyboard arrows) vary one parameter while vertical drag/arrows vary
#'     the other.
#' }
#' At least one range is required. Supplying three ranges is an error.
#'
#' For a 2D flipbook, the horizontal (x) and vertical (y) axes are assigned
#' from whichever parameters are ranged, by priority. The x axis takes the
#' highest-priority ranged parameter in the order `yaw`, `pitch`,
#' `roll`; the y axis takes the highest-priority remaining ranged
#' parameter in the order `roll`, `pitch`. In the common case
#' `yaw = c(0, 360), roll = c(...)` this puts yaw on horizontal drag and
#' roll on vertical drag.
#'
#' The range direction follows the order the values are written: for example
#' `yaw = c(360, 0)` rotates the opposite way from `yaw = c(0, 360)`.
#'
#' @section Looping:
#' When a variable axis returns to its starting view (for example a full
#' `yaw = c(0, 360)` turn), that axis loops seamlessly: dragging past
#' either end wraps around. This is detected per axis by comparing the first
#' and last angle modulo 360, so a full-turn axis wraps while a partial range
#' (such as `roll = c(-90, 90)`) clamps. `loop` can force this on or
#' off for all axes at once.
#'
#' @section Keyboard:
#' Once the widget has focus, the arrow keys step one frame: left/right move
#' the horizontal axis, and (in a 2D flipbook) up/down move the vertical axis.
#'
#' @inheritParams animate_3d
#' @param pitch,roll,yaw Rotation angles in degrees. Each is either a single
#'   value (held fixed) or a length-2 range `c(from, to)` that becomes a
#'   draggable axis. For `yaw`, using a larger value for `from` than `to` gives
#'   more natural results. Values specified here replace the ones specified in
#'   `coord_3d()`. See Details.
#' @param n Grid resolution. For a 1D flipbook, a single number giving the
#'   frame count. For a 2D flipbook, a length-2 vector giving the counts for
#'   the horizontal and vertical screen axes respectively (in that order, x then y).
#'   Its length must match the number of ranged parameters. Default is 36.
#' @param loop Controls seamless wraparound. `NULL` (default)
#'   auto-detects looping independently for each axis by checking whether that
#'   axis returns to its starting view. `TRUE` or `FALSE` force
#'   wrapping on or off for all axes.
#' @param start Initial viewing angle(s) the flipbook opens at, given in
#'   degrees as a named vector matching the variable axes, e.g.
#'   `start = c(yaw = 30)` or `start = c(yaw = 30, roll = 45)`. For a
#'   1D flipbook a bare scalar (e.g. `start = 30`) is also accepted. The
#'   flipbook opens at the frame nearest each given angle. Any variable axis not
#'   named in `start` opens at the midpoint of its range, as does every
#'   axis when `start = NULL` (the default). Naming an axis that is
#'   fixed rather than variable throas an error.
#' @param max_frames Safety cap on the total number of rendered frames
#'   (`prod(n)`). A 2D grid can become very large; if the requested grid
#'   exceeds this cap the function errors rather than rendering. Raise it to
#'   allow larger grids. Default is 500.
#' @param file Output path for the HTML file. If `NULL` (default), a
#'   temporary file is used.
#' @param viewer Logical. If `TRUE`, open the flipbook in the RStudio
#'   Viewer pane (falling back to the system browser when RStudio is not
#'   available). Defaults to `interactive()`.
#'
#' @return A `flipbook_3d` object: the path to the written HTML file,
#'   with class attributes for display in RStudio and knitr. Returned
#'   invisibly.
#'
#' @examplesIf requireNamespace("base64enc", quietly = TRUE)
#' \donttest{
#' p <- ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
#'   geom_surface_3d(linewidth = .25) +
#'   coord_3d(light = light(anchor = "camera", direction = c(1, 1, 0)),
#'            ratio = c(1.5, 2, 1)) +
#'   scale_fill_viridis_c() + scale_color_viridis_c() +
#'   theme_void()
#'
#' # 1D drag-to-rotate turntable (small n to minimize runtime)
#' flipbook_3d(p, yaw = c(360, 0), n = 12)
#'
#' # 2D flipbook: drag horizontally to spin (yaw), vertically to tilt (roll)
#' # (note you could increase `cores` to speed up compute time)
#' flipbook_3d(p, yaw = c(360, 0), roll = c(-90, 0), n = c(20, 10),
#'             width = 1000, height = 1000, cores = 1)
#'
#' # Save to a chosen path
#' flipbook_3d(p, yaw = c(360, 0), n = 12,
#'             file = file.path(tempdir(), "surface.html"))
#' }
#'
#' @seealso [animate_3d()] for GIF/MP4 output. See
#'   `vignette("animation")` for live, interactive examples.
#' @export
flipbook_3d <- function(plot,
                        pitch = NULL,
                        roll = NULL,
                        yaw = NULL,
                        n = 36,
                        loop = NULL,
                        start = NULL,
                        max_frames = 500,
                        width = 480,
                        height = 480,
                        res = 96,
                        cores = 1,
                        device = "png",
                        progress = interactive(),
                        file = NULL,
                        viewer = interactive()) {

      # Clear z scale cache to ensure clean state between renders
      .z_scale_cache$scale <- NULL

      # --- Validate input plot ---
      if (!inherits(plot, "gg")) {
            stop("`plot` must be a ggplot object.", call. = FALSE)
      }
      coord <- plot$coordinates
      if (!inherits(coord, "Coord3D")) {
            stop("`plot` must use coord_3d(). ",
                 "Add `+ coord_3d()` to your plot before making a flipbook.",
                 call. = FALSE)
      }

      # --- Validate cores / loop ---
      cores <- as.integer(cores)
      if (cores < 1) stop("`cores` must be at least 1.", call. = FALSE)
      if (!is.null(loop) && !(is.logical(loop) && length(loop) == 1)) {
            stop("`loop` must be NULL, TRUE, or FALSE.", call. = FALSE)
      }

      # --- Classify rotation parameters and assign drag axes ---
      # Each parameter is absent (NULL), fixed (length 1), or a variable range
      # (length 2). The set of ranged parameters determines dimensionality and
      # the x/y axis assignment.
      spec <- resolve_flipbook_axes(pitch = pitch, roll = roll, yaw = yaw)
      ndim <- length(spec$axes)

      # --- Validate n against dimensionality, and enforce the frame cap ---
      n <- as.integer(n)
      if (any(is.na(n)) || any(n < 1)) {
            stop("`n` must be one or more positive integers.", call. = FALSE)
      }
      if (length(n) != ndim) {
            stop(sprintf(
                  "`n` has length %d but %d rotation %s ranged (needs length %d). %s",
                  length(n), ndim, if (ndim == 1) "parameter is" else "parameters are",
                  ndim,
                  "Give one count per ranged parameter (x then y)."),
                 call. = FALSE)
      }
      total <- prod(n)
      if (total > max_frames) {
            stop(sprintf(
                  paste0("This flipbook would render %d frames (%s), exceeding ",
                         "max_frames = %d. Reduce `n`, or raise `max_frames` to ",
                         "allow a larger grid."),
                  total, paste(n, collapse = " x "), max_frames),
                 call. = FALSE)
      }

      # --- Build the (flattened) parameter grid ---
      # Each variable axis gets its own 1D range, with per-axis loop detection
      # and duplicate-endpoint handling; the axes are then crossed row-major
      # (x varying fastest) into a flat param_seqs data.frame. Fixed and absent
      # parameters are held constant using the supplied value or the coord's.
      grid <- build_param_grid(
            coord = coord,
            spec = spec,
            n = n,
            loop = loop,
            start = start
      )
      param_seqs <- grid$param_seqs
      dims <- grid$dims
      loop_flags <- grid$loop_flags
      start_cell <- grid$start

      # --- Render frames to PNG files ---
      frames <- render_3d_frames(
            plot = plot,
            coord = coord,
            param_seqs = param_seqs,
            width = width,
            height = height,
            res = res,
            device = device,
            cores = cores,
            progress = progress,
            prefix = "ggcube_flip_"
      )

      message("Assembling flipbook...")

      # --- Assemble self-contained HTML ---
      # `dims` describes the grid shape (length 1 for 1D, length 2 for 2D) and
      # drives the player's index arithmetic. `loop_flags` gives the per-axis
      # wraparound behavior.
      html <- build_flipbook_html(
            files = frames$files,
            width = frames$width,
            height = frames$height,
            dims = dims,
            loop_flags = loop_flags,
            start = start_cell
      )

      # Frames are inlined as base64; the temp PNGs are no longer needed.
      unlink(frames$tmpdir, recursive = TRUE)

      if (is.null(file)) {
            file <- tempfile(fileext = ".html")
      }
      writeLines(html, file)

      result <- flipbook_3d_obj(file, width = frames$width,
                                height = frames$height,
                                viewer = isTRUE(viewer))

      # Stash as last flipbook (parallels .ggcube_last_anim)
      .ggcube_last_flipbook$value <- result

      # Return the object visibly (not invisibly) so that, like a ggplot, a
      # bare flipbook_3d() call as the last expression of a chunk or at the
      # console is auto-printed -- which is what triggers the viewer
      # (interactively) or knit_print (when knitting). Opening the viewer is
      # therefore handled by print.flipbook_3d, not here, so it happens exactly
      # once and only when the object is actually printed. Assigning the result
      # to a variable suppresses auto-print, so no viewer opens and nothing is
      # embedded -- matching ggplot's behavior.
      result
}


# Axis resolution & grid construction -------------------------------------

#' Classify rotation parameters and assign drag axes
#'
#' Each of pitch/roll/yaw is absent (NULL), fixed (length 1), or a variable range
#' (length 2). Ranged parameters become draggable axes; the x axis takes the
#' highest-priority ranged parameter in order yaw > pitch > roll, and the y
#' axis takes the highest-priority remaining ranged parameter in order
#' roll > pitch. Fixed values are recorded to be held constant.
#'
#' @param pitch,roll,yaw User-supplied rotation arguments.
#' @return A list with `axes` (character vector of ranged parameter
#'   names, in x-then-y order), `ranges` (named list of the length-2
#'   ranges for those axes), and `fixed` (named list of scalar values for
#'   parameters the user pinned to a single value).
#' @keywords internal
#' @noRd
resolve_flipbook_axes <- function(pitch = NULL, roll = NULL, yaw = NULL) {
      args <- list(pitch = pitch, roll = roll, yaw = yaw)

      ranged <- character(0)
      fixed <- list()
      for (nm in names(args)) {
            v <- args[[nm]]
            if (is.null(v)) next
            if (length(v) == 1) {
                  fixed[[nm]] <- v
            } else if (length(v) == 2) {
                  ranged <- c(ranged, nm)
            } else {
                  stop(sprintf(
                        paste0("`%s` has length %d. Each rotation parameter must ",
                               "be a single value (held fixed) or a length-2 ",
                               "range c(from, to)."),
                        nm, length(v)), call. = FALSE)
            }
      }

      if (length(ranged) == 0) {
            stop(paste0("A flipbook needs at least one rotation parameter given ",
                        "as a range c(from, to). For example, yaw = c(0, 360)."),
                 call. = FALSE)
      }
      if (length(ranged) > 2) {
            stop(paste0("Three ranged rotation parameters were given. A flipbook ",
                        "supports at most two draggable axes; for motion through ",
                        "three angles use animate_3d()."),
                 call. = FALSE)
      }

      # Assign axes by priority.
      x_priority <- c("yaw", "pitch", "roll")
      y_priority <- c("roll", "pitch")

      x_axis <- x_priority[x_priority %in% ranged][1]
      remaining <- setdiff(ranged, x_axis)
      y_axis <- if (length(remaining) > 0) {
            y_priority[y_priority %in% remaining][1]
      } else {
            NA_character_
      }

      axes <- c(x_axis, if (!is.na(y_axis)) y_axis)
      ranges <- args[axes]

      list(axes = axes, ranges = ranges, fixed = fixed)
}


#' Build the flattened parameter grid for a flipbook
#'
#' Produces one 1D range per variable axis (with per-axis loop detection and
#' duplicate-endpoint handling), then crosses the axes row-major with the x
#' axis varying fastest. Fixed and absent parameters are held constant.
#'
#' @param coord The plot's Coord3D object (source of held-constant defaults).
#' @param spec Output of resolve_flipbook_axes().
#' @param n Integer resolution vector, one entry per axis, in x-then-y order.
#' @param loop `NULL` to auto-detect per axis, or a single logical to
#'   force all axes.
#' @return A list with `param_seqs` (flat data.frame of pitch/roll/yaw,
#'   one row per frame, row-major with x fastest), `dims` (integer vector
#'   of axis lengths after endpoint handling), and `loop_flags` (logical
#'   vector, one per axis).
#' @keywords internal
#' @noRd
build_param_grid <- function(coord, spec, n, loop, start = NULL) {
      axes <- spec$axes

      # Build each axis's 1D range of angle values, applying loop detection and
      # dropping the duplicate endpoint when the axis wraps.
      axis_values <- vector("list", length(axes))
      loop_flags <- logical(length(axes))
      dims <- integer(length(axes))

      for (i in seq_along(axes)) {
            rng <- spec$ranges[[axes[i]]]
            count <- n[i]

            # Determine whether this axis loops.
            is_loop <- if (is.null(loop)) {
                  angles_return_home(rng[1], rng[2])
            } else {
                  isTRUE(loop)
            }

            if (is_loop && count > 1L) {
                  # Sample count + 1 inclusive points, drop the duplicate end,
                  # leaving `count` unique points spanning the half-open range.
                  vals <- seq(rng[1], rng[2], length.out = count + 1L)
                  vals <- vals[-length(vals)]
            } else {
                  vals <- seq(rng[1], rng[2], length.out = count)
                  is_loop <- is_loop && count > 1L
            }

            axis_values[[i]] <- vals
            loop_flags[i] <- is_loop
            dims[i] <- length(vals)
      }

      # Cross the axes row-major with x (axis 1) varying fastest.
      if (length(axes) == 1) {
            grid_idx <- data.frame(x = seq_along(axis_values[[1]]))
      } else {
            # expand.grid varies its first argument fastest, which is what we
            # want for x.
            grid_idx <- expand.grid(
                  x = seq_along(axis_values[[1]]),
                  y = seq_along(axis_values[[2]])
            )
      }

      total <- nrow(grid_idx)

      # Start every parameter at its held-constant value, then overwrite the
      # axis parameters with their variable values.
      const <- function(nm) {
            if (!is.null(spec$fixed[[nm]])) spec$fixed[[nm]] else coord[[nm]]
      }
      out <- data.frame(
            pitch = rep(const("pitch"), total),
            roll  = rep(const("roll"),  total),
            yaw   = rep(const("yaw"),   total)
      )

      out[[axes[1]]] <- axis_values[[1]][grid_idx$x]
      if (length(axes) == 2) {
            out[[axes[2]]] <- axis_values[[2]][grid_idx$y]
      }

      rownames(out) <- NULL

      # Determine the 0-based starting cell per axis from the requested start
      # angles (or the midpoint when unspecified).
      start_cell <- flipbook_start_cell(axis_values, axes, start)

      list(param_seqs = out, dims = dims, loop_flags = loop_flags,
           start = start_cell)
}


#' Choose the flipbook's initial frame cell from requested start angles
#'
#' Returns a 0-based index per variable axis for the frame the player shows first.
#' `start` is an optional set of viewing angles in degrees: a named vector
#' whose names are variable axis names (e.g. `c(yaw = 30, roll = 45)`), or, for
#' a 1D flipbook, a bare scalar applied to the single variable axis. For each variable
#' axis named in `start`, the flipbook opens at the frame nearest that angle
#' (nearest by circular distance, modulo 360). Any variable axis not named opens at
#' the index midpoint of its range, as does every axis when `start` is
#' `NULL`.
#'
#' @param axis_values List of per-axis sampled angle vectors.
#' @param axes Character vector of variable axis names (x then y).
#' @param start `NULL`, a named numeric vector of angles, or (1D only) a
#'   bare scalar.
#' @return Integer vector of 0-based cell indices, one per axis.
#' @keywords internal
#' @noRd
flipbook_start_cell <- function(axis_values, axes, start = NULL) {
      midpoint <- function() {
            vapply(axis_values, function(v) as.integer(floor(length(v) / 2)),
                   integer(1))
      }

      if (is.null(start)) return(midpoint())

      # Normalize `start` to a named list of angles keyed by axis.
      if (is.null(names(start))) {
            # Unnamed: only valid as a single value for a 1D flipbook.
            if (length(axes) == 1 && length(start) == 1) {
                  targets <- stats::setNames(list(start[[1]]), axes)
            } else {
                  stop("`start` must be a named vector of angles (e.g. ",
                       "c(yaw = 30)); a bare value is only allowed for a 1D ",
                       "flipbook.", call. = FALSE)
            }
      } else {
            bad <- setdiff(names(start), axes)
            if (length(bad)) {
                  stop(sprintf(
                        "`start` names %s, which %s fixed rather than variable. Variable %s: %s.",
                        paste(bad, collapse = ", "),
                        if (length(bad) == 1) "is" else "are",
                        if (length(axes) == 1) "axis is" else "axes are",
                        paste(axes, collapse = ", ")),
                       call. = FALSE)
            }
            targets <- as.list(start)
      }

      circ_dist <- function(a, b) abs(((a - b) + 180) %% 360 - 180)

      out <- integer(length(axes))
      for (i in seq_along(axes)) {
            ax <- axes[i]
            vals <- axis_values[[i]]
            if (is.null(targets[[ax]]) || length(vals) == 1) {
                  out[i] <- as.integer(floor(length(vals) / 2))  # midpoint
            } else {
                  out[i] <- which.min(circ_dist(vals, targets[[ax]])) - 1L
            }
      }
      out
}


#' Do two angles (degrees) describe a full-circle return?
#'
#' Compares two angles modulo 360, so 0 and 360 (or 0 and 720) count as
#' returning home.
#'
#' @param a,b Angles in degrees.
#' @param tol Angular tolerance in degrees.
#' @return A single logical.
#' @keywords internal
#' @noRd
angles_return_home <- function(a, b, tol = 1e-6) {
      d <- abs(((a - b) + 180) %% 360 - 180)
      d <= tol
}


# HTML assembly -----------------------------------------------------------

#' Build the self-contained flipbook HTML document
#'
#' Inlines each PNG frame as a base64 data URI and emits a single HTML string
#' containing a minimal vanilla-JS drag-to-scrub player. Horizontal drag maps
#' across the x axis and (for a 2D grid) vertical drag across the y axis; the
#' arrow keys step one frame per axis; all frames are preloaded before
#' interaction is enabled. Each axis wraps or clamps according to its
#' `loop_flags` entry.
#'
#' Frames are ordered row-major with x varying fastest, so the frame at grid
#' position `(ix, iy)` is `iy * nx + ix`.
#'
#' @param files Character vector of PNG frame paths, in row-major order
#'   (x fastest).
#' @param width,height Frame dimensions in pixels.
#' @param dims Integer vector of axis lengths: `c(nx)` for 1D or
#'   `c(nx, ny)` for 2D.
#' @param loop_flags Logical vector, one per axis, giving per-axis wraparound.
#' @return A single character string of HTML.
#' @keywords internal
#' @noRd
build_flipbook_html <- function(files, width, height, dims, loop_flags,
                                start = NULL) {
      if (!requireNamespace("base64enc", quietly = TRUE)) {
            stop("The base64enc package is required to build a flipbook. ",
                 "Install it with install.packages('base64enc').",
                 call. = FALSE)
      }

      # Encode each frame as a data URI.
      data_uris <- vapply(files, function(f) {
            paste0("data:image/png;base64,", base64enc::base64encode(f))
      }, character(1))

      frames_js <- paste0(
            "[", paste0('"', data_uris, '"', collapse = ","), "]"
      )

      # Normalize grid dimensions to (nx, ny); 1D is ny = 1.
      nx <- dims[1]
      ny <- if (length(dims) >= 2) dims[2] else 1L
      loop_x <- isTRUE(loop_flags[1])
      loop_y <- length(loop_flags) >= 2 && isTRUE(loop_flags[2])
      two_d <- ny > 1L

      # Initial cell (0-based); defaults to origin if not supplied.
      start_ix <- if (length(start) >= 1) as.integer(start[1]) else 0L
      start_iy <- if (length(start) >= 2) as.integer(start[2]) else 0L

      aria <- if (two_d) {
            "3D plot. Drag or use the arrow keys to rotate."
      } else {
            "3D plot. Drag or use the left and right arrow keys to rotate."
      }

      # The player treats every flipbook as a grid of (ix, iy); a 1D flipbook
      # is simply ny = 1. Horizontal drag drives ix, vertical drag drives iy,
      # each wrapped or clamped by its own loop flag. The rendered frame at
      # (ix, iy) is iy * nx + ix (row-major, x fastest). Drag release and focus
      # handling are shared across both modes.
      template <- '<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
  html, body { margin: 0; padding: 0; background: transparent; }
  #ggcube-flipbook {
    display: inline-block;
    position: relative;
    max-width: 100%%;
    user-select: none;
    -webkit-user-select: none;
    touch-action: none;
    cursor: grab;
    outline: none;
  }
  #ggcube-flipbook.dragging { cursor: grabbing; }
  #ggcube-flipbook:focus:not(.focus-from-pointer),
  #ggcube-flipbook:focus-visible:not(.focus-from-pointer) {
    outline: 2px solid #4c8bf5;
    outline-offset: 2px;
  }
  #ggcube-flipbook img {
    display: block;
    width: %dpx;
    height: %dpx;
    max-width: 100%%;
    height: auto;
    pointer-events: none;
  }
  #ggcube-loading {
    position: absolute;
    top: 50%%; left: 50%%;
    transform: translate(-50%%, -50%%);
    font-family: sans-serif;
    font-size: 13px;
    color: #666;
  }
</style>
</head>
<body>
<div id="ggcube-flipbook" tabindex="0" aria-label="%s">
  <img id="ggcube-frame" alt="3D plot (drag or arrow keys to rotate)">
  <div id="ggcube-loading">Loading frames...</div>
</div>
<script>
(function() {
  var srcs = %s;
  var dims = %s;
  var nx = %d, ny = %d;
  var loopX = %s, loopY = %s;
  var n = srcs.length;
  var container = document.getElementById("ggcube-flipbook");
  var imgEl = document.getElementById("ggcube-frame");
  var loading = document.getElementById("ggcube-loading");

  var loaded = 0;
  var ix = %d, iy = %d;
  var curIndex = -1;
  var ready = false;

  function wrapAxis(i, len, loop) {
    if (len <= 1) return 0;
    if (loop) return ((i %% len) + len) %% len;
    if (i < 0) return 0;
    if (i > len - 1) return len - 1;
    return i;
  }

  function render() {
    ix = wrapAxis(ix, nx, loopX);
    iy = wrapAxis(iy, ny, loopY);
    var idx = iy * nx + ix;
    if (idx === curIndex) return;
    curIndex = idx;
    imgEl.src = srcs[idx];
  }

  function setPos(nix, niy) {
    ix = Math.round(nix);
    iy = Math.round(niy);
    render();
  }

  // Preload all frames before enabling interaction.
  for (var k = 0; k < n; k++) {
    (function(k) {
      var im = new Image();
      im.onload = im.onerror = function() {
        loaded++;
        if (loaded === n) {
          ready = true;
          loading.style.display = "none";
          render();
        }
      };
      im.src = srcs[k];
    })(k);
  }

  var dragging = false;
  var startX = 0, startY = 0;
  var startIx = 0, startIy = 0;

  function ixFromDx(dx) {
    var w = container.clientWidth || 1;
    // Full-width drag varies the x axis once.
    return startIx + dx * ((nx - 1) / w);
  }
  function iyFromDy(dy) {
    var h = container.clientHeight || 1;
    // Full-height drag varies the y axis once.
    return startIy + dy * ((ny - 1) / h);
  }

  function endDrag() {
    if (!dragging) return;
    dragging = false;
    container.classList.remove("dragging");
  }

  function onDown(e) {
    if (!ready) return;
    dragging = true;
    container.classList.add("dragging");
    startX = e.clientX; startY = e.clientY;
    startIx = ix; startIy = iy;
    if (container.setPointerCapture && e.pointerId !== undefined) {
      container.setPointerCapture(e.pointerId);
    }
    // preventDefault (below) suppresses the implicit focus a pointerdown would
    // give a tabindex element, so focus explicitly. Flag it as pointer focus
    // so the ring stays hidden until the keyboard drives.
    container.classList.add("focus-from-pointer");
    container.focus();
    e.preventDefault();
  }

  function onMove(e) {
    if (!dragging) return;
    setPos(ixFromDx(e.clientX - startX), iyFromDy(e.clientY - startY));
    e.preventDefault();
  }

  function onUp(e) { endDrag(); }

  if (window.PointerEvent) {
    container.addEventListener("pointerdown", onDown);
    // Track and release at the window level so movement continues over other
    // content and release is never missed outside the element.
    window.addEventListener("pointermove", onMove);
    window.addEventListener("pointerup", onUp);
    window.addEventListener("pointercancel", onUp);
  } else {
    container.addEventListener("mousedown", onDown);
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  // End the drag if focus leaves the page mid-drag (released outside the
  // browser window) so it cannot stay stuck.
  window.addEventListener("blur", endDrag);

  // Reset the pointer-focus flag on blur so a later Tab focus shows the ring.
  container.addEventListener("blur", function() {
    container.classList.remove("focus-from-pointer");
  });

  // Keyboard: left/right step the x axis; up/down step the y axis (2D only).
  // A single document-level listener covers both the focused-container case
  // and the freshly-loaded (body-focused) case without double-firing.
  function onKey(e) {
    if (!ready) return;
    var active = document.activeElement;
    var focused = active === container || active === null ||
                  active === document.body;
    if (!focused) return;
    var handled = false;
    if (e.key === "ArrowLeft" || e.keyCode === 37) {
      setPos(ix - 1, iy); handled = true;
    } else if (e.key === "ArrowRight" || e.keyCode === 39) {
      setPos(ix + 1, iy); handled = true;
    } else if (ny > 1 && (e.key === "ArrowUp" || e.keyCode === 38)) {
      setPos(ix, iy - 1); handled = true;
    } else if (ny > 1 && (e.key === "ArrowDown" || e.keyCode === 40)) {
      setPos(ix, iy + 1); handled = true;
    }
    if (handled) {
      container.classList.remove("focus-from-pointer");
      e.preventDefault();
    }
  }
  document.addEventListener("keydown", onKey);
})();
</script>
</body>
</html>'

      sprintf(template,
              width, height,
              aria,
              frames_js,
              paste0("[", paste(dims, collapse = ","), "]"),
              nx, ny,
              if (loop_x) "true" else "false",
              if (loop_y) "true" else "false",
              start_ix, start_iy)
}


# Display / open ----------------------------------------------------------

#' Open a flipbook in the RStudio Viewer or system browser
#' @keywords internal
#' @noRd
show_flipbook <- function(x) {
      path <- as.character(x)
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::isAvailable()) {
            rstudioapi::viewer(path)
      } else {
            viewer <- getOption("viewer")
            if (!is.null(viewer)) {
                  viewer(path)
            } else {
                  utils::browseURL(path)
            }
      }
      invisible(x)
}


# Output class ------------------------------------------------------------

# Environment to store the last rendered flipbook
.ggcube_last_flipbook <- new.env(parent = emptyenv())
.ggcube_last_flipbook$value <- NULL

#' Create a flipbook_3d output object
#' @param path Path to the HTML file.
#' @param width,height Frame dimensions in pixels.
#' @param viewer Whether printing should open the RStudio Viewer / browser.
#' @return A `flipbook_3d` object.
#' @keywords internal
#' @noRd
flipbook_3d_obj <- function(path, width = NULL, height = NULL, viewer = TRUE) {
      structure(path,
                class = c("flipbook_3d", "character"),
                width = width,
                height = height,
                viewer = viewer)
}

#' @export
print.flipbook_3d <- function(x, ...) {
      # Honor the viewer preference captured at creation: only open the Viewer
      # / browser when the flipbook was made with viewer = TRUE. This is the
      # single place the viewer is opened (flipbook_3d() itself no longer does
      # so), so printing the object once opens it exactly once.
      if (isTRUE(attr(x, "viewer"))) {
            show_flipbook(x)
      }
      invisible(x)
}

#' @exportS3Method knitr::knit_print flipbook_3d
knit_print.flipbook_3d <- function(x, options, ...) {
      # A flipbook document can be many megabytes (base64-inlined frames).
      # Inlining that into the rendered page -- whether via a raw HTML block or
      # a srcdoc attribute -- is fragile: large payloads get silently dropped
      # by the Pandoc/Quarto pipeline. Instead, following the approach the
      # htmlwidgets ecosystem uses for large content, we write the
      # self-contained document to a sidecar file in the same managed assets
      # directory that knitr uses for figures, and reference it from an iframe
      # with an external `src`. Because the file lives where figures live, the
      # render pipeline copies it alongside the output automatically, and the
      # external reference has no size limit. The iframe still isolates the
      # flipbook's CSS/JS from the host document.
      sidecar <- flipbook_sidecar_path(options)

      if (is.null(sidecar)) {
            # No usable figure path (e.g. knitting outside a chunk context, or
            # fig.path resolved to an absolute/temp location we can't reference
            # safely). Fall back to inlining via srcdoc: less robust for very
            # large flipbooks, but keeps smaller ones working.
            html <- paste(readLines(x, warn = FALSE), collapse = "\n")
            iframe <- flipbook_iframe_html(
                  srcdoc = html,
                  width  = attr(x, "width"),
                  height = attr(x, "height")
            )
      } else {
            # Copy the already-written self-contained document to the sidecar
            # location, and reference it by its relative path.
            dir.create(dirname(sidecar$path), showWarnings = FALSE,
                       recursive = TRUE)
            file.copy(x, sidecar$path, overwrite = TRUE)
            iframe <- flipbook_iframe_html(
                  src    = sidecar$rel,
                  width  = attr(x, "width"),
                  height = attr(x, "height")
            )
      }

      # Emit as a block-level HTML element surrounded by blank lines so Pandoc
      # treats it as an HTML block and passes it through.
      knitr::asis_output(paste0("\n\n", iframe, "\n\n"), cacheable = FALSE)
}


#' Determine a sidecar file path for a flipbook in the knitr assets directory
#'
#' Uses knitr's figure-path machinery so the file lands in the same managed
#' directory as chunk figures (which the render pipeline copies alongside the
#' output). Returns both the absolute path to write to and the relative path
#' to reference from the document. Returns `NULL` if a safe relative path
#' cannot be determined -- for example outside a knitr chunk context, or when
#' the figure path has resolved to an absolute location (a known rmarkdown
#' behavior when `output_dir` is set), which would produce a broken
#' `src`.
#'
#' @param options The knitr chunk options passed to `knit_print`.
#' @return A list with `path` (write target) and `rel` (reference),
#'   or `NULL`.
#' @keywords internal
#' @noRd
flipbook_sidecar_path <- function(options = NULL) {
      if (!requireNamespace("knitr", quietly = TRUE)) return(NULL)
      # Only meaningful while knitting.
      if (!isTRUE(getOption("knitr.in.progress"))) return(NULL)

      # Ask knitr for a figure path with an .html suffix. Pass through the
      # chunk options when available so the label/number are correct.
      base <- tryCatch(
            if (is.null(options)) knitr::fig_path("html")
            else knitr::fig_path("html", options = options),
            error = function(e) NULL
      )
      if (is.null(base) || !nzchar(base)) return(NULL)

      # Disambiguate flipbooks within one chunk (a chunk may print several).
      path <- local({
            i <- getOption("ggcube.flipbook_counter", 0L) + 1L
            options(ggcube.flipbook_counter = i)
            sub("\\.html$", sprintf("-fb%d.html", i), base)
      })

      # The reference must be relative to the output document. knitr's figure
      # paths are normally already relative; if we've been handed an absolute
      # path (see rmarkdown#2024), we cannot safely turn it into a document-
      # relative URL here, so decline and let the caller fall back.
      if (is_absolute_path(path)) return(NULL)

      list(path = path, rel = path)
}


#' Is a path absolute?
#' @keywords internal
#' @noRd
is_absolute_path <- function(p) {
      grepl("^(/|~|[A-Za-z]:[/\\\\])", p)
}


#' Wrap a flipbook in an isolating iframe
#'
#' Produces an `<iframe>` that renders the flipbook in its own CSS/JS
#' scope. Exactly one of `src` or `srcdoc` must be supplied:
#' \itemize{
#'   \item `src`: a URL/relative path to an external self-contained
#'     flipbook file (the preferred, size-unlimited path used when knitting).
#'   \item `srcdoc`: the complete document inlined into the attribute
#'     (a fallback for contexts without a usable sidecar location; the
#'     document is HTML-escaped for a double-quoted attribute, ampersands
#'     first so the ampersands introduced by escaping are left intact).
#' }
#'
#' The iframe is sized explicitly from the known frame dimensions, with a few
#' pixels of vertical slack so a focus ring or rounding does not add a
#' scrollbar, and a `max-width` for responsiveness. `data-external`
#' tells Quarto to leave the iframe's content alone.
#'
#' @param src,srcdoc Exactly one must be non-NULL (see Details).
#' @param width,height Frame dimensions in pixels (may be `NULL`).
#' @return A single string containing an `<iframe>` element.
#' @keywords internal
#' @noRd
flipbook_iframe_html <- function(src = NULL, srcdoc = NULL,
                                 width = NULL, height = NULL) {
      if (is.null(src) == is.null(srcdoc)) {
            stop("Provide exactly one of `src` or `srcdoc`.", call. = FALSE)
      }
      w <- if (is.null(width))  480L else as.integer(width)
      h <- if (is.null(height)) 480L else as.integer(height)
      frame_h <- h + 8L

      if (!is.null(src)) {
            source_attr <- sprintf('src="%s"', src)
      } else {
            # Escape for a double-quoted srcdoc attribute; ampersands first.
            escaped <- gsub('"', "&quot;",
                            gsub("&", "&amp;", srcdoc, fixed = TRUE),
                            fixed = TRUE)
            source_attr <- sprintf('srcdoc="%s"', escaped)
      }

      sprintf(
            paste0('<div class="ggcube-flipbook-embed">',
                   '<iframe %s width="%d" height="%d" ',
                   'style="width:%dpx;max-width:100%%;height:%dpx;border:none;',
                   'overflow:hidden;display:block;" scrolling="no" ',
                   'data-external="1" loading="lazy" ',
                   'title="Interactive 3D flipbook"></iframe></div>'),
            source_attr, w, frame_h, w, frame_h)
}
