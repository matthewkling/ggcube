#' Create an interactive drag-to-rotate ggcube
#'
#' Creates an interactive HTML widget that you can rotate by dragging.
#' Pre-renders a set of ggcube plots across a 1D or 2D grid of rotation angles
#' and packages them as an \pkg{htmlwidgets} widget. The widget displays in the
#' RStudio Viewer, embeds in R Markdown and Quarto documents, works in Shiny
#' (see [orbit3dOutput()]), and can be written to a self-contained HTML
#' file with `file =`. When viewing it, navigate through the views by dragging
#' the image or using the keyboard arrows.
#'
#' @section Specifying rotation and lighting:
#' Each of \code{pitch}, \code{roll}, \code{yaw}, \code{light_azimuth}, and
#' \code{light_elevation} is given either as a single value (held fixed) or as
#' a length-2 range \code{c(from, to)} that is a variable, draggable axis. The
#' number of ranges determines the interaction:
#' \itemize{
#'   \item One range gives a 1D orbit: horizontal drag (or left/right
#'     keyboard arrows) vary that one parameter across its range.
#'   \item Two ranges give a 2D orbit: horizontal drag (or left/right
#'     keyboard arrows) vary one parameter while vertical drag/arrows vary
#'     the other.
#' }
#' At least one range is required. Supplying three ranges is an error.
#'
#' For a 2D orbit, the horizontal (x) and vertical (y) axes are assigned
#' from whichever parameters are ranged, by priority. The x axis takes the
#' highest-priority ranged parameter in the order \code{yaw}, \code{pitch},
#' \code{roll}, \code{light_azimuth}, \code{light_elevation}, and the y axis
#' takes the other. Camera parameters outrank light parameters, so an orbit
#' that ranges one of each puts the camera on horizontal drag and the light on
#' vertical drag. In the common case
#' \code{yaw = c(0, 360), roll = c(...)} this puts yaw on horizontal drag and
#' roll on vertical drag; \code{yaw = c(0, 360), light_elevation = c(...)}
#' spins the camera horizontally and raises the light vertically; and
#' \code{light_azimuth = c(0, 360), light_elevation = c(...)} moves only the
#' light, with azimuth horizontal and elevation vertical.
#'
#' The range direction follows the order the values are written: for example
#' \code{yaw = c(360, 0)} rotates the opposite way from \code{yaw = c(0, 360)}.
#'
#' @section Looping:
#' When a variable axis returns to its starting view (for example a full
#' \code{yaw = c(0, 360)} turn), that axis loops seamlessly: dragging past
#' either end wraps around. This is detected per axis by comparing the first
#' and last angle modulo 360, so a full-turn axis wraps while a partial range
#' (such as \code{roll = c(-90, 90)}) clamps. \code{loop} can force this on or
#' off for all axes at once.
#'
#' @section Keyboard:
#' Once the widget has focus, the arrow keys step one frame: left/right move
#' the horizontal axis, and (in a 2D orbit) up/down move the vertical axis.
#' The `r` key shows or hides the angle readout (see `readout`).
#'
#' @inheritParams animate_3d
#' @param pitch,roll,yaw Rotation angles in degrees. Each is either a single
#'   value (held fixed) or a length-2 range \code{c(from, to)} that becomes a
#'   draggable axis. For `yaw`, using a larger value for `from` than `to` gives
#'   more natural results. Values specified here replace the ones specified in
#'   `coord_3d()`. See Details.
#' @param light_azimuth,light_elevation Light direction in degrees, using the
#'   same conventions as [direction()]: azimuth is a bearing clockwise from the
#'   positive y axis, and elevation is the angle above the xy plane (between
#'   -90 and 90). Each is either a single value (held fixed) or a length-2
#'   range \code{c(from, to)} that becomes a draggable axis. When either is
#'   given, the other is held at the plot light's current value unless also
#'   given; when both are `NULL` (the default), lighting is left as the plot
#'   specifies. These move the plot-level light set with [light()] or
#'   `coord_3d(light = )`, which must be directional (not `position`-based);
#'   it is an error to use them when lighting is disabled or when any layer
#'   has its own `light`. The light's `anchor` is respected: with the default
#'   `anchor = "scene"` the light moves around the data, so orbiting `yaw` and
#'   `light_azimuth` together partly cancels out; with `anchor = "camera"` it
#'   moves around the viewer.
#' @param n Grid resolution. For a 1D orbit, a single number giving the
#'   frame count. For a 2D orbit, a length-2 vector giving the counts for
#'   the horizontal and vertical screen axes respectively (in that order, x then y).
#'   Its length must match the number of ranged parameters. Default is 36.
#' @param loop Controls seamless wraparound. \code{NULL} (default)
#'   auto-detects looping independently for each axis by checking whether that
#'   axis returns to its starting view. \code{TRUE} or \code{FALSE} force
#'   wrapping on or off for all axes.
#' @param start Initial viewing angle(s) the orbit opens at, given in
#'   degrees as a named vector matching the variable axes, e.g.
#'   \code{start = c(yaw = 30)}, \code{start = c(yaw = 30, roll = 45)}, or
#'   \code{start = c(light_azimuth = 90)}. For a
#'   1D orbit a bare scalar (e.g. \code{start = 30}) is also accepted. The
#'   orbit opens at the frame nearest each given angle. Any variable axis not
#'   named in \code{start} opens at the midpoint of its range, as does every
#'   axis when \code{start = NULL} (the default). Naming an axis that is
#'   fixed rather than variable throws an error.
#' @param max_frames Safety cap on the total number of rendered frames
#'   (\code{prod(n)}). A 2D grid can become very large; if the requested grid
#'   exceeds this cap the function errors rather than rendering. Raise it to
#'   allow larger grids. Default is 500.
#' @param file Optional path to write a self-contained HTML file (via
#'   [htmlwidgets::saveWidget()]). If `NULL` (default), no file is written and
#'   the widget is simply returned for display or embedding.
#' @param readout Logical. Whether the orbit opens with a readout in its corner
#'   showing the angles of the current view, which you can copy into
#'   [coord_3d()] to reproduce it. The readout always lists `pitch`, `roll`,
#'   and `yaw`, and adds the light's azimuth and elevation (for use with
#'   [direction()]) when the orbit moves the light. Angles are shown in the
#'   terms you gave them: an orbit over `yaw = c(0, 360)` reports values from 0
#'   to 360. This sets only how the orbit opens: pressing `r` while the orbit has focus shows
#'   or hides the readout either way. Default is `FALSE`.
#'
#' @return A `orbit_3d` htmlwidget. Printed at the console it opens in the
#'   RStudio Viewer or browser; in a knitr document it embeds automatically;
#'   in Shiny use [orbit3dOutput()] and [renderOrbit3d()].
#'
#' @examplesIf requireNamespace("base64enc", quietly = TRUE) && requireNamespace("htmlwidgets", quietly = TRUE)
#' \donttest{
#' p <- ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
#'   geom_surface_3d(linewidth = .25) +
#'   coord_3d(light = light(anchor = "camera",direction = c(1, 1, 0)),
#'            ratio = c(1.5, 2, 1)) +
#'   scale_fill_viridis_c() + scale_color_viridis_c() +
#'   theme_void()
#'
#' # 1D drag-to-rotate turntable (small n to minimize runtime)
#' orbit_3d(p, yaw = c(360, 0), n = 12)
#'
#' # 2D orbit: drag horizontally to spin (yaw), vertically to tilt (roll)
#' # (note you could increase `cores` to speed up compute time)
#' orbit_3d(p, yaw = c(360, 0), roll = c(-90, 0), n = c(20, 10),
#'             width = 1000, height = 1000, cores = 1)
#'
#' # Light orbit: drag horizontally to move the light around,
#' # vertically to raise and lower it
#' orbit_3d(p, light_azimuth = c(0, 360), light_elevation = c(0, 90),
#'             n = c(12, 4))
#'
#' # Save to a chosen path
#' orbit_3d(p, yaw = c(360, 0), n = 12,
#'             file = file.path(tempdir(), "surface.html"))
#' }
#'
#' @seealso [animate_3d()] for GIF/MP4 output.
#'   [multiview_3d()] for a static figure showing the same views side by side.
#'   [orbit_3d-shiny] for including orbits in  Shiny apps.
#' @export
orbit_3d <- function(plot,
                     pitch = NULL,
                     roll = NULL,
                     yaw = NULL,
                     light_azimuth = NULL,
                     light_elevation = NULL,
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
                     readout = FALSE) {

      # --- Validate input plot ---
      if (!inherits(plot, "gg")) {
            stop("`plot` must be a ggplot object.", call. = FALSE)
      }
      coord <- plot$coordinates
      if (!inherits(coord, "Coord3D")) {
            stop("`plot` must use coord_3d(). ",
                 "Add `+ coord_3d()` to your plot before making a orbit.",
                 call. = FALSE)
      }

      # --- Validate cores / loop ---
      cores <- as.integer(cores)
      if (cores < 1) stop("`cores` must be at least 1.", call. = FALSE)
      if (!is.null(loop) && !(is.logical(loop) && length(loop) == 1)) {
            stop("`loop` must be NULL, TRUE, or FALSE.", call. = FALSE)
      }
      if (!is.logical(readout) || length(readout) != 1 || is.na(readout)) {
            stop("`readout` must be TRUE or FALSE.", call. = FALSE)
      }

      # --- Classify rotation parameters and assign drag axes ---
      # Each parameter is absent (NULL), fixed (length 1), or a variable range
      # (length 2). The set of ranged parameters determines dimensionality and
      # the x/y axis assignment.
      spec <- resolve_orbit_axes(pitch = pitch, roll = roll, yaw = yaw,
                                 light_azimuth = light_azimuth,
                                 light_elevation = light_elevation)
      ndim <- length(spec$axes)

      # --- Resolve the light to orbit, if any ---
      orbited <- resolve_light_angles(plot, light_azimuth, light_elevation)
      light <- orbited$light
      light_angles <- orbited$angles

      # --- Validate n against dimensionality, and enforce the frame cap ---
      n <- validate_grid_n(n, ndim)
      total <- prod(n)
      if (total > max_frames) {
            stop(sprintf(
                  paste0("This orbit would render %d frames (%s), exceeding ",
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
            start = start,
            light_angles = light_angles
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
            prefix = "ggcube_flip_",
            light = light
      )

      message("Assembling orbit (press `r` on the orbit to toggle its angle readout)...")

      # --- Encode frames and build the htmlwidget ---
      # The frame/parameter logic above is unchanged; only the output layer
      # differs. Frames are base64-encoded into data URIs and handed to the
      # widget along with the grid topology (`dims`), per-axis wraparound
      # (`loop_flags`), and the initial cell (`start`). htmlwidgets then
      # handles all rendering contexts (RStudio Viewer, R Markdown / Quarto
      # via its own knit_print, and Shiny) and, for `file=`, self-contained
      # HTML via saveWidget().
      if (!requireNamespace("base64enc", quietly = TRUE)) {
            stop("The base64enc package is required to build a orbit. ",
                 "Install it with install.packages('base64enc').",
                 call. = FALSE)
      }
      data_uris <- vapply(frames$files, function(f) {
            paste0("data:image/png;base64,", base64enc::base64encode(f))
      }, character(1), USE.NAMES = FALSE)

      # Frames are now inlined as data URIs; the temp PNGs are no longer needed.
      unlink(frames$tmpdir, recursive = TRUE)

      x <- list(
            frames = as.list(data_uris),
            dims = as.list(as.integer(dims)),
            loop_flags = as.list(as.logical(loop_flags)),
            start = as.list(as.integer(start_cell)),
            labels = as.list(orbit_readout_labels(param_seqs)),
            readout = readout
      )

      widget <- htmlwidgets::createWidget(
            name = "orbit_3d",
            x = x,
            width = frames$width,
            height = frames$height,
            package = "ggcube",
            sizingPolicy = htmlwidgets::sizingPolicy(
                  viewer.padding = 0,
                  browser.fill = FALSE,
                  knitr.figure = FALSE
            )
      )

      # If a file path is requested, write a self-contained HTML file. This
      # preserves the single-portable-file output of earlier versions.
      #
      # saveWidget(selfcontained = TRUE) has a long-standing quirk: it inlines
      # dependencies into the HTML but then fails to remove the temporary
      # "<name>_files" directory when the output path is not the current working
      # directory (htmlwidgets issue #296), leaving a stray sidecar folder next
      # to an otherwise self-contained file. Running saveWidget from within the
      # output directory, with a bare filename, makes its cleanup path resolve
      # correctly and yields a true single file.
      if (!is.null(file)) {
            file <- normalizePath(file, mustWork = FALSE)
            out_dir <- dirname(file)
            out_base <- basename(file)
            if (!dir.exists(out_dir)) {
                  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
            }
            old_wd <- setwd(out_dir)
            on.exit(setwd(old_wd), add = TRUE)
            htmlwidgets::saveWidget(widget, out_base, selfcontained = TRUE)
      }

      widget
}


# Axis resolution & grid construction -------------------------------------

#' Classify rotation parameters and assign drag axes
#'
#' Each of pitch/roll/yaw/light_azimuth/light_elevation is absent (NULL), fixed
#' (length 1), or a variable range (length 2). Ranged parameters become
#' draggable axes; the x axis takes the highest-priority ranged parameter in
#' order yaw > pitch > roll > light_azimuth > light_elevation, and the y axis
#' takes the other ranged parameter, if any. Fixed values are recorded to be
#' held constant.
#'
#' @param pitch,roll,yaw,light_azimuth,light_elevation User-supplied orbit
#'   arguments.
#' @return A list with \code{axes} (character vector of ranged parameter
#'   names, in x-then-y order), \code{ranges} (named list of the length-2
#'   ranges for those axes), and \code{fixed} (named list of scalar values for
#'   parameters the user pinned to a single value).
#' @keywords internal
#' @noRd
resolve_orbit_axes <- function(pitch = NULL, roll = NULL, yaw = NULL,
                               light_azimuth = NULL, light_elevation = NULL) {
      args <- list(pitch = pitch, roll = roll, yaw = yaw,
                   light_azimuth = light_azimuth,
                   light_elevation = light_elevation)

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
            stop(paste0("A orbit needs at least one rotation parameter given ",
                        "as a range c(from, to). For example, yaw = c(0, 360)."),
                 call. = FALSE)
      }
      if (length(ranged) > 2) {
            stop(sprintf(
                  paste0("%d ranged parameters were given (%s). A orbit ",
                         "supports at most two draggable axes; for motion through ",
                         "more parameters use animate_3d()."),
                  length(ranged), paste0("`", ranged, "`", collapse = ", ")),
                 call. = FALSE)
      }

      # The x axis takes the highest-priority ranged parameter and the y axis
      # takes the other, if any. Camera parameters outrank light parameters, so
      # a mixed orbit puts the camera on horizontal drag and the light on
      # vertical.
      x_priority <- c("yaw", "pitch", "roll", "light_azimuth", "light_elevation")
      x_axis <- x_priority[x_priority %in% ranged][1]

      axes <- c(x_axis, setdiff(ranged, x_axis))
      ranges <- args[axes]

      list(axes = axes, ranges = ranges, fixed = fixed)
}


#' Validate grid counts against the number of ranged parameters
#'
#' @param n User-supplied counts, one per ranged parameter (x then y).
#' @param ndim Number of ranged parameters.
#' @return \code{n} as an integer vector.
#' @keywords internal
#' @noRd
validate_grid_n <- function(n, ndim) {
      n <- suppressWarnings(as.integer(n))
      if (length(n) == 0 || any(is.na(n)) || any(n < 1)) {
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
      n
}


#' Build the flattened parameter grid for a orbit
#'
#' Produces one 1D range per variable axis (with per-axis loop detection and
#' duplicate-endpoint handling), then crosses the axes row-major with the x
#' axis varying fastest. Fixed and absent parameters are held constant.
#'
#' @param coord The plot's Coord3D object (source of held-constant defaults).
#' @param spec Output of resolve_orbit_axes().
#' @param n Integer resolution vector, one entry per axis, in x-then-y order.
#' @param loop \code{NULL} to auto-detect per axis, or a single logical to
#'   force all axes.
#' @param light_angles \code{NULL} when the light is not orbited, or a list
#'   with \code{azimuth} and \code{elevation} giving the plot light's current
#'   angles, used to hold unspecified light parameters constant.
#' @return A list with \code{param_seqs} (flat data.frame of pitch/roll/yaw,
#'   plus light_azimuth/light_elevation when \code{light_angles} is given,
#'   one row per frame, row-major with x fastest), \code{dims} (integer vector
#'   of axis lengths after endpoint handling), and \code{loop_flags} (logical
#'   vector, one per axis).
#' @keywords internal
#' @noRd
build_param_grid <- function(coord, spec, n, loop, start = NULL,
                             light_angles = NULL) {
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
      # axis parameters with their variable values. Camera parameters default
      # to the coord's angles; light parameters, present only when the light
      # is orbited, default to the plot light's current angles.
      const <- function(nm, default) {
            if (!is.null(spec$fixed[[nm]])) spec$fixed[[nm]] else default
      }
      out <- data.frame(
            pitch = rep(const("pitch", coord$pitch), total),
            roll  = rep(const("roll",  coord$roll),  total),
            yaw   = rep(const("yaw",   coord$yaw),   total)
      )
      if (!is.null(light_angles)) {
            out$light_azimuth <- rep(const("light_azimuth",
                                           light_angles$azimuth), total)
            out$light_elevation <- rep(const("light_elevation",
                                             light_angles$elevation), total)
      }

      out[[axes[1]]] <- axis_values[[1]][grid_idx$x]
      if (length(axes) == 2) {
            out[[axes[2]]] <- axis_values[[2]][grid_idx$y]
      }

      rownames(out) <- NULL

      # Determine the 0-based starting cell per axis from the requested start
      # angles (or the midpoint when unspecified).
      start_cell <- orbit_start_cell(axis_values, axes, start)

      list(param_seqs = out, dims = dims, loop_flags = loop_flags,
           start = start_cell)
}


#' Build the per-frame text for an orbit's angle readout
#'
#' One label per row of \code{param_seqs}: the camera angles on one line, and,
#' when the orbit moves the light, the light angles on a second. Angles are
#' shown as sampled, in the terms of the range the orbit was given.
#'
#' @param param_seqs Flat per-frame parameter data.frame from build_param_grid().
#' @return Character vector of labels, one per frame.
#' @keywords internal
#' @noRd
orbit_readout_labels <- function(param_seqs) {
      labels <- paste0("pitch ", format_angle(param_seqs$pitch),
                       "  roll ", format_angle(param_seqs$roll),
                       "  yaw ", format_angle(param_seqs$yaw))

      if (!is.null(param_seqs$light_azimuth)) {
            labels <- paste0(labels,
                             "\nlight azimuth ", format_angle(param_seqs$light_azimuth),
                             "  elevation ", format_angle(param_seqs$light_elevation))
      }

      labels
}


#' Format angles for display
#'
#' Rounds to one decimal, drops a trailing ".0", and avoids "-0". Angles are not
#' wrapped: they come from ranges the user gave, and wrapping them individually
#' would break up a sweep that passes the wrapping point (0, 90, 180, 270 would
#' read 0, 90, -180, -90).
#' @param x Numeric angles in degrees.
#' @return Character vector.
#' @keywords internal
#' @noRd
format_angle <- function(x) {
      x <- round(x, 1)
      x[x == 0] <- 0
      sub("\\.0$", "", sprintf("%.1f", x))
}


#' Choose the orbit's initial frame cell from requested start angles
#'
#' Returns a 0-based index per variable axis for the frame the player shows first.
#' \code{start} is an optional set of viewing angles in degrees: a named vector
#' whose names are variable axis names (e.g. \code{c(yaw = 30, roll = 45)}), or, for
#' a 1D orbit, a bare scalar applied to the single variable axis. For each variable
#' axis named in \code{start}, the orbit opens at the frame nearest that angle
#' (nearest by circular distance, modulo 360). Any variable axis not named opens at
#' the index midpoint of its range, as does every axis when \code{start} is
#' \code{NULL}.
#'
#' @param axis_values List of per-axis sampled angle vectors.
#' @param axes Character vector of variable axis names (x then y).
#' @param start \code{NULL}, a named numeric vector of angles, or (1D only) a
#'   bare scalar.
#' @return Integer vector of 0-based cell indices, one per axis.
#' @keywords internal
#' @noRd
orbit_start_cell <- function(axis_values, axes, start = NULL) {
      midpoint <- function() {
            vapply(axis_values, function(v) as.integer(floor(length(v) / 2)),
                   integer(1))
      }

      if (is.null(start)) return(midpoint())

      # Normalize `start` to a named list of angles keyed by axis.
      if (is.null(names(start))) {
            # Unnamed: only valid as a single value for a 1D orbit.
            if (length(axes) == 1 && length(start) == 1) {
                  targets <- stats::setNames(list(start[[1]]), axes)
            } else {
                  stop("`start` must be a named vector of angles (e.g. ",
                       "c(yaw = 30)); a bare value is only allowed for a 1D ",
                       "orbit.", call. = FALSE)
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


# Shiny bindings ----------------------------------------------------------

#' Shiny bindings for orbit_3d
#'
#' Output and render functions for using [orbit_3d()] within Shiny
#' applications and interactive R Markdown documents.
#'
#' @param outputId Output variable to read from.
#' @param width,height Must be a valid CSS unit (like `"100%"`, `"400px"`,
#'   `"auto"`) or a number, which will be coerced to a string and have `"px"`
#'   appended.
#' @param expr An expression that generates a orbit (a call to
#'   [orbit_3d()]).
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression (with `quote()`)? This is useful
#'   if you want to save an expression in a variable.
#'
#' @examplesIf interactive() && requireNamespace("shiny", quietly = TRUE) && requireNamespace("base64enc", quietly = TRUE)
#' library(shiny)
#'
#' p <- ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
#'   geom_surface_3d(linewidth = .25) +
#'   coord_3d(light = light(anchor = "camera", direction = c(1, 1, 0)),
#'            ratio = c(1.5, 2, 1)) +
#'   scale_fill_viridis_c() + scale_color_viridis_c() +
#'   theme_void()
#'
#' ui <- fluidPage(
#'   titlePanel("Interactive ggcube"),
#'   orbit3dOutput("fb", width = "500px", height = "500px")
#' )
#'
#' server <- function(input, output, session) {
#'   output$fb <- renderOrbit3d({
#'     orbit_3d(p, yaw = c(360, 0), n = 12)
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' @name orbit_3d-shiny
#' @export
orbit3dOutput <- function(outputId, width = "100%", height = "400px") {
      htmlwidgets::shinyWidgetOutput(outputId, "orbit_3d", width, height,
                                     package = "ggcube")
}

#' @rdname orbit_3d-shiny
#' @export
renderOrbit3d <- function(expr, env = parent.frame(), quoted = FALSE) {
      if (!quoted) expr <- substitute(expr)
      htmlwidgets::shinyRenderWidget(expr, orbit3dOutput, env,
                                     quoted = TRUE)
}
