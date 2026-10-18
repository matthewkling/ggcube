#' Animate a 3D plot
#'
#' Renders an animated GIF or MP4 of a rotating ggplot with [coord_3d()],
#' smoothly interpolating rotation angles across frames.
#'
#' @param plot A ggplot object that uses [coord_3d()].
#' @param pitch,roll,yaw Rotation parameters to animate. Each can be:
#'   \itemize{
#'     \item \code{NULL} (default): Hold at the value specified in the plot's
#'       [coord_3d()] call.
#'     \item A single numeric value: Hold at this value for all frames
#'       (overriding the plot's coord).
#'     \item A numeric vector of length 2+: Keyframe values that are
#'       interpolated linearly across the animation. Keyframes are evenly
#'       spaced, so \code{c(0, 180, 180)} with 100 frames means: start at 0,
#'       reach 180 at frame 50, hold at 180 until frame 100.
#'   }
#' @param light_azimuth,light_elevation Light direction parameters to animate,
#'   in degrees, using the same conventions as [direction()]: azimuth is a
#'   bearing clockwise from the positive y axis, and elevation is the angle
#'   above the xy plane (between -90 and 90). Each takes the same forms as the
#'   rotation parameters. When either is given, the other is held at the plot
#'   light's current value unless also given. When both are `NULL` (the
#'   default), lighting is left as the plot specifies. These move the
#'   plot-level light set with [light()] or `coord_3d(light = )`, which must be
#'   directional (not `position`-based); it is an error to use them when
#'   lighting is disabled or when any layer has its own `light`. The light's
#'   `anchor` is respected: with the default `anchor = "scene"` the light
#'   moves around the data, so animating `yaw` and `light_azimuth` together
#'   partly cancels out; with `anchor = "camera"` it moves around the viewer.
#' @param nframes,fps,duration Control animation length and speed. Specify
#'   any two of three; the third is computed as
#'   \code{duration = nframes / fps}. Defaults: \code{nframes = 100},
#'   \code{fps = 10}.
#' @param width,height Dimensions of the animation in pixels. Defaults to
#'   480 x 480.
#' @param res Resolution in ppi for the rendered frames. Default is 96.
#'   Controls the size of point-based text/lines/point elements relative to
#'   the plot. See `?grDevices::png` for details.
#' @param renderer A renderer function that combines image frames into
#'   an animation file. Built-in options:
#'   \itemize{
#'     \item \code{gifski_renderer_3d()} (default): Renders a GIF using the
#'       \pkg{gifski} package.
#'     \item \code{av_renderer_3d()}: Renders a video using the \pkg{av}
#'       package.
#'     \item \code{file_renderer_3d()}: Returns the frame image files
#'       without combining them.
#'   }
#'   Any function with signature \code{function(frames, fps)} can be used.
#' @param start_pause,end_pause Number of frames to hold at the beginning
#'   and end of the animation. Default is 0.
#' @param rewind Logical. If \code{TRUE}, the animation plays in reverse
#'   after completing, creating a seamless loop. Default is \code{FALSE}.
#' @param cores Number of CPU cores for parallel frame rendering. Default
#'   is 1 (sequential). Values greater than 1 use [parallel::parLapply()]
#'   to render frames in parallel, which can significantly speed up
#'   animation of complex plots. Note that progress reporting is not
#'   available during parallel rendering.
#' @param device The graphics device to use for rendering frames.
#'   Default is \code{"png"}. Other options include \code{"ragg_png"}
#'   (requires the \pkg{ragg} package).
#' @param progress Whether to print a progress bar. Ignored when `cores > 1`.
#'
#' @return The return value of the renderer function. For
#'   \code{gifski_renderer_3d()}, a \code{gif_3d} object (a file path with
#'   class attributes for display in RStudio and knitr). For
#'   \code{av_renderer_3d()}, a \code{video_3d} object (a file path with
#'   class attributes for display). For \code{file_renderer_3d()}, a
#'   character vector of file paths to the rendered frame images.
#'
#' @examplesIf requireNamespace("gifski", quietly = TRUE)
#' \donttest{
#' # Plot to animate. Note `anchor = "camera"` keeps light source from rotating.
#' p <- ggplot() +
#'   geom_function_3d(
#'     fun = function(x, y) sin(x) * cos(y),
#'     xlim = c(-pi, pi), ylim = c(-pi, pi),
#'     fill = "steelblue", color = "steelblue", linewidth = .5) +
#'   coord_3d(light = light(mode = "hsl", anchor = "camera",
#'                          direction = c(1, 1, 0))) +
#'   theme_void()
#'
#' # Simple turntable rotation
#' # (using small `nframes` to minimize runtime)
#' animate_3d(p, yaw = c(-30, 330), nframes = 12)
#'
#' # Multi-segment orbit with pitch change
#' animate_3d(p, yaw = c(0, 720), roll = c(-90, 0, -90),
#'            nframes = 12, fps = 15)
#'
#' # Sweep the light while the camera holds still
#' animate_3d(p, light_azimuth = c(0, 360), light_elevation = 30,
#'            nframes = 12)
#'
#' # Save to file (writes to a temporary path; specify your own to keep it)
#' anim <- animate_3d(p, yaw = c(0, 360), nframes = 12)
#' anim_save_3d(anim, file.path(tempdir(), "rotating_surface.gif"))
#' }
#'
#' @seealso [anim_save_3d()] for saving animations to file.
#'   [gifski_renderer_3d()] and [file_renderer_3d()] for renderer options.
#'   [orbit_3d()] for interactive drag-to-rotate ggcubes.
#'   [facet_wrap_views()] and [facet_grid_views()] for a static figure showing
#'   several views side by side.
#' @export
animate_3d <- function(plot,
                       pitch = NULL,
                       roll = NULL,
                       yaw = NULL,
                       light_azimuth = NULL,
                       light_elevation = NULL,
                       nframes = NULL,
                       fps = NULL,
                       duration = NULL,
                       width = 480,
                       height = 480,
                       res = 96,
                       renderer = gifski_renderer_3d(),
                       start_pause = 0,
                       end_pause = 0,
                       rewind = FALSE,
                       cores = 1,
                       device = "png",
                       progress = interactive()) {

      # --- Validate input plot ---
      if (!inherits(plot, "gg")) {
            stop("`plot` must be a ggplot object.", call. = FALSE)
      }
      coord <- plot$coordinates
      if (!inherits(coord, "Coord3D")) {
            stop("`plot` must use coord_3d(). ",
                 "Add `+ coord_3d()` to your plot before animating.",
                 call. = FALSE)
      }

      # --- Validate cores ---
      cores <- as.integer(cores)
      if (cores < 1) stop("`cores` must be at least 1.", call. = FALSE)

      # --- Resolve nframes / fps / duration ---
      timing <- resolve_timing(nframes, fps, duration)
      nframes <- timing$nframes
      fps <- timing$fps

      check_view_facet_angles(plot, list(pitch = pitch, roll = roll, yaw = yaw,
                                         light_azimuth = light_azimuth,
                                         light_elevation = light_elevation),
                              "animate_3d")

      # --- Resolve the light to animate, if any ---
      animated <- resolve_light_angles(plot, light_azimuth, light_elevation)
      light <- animated$light
      light_angles <- animated$angles

      # --- Build per-frame parameter sequences ---
      param_seqs <- build_param_sequences(
            coord = coord,
            pitch = pitch,
            roll = roll,
            yaw = yaw,
            nframes = nframes,
            light_azimuth = light_azimuth,
            light_elevation = light_elevation,
            light_angles = light_angles
      )

      # --- Apply start_pause, end_pause, rewind ---
      param_seqs <- apply_pause_and_rewind(param_seqs, start_pause, end_pause,
                                           rewind)

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
            prefix = "ggcube_anim_",
            light = light
      )

      message("Assembling animation...")

      # --- Combine frames via renderer ---
      result <- renderer(frames$files, fps, width, height)

      # Clean up temp files (unless using file_renderer)
      if (!inherits(result, "frame_files_3d")) {
            unlink(frames$tmpdir, recursive = TRUE)
      }

      # Stash as last animation for anim_save_3d()
      .ggcube_last_anim$value <- result

      result
}


# Frame rendering ---------------------------------------------------------

#' Render a sequence of camera states to PNG frame files
#'
#' Shared rendering core for [animate_3d()] and [orbit_3d()]. Takes a
#' fully-resolved sequence of per-frame rotation parameters (one row per
#' frame) and renders each to a PNG on disk, computing a single fixed
#' viewport across all frames so the plot does not rescale between rotations.
#'
#' This is the \code{[resolved camera states -> image files]} seam: callers
#' are responsible for producing \code{param_seqs} (including any
#' pause/rewind/keyframe expansion), and receive back the list of rendered
#' files plus the temp directory holding them.
#'
#' @param plot A ggplot object using [coord_3d()].
#' @param coord The plot's Coord3D object (\code{plot$coordinates}).
#' @param param_seqs A data.frame with columns \code{pitch}, \code{roll},
#'   \code{yaw}, plus \code{light_azimuth} and \code{light_elevation} when the
#'   light is animated; one row per frame.
#' @param width,height,res,device,cores,progress As in [animate_3d()].
#' @param prefix Prefix for the temporary frame directory.
#' @param light \code{NULL} to leave lighting as the plot specifies, or the
#'   base `light` object (from resolve_animated_light()) whose direction is
#'   set per frame from the light columns of \code{param_seqs}.
#'
#' @return A list with elements \code{files} (character vector of PNG paths),
#'   \code{tmpdir} (the directory containing them), and \code{width},
#'   \code{height}.
#' @keywords internal
#' @noRd
render_3d_frames <- function(plot, coord, param_seqs, width, height, res,
                             device, cores, progress,
                             prefix = "ggcube_frames_", light = NULL) {
      total_frames <- nrow(param_seqs)

      # --- Set up output directory ---
      tmpdir <- tempfile(prefix)
      dir.create(tmpdir)
      frame_files <- file.path(tmpdir,
                               sprintf("frame_%04d.png", seq_len(total_frames)))

      # Extract static coord params to carry through every frame
      static_params <- extract_static_coord_params(coord)

      # --- Compute one fixed viewport across all frames ---
      # This prevents the plot from rescaling at each rotation angle.
      global_bounds <- compute_frame_bounds(plot, static_params, param_seqs)

      # --- Per-frame lighting ---
      prepared <- prepare_frame_lights(plot, light, param_seqs)
      plot <- prepared$plot
      frame_lights <- prepared$frame_lights

      # --- Render frames ---
      if (cores > 1) {
            render_frames_parallel(plot, param_seqs, static_params, global_bounds,
                                   frame_files, width, height, res, device, cores,
                                   frame_lights)
      } else {
            render_frames_sequential(plot, param_seqs, static_params, global_bounds,
                                     frame_files, width, height, res, device,
                                     progress, frame_lights)
      }

      list(files = frame_files, tmpdir = tmpdir, width = width, height = height)
}


#' Render frames sequentially with progress bar
#' @keywords internal
#' @noRd
render_frames_sequential <- function(plot, param_seqs, static_params,
                                     global_bounds, frame_files,
                                     width, height, res, device, progress,
                                     frame_lights = NULL) {
      total_frames <- nrow(param_seqs)
      message(sprintf("Rendering %d frames...", total_frames))

      pb <- NULL
      if (progress && requireNamespace("progress", quietly = TRUE)) {
            pb <- progress::progress_bar$new(
                  format = "  frame :current/:total [:bar] :percent eta: :eta",
                  total = total_frames, clear = FALSE, width = 60
            )
      }

      for (i in seq_len(total_frames)) {
            if (!is.null(pb)) pb$tick()
            render_single_frame(plot, param_seqs, static_params, global_bounds,
                                frame_files[i], i, width, height, res, device,
                                frame_lights[[i]])
      }
}


#' Render frames in parallel using a socket cluster
#' @keywords internal
#' @noRd
render_frames_parallel <- function(plot, param_seqs, static_params,
                                   global_bounds, frame_files,
                                   width, height, res, device, cores,
                                   frame_lights = NULL) {
      total_frames <- nrow(param_seqs)
      cores <- min(cores, total_frames)
      message(sprintf("Rendering %d frames across %d cores...",
                      total_frames, cores))

      cl <- parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      # Load ggcube (which imports ggplot2) on each worker
      parallel::clusterEvalQ(cl, {
            library(ggcube)
      })

      # Export objects needed by the worker function. The z scale rides along
      # inside `plot`, so each worker's own build resolves it.
      parallel::clusterExport(cl, c("plot", "param_seqs", "static_params",
                                    "global_bounds", "frame_files",
                                    "width", "height", "res",
                                    "device", "frame_lights"),
                              envir = environment())

      # Render in parallel. The worker function is defined inline so that
      # it closes over the exported variables directly, avoiding the need
      # to locate unexported package functions from the worker processes.
      parallel::parLapply(cl, seq_len(total_frames), function(i) {
            frame_coord_args <- modifyList(
                  static_params,
                  list(
                        pitch = param_seqs$pitch[i],
                        roll = param_seqs$roll[i],
                        yaw = param_seqs$yaw[i],
                        fixed_bounds = global_bounds
                  )
            )
            if (!is.null(frame_lights)) {
                  frame_coord_args$light <- frame_lights[[i]]
            }
            frame_coord <- do.call(ggcube::coord_3d, frame_coord_args)

            frame_plot <- plot
            frame_plot$coordinates <- frame_coord[[1]]

            device_fun <- switch(device,
                                 png = grDevices::png,
                                 ragg_png = ragg::agg_png,
                                 stop("Unsupported device: ", device, call. = FALSE)
            )

            device_fun(filename = frame_files[i],
                       width = width, height = height, res = res)
            on.exit(grDevices::dev.off(), add = TRUE)
            print(frame_plot)
      })

      invisible(NULL)
}


#' Render a single animation frame
#'
#' Builds the coord for frame \code{i}, swaps it onto the plot, and writes
#' the PNG. Used by both sequential and parallel rendering paths.
#' @keywords internal
#' @noRd
render_single_frame <- function(plot, param_seqs, static_params, global_bounds,
                                file, i, width, height, res, device,
                                frame_light = NULL) {
      frame_plot <- build_frame_plot(plot, param_seqs, static_params,
                                     global_bounds, i, frame_light)
      render_frame(frame_plot, file, width, height, res, device)
}


#' Build the plot for a single animation frame
#'
#' Swaps a coord carrying frame \code{i}'s rotation (and, when the light is
#' animated, its light) onto the plot.
#' @keywords internal
#' @noRd
build_frame_plot <- function(plot, param_seqs, static_params, global_bounds,
                             i, frame_light = NULL) {
      frame_coord_args <- modifyList(
            static_params,
            list(
                  pitch = param_seqs$pitch[i],
                  roll = param_seqs$roll[i],
                  yaw = param_seqs$yaw[i],
                  fixed_bounds = global_bounds
            )
      )
      # Assigned directly rather than through modifyList(), which would merge a
      # light spec field by field into the static one instead of replacing it.
      if (!is.null(frame_light)) {
            frame_coord_args$light <- frame_light
      }
      frame_coord <- do.call(coord_3d, frame_coord_args)

      # coord_3d() returns a list of (ggproto, theme); we only need the
      # ggproto since the theme is already applied from the original plot.
      frame_plot <- plot
      frame_plot$coordinates <- frame_coord[[1]]
      frame_plot
}


# Animated lighting --------------------------------------------------------

#' Resolve the plot-level light that animated light angles act on
#'
#' Light angles can only move a directional light that applies to the whole
#' plot, so this errors when lighting is disabled, when the light is
#' positional, or when any layer carries its own light (which would ignore the
#' animated one).
#' @param plot A ggplot object using [coord_3d()].
#' @return A `light` object.
#' @keywords internal
#' @noRd
resolve_animated_light <- function(plot) {
      layer_lit <- vapply(plot$layers,
                          function(layer) !is.null(layer$stat_params$light),
                          logical(1))
      if (any(layer_lit)) {
            stop("`light_azimuth` and `light_elevation` move the plot-level ",
                 "light, but layer ", paste(which(layer_lit), collapse = ", "),
                 " ", if (sum(layer_lit) == 1) "sets its" else "set their",
                 " own `light`, which would not respond. Remove the layer-level ",
                 "`light` to animate lighting.", call. = FALSE)
      }

      pending <- attr(plot, "ggcube_pending_light")
      base <- resolve_light(if (!is.null(pending)) pending else plot$coordinates$light)

      if (is.null(base) || identical(base, "none") ||
          (inherits(base, "light") && identical(base$shade, "neither"))) {
            stop("`light_azimuth` and `light_elevation` require lighting, but ",
                 "lighting is disabled for this plot.", call. = FALSE)
      }
      if (!is.null(base$position)) {
            stop("`light_azimuth` and `light_elevation` require a directional ",
                 "light, but this plot's light uses `position`.", call. = FALSE)
      }

      base
}


#' Refuse to move angles that a view facet sets
#'
#' facet_wrap_views() and facet_grid_views() set the angles they vary, and any
#' they fix, for each panel, overriding the coord. Moving one of those angles in
#' an animation or orbit would silently do nothing.
#' @param plot A ggplot object.
#' @param args Named list of the angle arguments given to the caller.
#' @param fun Name of the calling function, for the error message.
#' @keywords internal
#' @noRd
check_view_facet_angles <- function(plot, args, fun) {
      spec <- plot$facet$params$view_spec
      if (is.null(spec)) return(invisible())
      set_by_facet <- c(spec$angles, names(spec$fixed))
      given <- names(args)[!vapply(args, is.null, logical(1))]
      clash <- intersect(given, set_by_facet)
      if (length(clash) > 0) {
            stop(sprintf(paste0(
                  "This plot's view facet sets %s, so %s() can't move %s. ",
                  "Move angles the facet doesn't set, or remove the facet."),
                  paste0("`", clash, "`", collapse = ", "), fun,
                  if (length(clash) == 1) "it" else "them"),
                 call. = FALSE)
      }
      invisible()
}


#' Resolve the light to move, if any light angle was given
#'
#' @param plot A ggplot object using [coord_3d()].
#' @param light_azimuth,light_elevation User-supplied light angle arguments.
#' @return A list with \code{light} (the base `light` object, or \code{NULL}
#'   when no light angle was given) and \code{angles} (its current azimuth and
#'   elevation, used to hold unspecified light angles constant, or
#'   \code{NULL}).
#' @keywords internal
#' @noRd
resolve_light_angles <- function(plot, light_azimuth, light_elevation) {
      if (is.null(light_azimuth) && is.null(light_elevation)) {
            return(list(light = NULL, angles = NULL))
      }
      validate_light_elevation(light_elevation)
      light <- resolve_animated_light(plot)
      list(light = light, angles = direction_angles(light$direction))
}


#' Check that animated light elevations are valid angles
#' @keywords internal
#' @noRd
validate_light_elevation <- function(light_elevation) {
      if (!is.null(light_elevation) &&
          (!is.numeric(light_elevation) || any(!is.finite(light_elevation)) ||
           any(light_elevation < -90 | light_elevation > 90))) {
            stop("`light_elevation` must be between -90 and 90 degrees.",
                 call. = FALSE)
      }
      invisible(light_elevation)
}


#' Prepare a plot and per-frame light specs for animated lighting
#'
#' When the light is animated, each frame carries its own light spec on its
#' coord, built from the base light with its direction set from that frame's
#' light angles. The plot-level `+ light()` stash is cleared so that it cannot
#' collide with the coord's now-explicit light.
#' @param plot A ggplot object using [coord_3d()].
#' @param light \code{NULL} when the light is not animated, or the base `light`
#'   object from resolve_animated_light().
#' @param param_seqs Per-frame parameters, including \code{light_azimuth} and
#'   \code{light_elevation} columns when \code{light} is given.
#' @return A list with \code{plot} and \code{frame_lights} (a list of `light`
#'   objects, one per frame, or \code{NULL} when the light is not animated).
#' @keywords internal
#' @noRd
prepare_frame_lights <- function(plot, light, param_seqs) {
      if (is.null(light)) {
            return(list(plot = plot, frame_lights = NULL))
      }

      frame_lights <- lapply(seq_len(nrow(param_seqs)), function(i) {
            frame_light <- light
            frame_light$direction <- direction(param_seqs$light_azimuth[i],
                                               param_seqs$light_elevation[i])
            frame_light
      })
      attr(plot, "ggcube_pending_light") <- NULL

      list(plot = plot, frame_lights = frame_lights)
}


# Internal helpers ---------------------------------------------------------

#' Resolve nframes / fps / duration (any 2 of 3)
#' @keywords internal
#' @noRd
resolve_timing <- function(nframes, fps, duration) {
      provided <- c(!is.null(nframes), !is.null(fps), !is.null(duration))

      if (sum(provided) > 2) {
            stop("Specify at most two of `nframes`, `fps`, and `duration`.",
                 call. = FALSE)
      }

      # Apply defaults when fewer than 2 are specified
      if (sum(provided) == 0) {
            nframes <- 100
            fps <- 10
      } else if (sum(provided) == 1) {
            if (is.null(fps)) fps <- 10
            if (is.null(nframes) && is.null(duration)) nframes <- 100
      }

      # Compute the missing value
      if (is.null(duration)) {
            duration <- nframes / fps
      } else if (is.null(nframes)) {
            nframes <- round(duration * fps)
      } else if (is.null(fps)) {
            fps <- nframes / duration
      }

      if (nframes < 1) stop("`nframes` must be at least 1.", call. = FALSE)
      if (fps <= 0) stop("`fps` must be positive.", call. = FALSE)

      list(nframes = as.integer(nframes), fps = fps, duration = duration)
}


#' Build parameter sequences for each frame
#' @keywords internal
#' @noRd
build_param_sequences <- function(coord, pitch, roll, yaw, nframes,
                                  light_azimuth = NULL, light_elevation = NULL,
                                  light_angles = NULL) {

      make_seq <- function(user_val, coord_val, n) {
            if (is.null(user_val)) {
                  # Use coord's value for all frames
                  rep(coord_val, n)
            } else if (length(user_val) == 1) {
                  # Single value: hold constant
                  rep(user_val, n)
            } else {
                  # Multiple values: piecewise linear interpolation
                  # Keyframes are evenly spaced across the animation
                  n_keyframes <- length(user_val)
                  keyframe_positions <- seq(1, n, length.out = n_keyframes)

                  # Interpolate to all frames
                  stats::approx(
                        x = keyframe_positions,
                        y = user_val,
                        xout = seq_len(n),
                        method = "linear"
                  )$y
            }
      }

      out <- data.frame(
            pitch = make_seq(pitch, coord$pitch, nframes),
            roll  = make_seq(roll,  coord$roll,  nframes),
            yaw   = make_seq(yaw,   coord$yaw,   nframes)
      )

      # Light columns are added only when the light is animated, holding any
      # unspecified light angle at the plot light's current value.
      if (!is.null(light_angles)) {
            out$light_azimuth <- make_seq(light_azimuth, light_angles$azimuth,
                                          nframes)
            out$light_elevation <- make_seq(light_elevation,
                                            light_angles$elevation, nframes)
      }

      out
}


#' Apply start_pause, end_pause, and rewind to parameter sequences
#' @keywords internal
#' @noRd
apply_pause_and_rewind <- function(param_seqs, start_pause, end_pause, rewind) {

      if (rewind) {
            # Append reversed frames (excluding the endpoints to avoid stutter)
            reversed <- param_seqs[rev(seq_len(nrow(param_seqs))), , drop = FALSE]
            if (nrow(reversed) > 2) {
                  reversed <- reversed[-c(1, nrow(reversed)), , drop = FALSE]
            }
            param_seqs <- rbind(param_seqs, reversed)
      }

      if (start_pause > 0) {
            first_row <- param_seqs[rep(1, start_pause), , drop = FALSE]
            param_seqs <- rbind(first_row, param_seqs)
      }

      if (end_pause > 0) {
            last_row <- param_seqs[rep(nrow(param_seqs), end_pause), , drop = FALSE]
            param_seqs <- rbind(param_seqs, last_row)
      }

      rownames(param_seqs) <- NULL
      param_seqs
}


#' Compute global plot bounds across all animation frames
#'
#' Projects the 8 cube corners at every unique rotation in the animation,
#' takes the union envelope, and adds padding. This gives a fixed viewport
#' that prevents the plot from rescaling as the rotation changes.
#' @keywords internal
#' @noRd
compute_global_bounds <- function(param_seqs, static_params, effective_ratios) {
      # Build cube corners using the effective ratios from the built plot.
      # These ratios account for both the user's ratio setting and any
      # data-dependent scaling (e.g., from scales = "fixed").
      # Data in the standardized domain lives in [-0.5, 0.5] * effective_ratios.
      corners <- as.matrix(expand.grid(
            x = c(-0.5, 0.5) * effective_ratios[1],
            y = c(-0.5, 0.5) * effective_ratios[2],
            z = c(-0.5, 0.5) * effective_ratios[3]
      ))

      # Find unique rotation combinations to avoid redundant projections.
      # Light angles don't affect geometry, so they're excluded.
      unique_params <- unique(param_seqs[c("pitch", "roll", "yaw")])

      # Collect all projected x/y across all frames
      all_x <- numeric(0)
      all_y <- numeric(0)

      for (i in seq_len(nrow(unique_params))) {
            proj <- list(
                  pitch = unique_params$pitch[i],
                  roll = unique_params$roll[i],
                  yaw = unique_params$yaw[i],
                  persp = static_params$persp,
                  dist = static_params$dist
            )
            projected <- transform_3d_standard(
                  as.data.frame(corners), proj
            )
            all_x <- c(all_x, projected$x)
            all_y <- c(all_y, projected$y)
      }

      # Compute bounds with generous padding (15%) to accommodate labels
      x_range <- range(all_x)
      y_range <- range(all_y)
      x_pad <- diff(x_range) * 0.15
      y_pad <- diff(y_range) * 0.15

      c(x_range[1] - x_pad, x_range[2] + x_pad,
        y_range[1] - y_pad, y_range[2] + y_pad)
}


#' Compute a fixed viewport shared by a set of frames
#'
#' Builds the plot once to get its effective axis ratios (which account for
#' both `ratio` and any data-dependent scaling), takes the union of the cube's
#' projections across every frame's rotation, and applies the coord's zoom.
#' @param plot A ggplot object using [coord_3d()].
#' @param static_params Output of extract_static_coord_params().
#' @param param_seqs Per-frame parameters with \code{pitch}, \code{roll},
#'   \code{yaw} columns.
#' @return Numeric bounds \code{c(xmin, xmax, ymin, ymax)}.
#' @keywords internal
#' @noRd
compute_frame_bounds <- function(plot, static_params, param_seqs) {
      built <- ggplot_build(plot)

      panel_params <- built$layout$panel_params[[1]]
      effective_ratios <- if (!is.null(panel_params$effective_ratios)) {
            panel_params$effective_ratios
      } else {
            static_params$ratio
      }

      global_bounds <- compute_global_bounds(param_seqs, static_params,
                                             effective_ratios)

      zoom <- static_params$zoom %||% 1
      if (zoom != 1) {
            x_center <- mean(global_bounds[1:2])
            y_center <- mean(global_bounds[3:4])
            x_half <- diff(global_bounds[1:2]) / 2 / zoom
            y_half <- diff(global_bounds[3:4]) / 2 / zoom
            global_bounds <- c(
                  x_center - x_half, x_center + x_half,
                  y_center - y_half, y_center + y_half
            )
      }

      global_bounds
}


#' Extract static (non-animated) coord_3d parameters
#' @keywords internal
#' @noRd
extract_static_coord_params <- function(coord) {
      list(
            persp = coord$persp,
            dist = coord$dist,
            expand = coord$expand,
            clip = coord$clip,
            panels = coord$panels,
            xlabels = coord$xlabels,
            ylabels = coord$ylabels,
            zlabels = coord$zlabels,
            rotate_labels = coord$rotate_labels,
            title_position = coord$title_position,
            scales = coord$scales,
            scale_depth = coord$scale_depth,
            ratio = coord$ratio,
            zoom = coord$zoom,
            light = coord$light
      )
}


#' Render a single frame to a PNG file
#' @keywords internal
#' @noRd
render_frame <- function(plot, file, width, height, res, device) {
      device_fun <- switch(device,
                           png = grDevices::png,
                           ragg_png = {
                                 if (!requireNamespace("ragg", quietly = TRUE)) {
                                       stop("The ragg package is required for device = 'ragg_png'.",
                                            call. = FALSE)
                                 }
                                 ragg::agg_png
                           },
                           stop("Unsupported device: ", device, call. = FALSE)
      )

      device_fun(filename = file, width = width, height = height, res = res)
      on.exit(grDevices::dev.off(), add = TRUE)

      # Build gtable after opening our device so ggplot2 uses our dimensions
      gt <- ggplot2::ggplotGrob(plot)
      grid::grid.draw(gt)
}


# Renderers ---------------------------------------------------------

#' Animation renderers for animate_3d
#'
#' Renderer functions that combine individual frame images into a final
#' animation file. These are factory functions that return the actual
#' rendering function.
#'
#' @param file Output file path. If \code{NULL}, a temporary file is used.
#' @param loop Logical. Should the GIF loop? Default is \code{TRUE}.
#' @param dir Directory to copy frame files into. Defaults to a session
#'   temporary directory (\code{tempdir()}). For animations you want to keep,
#'   pass an explicit directory.
#' @param prefix Filename prefix for copied frame files.
#' @param overwrite Logical. Overwrite existing files? Default is \code{FALSE}.
#' @param vfilter A video filter string for ffmpeg (passed to \pkg{av}).
#' @param codec Video codec name. Default lets \pkg{av} choose.
#'
#' @return A function with signature \code{function(frames, fps, width, height)}
#'   that produces the animation output.
#'
#' @examplesIf requireNamespace("gifski", quietly = TRUE) && requireNamespace("av", quietly = TRUE)
#' \donttest{
#' p <- ggplot() +
#'   geom_function_3d(
#'     fun = function(x, y) sin(x) * cos(y),
#'     xlim = c(-pi, pi), ylim = c(-pi, pi),
#'     fill = "steelblue", color = "steelblue") +
#'   coord_3d()
#'
#' # GIF output (default)
#' animate_3d(p, yaw = c(0, 360), renderer = gifski_renderer_3d())
#'
#' # Video output
#' animate_3d(p, yaw = c(0, 360), renderer = av_renderer_3d())
#'
#' # Keep individual frame files in a chosen directory
#' animate_3d(p, yaw = c(0, 360),
#'     renderer = file_renderer_3d(dir = file.path(tempdir(), "my_frames")))
#' }
#'
#' @name renderers_3d
NULL

#' @rdname renderers_3d
#' @export
gifski_renderer_3d <- function(file = NULL, loop = TRUE) {
      function(frames, fps, width, height) {
            if (!requireNamespace("gifski", quietly = TRUE)) {
                  stop("The gifski package is required for GIF output. ",
                       "Install it with install.packages('gifski').",
                       call. = FALSE)
            }

            if (is.null(file)) {
                  file <- tempfile(fileext = ".gif")
            }

            gifski::gifski(frames, gif_file = file,
                           width = width, height = height,
                           delay = 1 / fps, loop = loop,
                           progress = FALSE)

            gif_3d(file, width = width, height = height)
      }
}

#' @rdname renderers_3d
#' @export
av_renderer_3d <- function(file = NULL, vfilter = "null", codec = NULL) {
      function(frames, fps, width, height) {
            if (!requireNamespace("av", quietly = TRUE)) {
                  stop("The av package is required for video output. ",
                       "Install it with install.packages('av').",
                       call. = FALSE)
            }

            if (is.null(file)) {
                  file <- tempfile(fileext = ".mp4")
            }

            # av auto-detects dimensions from input frames
            av::av_encode_video(frames, output = file, framerate = fps,
                                vfilter = vfilter, codec = codec)

            video_3d(file, width = width, height = height)
      }
}

#' @rdname renderers_3d
#' @export
file_renderer_3d <- function(dir = tempdir(), prefix = "ggcube_frame",
                             overwrite = FALSE) {
      function(frames, fps, width, height) {
            if (!dir.exists(dir)) {
                  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
            }
            new_names <- file.path(dir,
                                   paste0(prefix, "_",
                                          sprintf("%04d", seq_along(frames)),
                                          ".png"))
            if (any(file.exists(new_names)) && !overwrite) {
                  stop("Output files already exist. Use `overwrite = TRUE` ",
                       "to replace them.", call. = FALSE)
            }
            file.copy(frames, new_names, overwrite = overwrite)

            result <- new_names
            class(result) <- c("frame_files_3d", "character")
            result
      }
}


# Output classes  ---------------------------------------------------------

#' Create a gif_3d output object
#' @param path Path to the GIF file.
#' @param width,height Dimensions of the GIF in pixels.
#' @return A \code{gif_3d} object.
#' @keywords internal
#' @noRd
gif_3d <- function(path, width = NULL, height = NULL) {
      structure(path,
                class = c("gif_3d", "character"),
                width = width,
                height = height)
}

#' @export
print.gif_3d <- function(x, ...) {
      viewer <- getOption("viewer")
      if (!is.null(viewer)) {
            viewer(x)
      } else {
            utils::browseURL(x)
      }
      invisible(x)
}

#' @exportS3Method knitr::knit_print gif_3d
knit_print.gif_3d <- function(x, options, ...) {
      # If the GIF is in a tempdir, copy it to knitr's figures directory
      # so it persists in the rendered output
      if (grepl(tempdir(), x, fixed = TRUE)) {
            fig_path <- knitr::fig_path(".gif")
            dir.create(dirname(fig_path), showWarnings = FALSE, recursive = TRUE)
            file.copy(x, fig_path, overwrite = TRUE)
            path <- fig_path
      } else {
            path <- x
      }

      # Use stored dimensions if chunk options don't override
      if (is.null(options$out.width) && !is.null(attr(x, "width"))) {
            options$out.width <- paste0(attr(x, "width"), "px")
      }
      if (is.null(options$out.height) && !is.null(attr(x, "height"))) {
            options$out.height <- paste0(attr(x, "height"), "px")
      }

      knitr::knit_print(knitr::include_graphics(path), options, ...)
}


#' Create a video_3d output object
#' @param path Path to the video file.
#' @param width,height Dimensions of the video in pixels.
#' @return A \code{video_3d} object.
#' @keywords internal
#' @noRd
video_3d <- function(path, width = NULL, height = NULL) {
      structure(path,
                class = c("video_3d", "character"),
                width = width,
                height = height)
}

#' @export
print.video_3d <- function(x, ...) {
      utils::browseURL(x)
      invisible(x)
}

#' @exportS3Method knitr::knit_print video_3d
knit_print.video_3d <- function(x, options, ...) {
      if (requireNamespace("base64enc", quietly = TRUE)) {
            ext <- tools::file_ext(x)
            mime <- switch(ext,
                           mp4 = "video/mp4",
                           webm = "video/webm",
                           "video/mp4")
            b64 <- base64enc::base64encode(x)
            html <- sprintf(
                  '<video controls autoplay loop><source src="data:%s;base64,%s" type="%s"></video>',
                  mime, b64, mime
            )
            knitr::asis_output(html)
      } else {
            # Fallback: copy tempfile to figures directory if needed
            if (grepl(tempdir(), x, fixed = TRUE)) {
                  fig_path <- knitr::fig_path(paste0(".", tools::file_ext(x)))
                  dir.create(dirname(fig_path), showWarnings = FALSE, recursive = TRUE)
                  file.copy(x, fig_path, overwrite = TRUE)
                  x <- fig_path
            }
            knitr::knit_print(knitr::include_graphics(x), options, ...)
      }
}



# Save helper  ---------------------------------------------------------

# Environment to store the last rendered animation
.ggcube_last_anim <- new.env(parent = emptyenv())
.ggcube_last_anim$value <- NULL

#' Save a 3D animation to a file
#'
#' Saves an animation object produced by [animate_3d()] to a file.
#' If no animation is provided, saves the most recently rendered animation.
#'
#' @param animation An animation object from [animate_3d()], or \code{NULL}
#'   to use the last rendered animation.
#' @param filename Output file path.
#'
#' @return Invisibly returns the output file path (a character string).
#'   Called primarily for its side effect of copying the animation to
#'   \code{filename}.
#'
#' @examplesIf requireNamespace("gifski", quietly = TRUE)
#' \donttest{
#' p <- ggplot() +
#'   geom_function_3d(
#'     fun = function(x, y) sin(x) * cos(y),
#'     xlim = c(-pi, pi), ylim = c(-pi, pi),
#'     fill = "steelblue", color = "steelblue") +
#'   coord_3d()
#'
#' animate_3d(p, yaw = c(0, 360))
#' anim_save_3d(filename = file.path(tempdir(), "my_animation.gif"))
#' }
#'
#' @export
anim_save_3d <- function(animation = NULL, filename) {
      if (is.null(animation)) {
            animation <- .ggcube_last_anim$value
            if (is.null(animation)) {
                  stop("No animation to save. Run `animate_3d()` first.",
                       call. = FALSE)
            }
      }

      if (inherits(animation, "frame_files_3d")) {
            stop("Cannot save frame files as a single animation. ",
                 "Use `gifski_renderer_3d()` or `av_renderer_3d()` instead.",
                 call. = FALSE)
      }

      file.copy(as.character(animation), filename, overwrite = TRUE)
      message("Animation saved to ", filename)
      invisible(filename)
}
