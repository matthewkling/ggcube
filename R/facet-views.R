#' Facet a plot by viewing angle
#'
#' These facets show a plot from several angles at once, with one panel per
#' view. Where [ggplot2::facet_wrap()] and [ggplot2::facet_grid()] split the
#' data by the values of data variables, `facet_wrap_views()` and
#' `facet_grid_views()` give every panel all of the data and vary the angle it
#' is seen from instead: the camera angles `pitch`, `roll`, and `yaw`, and the
#' light angles `light_azimuth` and `light_elevation`. `facet_wrap_views()`
#' wraps a sequence of views into rows, and `facet_grid_views()` lays views
#' out with one set of angles down the rows and another across the columns.
#'
#' @section Choosing views:
#' The formula names the angles that vary from panel to panel, in the same way
#' a `facet_wrap()` or `facet_grid()` formula names data variables:
#' `facet_wrap_views(~yaw)` shows views around the vertical axis, and
#' `facet_grid_views(roll ~ yaw)` varies `roll` down the rows and `yaw` across
#' the columns. A side of a grid formula can be left empty with `.`, as in
#' `. ~ yaw`, and several angles can be combined with `+`, as in
#' `roll ~ yaw + light_azimuth`, which crosses them.
#'
#' Each angle named in the formula is sampled at `n` evenly spaced values
#' across a range. The range can be set with the argument of the same name,
#' such as `yaw = c(-90, 90)`; otherwise a default range is used:
#' \itemize{
#'   \item `yaw` and `light_azimuth`: a full turn, `c(0, 360)`.
#'   \item `roll`: `c(-90, 0)`, from a side view to looking straight down.
#'   \item `pitch`: `c(-45, 45)`.
#'   \item `light_elevation`: `c(0, 90)`, from the horizon to overhead.
#' }
#' A range that returns to its starting view, such as a full turn, drops its
#' duplicate endpoint, so four views of `yaw = c(0, 360)` are at 0, 90, 180,
#' and 270 degrees.
#'
#' Angles not named in the formula can be fixed for every panel with a single
#' value, such as `roll = -20`; otherwise they are held at the values set in
#' [coord_3d()] and the plot's [light()]. Angles use the same conventions as
#' [coord_3d()] and [direction()].
#'
#' @section Lighting:
#' Light angles move the plot-level light, set with [light()] or
#' `coord_3d(light = )`, which must be directional (not `position`-based). It is
#' an error to use them when lighting is disabled or when any layer sets its own
#' `light`. The light's `anchor` is respected: with the default
#' `anchor = "scene"` the light moves around the data, and with
#' `anchor = "camera"` it moves around the viewer.
#'
#' @section Layout:
#' Every panel is framed identically, so the plot keeps its size from panel to
#' panel and differences between panels come only from the view. To leave room
#' for each panel's axis labels, these facets set a wider default
#' `panel.spacing` in the plot's theme, which a later
#' `theme(panel.spacing = )` overrides.
#'
#' These facets can't be combined with faceting by data variables, since a
#' plot has only one facet. When animating a plot that uses them with
#' [animate_3d()] or [orbit_3d()], only angles the facet doesn't set can be
#' moved.
#'
#' @param facets For `facet_wrap_views()`, a one-sided formula naming the
#'   angles to vary, such as `~yaw` or `~yaw + roll`. A character vector of
#'   angle names also works.
#' @param pitch,roll,yaw,light_azimuth,light_elevation Angles in degrees. For an
#'   angle named in the formula, an optional length-2 range `c(from, to)` to
#'   sample it over (see Details for the defaults). For any other angle, an
#'   optional single value to fix it at in every panel.
#' @param n Number of values to sample each angle in the formula at. A single
#'   number applies to every angle; otherwise give one number per angle, in the
#'   order the angles appear in the formula. Default is 4.
#' @param nrow,ncol Number of rows and columns.
#' @param dir,strip.position,labeller Passed to [ggplot2::facet_wrap()] or
#'   [ggplot2::facet_grid()]. The default labeller shows each angle's name and
#'   value, such as "yaw: 90"; use `labeller = "label_value"` to show the value
#'   alone.
#'
#' @return A list containing the facet and a theme setting, to be added to a
#'   plot with `+`.
#'
#' @examples
#' p <- ggplot(mountain, aes(x, y, z, fill = z)) +
#'   geom_surface_3d(linewidth = .25) +
#'   coord_3d(ratio = c(1.5, 2, 1)) +
#'   scale_fill_viridis_c() +
#'   theme_void()
#'
#' \donttest{
#' # Four views around the vertical axis
#' p + facet_wrap_views(~yaw)
#'
#' # Yaw across the columns and roll down the rows
#' p + facet_grid_views(roll ~ yaw, n = c(3, 4))
#'
#' # Set the range of an angle, and fix another for every panel
#' p + facet_wrap_views(~yaw, yaw = c(-90, 90), n = 3, roll = -20)
#'
#' # Compare light angles from a fixed view
#' p + light("hard") + facet_wrap_views(~light_azimuth)
#' }
#'
#' @seealso [orbit_3d()] for an interactive view you drag between angles, and
#'   [animate_3d()] for animations.
#' @export
facet_wrap_views <- function(facets,
                             pitch = NULL, roll = NULL, yaw = NULL,
                             light_azimuth = NULL, light_elevation = NULL,
                             n = 4, nrow = NULL, ncol = NULL, dir = "h",
                             strip.position = "top",
                             labeller = "label_both") {
      angles <- view_facet_angles(facets, "facet_wrap_views")
      spec <- new_view_spec(angles,
                            list(pitch = pitch, roll = roll, yaw = yaw,
                                 light_azimuth = light_azimuth,
                                 light_elevation = light_elevation),
                            n)

      base <- ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(angles)),
                                  nrow = nrow, ncol = ncol, dir = dir,
                                  strip.position = strip.position,
                                  labeller = labeller)
      facet <- ggproto(NULL, FacetWrapViews, shrink = TRUE,
                       params = c(base$params, list(view_spec = spec)))
      list(facet, view_facet_theme())
}


#' @rdname facet_wrap_views
#' @param rows For `facet_grid_views()`, a formula with the angles to vary
#'   down the rows on the left and across the columns on the right, such as
#'   `roll ~ yaw`. Use `.` for an empty side, as in `. ~ yaw`.
#' @param switch Passed to [ggplot2::facet_grid()], to move strips to the
#'   opposite sides of the panels.
#' @export
facet_grid_views <- function(rows,
                             pitch = NULL, roll = NULL, yaw = NULL,
                             light_azimuth = NULL, light_elevation = NULL,
                             n = 4, switch = NULL,
                             labeller = "label_both") {
      sides <- view_grid_angles(rows)
      angles <- c(sides$rows, sides$cols)
      spec <- new_view_spec(angles,
                            list(pitch = pitch, roll = roll, yaw = yaw,
                                 light_azimuth = light_azimuth,
                                 light_elevation = light_elevation),
                            n)

      base <- ggplot2::facet_grid(rows = ggplot2::vars(!!!rlang::syms(sides$rows)),
                                  cols = ggplot2::vars(!!!rlang::syms(sides$cols)),
                                  switch = switch, labeller = labeller)
      facet <- ggproto(NULL, FacetGridViews, shrink = TRUE,
                       params = c(base$params, list(view_spec = spec)))
      list(facet, view_facet_theme())
}


# ggproto classes ----------------------------------------------------------

# Both facets lay panels out with their parent's compute_layout(), fed a table
# of views in place of data, so wrapping, crossing several angles, and strip
# labels all work as they do for data variables. Every panel then gets all of
# the data, and setup_panel_params() rebuilds each panel's params from its own
# view.

FacetWrapViews <- ggproto("FacetWrapViews", ggplot2::FacetWrap,
      compute_layout = function(self, data, params) {
            layout <- ggproto_parent(ggplot2::FacetWrap, self)$compute_layout(
                  list(view_table(params$view_spec)), params)
            mark_view_layout(layout)
      },
      map_data = function(data, layout, params) view_map_data(data, layout),
      setup_panel_params = function(self, panel_params, coord, ...) {
            view_setup_panel_params(self, panel_params, coord)
      }
)

FacetGridViews <- ggproto("FacetGridViews", ggplot2::FacetGrid,
      compute_layout = function(self, data, params) {
            layout <- ggproto_parent(ggplot2::FacetGrid, self)$compute_layout(
                  list(view_table(params$view_spec)), params)
            mark_view_layout(layout)
      },
      map_data = function(data, layout, params) view_map_data(data, layout),
      setup_panel_params = function(self, panel_params, coord, ...) {
            view_setup_panel_params(self, panel_params, coord)
      }
)


# Specification ---------------------------------------------------------------

view_angle_names <- c("pitch", "roll", "yaw", "light_azimuth", "light_elevation")

view_default_ranges <- list(
      pitch = c(-45, 45),
      roll = c(-90, 0),
      yaw = c(0, 360),
      light_azimuth = c(0, 360),
      light_elevation = c(0, 90)
)

# Angle names from a facet_wrap_views() formula or character vector.
view_facet_angles <- function(facets, fun) {
      angles <- if (is.character(facets)) {
            facets
      } else if (inherits(facets, "formula")) {
            all.vars(facets)
      } else {
            stop("`facets` must be a formula such as `~yaw`, or a character ",
                 "vector of angle names.", call. = FALSE)
      }
      check_view_angles(setdiff(angles, "."), fun)
}

# Row and column angle names from a facet_grid_views() formula.
view_grid_angles <- function(rows) {
      if (is.character(rows) && length(rows) == 1) {
            rows <- stats::as.formula(rows)
      }
      if (!inherits(rows, "formula")) {
            stop("`rows` must be a formula such as `roll ~ yaw`.", call. = FALSE)
      }
      if (length(rows) == 3) {
            row_angles <- setdiff(all.vars(rows[[2]]), ".")
            col_angles <- setdiff(all.vars(rows[[3]]), ".")
      } else {
            row_angles <- character(0)
            col_angles <- setdiff(all.vars(rows[[2]]), ".")
      }
      check_view_angles(c(row_angles, col_angles), "facet_grid_views")
      list(rows = row_angles, cols = col_angles)
}

check_view_angles <- function(angles, fun) {
      if (length(angles) == 0) {
            stop("The formula must name at least one angle to vary, such as ",
                 "`yaw`.", call. = FALSE)
      }
      unknown <- setdiff(angles, view_angle_names)
      if (length(unknown) > 0) {
            stop(sprintf(paste0(
                  "%s %s not a viewing angle. %s() varies %s; to facet by ",
                  "data variables, use %s()."),
                  paste0("`", unknown, "`", collapse = ", "),
                  if (length(unknown) == 1) "is" else "are",
                  fun,
                  paste0("`", view_angle_names, "`", collapse = ", "),
                  sub("_views$", "", fun)),
                 call. = FALSE)
      }
      dups <- unique(angles[duplicated(angles)])
      if (length(dups) > 0) {
            stop(sprintf("%s appears more than once in the formula.",
                         paste0("`", dups, "`", collapse = ", ")),
                 call. = FALSE)
      }
      angles
}

# Resolve the angles a view facet varies and fixes, and sample the varying ones.
new_view_spec <- function(angles, args, n) {
      ranges <- list()
      fixed <- list()
      for (a in view_angle_names) {
            v <- args[[a]]
            if (!is.null(v) && (!is.numeric(v) || any(!is.finite(v)))) {
                  stop(sprintf("`%s` must be numeric.", a), call. = FALSE)
            }
            if (a %in% angles) {
                  if (is.null(v)) {
                        v <- view_default_ranges[[a]]
                  } else if (length(v) != 2) {
                        stop(sprintf(paste0(
                              "`%s` is in the formula, so it needs a range ",
                              "c(from, to) rather than %s. Leave it out to ",
                              "use the default range."),
                              a, if (length(v) == 1) "a single value" else
                                    sprintf("%d values", length(v))),
                             call. = FALSE)
                  }
                  ranges[[a]] <- v
            } else if (!is.null(v)) {
                  if (length(v) != 1) {
                        stop(sprintf(paste0(
                              "`%s` isn't in the formula, so it can only be ",
                              "fixed at a single value. Add it to the formula ",
                              "to vary it across panels."), a),
                             call. = FALSE)
                  }
                  fixed[[a]] <- v
            }
      }
      validate_light_elevation(c(ranges$light_elevation, fixed$light_elevation))

      n <- suppressWarnings(as.integer(n))
      if (length(n) == 0 || any(is.na(n)) || any(n < 1)) {
            stop("`n` must be one or more positive integers.", call. = FALSE)
      }
      if (length(n) == 1) n <- rep(n, length(angles))
      if (length(n) != length(angles)) {
            stop(sprintf(paste0(
                  "`n` has length %d but the formula names %d angles. Give one ",
                  "number, or one per angle in the order they appear in the ",
                  "formula."), length(n), length(angles)),
                 call. = FALSE)
      }

      values <- list()
      labels <- list()
      for (i in seq_along(angles)) {
            a <- angles[i]
            v <- sample_angle_range(ranges[[a]], n[i])$values
            lab <- format_angle(v)
            if (anyDuplicated(lab)) lab <- format(v, digits = 10, trim = TRUE)
            if (anyDuplicated(lab)) {
                  stop(sprintf("The range of `%s` gives repeated angles.", a),
                       call. = FALSE)
            }
            values[[a]] <- v
            labels[[a]] <- lab
      }

      list(angles = angles, ranges = ranges, fixed = fixed, n = n,
           values = values, labels = labels,
           light = any(c("light_azimuth", "light_elevation") %in%
                             c(angles, names(fixed))))
}

# The cross of every varying angle, as factors whose levels keep the sampled
# order. Facet layout code treats this as the data to facet by.
view_table <- function(spec) {
      columns <- lapply(spec$angles, function(a) {
            factor(spec$labels[[a]], levels = spec$labels[[a]])
      })
      names(columns) <- spec$angles
      do.call(expand.grid, c(columns, list(KEEP.OUT.ATTRS = FALSE,
                                           stringsAsFactors = FALSE)))
}

# The numeric angles of each panel in a layout.
view_layout_angles <- function(layout, spec) {
      out <- lapply(spec$angles, function(a) {
            spec$values[[a]][match(as.character(layout[[a]]), spec$labels[[a]])]
      })
      names(out) <- spec$angles
      as.data.frame(out)
}

# Coord3D gives each panel marked with a view its own params.
mark_view_layout <- function(layout) {
      layout$.view <- seq_len(nrow(layout))
      layout
}

view_facet_theme <- function() {
      theme(panel.spacing = grid::unit(20, "pt"))
}


# Building -------------------------------------------------------------------

# Every panel shows all of the data.
view_map_data <- function(data, layout) {
      if (is.null(data) || !is.data.frame(data)) return(data)
      if (nrow(data) == 0) {
            data$PANEL <- factor(integer(0), levels = levels(layout$PANEL))
            return(data)
      }
      rows <- rep(seq_len(nrow(data)), times = nrow(layout))
      out <- data[rows, , drop = FALSE]
      out$PANEL <- rep(layout$PANEL, each = nrow(data))
      rownames(out) <- NULL
      out
}

# Rebuild each panel's params from its own view: once to find each view's
# bounds, then again with every panel in one shared frame.
view_setup_panel_params <- function(self, panel_params, coord) {
      spec <- self$params$view_spec
      layout <- self$compute_layout(list(), self$params)
      views <- view_layout_angles(layout, spec)
      n_panels <- nrow(layout)

      angle <- function(a, default) {
            if (!is.null(views[[a]])) return(views[[a]])
            rep(spec$fixed[[a]] %||% default, n_panels)
      }
      pitch <- angle("pitch", coord$pitch)
      roll <- angle("roll", coord$roll)
      yaw <- angle("yaw", coord$yaw)

      light <- NULL
      if (spec$light) {
            plot <- find_build_plot()
            light <- resolve_animated_light(plot)
            current <- direction_angles(light$direction)
            azimuth <- angle("light_azimuth", current$azimuth)
            elevation <- angle("light_elevation", current$elevation)
      }

      coord_params <- coord$setup_params(NULL)

      # setup_panel_params() blanks the names of the scales it's given, and
      # clone() drops a scale's trained range, so each call gets fresh copies.
      fresh <- function(scale) {
            copy <- scale$clone()
            copy$range <- scale$range
            copy
      }

      build <- function(i, fixed_bounds, zoom) {
            view_coord <- ggproto(NULL, coord,
                                  pitch = pitch[i], roll = roll[i], yaw = yaw[i],
                                  fixed_bounds = fixed_bounds, zoom = zoom)
            scales <- panel_params[[i]]$source_scales
            pp <- view_coord$setup_panel_params(fresh(scales$x), fresh(scales$y),
                                                coord_params)
            if (!is.null(light)) {
                  pp$light <- light
                  pp$light$direction <- direction(azimuth[i], elevation[i])
            }
            pp
      }

      first <- lapply(seq_len(n_panels), build, fixed_bounds = NULL, zoom = 1)
      b <- do.call(rbind, lapply(first, `[[`, "plot_bounds"))
      shared <- c(min(b[, 1]), max(b[, 2]), min(b[, 3]), max(b[, 4]))

      # Pad the shared frame so axis labels, which a standalone plot lets spill
      # into its margin, have room inside the panel. animate_3d() and orbit_3d()
      # pad their shared frames by the same proportion.
      pad <- c(diff(shared[1:2]), diff(shared[3:4])) * view_frame_padding
      shared <- shared + c(-pad[1], pad[1], -pad[2], pad[2])

      lapply(seq_len(n_panels), build, fixed_bounds = shared,
             zoom = coord$zoom %||% 1)
}

view_frame_padding <- 0.15
