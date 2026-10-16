#' Show a 3D plot from several angles at once
#'
#' Lays out copies of a ggcube plot rendered across a 1D or 2D grid of viewing
#' angles, for comparing views side by side. It takes the same view arguments as
#' [orbit_3d()], so a call to one can be turned into the other by changing the
#' function name: where `orbit_3d()` lets you drag between the views,
#' `multiview_3d()` shows them all at once as a static figure.
#'
#' @section Specifying views:
#' Each of \code{pitch}, \code{roll}, \code{yaw}, \code{light_azimuth}, and
#' \code{light_elevation} is given either as a single value (held fixed) or as
#' a length-2 range \code{c(from, to)} sampled at \code{n} evenly spaced
#' values. One range gives a single sequence of panels, wrapped into rows; two
#' ranges give a grid with one parameter across the columns and the other down
#' the rows. Ranges are assigned to columns and rows the same way [orbit_3d()]
#' assigns them to horizontal and vertical drag: columns take the
#' highest-priority ranged parameter in the order \code{yaw}, \code{pitch},
#' \code{roll}, \code{light_azimuth}, \code{light_elevation}, and rows take the
#' other.
#'
#' A range that returns to its starting view, such as \code{yaw = c(0, 360)},
#' drops its duplicate endpoint, so \code{n = 4} gives views at 0, 90, 180, and
#' 270 degrees. \code{loop} controls this as in [orbit_3d()].
#'
#' Every panel is framed identically, so the plot stays the same size from
#' panel to panel and differences between panels come only from the view.
#'
#' @section Labels and layout:
#' With \code{labels = TRUE}, each panel is titled with the angles that vary
#' across the figure, in the terms of the ranges you gave: \code{yaw = c(0,
#' 360)} labels its panels 0, 90, 180, and 270. A title, subtitle, or caption
#' on the plot itself is shown once over the whole figure rather than on every
#' panel.
#'
#' With two ranged parameters, the grid has one column per value of the column
#' parameter and one row per value of the row parameter, so each column shares
#' one angle and each row shares the other. With one ranged parameter, the
#' panels are left to patchwork's default arrangement, which wraps them into a
#' roughly square grid (four panels make a 2 x 2 grid, for example).
#'
#' The result is a \pkg{patchwork} object, so it can be saved with
#' [ggplot2::ggsave()] and modified with patchwork functions, for example
#' \code{& theme(...)} to style every panel. Identical legends are collected
#' into one. \code{+ patchwork::plot_layout(ncol = , nrow = )} rearranges the
#' panels by filling them row by row into the new shape. That suits a single
#' sequence of panels, but for a two-parameter grid it means columns and rows
#' no longer each share an angle; each panel's title still identifies its view.
#'
#' @inheritParams orbit_3d
#' @param n Number of views along each ranged parameter. For a single range,
#'   one number; for two, a length-2 vector giving the counts for the columns
#'   and rows respectively. Default is 4.
#' @param labels Logical. Whether to title each panel with its varying angles.
#'   Default is `TRUE`.
#'
#' @return A \pkg{patchwork} object.
#'
#' @examplesIf rlang::is_installed("patchwork", version = "1.3.1")
#' \donttest{
#' p <- ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
#'   geom_surface_3d(linewidth = .25) +
#'   coord_3d(ratio = c(1.5, 2, 1)) +
#'   scale_fill_viridis_c() + scale_color_viridis_c() +
#'   theme_void()
#'
#' # Four views around the vertical axis
#' multiview_3d(p, yaw = c(0, 360), n = 4)
#'
#' # Yaw across the columns and roll down the rows
#' multiview_3d(p, yaw = c(0, 360), roll = c(-90, 0), n = c(4, 3))
#'
#' # Compare light angles from a fixed view
#' multiview_3d(p + light("direct"), light_azimuth = c(0, 360), n = 4)
#' }
#'
#' @seealso [orbit_3d()] for an interactive view you drag between angles, and
#'   [animate_3d()] for animations.
#' @export
multiview_3d <- function(plot,
                         pitch = NULL,
                         roll = NULL,
                         yaw = NULL,
                         light_azimuth = NULL,
                         light_elevation = NULL,
                         n = 4,
                         loop = NULL,
                         labels = TRUE) {

      # Earlier patchwork releases fail with ggplot2 4.0 when collecting legends.
      rlang::check_installed("patchwork", version = "1.3.1",
                             reason = "for `multiview_3d()`.")

      # --- Validate inputs ---
      if (!inherits(plot, "gg")) {
            stop("`plot` must be a ggplot object.", call. = FALSE)
      }
      coord <- plot$coordinates
      if (!inherits(coord, "Coord3D")) {
            stop("`plot` must use coord_3d(). ",
                 "Add `+ coord_3d()` to your plot before using multiview_3d().",
                 call. = FALSE)
      }
      if (!is.null(loop) && !(is.logical(loop) && length(loop) == 1)) {
            stop("`loop` must be NULL, TRUE, or FALSE.", call. = FALSE)
      }
      if (!is.logical(labels) || length(labels) != 1 || is.na(labels)) {
            stop("`labels` must be TRUE or FALSE.", call. = FALSE)
      }

      # --- Resolve the grid of views ---
      spec <- resolve_orbit_axes(pitch = pitch, roll = roll, yaw = yaw,
                                 light_azimuth = light_azimuth,
                                 light_elevation = light_elevation)
      n <- validate_grid_n(n, length(spec$axes))
      lit <- resolve_light_angles(plot, light_azimuth, light_elevation)
      grid <- build_param_grid(coord, spec, n = n, loop = loop,
                               light_angles = lit$angles)
      param_seqs <- grid$param_seqs

      # --- Shared framing and per-panel lights ---
      static_params <- extract_static_coord_params(coord)
      global_bounds <- compute_frame_bounds(plot, static_params, param_seqs)
      prepared <- prepare_frame_lights(plot, lit$light, param_seqs)

      # The plot's own title, subtitle, and caption go over the whole figure
      # rather than being repeated on every panel.
      annotation <- plot$labels[c("title", "subtitle", "caption")]
      base <- prepared$plot + labs(title = NULL, subtitle = NULL, caption = NULL)

      titles <- multiview_labels(param_seqs, spec$axes)
      panels <- lapply(seq_len(nrow(param_seqs)), function(i) {
            panel <- build_frame_plot(base, param_seqs, static_params,
                                      global_bounds, i,
                                      prepared$frame_lights[[i]])
            if (labels) panel <- panel + labs(title = titles[i])
            panel
      })

      # A grid places the first ranged parameter across the columns, in the
      # same x-fastest order build_param_grid() produces; a single sequence is
      # left to patchwork to wrap.
      layout <- if (length(grid$dims) == 2) {
            list(ncol = grid$dims[1], nrow = grid$dims[2])
      } else {
            list()
      }

      figure <- do.call(patchwork::wrap_plots,
                        c(list(panels, byrow = TRUE, guides = "collect"), layout))
      figure + patchwork::plot_annotation(title = annotation$title,
                                          subtitle = annotation$subtitle,
                                          caption = annotation$caption)
}


#' Build panel titles for multiview_3d()
#'
#' Names each varying parameter with its formatted angle, e.g.
#' "yaw -30, roll -60".
#' @param param_seqs Flat per-frame parameter data.frame from build_param_grid().
#' @param axes Names of the varying parameters, in column-then-row order.
#' @return Character vector of titles, one per panel.
#' @keywords internal
#' @noRd
multiview_labels <- function(param_seqs, axes) {
      parts <- lapply(axes, function(axis) {
            paste(gsub("_", " ", axis), format_angle(param_seqs[[axis]]))
      })
      do.call(paste, c(parts, sep = ", "))
}
