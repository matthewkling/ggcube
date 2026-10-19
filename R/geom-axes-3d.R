# Geom -------------------------------------------------------------------------

GeomAxes3D <- ggproto("GeomAxes3D", Geom,

                      required_aes = character(),

                      default_aes = aes(
                            colour = "grey30", linewidth = 0.5, linetype = 1, alpha = 1,
                            size = 3.88, family = "", fontface = 1
                      ),

                      draw_panel = function(data, panel_params, coord, x = 0, y = 0, z = 0,
                                            labels = c("x", "y", "z"), arrow = NULL,
                                            arrow.fill = NULL, scale_depth = TRUE,
                                            on_top = FALSE, na.rm = FALSE) {

                            validate_coord3d(coord)
                            if (nrow(data) == 0) return(grid::nullGrob())

                            rows <- axes_rows(data, c(x = x, y = y, z = z), labels,
                                              panel_limits(panel_params))

                            coords <- coord$transform(rows, panel_params, sort = FALSE)
                            finish <- function(coords) apply_depth_scaling(coords, scale_depth)

                            layer_scene_grob(coords, panel_params, finish,
                                             list(arrow = arrow, arrow.fill = arrow.fill, lineend = "butt"),
                                             on_top = on_top)
                      },

                      draw_key = draw_key_path
)


#' Segment and label rows of axes spanning a panel
#'
#' @param data Layer data, one row per axis (`group` 1, 2, 3 for x, y, z),
#'   with styling columns.
#' @param origin Named length-3 vector: the point the axes pass through.
#' @param labels Length-3 labels, or NULL.
#' @param limits Named list of the panel's x, y, z limits.
#' @return Data frame of segment and text rows in data space, with styling.
#' @keywords internal
#' @noRd
axes_rows <- function(data, origin, labels, limits) {
      outside <- vapply(c("x", "y", "z"), function(a) {
            origin[[a]] < min(limits[[a]]) || origin[[a]] > max(limits[[a]])
      }, logical(1))
      if (any(outside)) {
            rlang::warn(paste0("The axes' origin lies outside the panel range on ",
                               paste(names(outside)[outside], collapse = ", "),
                               ", so some axes are drawn outside the panel (`geom_axes_3d()`)."))
      }

      axes <- c("x", "y", "z")
      style_cols <- intersect(c("colour", "linewidth", "linetype", "alpha",
                                "size", "family", "fontface", "PANEL"), names(data))
      rows <- list()
      for (k in seq_len(nrow(data))) {
            a <- axes[data$group[k]]
            lim <- range(limits[[a]])
            start <- origin; start[[a]] <- lim[1]
            end <- origin; end[[a]] <- lim[2]
            style <- data[k, style_cols, drop = FALSE]

            seg <- data.frame(x = c(start[["x"]], end[["x"]]),
                              y = c(start[["y"]], end[["y"]]),
                              z = c(start[["z"]], end[["z"]]),
                              group = paste0("axis__", a),
                              point_type = c("start", "end"), segment_id = k,
                              .prim = "segment")
            rows <- c(rows, list(cbind(seg, style[c(1, 1), , drop = FALSE])))

            label <- if (is.null(labels)) NA else labels[data$group[k]]
            if (!is.na(label) && nzchar(label)) {
                  pos <- end; pos[[a]] <- lim[2] + 0.06 * diff(lim)
                  txt <- data.frame(x = pos[["x"]], y = pos[["y"]], z = pos[["z"]],
                                    group = paste0("axis__", a, "_label"),
                                    label = label, hjust = 0.5, vjust = 0.5,
                                    angle = 0, lineheight = 1.2, .prim = "text")
                  rows <- c(rows, list(cbind(txt, style)))
            }
      }
      rows <- dplyr::bind_rows(rows)
      rownames(rows) <- NULL
      rows
}


# Constructor ------------------------------------------------------------------

#' 3D coordinate axes
#'
#' `geom_axes_3d()` draws a line along each of the x, y, and z axes through
#' an origin, each spanning the panel's full range along its axis, with an
#' arrowhead and a label at its high end. Unlike the axes drawn on the edges
#' of the panel by [coord_3d()], these are part of the 3D scene, so they are
#' depth-sorted with the plot's other geometry. This suits diagrams, such as
#' plots of vectors, where the axes should pass through the origin.
#'
#' Axes are built from the panel's limits when the plot is drawn, so they
#' don't expand the plot's axis ranges. Positions are in the plot's
#' (transformed) scale space.
#'
#' @param x,y,z Coordinates of the origin the axes pass through, each a
#'   single number. Default `0`.
#' @param labels Labels for the x, y, and z axes, drawn just beyond each
#'   axis's high end: a length-3 vector, with `NA` or `""` to leave an axis
#'   unlabelled, or `NULL` for no labels.
#' @param arrow Arrowhead specification created by [grid::arrow()], or `NULL`
#'   for none. The default is a closed 6pt arrowhead.
#' @param arrow.fill Fill colour for closed arrowheads. `NULL` (the default)
#'   uses each axis's colour.
#' @param ... Aesthetics applied as fixed values: `colour`, `alpha`, and, for
#'   the lines, `linewidth` and `linetype`, and for the labels, `size`,
#'   `family`, and `fontface`. Each can have length 1, or length 3 to style
#'   the x, y, and z axes separately.
#' @inheritParams depth_params
#' @inheritParams on_top_param
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' # Axes through the origin, depth-sorted with a surface
#' ggplot() +
#'   geom_function_3d(fun = function(x, y) (x^2 - y^2) / 2,
#'                    xlim = c(-1, 1), ylim = c(-1, 1), n = 20) +
#'   geom_axes_3d(linewidth = 0.8) +
#'   coord_3d(panels = "none") +
#'   theme_void()
#'
#' # Styled per axis, with an offset origin
#' ggplot(sphere_points, aes(x, y, z)) +
#'   geom_hull_3d(fill = "grey80", colour = "grey60", alpha = 0.5) +
#'   geom_axes_3d(x = -1, y = -1, z = -1,
#'                colour = c("tomato", "seagreen", "steelblue"),
#'                labels = c("east", "north", "up")) +
#'   coord_3d()
#' @export
geom_axes_3d <- function(x = 0, y = 0, z = 0, labels = c("x", "y", "z"),
                         arrow = grid::arrow(length = grid::unit(6, "pt"), type = "closed"),
                         arrow.fill = NULL, ..., scale_depth = TRUE, on_top = FALSE) {

      for (nm in c("x", "y", "z")) {
            v <- get(nm)
            if (!is.numeric(v) || length(v) != 1 || !is.finite(v)) {
                  rlang::abort(paste0("`", nm, "` must be a single finite number."))
            }
      }
      if (!is.null(labels) && length(labels) != 3) {
            rlang::abort("`labels` must have length 3 (one per axis), or be `NULL`.")
      }
      if (!is.null(arrow) && !inherits(arrow, "arrow")) {
            rlang::abort("`arrow` must be created by `grid::arrow()`, or be `NULL`.")
      }
      check_on_top(on_top)

      dots <- list(...)
      names(dots) <- standardise_aes_names(names(dots))
      styles <- intersect(names(dots), GeomAxes3D$aesthetics())
      bad <- styles[!lengths(dots[styles]) %in% c(1, 3)]
      if (length(bad) > 0) {
            rlang::abort(paste0("Axis styling must have length 1 or 3 (one per axis): ",
                                paste0("`", bad, "`", collapse = ", "), "."))
      }

      layer(
            geom = GeomAxes3D, mapping = aes(group = axis), data = data.frame(axis = 1:3),
            stat = "identity", position = "identity", show.legend = FALSE, inherit.aes = FALSE,
            params = c(list(x = unname(x), y = unname(y), z = unname(z),
                            labels = if (is.null(labels)) NULL else as.character(labels),
                            arrow = arrow, arrow.fill = arrow.fill, scale_depth = scale_depth,
                            on_top = on_top),
                       dots)
      )
}
