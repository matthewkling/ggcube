# annotate-3d.R
# 3D annotations
#
# annotate_3d() builds a layer of fixed-position annotations, the 3D
# counterpart of ggplot2::annotate(). Like every ggcube layer, it is
# depth-sorted together with the plot's other 3D layers.


# Constructor ------------------------------------------------------------------

#' Create a 3D annotation layer
#'
#' `annotate_3d()` adds annotations to a plot: points, text labels, segments,
#' polygons, axes, reference lines, or planes, at positions given as
#' parameters rather than mapped from data. It works like
#' [ggplot2::annotate()], but builds a ggcube layer, which is depth-sorted
#' together with the plot's other 3D layers, so annotations pass in front of
#' and behind the plot's geometry correctly.
#'
#' Parameters can be vectors to create several annotations of the same type
#' in one call, with values of length 1 recycled. Annotations never inherit
#' the plot's aesthetics or appear in legends.
#'
#' @param type Character string specifying the annotation type. One of
#'   `"point"`, `"text"`, `"segment"`, `"polygon"`, `"axes"`, `"refline"`, or
#'   `"plane"`.
#' @param ... Type-specific parameters, described below, along with parameters
#'   of the layer function the type uses, such as `scale_depth`, `on_top`, or
#'   `light`.
#'
#' @details
#' Each type builds a layer with a ggcube layer function, and takes that
#' function's parameters:
#'
#' ## Point annotations
#' Uses [geom_point_3d()]. Requires `x`, `y`, `z`. Optional styling:
#' `colour`/`color` (default `"black"`), `fill`, `size` (default `2`),
#' `shape` (default `19`), `alpha`, `stroke`.
#'
#' ## Text annotations
#' Uses [geom_text_3d()]. Requires `x`, `y`, `z`, `label`. Optional styling:
#' `colour`/`color`, `size`, `alpha`, `family`, `fontface`, `hjust`, `vjust`,
#' `angle`, `lineheight`. Labels are depth-sorted with the rest of the scene,
#' so geometry in front of a label hides it; use `on_top = TRUE` to draw
#' labels on top of everything else instead, keeping them legible wherever
#' they fall.
#'
#' ## Segment annotations
#' Uses [geom_segment_3d()]. Requires `x`, `y`, `z`, `xend`, `yend`, `zend`.
#' Optional styling: `colour`/`color`, `linewidth`, `linetype`, `alpha`, and
#' `arrow`, an arrow specification created by [grid::arrow()]. Closed
#' arrowheads are filled with the segment colour, unless `arrow.fill` is
#' given. When a segment passes through a polygon and is split, its
#' arrowheads stay at the segment's original ends.
#'
#' ## Polygon annotations
#' Uses [geom_polygon_3d()]. Requires `x`, `y`, `z` giving the polygon's
#' vertices in order. To draw several polygons in one call, give `group`, with
#' one value per vertex; vertices sharing a value form one polygon. Each
#' polygon needs at least 3 vertices. Optional styling: `fill` (default
#' `"grey60"`), `alpha` (default `0.5`), `colour`/`color` for the outline
#' (default `"grey30"`), `linewidth` (default `0.5`), `linetype`. Styling
#' values apply per polygon, so they must have length 1 or one value per
#' polygon, in the order the groups first appear.
#'
#' ## Axes annotations
#' Uses [geom_axes_3d()], which draws a line along each of the x, y, and z
#' axes through an origin `x`, `y`, `z` (default `0`), spanning the panel,
#' with arrowheads and labels at their high ends. See [geom_axes_3d()] for its
#' parameters.
#'
#' ## Reference line annotations
#' Uses [geom_refline_3d()], which draws infinite lines through the point
#' `x`, `y`, `z` (default `0`) in the direction `dx`, `dy`, `dz`, clipped to
#' the panel. For example, the line spanned by a vector `v` is
#' `annotate_3d("refline", dx = v[1], dy = v[2], dz = v[3])`, and the vertical
#' line at `x = 1`, `y = 2` is `annotate_3d("refline", x = 1, y = 2, dz = 1)`.
#'
#' ## Plane annotations
#' Uses [geom_plane_3d()], which draws planes clipped to the panel, given as
#' `xintercept`, `yintercept`, or `zintercept`; as `intercept`, `slope_x`,
#' and `slope_y`; or as general coefficients `a`, `b`, `c`, `d`. See
#' [geom_plane_3d()] for details.
#'
#' Axes, reference lines, and planes are built from the panel's limits when
#' the plot is drawn, so unlike the other types, they don't expand the plot's
#' axis ranges.
#'
#' ## Coordinates
#' Annotation coordinates are placed directly in the plot's (transformed)
#' scale space: with `scale_x_log10()`, for example, give x positions as
#' log10 values.
#'
#' ## Coincident elements
#' A point, segment, line, or text label lying exactly in the plane of a
#' polygon (such as a line drawn along a plane, or along the intersection of
#' two planes) is drawn on top of that polygon.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' p <- ggplot(mountain, aes(x, y, z)) +
#'   geom_surface_3d() +
#'   coord_3d(ratio = c(2, 3, 1),
#'            light = light(mode = "hsl", direction = c(-1, 0, 0)))
#'
#' # A point, depth-sorted with the surface
#' p + annotate_3d("point", x = .5, y = .25, z = 100, color = "red", size = 3)
#'
#' # Vectorized: several points in one call
#' p + annotate_3d("point", x = .5, y = .25, z = seq(50, 100, 5),
#'                 color = "red", size = 3)
#'
#' # Several annotation types
#' p +
#'   annotate_3d("segment", x = .5, y = .25, z = 100,
#'               xend = .5, yend = .25, zend = 50, color = "red") +
#'   annotate_3d("point", x = .5, y = .25, z = 100, color = "red", size = 3) +
#'   annotate_3d("text", x = .5, y = .25, z = 100, label = "Look here!",
#'               color = "red", vjust = -1, fontface = "bold", on_top = TRUE)
#'
#' # Planes of a linear system, meeting at its solution, and the line along
#' # which the first two planes intersect
#' A <- rbind(c(1, 1, 1), c(1, -1, 0), c(0, 1, -1))
#' ggplot(data.frame(x = 1/3, y = 1/3, z = 1/3), aes(x, y, z)) +
#'   geom_point_3d(size = 3) +
#'   annotate_3d("plane", a = A[, 1], b = A[, 2], c = A[, 3], d = c(1, 0, 0),
#'               fill = c("tomato", "seagreen", "steelblue")) +
#'   annotate_3d("refline", x = 1/3, y = 1/3, z = 1/3,
#'               dx = 1, dy = 1, dz = -2, linewidth = 1) +
#'   lims(x = c(-1, 1), y = c(-1, 1)) + zlim(-1, 1) +
#'   coord_3d()
#'
#' # A polygon: the triangle spanned by two vectors and the origin
#' u <- c(1, 0.2, 0.5); v <- c(0.3, 1, 0.8)
#' ggplot(data.frame(x = 0, y = 0, z = 0, xend = c(u[1], v[1]),
#'                   yend = c(u[2], v[2]), zend = c(u[3], v[3])),
#'        aes(x, y, z, xend = xend, yend = yend, zend = zend)) +
#'   geom_segment_3d(arrow = arrow(type = "closed", length = unit(8, "pt"))) +
#'   annotate_3d("polygon", x = c(0, u[1], v[1]), y = c(0, u[2], v[2]),
#'               z = c(0, u[3], v[3]), fill = "steelblue") +
#'   coord_3d()
#'
#' # Axes through the origin, depth-sorted with a surface
#' ggplot() +
#'   geom_function_3d(fun = function(x, y) (x^2 - y^2) / 2,
#'                    xlim = c(-1, 1), ylim = c(-1, 1), n = 20) +
#'   annotate_3d("axes", linewidth = 0.8) +
#'   coord_3d(panels = "none") +
#'   theme_void()
#'
#' @export
annotate_3d <- function(type, ...) {

      valid_types <- c("point", "text", "segment", "polygon", "axes", "refline", "plane")
      if (!is.character(type) || length(type) != 1 || !type %in% valid_types) {
            rlang::abort(paste0("`type` must be one of ",
                                paste0("\"", valid_types, "\"", collapse = ", "), "."))
      }

      params <- list(...)
      if (length(params) > 0) names(params) <- standardise_aes_names(names(params))

      if ("arrow" %in% names(params)) {
            if (!type %in% c("segment", "axes")) {
                  rlang::abort("`arrow` applies only to segment and axes annotations.")
            }
            if (!is.null(params$arrow) && !inherits(params$arrow, "arrow")) {
                  rlang::abort("`arrow` must be created by `grid::arrow()`.")
            }
      }

      switch(type,
             point = annotate_positions(
                   "point", geom_point_3d, GeomPoint3D, params, c("x", "y", "z"),
                   defaults = list(colour = "black", size = 2, shape = 19, stroke = 0.5)),
             text = annotate_positions(
                   "text", geom_text_3d, GeomText3DBillboard, params, c("x", "y", "z", "label")),
             segment = annotate_positions(
                   "segment", geom_segment_3d, GeomSegment3D, params,
                   c("x", "y", "z", "xend", "yend", "zend")),
             polygon = annotate_polygon(params),
             axes = do.call(geom_axes_3d, params),
             refline = do.call(geom_refline_3d, params),
             plane = do.call(geom_plane_3d, params))
}


#' Build an annotation layer from positions given as parameters
#'
#' Positions (and labels) go into the layer's data; aesthetics given as
#' parameters must have length 1 or the common length of the annotations,
#' as in [ggplot2::annotate()]; other parameters pass through to the layer
#' function unchanged.
#'
#' @param type Annotation type, for messages.
#' @param fn Layer function.
#' @param geom The layer's Geom, whose aesthetics are checked for length.
#' @param params Named list of parameters, with standardized aesthetic names.
#' @param positions Names of the required parameters that go into the data.
#' @param defaults Default styling, used where not given.
#' @return A layer.
#' @keywords internal
#' @noRd
annotate_positions <- function(type, fn, geom, params, positions, defaults = list()) {
      missing <- setdiff(positions, names(params))
      if (length(missing) > 0) {
            rlang::abort(paste0("annotate_3d(\"", type, "\") requires ",
                                paste0("`", missing, "`", collapse = ", "), "."))
      }
      for (nm in setdiff(names(defaults), names(params))) params[[nm]] <- defaults[[nm]]

      aes_names <- intersect(names(params), c(positions, geom$aesthetics()))
      n <- annotation_length(params[aes_names], type)

      data <- as.data.frame(lapply(params[positions], function(v) rep_len(unname(v), n)),
                            stringsAsFactors = FALSE)
      mapping <- do.call(aes, lapply(stats::setNames(nm = positions), as.name))
      do.call(fn, c(list(mapping = mapping, data = data, inherit.aes = FALSE,
                         show.legend = FALSE),
                    params[setdiff(names(params), positions)]))
}


#' Common length of annotation values
#'
#' @param values Named list of values that must have length 1 or a common
#'   length.
#' @param type Annotation type, for messages.
#' @return The common length.
#' @keywords internal
#' @noRd
annotation_length <- function(values, type) {
      lens <- lengths(values)
      n <- unique(lens)
      if (length(n) > 1) n <- setdiff(n, 1L)
      if (length(n) > 1 || any(lens == 0)) {
            rlang::abort(c(paste0("annotate_3d(\"", type, "\"): incompatible parameter lengths."),
                           i = paste0("Values must have length 1 or a common length; got ",
                                      paste0("`", names(values), "` (", lens, ")", collapse = ", "),
                                      ".")))
      }
      n
}


#' Build a polygon annotation layer
#'
#' Vertices are grouped into polygons by `group`; styling is given per
#' polygon and expanded to the vertices.
#'
#' @param params Named list of parameters, with standardized aesthetic names.
#' @return A layer.
#' @keywords internal
#' @noRd
annotate_polygon <- function(params) {
      missing <- setdiff(c("x", "y", "z"), names(params))
      if (length(missing) > 0) {
            rlang::abort(paste0("annotate_3d(\"polygon\") requires ",
                                paste0("`", missing, "`", collapse = ", "), "."))
      }
      n <- annotation_length(params[c("x", "y", "z")], "polygon")

      group <- params$group %||% 1
      if (length(group) == 1) group <- rep(group, n)
      if (length(group) != n) {
            rlang::abort("annotate_3d(\"polygon\"): `group` must have one value per vertex.")
      }
      ids <- unique(group)
      poly <- match(group, ids)
      if (any(tabulate(poly) < 3)) {
            rlang::abort("annotate_3d(\"polygon\"): each polygon needs at least 3 vertices.")
      }

      defaults <- list(fill = "grey60", alpha = 0.5, colour = "grey30", linewidth = 0.5)
      for (nm in setdiff(names(defaults), names(params))) params[[nm]] <- defaults[[nm]]

      style <- intersect(setdiff(names(params), c("x", "y", "z", "group")),
                         GeomPolygon3D$aesthetics())
      bad <- style[!lengths(params[style]) %in% c(1, length(ids))]
      if (length(bad) > 0) {
            rlang::abort(paste0("annotate_3d(\"polygon\"): styling values must have length 1 or one ",
                                "per polygon (", length(ids), "): ",
                                paste0("`", bad, "`", collapse = ", "), "."))
      }
      for (nm in style) {
            if (length(params[[nm]]) > 1) params[[nm]] <- unname(params[[nm]])[poly]
      }

      data <- data.frame(x = rep_len(unname(params$x), n), y = rep_len(unname(params$y), n),
                         z = rep_len(unname(params$z), n), group = poly)
      do.call(geom_polygon_3d, c(list(mapping = aes(x = x, y = y, z = z, group = group),
                                      data = data, inherit.aes = FALSE, show.legend = FALSE),
                                 params[setdiff(names(params), c("x", "y", "z", "group"))]))
}


# Deprecated in-layer annotations ------------------------------------------------

#' Add a layer function's deprecated `annotate` argument as layers
#'
#' Annotations used to be embedded in a layer through its `annotate`
#' argument. Since all 3D layers in a panel are now depth-sorted together,
#' the annotations are returned as layers of their own, alongside the layer.
#'
#' @param layer The layer built by the layer function.
#' @param annotate The function's `annotate` argument.
#' @param fn Name of the layer function.
#' @param user_env Environment of the user's call to the layer function.
#' @return `layer`, or a list of `layer` and the annotation layers.
#' @keywords internal
#' @noRd
with_annotations <- function(layer, annotate, fn, user_env = rlang::caller_env(2)) {
      if (!lifecycle::is_present(annotate)) return(layer)
      lifecycle::deprecate_warn(
            "0.4.0", paste0(fn, "(annotate)"),
            details = paste("Add annotations to the plot with `+ annotate_3d()` instead.",
                            "All 3D layers in a panel are now depth-sorted together."),
            user_env = user_env
      )
      c(list(layer), annotation_layers(annotate))
}


#' Flatten annotations into a list of layers
#'
#' @param annotate An `annotate_3d()` layer, or a (possibly nested) list of
#'   them, or NULL.
#' @return List of layers.
#' @keywords internal
#' @noRd
annotation_layers <- function(annotate) {
      if (is.null(annotate)) return(list())
      if (inherits(annotate, "Layer")) return(list(annotate))
      if (is.list(annotate) && !is.object(annotate)) {
            return(unlist(lapply(annotate, annotation_layers), recursive = FALSE))
      }
      rlang::abort("`annotate` must be built by `annotate_3d()`, or be a list of them.",
                   call = NULL)
}
