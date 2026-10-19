# Geom -------------------------------------------------------------------------

GeomRefline3D <- ggproto("GeomRefline3D", Geom,

                         required_aes = character(),

                         optional_aes = c("dx", "dy", "dz"),

                         default_aes = aes(
                               colour = "black", linewidth = 0.5, linetype = 1, alpha = 1
                         ),

                         setup_data = function(data, params) {
                               dirs <- c("dx", "dy", "dz")
                               if (!any(dirs %in% names(data))) {
                                     rlang::abort(c("Lines need a direction.",
                                                    i = "Give or map one or more of `dx`, `dy`, and `dz`."),
                                                  call = NULL)
                               }
                               for (nm in dirs) {
                                     if (!nm %in% names(data)) data[[nm]] <- 0
                               }
                               finite <- is.finite(data$dx) & is.finite(data$dy) & is.finite(data$dz)
                               if (!all(finite)) {
                                     if (!isTRUE(params$na.rm)) {
                                           rlang::warn(paste0("Removed ", sum(!finite), " row",
                                                              if (sum(!finite) > 1) "s", " containing ",
                                                              "non-finite directions (`geom_refline_3d()`)."))
                                     }
                                     data <- data[finite, , drop = FALSE]
                               }
                               check_line_directions(data)
                               data
                         },

                         draw_panel = function(data, panel_params, coord, x = 0, y = 0, z = 0,
                                               scale_depth = TRUE, on_top = FALSE, na.rm = FALSE) {

                               validate_coord3d(coord)
                               if (nrow(data) == 0) return(grid::nullGrob())

                               rows <- refline_rows(data, list(x = x, y = y, z = z),
                                                    panel_limits(panel_params))
                               if (nrow(rows) == 0) return(grid::nullGrob())

                               coords <- coord$transform(rows, panel_params, sort = FALSE)
                               finish <- function(coords) apply_depth_scaling(coords, scale_depth)

                               layer_scene_grob(coords, panel_params, finish, list(lineend = "butt"),
                                                on_top = on_top)
                         },

                         draw_key = draw_key_path
)


#' Segment rows of lines clipped to a panel
#'
#' @param data Layer data, one row per line, with directions `dx`, `dy`,
#'   `dz` and styling columns.
#' @param point List of `x`, `y`, `z`: the point each line passes through,
#'   recycled across lines.
#' @param limits Named list of the panel's x, y, z limits.
#' @return Data frame of segment rows in data space, with styling. Lines that
#'   miss the panel are skipped with a warning.
#' @keywords internal
#' @noRd
refline_rows <- function(data, point, limits) {
      n <- nrow(data)
      pts <- cbind(rep_len(point$x, n), rep_len(point$y, n), rep_len(point$z, n))
      segs <- lapply(seq_len(n), function(k) {
            direction <- c(data$dx[k], data$dy[k], data$dz[k])
            ends <- clip_line_to_box(pts[k, ], direction, limits)
            if (is.null(ends)) {
                  fmt <- function(v) paste0("(", paste(signif(v, 4), collapse = ", "), ")")
                  rlang::warn(paste0("The line through ", fmt(pts[k, ]), " in direction ",
                                     fmt(direction), " doesn't cross the panel, so it ",
                                     "isn't drawn (`geom_refline_3d()`)."))
                  return(NULL)
            }
            data.frame(x = ends[, 1], y = ends[, 2], z = ends[, 3],
                       group = sprintf("refline__%06d", k),
                       point_type = c("start", "end"), segment_id = k, .row = k)
      })
      rows <- dplyr::bind_rows(segs)
      if (nrow(rows) == 0) return(data.frame())

      style <- data[rows$.row, setdiff(names(data), c("x", "y", "z", "group", "dx", "dy", "dz")),
                    drop = FALSE]
      rows <- cbind(rows[c("x", "y", "z", "group", "point_type", "segment_id")], style)
      rownames(rows) <- NULL
      rows$.prim <- "segment"
      rows
}

refline_mapping <- function() aes(dx = dx, dy = dy, dz = dz)

check_line_directions <- function(dirs, call = NULL) {
      if (any(dirs$dx == 0 & dirs$dy == 0 & dirs$dz == 0)) {
            rlang::abort("Line directions `dx`, `dy`, and `dz` can't all be zero.", call = call)
      }
      invisible()
}


#' Clip a line to an axis-aligned box
#'
#' @param point Length-3 point on the line.
#' @param direction Length-3 direction vector (not all zero).
#' @param limits Named list of x, y, z limits.
#' @return 2 x 3 matrix of the clipped segment's endpoints, or NULL if the
#'   line misses the box or only touches its boundary at a point.
#' @keywords internal
#' @noRd
clip_line_to_box <- function(point, direction, limits) {
      lo <- vapply(limits[c("x", "y", "z")], min, numeric(1))
      hi <- vapply(limits[c("x", "y", "z")], max, numeric(1))
      t0 <- -Inf
      t1 <- Inf
      for (k in 1:3) {
            if (direction[k] == 0) {
                  if (point[k] < lo[k] || point[k] > hi[k]) return(NULL)
            } else {
                  ta <- (lo[k] - point[k]) / direction[k]
                  tb <- (hi[k] - point[k]) / direction[k]
                  t0 <- max(t0, min(ta, tb))
                  t1 <- min(t1, max(ta, tb))
            }
      }
      if ((t1 - t0) * sqrt(sum(direction^2)) <= 1e-10 * sqrt(sum((hi - lo)^2))) return(NULL)
      rbind(point + t0 * direction, point + t1 * direction)
}


# Constructor ------------------------------------------------------------------

#' 3D reference lines
#'
#' `geom_refline_3d()` draws infinite straight lines, clipped to the panel,
#' such as the line spanned by a vector, or a vertical reference line. Each
#' line passes through the point `x`, `y`, `z` in the direction `dx`, `dy`,
#' `dz`.
#'
#' Directions can be given as parameters, as vectors to draw several lines,
#' or mapped from data in `aes()`. The point a line passes through is always
#' a parameter, since it would otherwise affect the axes' ranges; when lines
#' are given as parameters, it can be a vector giving each line's point, and
#' when directions are mapped, it is a single point shared by all lines.
#'
#' Lines are built from the panel's limits when the plot is drawn, so they
#' don't expand the plot's axis ranges; a line that misses the panel entirely
#' is skipped with a warning. They are depth-sorted with the rest of the
#' scene, and drawn on top of any plane or polygon they lie in. Positions and
#' directions are in the plot's (transformed) scale space.
#'
#' @param mapping Set of aesthetic mappings created by [aes()], mapping the
#'   direction components `dx`, `dy`, and `dz`. Used only when no direction
#'   is given as a parameter.
#' @param data The data to be displayed in this layer.
#' @param x,y,z Coordinates of the point each line passes through. Default
#'   `0`.
#' @param dx,dy,dz Components of each line's direction. Each defaults to `0`,
#'   but they can't all be zero.
#' @param ... Other arguments passed on to [layer()], such as aesthetics like
#'   `colour`, `linewidth`, `linetype`, and `alpha`.
#' @inheritParams depth_params
#' @inheritParams on_top_param
#' @param na.rm If `FALSE`, lines with missing directions are removed with a
#'   warning.
#' @param show.legend Logical indicating whether this layer should be included
#'   in legends. Lines given as parameters are never included.
#' @param inherit.aes If `TRUE`, combine the plot's default aesthetics with
#'   `mapping`. Defaults to `FALSE`.
#'
#' @section Aesthetics:
#' `geom_refline_3d()` understands the following aesthetics: `dx`, `dy`,
#' `dz`, `colour`, `linewidth`, `linetype`, and `alpha`.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' # The line spanned by a vector, and a vertical line at x = 1, y = 2
#' v <- c(2, 1, 1.5)
#' ggplot(data.frame(x = 0, y = 0, z = 0, xend = v[1], yend = v[2], zend = v[3]),
#'        aes(x, y, z, xend = xend, yend = yend, zend = zend)) +
#'   geom_segment_3d(arrow = arrow(type = "closed", length = unit(8, "pt"))) +
#'   geom_refline_3d(dx = v[1], dy = v[2], dz = v[3], linetype = "dashed") +
#'   geom_refline_3d(x = 1, y = 2, dz = 1, colour = "red") +
#'   lims(x = c(-1, 3), y = c(-1, 3)) + zlim(-1, 3) +
#'   coord_3d()
#'
#' # The line along which two planes intersect
#' ggplot() +
#'   geom_plane_3d(a = c(1, 1), b = c(1, -1), c = c(1, 0), d = c(1, 0),
#'                 fill = c("tomato", "steelblue")) +
#'   geom_refline_3d(x = 0.5, y = 0.5, z = 0, dx = 1, dy = 1, dz = -2,
#'                   linewidth = 1) +
#'   lims(x = c(-1, 1), y = c(-1, 1)) + zlim(-1, 1) +
#'   coord_3d()
#' @export
geom_refline_3d <- function(mapping = NULL, data = NULL, ...,
                            x = 0, y = 0, z = 0, dx, dy, dz,
                            scale_depth = TRUE, on_top = FALSE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {

      env <- rlang::current_env()
      check_on_top(on_top)
      point <- list(x = x, y = y, z = z)
      for (nm in names(point)) {
            v <- point[[nm]]
            if (!is.numeric(v) || length(v) == 0 || any(!is.finite(v))) {
                  rlang::abort(paste0("`", nm, "` must contain only finite numbers."))
            }
            point[[nm]] <- unname(v)
      }

      given <- c(dx = !missing(dx), dy = !missing(dy), dz = !missing(dz))
      given <- names(given)[given]

      if (length(given) > 0) {
            if (!is.null(mapping) || !is.null(data)) {
                  rlang::abort(c("Give lines either as parameters or with `mapping` and `data`, not both.",
                                 i = paste0("Directions were given as parameters: ",
                                            paste0("`", given, "`", collapse = ", "), ".")))
            }
            values <- c(mget(given, envir = env), point)
            for (nm in given) {
                  v <- values[[nm]]
                  if (!is.numeric(v) || length(v) == 0 || any(!is.finite(v))) {
                        rlang::abort(paste0("`", nm, "` must contain only finite numbers."))
                  }
                  values[[nm]] <- unname(v)
            }
            n <- max(lengths(values))
            if (any(!lengths(values) %in% c(1, n))) {
                  rlang::abort("Line points and directions must have length 1 or a common length.")
            }
            values <- lapply(values, rep_len, n)
            data <- data.frame(dx = values$dx %||% 0, dy = values$dy %||% 0, dz = values$dz %||% 0)
            check_line_directions(data, call = env)
            point <- values[c("x", "y", "z")]
            mapping <- refline_mapping()
            show.legend <- FALSE
            inherit.aes <- FALSE
      } else {
            if (any(lengths(point) != 1)) {
                  rlang::abort(c("With mapped directions, `x`, `y`, and `z` must each be a single number.",
                                 i = "All mapped lines pass through the same point."))
            }
            if (is.null(mapping) && !isTRUE(inherit.aes)) {
                  rlang::abort(c("Lines need a direction.",
                                 i = "Give or map one or more of `dx`, `dy`, and `dz`."))
            }
      }

      layer(
            geom = GeomRefline3D, mapping = mapping, data = data, stat = "identity",
            position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(x = point$x, y = point$y, z = point$z, scale_depth = scale_depth,
                          on_top = on_top, na.rm = na.rm, ...)
      )
}
