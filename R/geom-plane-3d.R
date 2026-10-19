# Geom -------------------------------------------------------------------------

GeomPlane3D <- ggproto("GeomPlane3D", Geom,

                       required_aes = character(),

                       optional_aes = c("a", "b", "c", "d", "intercept", "slope_x", "slope_y"),

                       default_aes = aes(
                             fill = "grey60", colour = NA, alpha = 0.5, linewidth = 0.5, linetype = 1
                       ),

                       setup_data = function(data, params) {
                             coefs <- plane_coefficients(as.list(data), forms = c("regression", "general"),
                                                         mapped = TRUE)
                             data <- data[setdiff(names(data), unlist(.plane_forms))]
                             data <- cbind(data, coefs)

                             finite <- is.finite(data$a) & is.finite(data$b) & is.finite(data$c) & is.finite(data$d)
                             if (!all(finite)) {
                                   if (!isTRUE(params$na.rm)) {
                                         rlang::warn(paste0("Removed ", sum(!finite), " row",
                                                            if (sum(!finite) > 1) "s", " containing ",
                                                            "non-finite plane coefficients (`geom_plane_3d()`)."))
                                   }
                                   data <- data[finite, , drop = FALSE]
                             }
                             check_plane_normals(data)
                             data
                       },

                       draw_panel = function(data, panel_params, coord, light = NULL,
                                             scale_depth = TRUE, on_top = FALSE, na.rm = FALSE) {

                             validate_coord3d(coord)
                             if (nrow(data) == 0) return(grid::nullGrob())

                             rows <- plane_rows(data, panel_limits(panel_params))
                             if (nrow(rows) == 0) return(grid::nullGrob())
                             rows <- attach_light(rows, light)

                             coords <- coord$transform(rows, panel_params, sort = FALSE)

                             finish <- function(coords) {
                                   coords <- apply_depth_scaling(coords, scale_depth)
                                   blend_light(coords)
                             }

                             layer_scene_grob(coords, panel_params, finish, list(rule = "evenodd"),
                                              on_top = on_top)
                       },

                       draw_key = draw_key_polygon
)


#' Polygon rows of planes clipped to a panel
#'
#' @param data Layer data, one row per plane, with coefficients `a`, `b`,
#'   `c`, `d` and styling columns.
#' @param limits Named list of the panel's x, y, z limits.
#' @return Data frame of polygon vertices in data space, with styling,
#'   `.prim = "polygon"`, and `.two_sided = TRUE`. Planes that miss the panel
#'   are skipped with a warning.
#' @keywords internal
#' @noRd
plane_rows <- function(data, limits) {
      polys <- lapply(seq_len(nrow(data)), function(k) {
            coefs <- c(data$a[k], data$b[k], data$c[k], data$d[k])
            verts <- clip_plane_to_box(coefs[1:3], coefs[4], limits)
            if (is.null(verts)) {
                  rlang::warn(paste0("The plane ", format_plane_equation(coefs),
                                     " doesn't cross the panel, so it isn't drawn ",
                                     "(`geom_plane_3d()`)."))
                  return(NULL)
            }
            data.frame(x = verts[, 1], y = verts[, 2], z = verts[, 3],
                       group = sprintf("plane__%06d", k), .row = k)
      })
      rows <- dplyr::bind_rows(polys)
      if (nrow(rows) == 0) return(data.frame())

      style <- data[rows$.row, setdiff(names(data), c("x", "y", "z", "group", "a", "b", "c", "d")),
                    drop = FALSE]
      rows <- cbind(rows[c("x", "y", "z", "group")], style)
      rownames(rows) <- NULL
      rows$.prim <- "polygon"
      rows$.two_sided <- TRUE
      rows
}


# Plane coefficients ------------------------------------------------------------

# The forms in which planes can be given
.plane_forms <- list(
      intercept  = c("xintercept", "yintercept", "zintercept"),
      regression = c("intercept", "slope_x", "slope_y"),
      general    = c("a", "b", "c", "d")
)

#' Convert planes given in any form to general coefficients
#'
#' Planes can be given as axis intercepts (perpendicular to one axis), as a
#' regression plane `z = intercept + slope_x * x + slope_y * y`, or by general
#' coefficients `a * x + b * y + c * z = d`. All are converted to the general
#' form.
#'
#' @param values Named list of values, which may include values of other
#'   names (ignored).
#' @param forms Forms accepted.
#' @param mapped Whether the values come from mapped aesthetics, in which
#'   case they aren't checked for finiteness (rows with missing values are
#'   dropped later) and are already of equal length.
#' @param call Environment of the call to report in errors.
#' @return Data frame with columns `a`, `b`, `c`, `d`, one row per plane.
#' @keywords internal
#' @noRd
plane_coefficients <- function(values, forms = names(.plane_forms), mapped = FALSE,
                               call = rlang::caller_env()) {
      accepted <- .plane_forms[forms]
      used <- names(accepted)[vapply(accepted, function(f) any(f %in% names(values)), logical(1))]
      describe <- c(intercept = "`xintercept`, `yintercept`, or `zintercept`",
                    regression = "`intercept`, `slope_x`, and/or `slope_y`",
                    general = "`a`, `b`, `c`, and `d`")
      how <- if (mapped) "Map" else "Give"
      if (length(used) == 0) {
            rlang::abort(c(paste0("Planes need coefficients: ",
                                  paste(describe[forms], collapse = "; or "), "."),
                           i = if (mapped) "In `aes()`, only the `intercept`/`slope_x`/`slope_y` and `a`/`b`/`c`/`d` forms can be mapped."),
                         call = call)
      }
      if (length(used) > 1) {
            rlang::abort(c("Planes can't mix coefficient forms.",
                           i = paste0(how, " either ", paste(describe[used], collapse = ", or "),
                                      ", and use a separate layer for each form.")),
                         call = call)
      }
      form <- used
      given <- intersect(accepted[[form]], names(values))

      if (form == "intercept" && length(given) > 1) {
            rlang::abort(c("Give only one of `xintercept`, `yintercept`, and `zintercept`.",
                           i = "Use a separate layer for each axis."),
                         call = call)
      }
      if (form == "general") {
            missing <- setdiff(c("a", "b", "c"), given)
            if (length(missing) > 0) {
                  rlang::abort(paste0("Planes in the form `a * x + b * y + c * z = d` need ",
                                      paste0("`", missing, "`", collapse = ", "), "."),
                               call = call)
            }
      }

      vals <- lapply(values[given], function(v) unname(v))
      for (nm in given) {
            v <- vals[[nm]]
            if (!is.numeric(v) || length(v) == 0 || (!mapped && any(!is.finite(v)))) {
                  rlang::abort(paste0("`", nm, "` must contain only finite numbers."), call = call)
            }
      }
      n <- max(lengths(vals))
      if (any(!lengths(vals) %in% c(1, n))) {
            rlang::abort(paste0("Plane coefficients must have length 1 or a common length: ",
                                paste0("`", given, "`", collapse = ", "), "."),
                         call = call)
      }
      vals <- lapply(vals, rep_len, n)
      zero <- rep(0, n)

      coefs <- switch(form,
            intercept = {
                  axis <- substr(given, 1, 1)
                  list(a = zero + (axis == "x"), b = zero + (axis == "y"),
                       c = zero + (axis == "z"), d = vals[[given]])
            },
            regression = list(a = -(vals[["slope_x"]] %||% zero), b = -(vals[["slope_y"]] %||% zero),
                              c = zero + 1, d = vals[["intercept"]] %||% zero),
            general = list(a = vals[["a"]], b = vals[["b"]], c = vals[["c"]], d = vals[["d"]] %||% zero)
      )
      out <- as.data.frame(coefs)
      if (!mapped) check_plane_normals(out, call = call)
      out
}

plane_mapping <- function() aes(a = a, b = b, c = c, d = d)

check_plane_normals <- function(coefs, call = NULL) {
      if (any(coefs$a == 0 & coefs$b == 0 & coefs$c == 0, na.rm = TRUE)) {
            rlang::abort("Plane coefficients `a`, `b`, and `c` can't all be zero.", call = call)
      }
      invisible()
}


#' Format a plane's equation for messages
#'
#' @param coefs Numeric vector of coefficients `a`, `b`, `c`, `d`.
#' @return Character string such as `"y = 2"` or `"x - 2y + z = 1"`.
#' @keywords internal
#' @noRd
format_plane_equation <- function(coefs) {
      lhs <- c(x = coefs[1], y = coefs[2], z = coefs[3])
      lhs <- lhs[lhs != 0]
      num <- function(v) format(signif(v, 4))
      terms <- paste0(ifelse(abs(lhs) == 1, "", num(abs(lhs))), names(lhs))
      signs <- ifelse(lhs < 0, "-", "+")
      out <- paste0(if (signs[1] == "-") "-" else "", terms[1])
      for (i in seq_along(terms)[-1]) out <- paste(out, signs[i], terms[i])
      paste(out, "=", num(coefs[4]))
}


# Panel geometry ------------------------------------------------------------------

#' A panel's limits along each axis, in (transformed) scale space
#' @keywords internal
#' @noRd
panel_limits <- function(panel_params) {
      list(x = panel_params$scale_info$x$limits,
           y = panel_params$scale_info$y$limits,
           z = panel_params$scale_info$z$limits)
}


#' Clip a plane to an axis-aligned box
#'
#' Finds where the plane `normal . p = offset` crosses the box's edges (and
#' any corners lying on it), and orders those points around the resulting
#' convex polygon.
#'
#' @param normal Length-3 numeric vector `c(a, b, c)`.
#' @param offset Plane offset `d`.
#' @param limits Named list of x, y, z limits.
#' @return Matrix of polygon vertices (n x 3, n >= 3), or NULL if the plane
#'   doesn't cross the box.
#' @keywords internal
#' @noRd
clip_plane_to_box <- function(normal, offset, limits) {
      lo <- vapply(limits[c("x", "y", "z")], min, numeric(1))
      hi <- vapply(limits[c("x", "y", "z")], max, numeric(1))
      corners <- as.matrix(expand.grid(x = c(lo[1], hi[1]), y = c(lo[2], hi[2]),
                                       z = c(lo[3], hi[3])))
      f <- as.vector(corners %*% normal - offset)
      tol <- 1e-10 * sqrt(sum(normal^2)) * sqrt(sum((hi - lo)^2))
      f[abs(f) <= tol] <- 0

      pts <- corners[f == 0, , drop = FALSE]
      for (i in 1:7) {
            for (j in (i + 1):8) {
                  if (sum(corners[i, ] != corners[j, ]) != 1) next    # box edges only
                  if (f[i] * f[j] < 0) {
                        t <- f[i] / (f[i] - f[j])
                        pts <- rbind(pts, corners[i, ] + t * (corners[j, ] - corners[i, ]))
                  }
            }
      }
      if (nrow(pts) < 3) return(NULL)

      # Drop near-duplicates, then order by angle around the centroid
      scale <- hi - lo
      scale[scale == 0] <- 1
      key <- round(sweep(pts, 2, scale, "/"), 9)
      pts <- pts[!duplicated(key), , drop = FALSE]
      if (nrow(pts) < 3) return(NULL)

      n <- normal / sqrt(sum(normal^2))
      helper <- diag(3)[which.min(abs(n)), ]
      u <- cross3(n, helper); u <- u / sqrt(sum(u^2))
      v <- cross3(n, u)
      centred <- sweep(pts, 2, colMeans(pts))
      angle <- atan2(centred %*% v, centred %*% u)
      unname(pts[order(angle), , drop = FALSE])
}


# Constructor ------------------------------------------------------------------

#' 3D reference planes
#'
#' `geom_plane_3d()` draws planes clipped to the panel, such as reference
#' planes at a fixed value of one variable, a fitted regression plane, or the
#' planes of a system of linear equations. Like [ggplot2::geom_hline()], it
#' can draw planes given as parameters, or planes mapped from data.
#'
#' @section Plane forms:
#' Planes can be given in one of three forms:
#'
#' * `xintercept`, `yintercept`, or `zintercept`: planes perpendicular to an
#'   axis, e.g. `yintercept = 2` for the plane `y = 2`. Only one of the three
#'   can be used per layer. This form can only be given as parameters, not
#'   mapped, since it would otherwise affect the axis's range.
#' * `intercept`, `slope_x`, `slope_y`: the plane
#'   `z = intercept + slope_x * x + slope_y * y`, as from a linear model. Any
#'   of the three left out defaults to `0`.
#' * `a`, `b`, `c`, `d`: the general form `a * x + b * y + c * z = d`.
#'   Requires `a`, `b`, and `c` (not all zero); `d` defaults to `0`.
#'
#' Given as parameters, values can be vectors to draw several planes, along
#' with styling parameters of the same length. In the general form, for
#' example, `a`, `b`, `c` can be the columns of a matrix `A` and `d` the
#' right-hand side of the linear system `A x = d`. To draw planes from a data
#' frame instead, map the coefficients of the regression or general form in
#' `aes()`. Each layer uses one form.
#'
#' @section Rendering:
#' Planes are built from the panel's limits when the plot is drawn, so they
#' don't expand the plot's axis ranges; a plane that misses the panel
#' entirely is skipped with a warning. They are depth-sorted with the rest of
#' the scene, split where they intersect other polygons, lit from whichever
#' side faces the viewer, and never removed by back-face culling. Points,
#' segments, lines, and text lying exactly in a plane are drawn on top of it.
#'
#' Coefficients are in the plot's (transformed) scale space: with
#' `scale_x_log10()`, for example, `xintercept = 2` is the plane at `x = 100`.
#'
#' @param mapping Set of aesthetic mappings created by [aes()], mapping the
#'   coefficients `intercept`, `slope_x`, and `slope_y`, or `a`, `b`, `c`, and
#'   `d`. Used only when no coefficients are given as parameters.
#' @param data The data to be displayed in this layer.
#' @param xintercept,yintercept,zintercept Positions of planes perpendicular to
#'   the x, y, or z axis.
#' @param intercept,slope_x,slope_y Coefficients of regression planes
#'   `z = intercept + slope_x * x + slope_y * y`.
#' @param a,b,c,d Coefficients of planes `a * x + b * y + c * z = d`.
#' @param ... Other arguments passed on to [layer()], such as aesthetics like
#'   `fill`, `alpha`, `colour` (the outline, none by default), `linewidth`,
#'   and `linetype`.
#' @inheritParams light_param
#' @inheritParams depth_params
#' @inheritParams on_top_param
#' @param na.rm If `FALSE`, planes with missing coefficients are removed with
#'   a warning.
#' @param show.legend Logical indicating whether this layer should be included
#'   in legends. Planes given as parameters are never included.
#' @param inherit.aes If `TRUE`, combine the plot's default aesthetics with
#'   `mapping`. Defaults to `FALSE`, since planes rarely share the plot's
#'   aesthetics.
#'
#' @section Aesthetics:
#' `geom_plane_3d()` understands the following aesthetics: `a`, `b`, `c`,
#' `d`, `intercept`, `slope_x`, `slope_y` (see above), `fill`, `alpha`,
#' `colour`, `linewidth`, and `linetype`.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' # Reference planes through a point cloud
#' p <- ggplot(mtcars, aes(wt, hp, mpg)) +
#'   geom_point_3d() +
#'   coord_3d(yaw = 120)
#' p + geom_plane_3d(zintercept = 20)
#'
#' # A fitted regression plane
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' b <- coef(fit)
#' p + geom_plane_3d(intercept = b[1], slope_x = b["wt"], slope_y = b["hp"],
#'                   fill = "steelblue")
#'
#' # The planes of a linear system, meeting at its solution
#' A <- rbind(c(1, 1, 1), c(1, -1, 0), c(0, 1, -1))
#' ggplot() +
#'   geom_plane_3d(a = A[, 1], b = A[, 2], c = A[, 3], d = c(1, 0, 0),
#'                 fill = c("tomato", "seagreen", "steelblue")) +
#'   geom_point_3d(aes(x, y, z), data = data.frame(x = 1/3, y = 1/3, z = 1/3),
#'                 size = 3) +
#'   lims(x = c(-1, 1), y = c(-1, 1)) + zlim(-1, 1) +
#'   coord_3d()
#'
#' # Planes mapped from data, with a fill scale
#' planes <- data.frame(slope_x = c(-0.5, 0, 0.5), model = c("a", "b", "c"))
#' ggplot() +
#'   geom_plane_3d(aes(slope_x = slope_x, fill = model), data = planes) +
#'   lims(x = c(-1, 1), y = c(-1, 1)) + zlim(-1, 1) +
#'   coord_3d()
#' @export
geom_plane_3d <- function(mapping = NULL, data = NULL, ...,
                          xintercept, yintercept, zintercept,
                          intercept, slope_x, slope_y,
                          a, b, c, d,
                          light = NULL, scale_depth = TRUE, on_top = FALSE,
                          na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {

      # `c` is an argument here, so base::c() is called explicitly
      env <- rlang::current_env()
      given <- base::c(xintercept = !missing(xintercept), yintercept = !missing(yintercept),
                       zintercept = !missing(zintercept), intercept = !missing(intercept),
                       slope_x = !missing(slope_x), slope_y = !missing(slope_y),
                       a = !missing(a), b = !missing(b), c = !missing(c), d = !missing(d))
      given <- names(given)[given]
      check_on_top(on_top)

      if (length(given) > 0) {
            if (!is.null(mapping) || !is.null(data)) {
                  rlang::abort(base::c("Give planes either as parameters or with `mapping` and `data`, not both.",
                                       i = paste0("Coefficients were given as parameters: ",
                                                  paste0("`", given, "`", collapse = ", "), ".")))
            }
            data <- plane_coefficients(mget(given, envir = env), call = env)
            mapping <- plane_mapping()
            show.legend <- FALSE
            inherit.aes <- FALSE
      } else if (is.null(mapping) && !isTRUE(inherit.aes)) {
            plane_coefficients(list(), call = env)
      }

      layer(
            geom = GeomPlane3D, mapping = mapping, data = data, stat = "identity",
            position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(light = light, scale_depth = scale_depth, on_top = on_top,
                          na.rm = na.rm, ...)
      )
}
