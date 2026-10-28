# Isosurfaces of a function of three variables, evaluated on a grid and
# triangulated by marching cubes


StatIsofunction3D <- ggproto("StatIsofunction3D", Stat,
                             required_aes = character(0),

                             setup_data = function(data, params) {
                                   if (nrow(data) == 0) {
                                         data <- data.frame(group = -1L)
                                   }
                                   data
                             },

                             compute_panel = function(data, scales, fun = NULL,
                                                      xlim = NULL, ylim = NULL, zlim = NULL,
                                                      n = 30, levels = 0, inside = "below",
                                                      cap = FALSE, cull_backfaces = FALSE,
                                                      light = NULL) {
                                   check_isosurface_engine()
                                   check_cap(cap)
                                   check_inside(inside)
                                   check_levels(levels)
                                   if (!is.function(fun)) {
                                         rlang::abort("`fun` must be a function of `x`, `y`, and `z`.")
                                   }
                                   if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 2) {
                                         rlang::abort("`n` must be a single number of at least 2.")
                                   }

                                   # ggplot2 trains the z scale only after stats are computed, so
                                   # only limits set on it are available here
                                   z_scale <- find_plot_z_scale()
                                   z_limits <- if (!is.null(z_scale) && is.numeric(z_scale$limits)) z_scale$limits
                                   lims <- list(
                                         x = isofunction_limits(xlim, scales$x, "xlim"),
                                         y = isofunction_limits(ylim, scales$y, "ylim"),
                                         z = isofunction_limits(zlim %||% z_limits, NULL, "zlim")
                                   )
                                   grid <- isofunction_grid(fun, lims, n)

                                   isosurface_layer(grid, levels, inside, scales, cap, "isofunction",
                                                    "`fun`", cull_backfaces, light)
                             }
)


#' Resolve the domain of a function along one axis
#'
#' @param lim A length-2 range given by the user, or `NULL`.
#' @param scale The axis's scale, whose range is used when `lim` is `NULL`.
#' @param name Name of the argument, for messages.
#' @return A length-2 numeric range.
#' @keywords internal
#' @noRd
isofunction_limits <- function(lim, scale, name) {
      if (is.null(lim)) {
            if (is.null(scale) || scale$is_empty()) {
                  rlang::abort(c(
                        paste0("`", name, "` must be given, as the range of its axis isn't known."),
                        i = "Give the function's domain with `xlim`, `ylim`, and `zlim`."))
            }
            lim <- scale$dimension()
      }
      if (!is.numeric(lim) || length(lim) != 2 || any(!is.finite(lim)) || lim[1] >= lim[2]) {
            rlang::abort(paste0("`", name, "` must be two finite increasing numbers."))
      }
      lim
}


#' Evaluate a function of three variables on a grid
#'
#' @param fun Function of `x`, `y`, and `z`, vectorized over them.
#' @param lims List of length-2 ranges for `x`, `y`, and `z`.
#' @param n Number of grid points per axis.
#' @return List with grid coordinates `x`, `y`, `z`, and a `values` array, as
#'   from `isosurface_grid()`.
#' @keywords internal
#' @noRd
isofunction_grid <- function(fun, lims, n) {
      axes <- lapply(lims, function(l) seq(l[1], l[2], length.out = n))
      points <- expand.grid(axes)

      values <- tryCatch(
            fun(points$x, points$y, points$z),
            error = function(e) {
                  rlang::abort("`fun` failed when evaluated over the grid.", parent = e)
            }
      )
      if (!is.numeric(values) || length(values) != nrow(points)) {
            rlang::abort(paste0(
                  "`fun` must return one number for each set of `x`, `y`, and `z` values, ",
                  "taking vectors of each."))
      }
      if (any(!is.finite(values))) {
            rlang::abort("`fun` must return finite values everywhere in the domain.")
      }

      list(x = axes$x, y = axes$y, z = axes$z, values = array(values, rep(n, 3)))
}


#' 3D isosurfaces of functions
#'
#' Draws isosurfaces of a function of three variables, i.e. the surfaces on which
#' it equals given values. By default it draws the implicit surface where the
#' function equals zero, enclosing the region where it's negative, so that e.g.
#' `function(x, y, z) x^2 + y^2 + z^2 - 1` gives a unit ball. This is to [geom_isosurface_3d()] what
#' [geom_function_3d()] is to [geom_surface_3d()]: rather than reading a field
#' from gridded data, it evaluates a function over a grid spanning a given
#' domain. Surfaces are triangulated with the marching cubes algorithm.
#'
#' Where surfaces reach the edges of the domain, they're left open there,
#' unless `cap = TRUE`. Limits set on the plot's scales, e.g. with
#' [ggplot2::xlim()] or [zlim()], cut surfaces in the same way.
#'
#' @param fun A function of `x`, `y`, and `z`, taking vectors of each and
#'   returning a vector of values.
#' @param xlim,ylim,zlim Length-2 numeric vectors giving the domain over which
#'   to evaluate `fun`. If `xlim` or `ylim` is `NULL`, the range of the plot's
#'   scale for that axis is used, as set by other layers or by limits. If
#'   `zlim` is `NULL`, limits set on the plot's z scale are used, e.g. with
#'   [zlim()]; the range of other layers' data isn't yet known when the
#'   function is evaluated.
#' @param n Number of grid points along each axis at which `fun` is evaluated.
#'   Larger values give smoother surfaces, at a cost in time that grows with
#'   its cube.
#' @param levels Numeric vector of values of `fun` at which to draw
#'   isosurfaces, one surface per value. Defaults to `0`, the implicit surface
#'   `fun(x, y, z) = 0`.
#' @param inside Which side of each surface is its inside, the region it
#'   encloses: `"below"` (the default) for where `fun` is less than the level,
#'   following the usual convention for implicit surfaces, or `"above"` for
#'   where it's greater. Surfaces face outward from their insides, which
#'   determines where `cap` adds caps, which triangles `cull_backfaces`
#'   removes, and the direction of the computed normals.
#' @param cap Logical indicating whether to close surfaces with flat caps
#'   where they meet the edges of the domain, or limits set on the plot's
#'   scales, so that they show solid cross sections rather than being left
#'   open there. Defaults to `FALSE`.
#' @param data Not needed, as the layer draws a function rather than data.
#' @param stat The statistical transformation to use on the data. Defaults to
#'   `StatIsofunction3D`.
#' @param geom The geometric object used to display the data. Defaults to
#'   `GeomIsosurface3D`, a variant of [geom_polygon_3d()].
#' @inheritParams geom_isosurface_3d
#'
#' @section Aesthetics:
#' `geom_isofunction_3d()` understands the same aesthetics as
#' [geom_isosurface_3d()].
#'
#' @section Computed variables:
#' - `level`: The value of `fun` on the surface
#' - `normal_x`, `normal_y`, `normal_z`: Components of each triangle's unit
#'   normal, pointing outward from the region the surface encloses (see
#'   `inside`)
#' - `cap`: Whether a triangle is part of a cap (see `cap`)
#'
#' @section Dependencies:
#' Isosurfaces are computed with the \pkg{rmarchingcubes} package, which must
#' be installed.
#'
#' @examplesIf requireNamespace("rmarchingcubes", quietly = TRUE)
#' # An ellipsoid: the region where the quadratic form t(v) %*% A %*% v is
#' # below 1, as for a covariance ellipsoid
#' A <- solve(matrix(c(3, 1, 0.5,
#'                     1, 2, 0.3,
#'                     0.5, 0.3, 1), 3))
#' quadratic_form <- function(x, y, z) {
#'   v <- rbind(x, y, z)
#'   colSums(v * (A %*% v))
#' }
#' ggplot() +
#'   geom_isofunction_3d(fun = quadratic_form, levels = 1,
#'                       xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2),
#'                       fill = "steelblue") +
#'   coord_3d(scales = "fixed")
#'
#' \donttest{
#' # The gyroid, an implicit surface, cut by limits and capped
#' gyroid <- function(x, y, z) sin(x) * cos(y) + sin(y) * cos(z) + sin(z) * cos(x)
#' ggplot() +
#'   geom_isofunction_3d(fun = gyroid, cap = TRUE, fill = "#993333", n = 40,
#'                       xlim = c(0, 4 * pi), ylim = c(0, 4 * pi), zlim = c(0, 4 * pi)) +
#'   coord_3d() + light(mode = "hsl") + theme_void()
#' }
#'
#' @seealso [geom_isosurface_3d()] for isosurfaces of gridded data,
#'   [geom_isodensity_3d()] for isosurfaces of densities of points,
#'   [geom_function_3d()] for surfaces of functions of two variables.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_isofunction_3d
#' @export
geom_isofunction_3d <- function(mapping = NULL, data = ensure_nonempty_data,
                                stat = StatIsofunction3D,
                                position = "identity",
                                ...,
                                fun = NULL,
                                xlim = NULL, ylim = NULL, zlim = NULL,
                                n = 30,
                                levels = 0,
                                inside = "below",
                                cap = FALSE,
                                light = NULL,
                                cull_backfaces = FALSE,
                                scale_depth = TRUE,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
      layer(data = data, mapping = mapping,
            stat = get_proto(stat, "stat"), geom = GeomIsosurface3D,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(fun = fun, xlim = xlim, ylim = ylim, zlim = zlim, n = n,
                          levels = levels, inside = inside, cap = cap, light = light,
                          cull_backfaces = cull_backfaces, scale_depth = scale_depth,
                          na.rm = na.rm, ...))
}

#' @rdname geom_isofunction_3d
#' @export
stat_isofunction_3d <- function(mapping = NULL, data = ensure_nonempty_data,
                                geom = GeomIsosurface3D,
                                position = "identity",
                                ...,
                                fun = NULL,
                                xlim = NULL, ylim = NULL, zlim = NULL,
                                n = 30,
                                levels = 0,
                                inside = "below",
                                cap = FALSE,
                                light = NULL,
                                cull_backfaces = FALSE,
                                scale_depth = TRUE,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
      layer(data = data, mapping = mapping,
            stat = StatIsofunction3D, geom = get_proto(geom, "geom"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(fun = fun, xlim = xlim, ylim = ylim, zlim = zlim, n = n,
                          levels = levels, inside = inside, cap = cap, light = light,
                          cull_backfaces = cull_backfaces, scale_depth = scale_depth,
                          na.rm = na.rm, ...))
}
