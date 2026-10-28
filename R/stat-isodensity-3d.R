# Isosurfaces of a 3D kernel density estimate, enclosing given fractions of
# probability mass


StatIsodensity3D <- ggproto("StatIsodensity3D", Stat,
                            required_aes = c("x", "y", "z"),
                            optional_aes = "weight",
                            dropped_aes = "weight",

                            setup_data = function(data, params) {
                                  censor_z_limits(data)
                            },

                            compute_group = function(data, scales, probs = 0.5, n = 30,
                                                     h = NULL, adjust = 1, cap = FALSE,
                                                     cull_backfaces = FALSE, light = NULL) {
                                  check_isosurface_engine()
                                  check_cap(cap)

                                  if (!is.numeric(probs) || length(probs) == 0 || anyNA(probs) ||
                                      any(probs <= 0 | probs >= 1)) {
                                        rlang::abort("`probs` must be a numeric vector of values between 0 and 1.")
                                  }
                                  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 2) {
                                        rlang::abort("`n` must be a single number of at least 2.")
                                  }
                                  if (nrow(data) < 2) {
                                        rlang::warn("Isodensity surfaces need at least two points per group; dropping a group with fewer.")
                                        return(data.frame())
                                  }

                                  weight <- data$weight %||% rep(1, nrow(data))
                                  h <- isodensity_bandwidth(data, h, adjust)
                                  ranges <- isodensity_ranges(data, h)
                                  grid <- isodensity_grid(data$x, data$y, data$z, weight, h, n, ranges)
                                  levels <- isodensity_levels(grid$values, probs)

                                  # Cut surfaces where they cross the scale limits, by evaluating
                                  # the density over just the range within them. Levels come from
                                  # the full grid, so that limits don't change the surfaces' shape.
                                  limited <- Map(function(r, lim) c(max(r[1], lim[1]), min(r[2], lim[2])),
                                                 ranges, scale_limits_3d(scales))
                                  if (any(vapply(limited, diff, numeric(1)) <= 0)) return(data.frame())
                                  if (!identical(limited, ranges)) {
                                        grid <- isodensity_grid(data$x, data$y, data$z, weight, h, n, limited)
                                  }

                                  # Caps close surfaces at the limits, the only places they're cut
                                  caps <- if (cap) Map(function(r, l) c(l[1] > r[1], l[2] < r[2]), ranges, limited)

                                  surfaces <- lapply(seq_along(probs), function(i) {
                                        tri <- isosurface_triangles(grid, levels[i])
                                        if (is.null(tri)) return(NULL)
                                        tri$group <- paste0("isodensity", data$group[1], "_prob", i,
                                                            "_tri", tri$group)
                                        tri$prob <- probs[i]
                                        tri$level <- levels[i]
                                        tri
                                  })
                                  out <- do.call(rbind, surfaces)

                                  if (!is.null(caps)) {
                                        cap_polys <- isosurface_caps(grid, levels, caps)
                                        if (!is.null(cap_polys)) {
                                              cap_polys$group <- paste0("isodensity", data$group[1], "_prob",
                                                                        cap_polys$level_index, "_cap", cap_polys$group)
                                              cap_polys$prob <- probs[cap_polys$level_index]
                                              cap_polys$level <- levels[cap_polys$level_index]
                                              out <- bind_caps(out, cap_polys)
                                        }
                                  }
                                  if (is.null(out)) return(data.frame())

                                  out$cull_backfaces <- cull_backfaces
                                  attach_light(out, light)
                            }
)


#' Bandwidths for a 3D kernel density estimate
#'
#' Bandwidths follow the convention of [MASS::kde2d()], and of
#' [stat_density_3d()]: four times the standard deviation of the Gaussian
#' kernel. The default is the normal reference rule for three dimensions,
#' giving kernel standard deviations of `(4/5)^(1/7) * s * n^(-1/7)`, where `s`
#' is the smaller of the standard deviation and the interquartile range
#' divided by 1.34.
#'
#' @param data Data frame with `x`, `y`, and `z` columns.
#' @param h `NULL`, or bandwidths: a single number or one per axis.
#' @param adjust Multiplier for the bandwidths: a single number or one per axis.
#' @return Length-3 vector of bandwidths.
#' @keywords internal
#' @noRd
isodensity_bandwidth <- function(data, h = NULL, adjust = 1) {
      if (is.null(h)) {
            h <- vapply(data[c("x", "y", "z")], function(v) {
                  s <- min(stats::sd(v), stats::IQR(v) / 1.34)
                  if (s == 0) s <- stats::sd(v)
                  4 * (4 / 5)^(1 / 7) * s * length(v)^(-1 / 7)
            }, numeric(1))
            if (any(h == 0)) {
                  rlang::abort("Isodensity surfaces need variation in each of `x`, `y`, and `z`.")
            }
      }
      if (!is.numeric(h) || !length(h) %in% c(1, 3) || anyNA(h) || any(h <= 0)) {
            rlang::abort("`h` must be `NULL`, or one or three positive numbers.")
      }
      if (!is.numeric(adjust) || !length(adjust) %in% c(1, 3) || anyNA(adjust) || any(adjust <= 0)) {
            rlang::abort("`adjust` must be one or three positive numbers.")
      }
      unname(rep_len(h, 3) * rep_len(adjust, 3))
}


#' Ranges over which to evaluate a 3D kernel density estimate
#'
#' Each spans the data, padded on each side by three kernel standard
#' deviations, so that the density is negligible at the edges and its
#' isosurfaces close.
#'
#' @param data Data frame with `x`, `y`, and `z` columns.
#' @param h Length-3 vector of bandwidths, following `isodensity_bandwidth()`.
#' @return List of length-2 ranges for `x`, `y`, and `z`.
#' @keywords internal
#' @noRd
isodensity_ranges <- function(data, h) {
      Map(function(v, s) range(v) + c(-3, 3) * s, data[c("x", "y", "z")], h / 4)
}


#' Evaluate a 3D Gaussian kernel density estimate on a grid
#'
#' @param x,y,z Point coordinates.
#' @param weight Point weights.
#' @param h Length-3 vector of bandwidths, following `isodensity_bandwidth()`.
#' @param n Number of grid points per axis.
#' @param ranges List of length-2 ranges for the grid's `x`, `y`, and `z`.
#' @return List with grid coordinates `x`, `y`, `z`, and a `values` array of
#'   densities.
#' @keywords internal
#' @noRd
isodensity_grid <- function(x, y, z, weight, h, n, ranges) {
      sd <- h / 4
      gx <- seq(ranges$x[1], ranges$x[2], length.out = n)
      gy <- seq(ranges$y[1], ranges$y[2], length.out = n)
      gz <- seq(ranges$z[1], ranges$z[2], length.out = n)

      kernel <- function(g, v, s) outer(g, v, function(a, b) stats::dnorm(a, b, s))
      kx <- kernel(gx, x, sd[1])
      ky <- kernel(gy, y, sd[2])
      kz <- kernel(gz, z, sd[3])

      # The density is a sum over points of a separable kernel, so each z slice
      # is a matrix product
      w <- weight / sum(weight)
      values <- array(0, c(n, n, n))
      for (k in seq_len(n)) {
            values[, , k] <- (kx * rep(w * kz[k, ], each = n)) %*% t(ky)
      }
      list(x = gx, y = gy, z = gz, values = values)
}


#' Density levels of highest-density regions
#'
#' @param values Array of densities on an evenly spaced grid.
#' @param probs Fractions of probability mass to enclose.
#' @return The density above which each fraction of the mass lies.
#' @keywords internal
#' @noRd
isodensity_levels <- function(values, probs) {
      dens <- sort(as.vector(values), decreasing = TRUE)
      mass <- cumsum(dens) / sum(dens)
      dens[pmin(findInterval(probs, mass, left.open = TRUE) + 1, length(dens))]
}


#' 3D density surfaces
#'
#' Estimates the density of a 3D point cloud, and draws surfaces of constant
#' density enclosing given fractions of it: the 3D analog of
#' [ggplot2::geom_density_2d()]. Each surface bounds the smallest region
#' containing a given fraction of the estimated probability mass (a "highest
#' density region"), so that e.g. `probs = 0.5` shows where the densest half of
#' the data lie.
#'
#' The density is a Gaussian kernel density estimate, evaluated on a grid that
#' extends beyond the data far enough for surfaces to close, and surfaces are
#' triangulated with the marching cubes algorithm. Where limits are set on the
#' plot's scales, e.g. with [ggplot2::xlim()] or [zlim()], points outside them
#' are removed before the density is estimated, as elsewhere in ggplot2, and
#' surfaces are cut at them, leaving them open there unless `cap = TRUE`. Cutting
#' doesn't change the fractions of mass the surfaces enclose, which refer to the
#' whole estimate.
#'
#' @param probs Numeric vector of fractions of probability mass, between 0 and
#'   1, for each surface to enclose. Defaults to `0.5`, a single surface around
#'   the densest half of the data. Nested surfaces hide one another unless they
#'   are translucent, e.g. with `alpha`.
#' @param n Number of grid points along each axis at which the density is
#'   estimated. Larger values give smoother surfaces, at a cost in time that
#'   grows with its cube.
#' @param h Bandwidths of the kernel: a single number, or one for each of `x`,
#'   `y`, and `z`. As in [stat_density_3d()] and [MASS::kde2d()], a bandwidth
#'   is four times the standard deviation of the Gaussian kernel. If `NULL`
#'   (the default), each is estimated with the normal reference rule for three
#'   dimensions, `4 * (4/5)^(1/7) * s * n^(-1/7)`, where `n` is the number of
#'   points and `s` the smaller of their standard deviation and interquartile
#'   range divided by 1.34. Weights are ignored in estimating bandwidths.
#' @param adjust Multiplier for the bandwidths, a single number or one per
#'   axis, e.g. `adjust = 2` for a smoother estimate.
#' @param cap Logical indicating whether to close surfaces with flat caps where
#'   they're cut by limits set on the plot's scales, so that they show solid
#'   cross sections rather than being left open there. Defaults to `FALSE`.
#' @inheritParams geom_isosurface_3d
#'
#' @section Aesthetics:
#' `stat_isodensity_3d()` requires the following aesthetics:
#' - **x**, **y**, **z**: Point coordinates
#'
#' and understands **weight**, giving points' relative weights in the
#' density. Grouping aesthetics give a separate density for each group.
#'
#' `geom_isodensity_3d()` draws surfaces like [geom_isosurface_3d()], and
#' understands the same aesthetics.
#'
#' @section Computed variables:
#' - `prob`: The fraction of probability mass the surface encloses
#' - `level`: The density on the surface
#' - `normal_x`, `normal_y`, `normal_z`: Components of each triangle's unit
#'   normal, pointing outward from the region the surface encloses
#' - `cap`: Whether a triangle is part of a cap (see `cap`)
#'
#' @section Dependencies:
#' Surfaces are computed with the \pkg{rmarchingcubes} package, which must be
#' installed.
#'
#' @examplesIf requireNamespace("rmarchingcubes", quietly = TRUE)
#' # The densest half of each species
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, Petal.Length, fill = Species)) +
#'   geom_isodensity_3d() +
#'   light(mode = "hsl", direction = c(-1, 1, 0)) +
#'   coord_3d()
#'
#' # Nested translucent surfaces around Fiji earthquakes, which trace
#' # the Tonga slab subducting beneath the Pacific plate
#' \donttest{
#' ggplot(quakes, aes(long, lat, -depth)) +
#'   geom_isodensity_3d(alpha = .5, aes(fill = after_stat(prob)),
#'                      probs = c(0.25, 0.5, 0.75)) +
#'   geom_point_3d(size = 0.2) +
#'   scale_fill_viridis_c() +
#'   coord_3d(yaw = 180, roll = -70, scales = "fixed", ratio = c(1, 1, .01)) +
#'   labs(z = "depth (km)") +
#'   light(direction = c(-1, 1, 1)) +
#'   zlim(NA, 0) +
#'   theme_dark() + theme(panel.border = element_rect(color = "gray70"))
#' }
#'
#' @seealso [geom_isosurface_3d()] and [geom_isofunction_3d()] for
#'   isosurfaces of gridded fields and of functions, [geom_hull_3d()] for hulls
#'   of point clouds, [stat_density_3d()] for 2D densities drawn as surfaces.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_isodensity_3d
#' @export
geom_isodensity_3d <- function(mapping = NULL, data = NULL,
                               stat = StatIsodensity3D,
                               position = "identity",
                               ...,
                               probs = 0.5,
                               n = 30,
                               h = NULL,
                               adjust = 1,
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
            params = list(probs = probs, n = n, h = h, adjust = adjust, cap = cap, light = light,
                          cull_backfaces = cull_backfaces, scale_depth = scale_depth,
                          na.rm = na.rm, ...))
}

#' @rdname geom_isodensity_3d
#' @export
stat_isodensity_3d <- function(mapping = NULL, data = NULL,
                               geom = GeomIsosurface3D,
                               position = "identity",
                               ...,
                               probs = 0.5,
                               n = 30,
                               h = NULL,
                               adjust = 1,
                               cap = FALSE,
                               light = NULL,
                               cull_backfaces = FALSE,
                               scale_depth = TRUE,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
      layer(data = data, mapping = mapping,
            stat = StatIsodensity3D, geom = get_proto(geom, "geom"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(probs = probs, n = n, h = h, adjust = adjust, cap = cap, light = light,
                          cull_backfaces = cull_backfaces, scale_depth = scale_depth,
                          na.rm = na.rm, ...))
}
