# Isosurfaces of a scalar field sampled on a 3D grid, triangulated by marching cubes


StatIsosurface3D <- ggproto("StatIsosurface3D", Stat,
                            required_aes = c("x", "y", "z", "value"),
                            dropped_aes = "value",

                            compute_group = function(data, scales, levels = NULL, inside = "above",
                                                     cap = FALSE, cull_backfaces = FALSE, light = NULL) {
                                  check_isosurface_engine()
                                  check_cap(cap)
                                  check_inside(inside)
                                  grid <- isosurface_grid(data)

                                  if (is.null(levels)) levels <- mean(range(grid$values))
                                  check_levels(levels)

                                  isosurface_layer(grid, levels, inside, scales, cap, paste0("iso", data$group[1]),
                                                   "`value`", cull_backfaces, light)
                            }
)


# Isosurfaces are outlined in their fill color, like other polygon layers (see
# `GeomPolygon3D$outline_matches_fill`)
GeomIsosurface3D <- ggproto("GeomIsosurface3D", GeomPolygon3D)


#' Stop unless the marching cubes engine is installed
#' @keywords internal
#' @noRd
check_isosurface_engine <- function() {
      if (!isosurface_engine_available()) {
            rlang::abort(c("The rmarchingcubes package is required for isosurfaces.",
                           i = 'Install it with `install.packages("rmarchingcubes")`.'))
      }
}

isosurface_engine_available <- function() {
      requireNamespace("rmarchingcubes", quietly = TRUE)
}


#' Arrange gridded data as a 3D array
#'
#' @param data Data frame with `x`, `y`, `z`, and `value` columns, one row per
#'   point of a complete rectilinear grid, in any order.
#' @return List with the sorted unique `x`, `y`, and `z` coordinates, and a
#'   `values` array indexed by them.
#' @keywords internal
#' @noRd
isosurface_grid <- function(data) {
      axes <- lapply(data[c("x", "y", "z")], function(v) sort(unique(v)))
      n <- lengths(axes)

      if (any(n < 2)) {
            rlang::abort("Isosurfaces need at least two distinct values of each of `x`, `y`, and `z`.")
      }
      if (any(!is.finite(data$value))) {
            rlang::abort("Isosurfaces need a finite `value` at every grid point.")
      }

      i <- match(data$x, axes$x)
      j <- match(data$y, axes$y)
      k <- match(data$z, axes$z)
      cell <- i + (j - 1) * n[1] + (k - 1) * n[1] * n[2]

      if (anyDuplicated(cell)) {
            rlang::abort("Isosurfaces need one `value` per grid point, but some points appear more than once.")
      }
      if (length(cell) != prod(n)) {
            rlang::abort(c(paste0(
                  "Isosurfaces need data on a complete grid, with a row for every combination of ",
                  "`x`, `y`, and `z` values: expected ", prod(n), " rows (",
                  paste(n, collapse = " x "), ") but found ", length(cell), "."),
                  i = "Rows with missing values are removed before isosurfaces are computed."))
      }

      values <- array(NA_real_, n)
      values[cell] <- data$value
      list(x = axes$x, y = axes$y, z = axes$z, values = values)
}


#' Isosurfaces of a gridded field, as layer data
#'
#' Shared by the isosurface stats: crops the grid to the scale limits,
#' triangulates a surface at each level, and adds caps if requested.
#'
#' @param grid List from `isosurface_grid()`.
#' @param levels Values at which to draw surfaces.
#' @param inside `"above"` or `"below"`: which side of each level surfaces
#'   enclose, and so face away from.
#' @param scales The x and y scales, as passed to a stat's compute functions.
#' @param cap Whether to cap surfaces at the edges of the (cropped) grid.
#' @param id Prefix for group names, unique to the layer's group.
#' @param field Name of the field, for messages.
#' @param cull_backfaces,light As for the layer.
#' @return Layer data, or an empty data frame with a warning if there are no
#'   surfaces.
#' @keywords internal
#' @noRd
isosurface_layer <- function(grid, levels, inside, scales, cap, id, field, cull_backfaces, light) {
      full <- grid

      # Surfaces are triangulated to enclose values above their levels, so to
      # enclose those below, the field and levels are negated
      sign <- if (inside == "below") -1 else 1
      grid$values <- sign * grid$values

      # Cut surfaces where they cross the scale limits, rather than leaving
      # ggplot2 to drop the triangles that cross them
      full_range <- vapply(grid[c("x", "y", "z")], function(v) diff(range(v)), numeric(1))
      grid <- crop_grid(grid, scale_limits_3d(scales))
      cropped <- is.null(grid) ||
            any(vapply(grid[c("x", "y", "z")], function(v) diff(range(v)), numeric(1)) < full_range)

      surfaces <- if (is.null(grid)) list() else lapply(seq_along(levels), function(i) {
            tri <- isosurface_triangles(grid, sign * levels[i])
            if (is.null(tri)) return(NULL)
            tri$group <- paste0(id, "_level", i, "_tri", tri$group)
            tri$level <- levels[i]
            tri
      })
      out <- do.call(rbind, surfaces)

      if (cap && !is.null(grid)) {
            caps <- isosurface_caps(grid, sign * levels, rep(list(c(TRUE, TRUE)), 3))
            if (!is.null(caps)) {
                  caps$group <- paste0(id, "_level", caps$level_index, "_cap", caps$group)
                  caps$level <- levels[caps$level_index]
                  out <- bind_caps(out, caps)
            }
      }

      if (is.null(out)) {
            rlang::warn(if (cropped) {
                  "No isosurface found at `levels` within the scale limits."
            } else {
                  paste0("No isosurface found at `levels`; ", field, " ranges from ",
                         signif(min(full$values), 3), " to ", signif(max(full$values), 3), ".")
            })
            return(data.frame())
      }

      out$cull_backfaces <- cull_backfaces
      attach_light(out, light)
}


#' Check an `inside` argument
#' @keywords internal
#' @noRd
check_inside <- function(inside) {
      if (!is.character(inside) || length(inside) != 1 || !inside %in% c("above", "below")) {
            rlang::abort('`inside` must be "above" or "below".')
      }
}


#' Check a `levels` argument
#' @keywords internal
#' @noRd
check_levels <- function(levels) {
      if (!is.numeric(levels) || length(levels) == 0 || anyNA(levels) || any(!is.finite(levels))) {
            rlang::abort("`levels` must be a numeric vector of finite values.")
      }
}


#' Limits set on the x, y, and z scales
#'
#' @param scales The x and y scales, as passed to a stat's compute functions.
#'   The z scale, which ggplot2 doesn't pass to stats, is found from the plot.
#' @return List of length-2 limits for `x`, `y`, and `z`, with `-Inf` or `Inf`
#'   where no limit is set.
#' @keywords internal
#' @noRd
scale_limits_3d <- function(scales) {
      lapply(list(x = scales$x, y = scales$y, z = find_plot_z_scale()), function(scale) {
            lim <- scale$limits
            if (is.function(lim)) lim <- scale$get_limits()
            if (!is.numeric(lim) || length(lim) != 2) return(c(-Inf, Inf))
            lim[is.na(lim)] <- c(-Inf, Inf)[is.na(lim)]
            lim
      })
}


#' Crop a grid to the points within limits
#'
#' @param grid List from `isosurface_grid()`.
#' @param limits List from `scale_limits_3d()`.
#' @return The grid, restricted to points within `limits`, or `NULL` if fewer
#'   than two remain along any axis.
#' @keywords internal
#' @noRd
crop_grid <- function(grid, limits) {
      keep <- Map(function(v, lim) v >= lim[1] & v <= lim[2], grid[c("x", "y", "z")], limits)
      if (any(vapply(keep, sum, numeric(1)) < 2)) return(NULL)
      list(x = grid$x[keep$x], y = grid$y[keep$y], z = grid$z[keep$z],
           values = grid$values[keep$x, keep$y, keep$z, drop = FALSE])
}


#' Triangulate one isosurface of a gridded field
#'
#' Marching cubes is run in index space, and vertices are then mapped to data
#' coordinates axis by axis, so that grids with uneven spacing are supported
#' (rmarchingcubes assumes even spacing). Since marching cubes places vertices
#' by linear interpolation along axis-aligned cell edges, this mapping is
#' exact.
#'
#' Triangles are wound counterclockwise as seen from the side of lower values,
#' so that surfaces face outward from the regions they enclose, where `value`
#' exceeds `level`.
#'
#' @param grid List from `isosurface_grid()`.
#' @param level Value at which to contour.
#' @return Data frame of triangle vertices with `x`, `y`, `z`, an integer
#'   `group` per triangle, unit normals `normal_x`, `normal_y`, `normal_z`, and
#'   `cap` (always `FALSE`); or `NULL` if there is no surface at `level`.
#' @keywords internal
#' @noRd
isosurface_triangles <- function(grid, level) {
      # rmarchingcubes counts values within a fixed single-precision epsilon of
      # the level as crossing it, which misplaces surfaces in fields whose
      # values are small, like densities. So it's given the field relative to
      # the level, scaled to unit range.
      field <- grid$values - level
      range <- max(abs(field))
      if (range == 0) return(NULL)
      field <- field / range

      n <- dim(field)
      mc <- rmarchingcubes::contour3d(field, 0, seq_len(n[1]), seq_len(n[2]), seq_len(n[3]))
      if (nrow(mc$triangles) == 0) return(NULL)

      index <- refine_vertices(mc$vertices, field, 0)
      vertices <- cbind(
            stats::approx(seq_len(n[1]), grid$x, index[, 1])$y,
            stats::approx(seq_len(n[2]), grid$y, index[, 2])$y,
            stats::approx(seq_len(n[3]), grid$z, index[, 3])$y
      )

      # rmarchingcubes winds triangles counterclockwise seen from higher values
      tri <- mc$triangles[, c(1, 3, 2), drop = FALSE]

      a <- vertices[tri[, 1], , drop = FALSE]
      e1 <- vertices[tri[, 2], , drop = FALSE] - a
      e2 <- vertices[tri[, 3], , drop = FALSE] - a
      normals <- cbind(e1[, 2] * e2[, 3] - e1[, 3] * e2[, 2],
                       e1[, 3] * e2[, 1] - e1[, 1] * e2[, 3],
                       e1[, 1] * e2[, 2] - e1[, 2] * e2[, 1])
      len <- sqrt(rowSums(normals^2))

      # Marching cubes yields zero-area triangles where the surface passes
      # through grid points
      keep <- len > 0
      if (!any(keep)) return(NULL)
      tri <- tri[keep, , drop = FALSE]
      normals <- normals[keep, , drop = FALSE] / len[keep]

      idx <- as.vector(t(tri))
      n_tri <- nrow(tri)
      data.frame(
            x = vertices[idx, 1],
            y = vertices[idx, 2],
            z = vertices[idx, 3],
            group = rep(seq_len(n_tri), each = 3),
            normal_x = rep(normals[, 1], each = 3),
            normal_y = rep(normals[, 2], each = 3),
            normal_z = rep(normals[, 3], each = 3),
            cap = FALSE
      )
}


#' Recompute marching cubes vertices in double precision
#'
#' rmarchingcubes computes in single precision. Each vertex lies on a grid
#' edge, between grid points at whole-number indices along two axes, so its
#' position along the third is recomputed from the values at that edge's ends.
#' This makes vertices agree with those that marching squares computes on the
#' same edges, as for caps.
#'
#' @param index Matrix of vertices in index space.
#' @param values Array of grid values.
#' @param level Value at which surfaces were contoured.
#' @return The refined matrix.
#' @keywords internal
#' @noRd
refine_vertices <- function(index, values, level) {
      whole <- round(index)
      on_node <- abs(index - whole) < 1e-4
      index[on_node] <- whole[on_node]
      for (a in 1:3) {
            rows <- which(!on_node[, a])
            if (length(rows) == 0) next
            lo <- whole[rows, , drop = FALSE]
            lo[, a] <- floor(index[rows, a])
            hi <- lo
            hi[, a] <- lo[, a] + 1
            v0 <- values[lo]
            v1 <- values[hi]
            # Kept within the edge, where rounding has marching cubes place a
            # vertex on an edge whose ends are both on one side of the level
            t <- pmin(pmax((level - v0) / (v1 - v0), 0), 1)
            index[rows, a] <- lo[, a] + ifelse(is.finite(t), t, index[rows, a] - lo[, a])
      }
      index
}


#' Caps closing isosurfaces at the ends of a grid
#'
#' On each end of the grid, the regions above each level are filled contour
#' bands of the grid's outermost slice, which meet the rims that surfaces
#' leave there, since marching cubes and marching squares interpolate along
#' the same cell edges. Each band runs from its level up to the next higher
#' one, so that the caps of nested surfaces tile the end rather than
#' overlapping. Bands are split into polygons without holes (see
#' `split_band()`), each wound counterclockwise as seen from outside the grid,
#' so that caps face outward like the surfaces they close.
#'
#' @param grid List from `isosurface_grid()`.
#' @param levels Values at which surfaces are drawn.
#' @param ends List of three length-2 logical vectors giving the low and high
#'   ends of the grid to cap along `x`, `y`, and `z`.
#' @return Data frame of cap polygon vertices with `x`, `y`, `z`, `group`
#'   (one per polygon), `level_index`, unit normals, and `cap` (always
#'   `TRUE`); or `NULL` if there are no caps.
#' @keywords internal
#' @noRd
isosurface_caps <- function(grid, levels, ends) {
      axes <- c("x", "y", "z")
      upper <- vapply(levels, function(l) min(c(levels[levels > l], Inf)), numeric(1))
      caps <- list()

      for (a in 1:3) {
            for (e in 1:2) {
                  if (!ends[[a]][e]) next
                  k <- if (e == 1) 1 else length(grid[[axes[a]]])
                  slice <- switch(a, grid$values[k, , ], grid$values[, k, ], grid$values[, , k])
                  uv <- setdiff(1:3, a)
                  u <- grid[[axes[uv[1]]]]
                  v <- grid[[axes[uv[2]]]]
                  bands <- isoband::isobands(x = u, y = v, z = t(slice),
                                             levels_low = levels, levels_high = upper)
                  normal <- c(0, 0, 0)
                  normal[a] <- if (e == 1) -1 else 1
                  end_name <- paste0(axes[a], c("min", "max")[e])

                  for (i in seq_along(levels)) {
                        band <- bands[[i]]
                        if (length(band$x) == 0) next
                        rings <- lapply(split(seq_along(band$id), band$id),
                                        function(r) list(x = band$x[r], y = band$y[r]))
                        pieces <- split_band(rings, u, v)

                        for (j in seq_along(pieces)) {
                              pts <- matrix(grid[[axes[a]]][k], length(pieces[[j]]$x), 3)
                              pts[, uv[1]] <- pieces[[j]]$x
                              pts[, uv[2]] <- pieces[[j]]$y
                              if (sum(newell_normal(pts) * normal) < 0) {
                                    pts <- pts[rev(seq_len(nrow(pts))), , drop = FALSE]
                              }

                              caps[[length(caps) + 1]] <- data.frame(
                                    x = pts[, 1], y = pts[, 2], z = pts[, 3],
                                    group = paste0(end_name, "_", j),
                                    level_index = i,
                                    normal_x = normal[1], normal_y = normal[2], normal_z = normal[3],
                                    cap = TRUE
                              )
                        }
                  }
            }
      }
      if (length(caps) == 0) return(NULL)
      do.call(rbind, caps)
}


#' Split a filled contour band into polygons without holes
#'
#' A band can comprise several disjoint polygons, some with holes. Depth
#' sorting never cuts polygons with several rings, and so can order them
#' wrongly against the surfaces they meet. So bands are split into separate
#' polygons, and polygons with holes are cut along grid lines until none
#' remain: the band is intersected with blocks of grid cells, halving the
#' blocks' size until no piece has a hole. Each set of intersections uses
#' blocks that don't touch one another, so that pieces in different blocks
#' stay separate.
#'
#' @param rings List of the band's rings, as `list(x, y)`.
#' @param u,v Grid coordinates along the slice's two axes.
#' @param size Number of grid cells along each side of a block.
#' @return List of polygons, as `list(x, y)`.
#' @keywords internal
#' @noRd
split_band <- function(rings, u, v, size = max(length(u), length(v))) {
      cuts <- function(w) unique(c(seq(1, length(w), by = size), length(w)))
      cu <- cuts(u)
      cv <- cuts(v)

      pieces <- list()
      for (pu in 0:1) {
            for (pv in 0:1) {
                  blocks <- list()
                  for (i in seq_len(length(cu) - 1)) {
                        for (j in seq_len(length(cv) - 1)) {
                              if (i %% 2 != pu || j %% 2 != pv) next
                              bu <- u[cu[c(i, i + 1)]]
                              bv <- v[cv[c(j, j + 1)]]
                              blocks[[length(blocks) + 1]] <- list(x = bu[c(1, 2, 2, 1)],
                                                                   y = bv[c(1, 1, 2, 2)])
                        }
                  }
                  if (length(blocks) == 0) next
                  pieces <- c(pieces, polyclip::polyclip(rings, blocks, op = "intersection",
                                                         fillA = "evenodd", fillB = "nonzero"))
            }
      }

      # Holes are returned as rings wound opposite to the polygons they're in
      area <- vapply(pieces, function(p) {
            nxt <- c(seq_along(p$x)[-1], 1)
            sum(p$x * p$y[nxt] - p$x[nxt] * p$y)
      }, numeric(1))
      if (size > 1 && length(unique(sign(area))) > 1) {
            return(split_band(rings, u, v, ceiling(size / 2)))
      }
      pieces
}


#' Unnormalized normal of a polygon ring, by Newell's method
#'
#' @param p Matrix of ring vertices, one row per vertex.
#' @return Length-3 vector pointing toward the side from which the ring runs
#'   counterclockwise.
#' @keywords internal
#' @noRd
newell_normal <- function(p) {
      q <- p[c(seq_len(nrow(p))[-1], 1), , drop = FALSE]
      c(sum((p[, 2] - q[, 2]) * (p[, 3] + q[, 3])),
        sum((p[, 3] - q[, 3]) * (p[, 1] + q[, 1])),
        sum((p[, 1] - q[, 1]) * (p[, 2] + q[, 2])))
}


#' Combine surface triangles with caps
#'
#' @param triangles Data frame from `isosurface_triangles()`, or `NULL`.
#' @param caps Data frame from `isosurface_caps()`.
#' @return The combined data frame, without `level_index`.
#' @keywords internal
#' @noRd
bind_caps <- function(triangles, caps) {
      caps$level_index <- NULL
      if (is.null(triangles)) return(caps)
      rbind(triangles, caps[names(triangles)])
}


#' Check a `cap` argument
#' @keywords internal
#' @noRd
check_cap <- function(cap) {
      if (!isTRUE(cap) && !isFALSE(cap)) {
            rlang::abort("`cap` must be `TRUE` or `FALSE`.")
      }
}


#' 3D isosurfaces
#'
#' Draws isosurfaces of a 3D scalar field, i.e. the surfaces on
#' which the field equals given values. These are the 3D analog of contour
#' lines, and can show the shape of a density, a concentration plume, an
#' implicit surface, or any other function of three variables. Surfaces are
#' triangulated with the marching cubes algorithm.
#'
#' Where limits are set on the plot's scales, e.g. with [ggplot2::xlim()] or [zlim()],
#' surfaces are cut at the last grid points within the limits, leaving them open
#' there unless `cap = TRUE`.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer: a grid of points, with
#'   one row for every combination of `x`, `y`, and `z` values, such as
#'   produced by [expand.grid()]. Even grid spacing is not required.
#' @param stat The statistical transformation to use on the data. Defaults to
#'   `StatIsosurface3D`.
#' @param geom The geometric object used to display the data. Defaults to
#'   `GeomIsosurface3D`, a variant of [geom_polygon_3d()].
#' @param levels Numeric vector of values of the field at which to draw
#'   isosurfaces, one surface per value. Defaults to a single surface at the
#'   midpoint of the range of `value`.
#' @param inside Which side of each surface is its inside, the region it
#'   encloses: `"above"` (the default) for where `value` exceeds the level, as
#'   for a density or concentration, or `"below"` for where it's less. Surfaces
#'   face outward from their insides, which determines where `cap` adds caps,
#'   which triangles `cull_backfaces` removes, and the direction of the
#'   computed normals.
#' @param cap Logical indicating whether to close surfaces with flat caps
#'   where they meet the edges of the grid, or limits set on the plot's scales,
#'   so that they show solid cross sections rather than being left open there.
#'   Defaults to `FALSE`.
#' @param cull_backfaces Logical indicating whether to remove triangles that
#'   face away from the viewer. Surfaces face outward from the regions they
#'   enclose, so for opaque surfaces that close on themselves, `TRUE` removes
#'   only hidden triangles and speeds up rendering. Defaults to `FALSE`, since
#'   surfaces that are translucent or cut open by the edge of the grid show
#'   their inner side.
#' @inheritParams polygon_params
#' @inheritParams light_param
#' @inheritParams position_param
#'
#' @section Aesthetics:
#' `stat_isosurface_3d()` requires the following aesthetics:
#' - **x**, **y**, **z**: Grid point coordinates
#' - **value**: The value of the field at each grid point
#'
#' Grouping aesthetics give separate fields, each needing its own complete
#' grid.
#'
#' `geom_isosurface_3d()` understands the same aesthetics as
#' [geom_polygon_3d()]. Unless `color` is mapped or set, opaque triangles are
#' outlined in their fill color, which hides seams between them, and
#' translucent triangles (`alpha` below 1) are not outlined.
#'
#' @section Computed variables:
#' - `level`: The value of the field on the surface
#' - `normal_x`, `normal_y`, `normal_z`: Components of each triangle's unit
#'   normal, pointing outward from the region the surface encloses (see
#'   `inside`)
#' - `cap`: Whether a triangle is part of a cap (see `cap`), e.g. for
#'   `aes(fill = after_stat(cap))`
#'
#' @section Dependencies:
#' Isosurfaces are computed with the \pkg{rmarchingcubes} package, which must
#' be installed.
#'
#' @examplesIf requireNamespace("rmarchingcubes", quietly = TRUE)
#' # A field with two peaks
#' blobs <- expand.grid(x = seq(-2.5, 2.5, length.out = 24),
#'                      y = seq(-2, 2, length.out = 24),
#'                      z = seq(-2, 2, length.out = 24))
#' blobs$value <- with(blobs, exp(-((x - 0.8)^2 + y^2 + z^2)) +
#'                            exp(-((x + 0.8)^2 + y^2 + z^2)))
#'
#' # A single surface, by default at the midpoint of the value range
#' ggplot(blobs, aes(x, y, z, value = value)) +
#'   geom_isosurface_3d(fill = "steelblue") +
#'   coord_3d(scales = "fixed")
#'
#' # Nested translucent surfaces, colored by level
#' ggplot(blobs, aes(x, y, z, value = value, fill = after_stat(level))) +
#'   geom_isosurface_3d(levels = c(0.2, 0.5, 0.9), alpha = 0.4) +
#'   scale_fill_viridis_c(direction = -1) +
#'   coord_3d(scales = "fixed")
#'
#' \donttest{
#' # Cut by a limit, with caps showing the cross section
#' ggplot(blobs, aes(x, y, z, value = value, fill = after_stat(level))) +
#'   geom_isosurface_3d(levels = c(0.2, 0.5, 0.9), cap = TRUE) +
#'   scale_fill_viridis_c() +
#'   xlim(-1, NA) +
#'   coord_3d(scales = "fixed")
#'
#' # An implicit surface: the gyroid, sin(x)cos(y) + sin(y)cos(z) + sin(z)cos(x) = 0
#' gyroid <- expand.grid(x = seq(0, 4 * pi, length.out = 40),
#'                       y = seq(0, 4 * pi, length.out = 40),
#'                       z = seq(0, 4 * pi, length.out = 40))
#' gyroid$value <- with(gyroid, sin(x) * cos(y) + sin(y) * cos(z) + sin(z) * cos(x))
#'
#' ggplot(gyroid, aes(x, y, z, value = value)) +
#'   geom_isosurface_3d(levels = 0.5, fill = "darkred") +
#'   coord_3d(panels = "none") +
#'   light(mode = "hsl") +
#'   theme_void()
#' }
#'
#' @seealso [geom_isofunction_3d()] for isosurfaces of functions,
#'   [geom_isodensity_3d()] for contours of 3D density fields,
#'   [geom_contour_3d()] for contours of surfaces, [geom_hull_3d()] for
#'   surfaces enclosing point clouds, [light()] for lighting.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_isosurface_3d
#' @export
geom_isosurface_3d <- function(mapping = NULL, data = NULL,
                               stat = StatIsosurface3D,
                               position = "identity",
                               ...,
                               levels = NULL,
                               inside = "above",
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
            params = list(levels = levels, inside = inside, cap = cap, light = light,
                          cull_backfaces = cull_backfaces, scale_depth = scale_depth,
                          na.rm = na.rm, ...))
}

#' @rdname geom_isosurface_3d
#' @export
stat_isosurface_3d <- function(mapping = NULL, data = NULL,
                               geom = GeomIsosurface3D,
                               position = "identity",
                               ...,
                               levels = NULL,
                               inside = "above",
                               cap = FALSE,
                               light = NULL,
                               cull_backfaces = FALSE,
                               scale_depth = TRUE,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
      layer(data = data, mapping = mapping,
            stat = StatIsosurface3D, geom = get_proto(geom, "geom"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(levels = levels, inside = inside, cap = cap, light = light,
                          cull_backfaces = cull_backfaces, scale_depth = scale_depth,
                          na.rm = na.rm, ...))
}
