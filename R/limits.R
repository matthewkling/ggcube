# Scale limits ------------------------------------------------------------------

#' Limits set by the user on a continuous scale
#'
#' @param scale A scale object, or `NULL`.
#' @return Length-2 numeric vector of limits, in transformed units, with `-Inf`
#'   or `Inf` where no limit is set. Limits given as a function, which can only
#'   be resolved once the scale is trained, count as unset.
#' @keywords internal
#' @noRd
user_limits <- function(scale) {
      none <- c(-Inf, Inf)
      if (is.null(scale) || !inherits(scale, "ScaleContinuous")) return(none)
      lim <- scale$limits
      if (!is.numeric(lim) || length(lim) != 2) return(none)
      lim[is.na(lim)] <- none[is.na(lim)]
      lim
}

#' Whether a scale's out-of-bounds handling removes values beyond its limits
#'
#' Probes the scale's `oob` function with a value beyond a finite limit, so
#' that `scales::censor()` and variants count, but `scales::squish()` and
#' `scales::oob_keep()` don't.
#'
#' @param scale A continuous scale.
#' @param lim Limits from `user_limits()`, at least one of them finite.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
censors_oob <- function(scale, lim) {
      probe <- if (is.finite(lim[2])) lim[2] + 1 + abs(lim[2]) else lim[1] - 1 - abs(lim[1])
      out <- tryCatch(scale$oob(probe, lim), error = function(e) probe)
      length(out) == 1 && is.na(out)
}

#' Limit planes of the plot being built
#'
#' Finds the x, y, and z scales of the plot that `ggplot_build()` is working
#' on, and returns the limits set on them whose out-of-bounds handling removes
#' values, i.e. the planes beyond which ggplot2 would censor geometry.
#'
#' @return List of planes, each a list with `axis` (`"x"`, `"y"`, or `"z"`),
#'   `bound`, and `sign`: `1` for an upper limit and `-1` for a lower one, so
#'   that points with `sign * (value - bound) > 0` are outside.
#' @keywords internal
#' @noRd
limit_planes <- function() {
      plot <- find_build_plot()
      if (is.null(plot) || is.null(plot$scales)) return(list())
      planes <- list()
      for (axis in c("x", "y", "z")) {
            scale <- plot$scales$get_scales(axis)
            lim <- user_limits(scale)
            if (all(is.infinite(lim)) || !censors_oob(scale, lim)) next
            if (is.finite(lim[1])) planes <- c(planes, list(list(axis = axis, bound = lim[1], sign = -1)))
            if (is.finite(lim[2])) planes <- c(planes, list(list(axis = axis, bound = lim[2], sign = 1)))
      }
      planes
}


# Applying z limits before stats -------------------------------------------------

#' Apply z scale limits to data before a stat is computed
#'
#' ggplot2 applies the limits of the x and y scales to a layer's data before
#' computing its stat, so that a stat summarizes only data within them, but it
#' treats z as an ordinary aesthetic and applies its limits only afterward.
#' Used as the `setup_data` method of stats that summarize observations, this
#' gives z limits the same effect: values of `z` and `zend` beyond them are
#' handled by the z scale's `oob` function, which by default makes them `NA`,
#' so that the stat removes their rows with ggplot2's usual warning.
#'
#' Stats whose rows are vertices of surfaces don't use this, since removing
#' vertices would leave holes; their geometry is cut at the limits by
#' `clip_to_limits()` instead.
#'
#' @param data Layer data, as passed to a stat's `setup_data` method.
#' @return `data`, with `z` and `zend` handled by the z scale's `oob` function.
#' @keywords internal
#' @noRd
censor_z_limits <- function(data) {
      scale <- find_plot_z_scale()
      lim <- user_limits(scale)
      if (all(is.infinite(lim))) return(data)
      for (aes in intersect(c("z", "zend"), names(data))) {
            if (is.numeric(data[[aes]])) data[[aes]] <- scale$oob(data[[aes]], lim)
      }
      data
}


# Paths ------------------------------------------------------------------------

#' Break paths at missing points
#'
#' Splits each group at rows with a missing or non-finite coordinate, such as
#' points outside the scale limits, so that a path is drawn in pieces rather
#' than joined across the gap once those rows are removed.
#'
#' @param data Layer data with `x`, `y`, `z`, and `group` columns.
#' @return `data`, with `group` renumbered so that each unbroken run of rows is
#'   its own group.
#' @keywords internal
#' @noRd
break_paths <- function(data) {
      coords <- intersect(c("x", "y", "z"), names(data))
      if (nrow(data) == 0 || length(coords) == 0) return(data)
      missing <- Reduce(`|`, lapply(data[coords], function(v) {
            if (is.numeric(v)) !is.finite(v) else is.na(v)
      }))
      if (!any(missing)) return(data)
      group <- data$group %||% rep(-1L, nrow(data))
      run <- stats::ave(as.integer(missing), group, FUN = cumsum)
      key <- paste(group, run, sep = "\r")
      data$group <- match(key, unique(key))
      data
}


# Clipping polygons at the limits -----------------------------------------------

#' Columns identifying discrete objects in polygon data
#'
#' Columns, bars, and voxels are each drawn as several faces, and are kept or
#' removed whole rather than cut at the limits, as ggplot2 removes bars and
#' tiles that extend beyond them.
#' @keywords internal
#' @noRd
discrete_object_cols <- c("col_id", "voxel_id")

#' Cut polygon data at the limits of the plot's scales
#'
#' ggplot2 replaces coordinates beyond the scale limits with `NA` after a
#' layer's stat and geom setup are computed, which would leave polygons that
#' cross the limits with missing vertices. Called at the end of the polygon
#' geoms' `setup_data` methods, this cuts them at the limits first, so that
#' they end cleanly there:
#'
#' - Polygons are clipped to the limits, with new vertices where their edges
#'   cross them. Other varying numeric columns are interpolated at the new
#'   vertices, and the rest are taken from the nearest original vertex.
#' - Two-point segments (`.prim == "segment"`) are shortened to the limits.
#' - Columns, bars, and voxels are removed whole if any part lies beyond the
#'   limits.
#' - Other rows (e.g. points) beyond the limits are left for ggplot2 to censor.
#'
#' Limits whose scales don't censor out-of-bounds values, e.g. with
#' `oob = scales::squish`, are left alone.
#'
#' @param data Polygon layer data, after the geom's own setup.
#' @return `data`, cut at the limits.
#' @keywords internal
#' @noRd
clip_to_limits <- function(data) {
      if (nrow(data) == 0) return(data)
      planes <- limit_planes()
      planes <- Filter(function(p) is.numeric(data[[p$axis]]), planes)
      if (length(planes) == 0) return(data)

      outside <- Reduce(`|`, lapply(planes, function(p) {
            d <- p$sign * (data[[p$axis]] - p$bound)
            !is.na(d) & d > 0
      }))
      if (!any(outside)) return(data)

      panel <- if ("PANEL" %in% names(data)) as.character(data$PANEL) else rep("", nrow(data))

      # Discrete objects are kept or removed whole
      id_col <- intersect(discrete_object_cols, names(data))
      if (length(id_col) > 0) {
            object <- paste(panel, data[[id_col[1]]], sep = "\r")
            data <- data[!object %in% object[outside], , drop = FALSE]
            return(data)
      }

      prim <- if (".prim" %in% names(data)) data$.prim else rep(NA_character_, nrow(data))
      prim[is.na(prim)] <- "polygon"

      # Points beyond the limits are left to ggplot2's censoring
      ring_rows <- prim %in% c("polygon", "segment")
      sg_col <- intersect(c(".subgroup", "subgroup"), names(data))
      ring <- paste(panel, data$group,
                    if (length(sg_col) > 0) data[[sg_col[1]]] else "", sep = "\r")
      ring[!ring_rows] <- NA

      # Only rings with a vertex beyond the limits need clipping
      cut <- !is.na(ring) & ring %in% ring[outside]
      if (!any(cut)) return(data)

      coords <- c("x", "y", "z")
      xyz <- matrix(vapply(coords, function(axis) as.numeric(data[[axis]]), numeric(nrow(data))),
                    ncol = 3, dimnames = list(NULL, coords))
      doubles <- setdiff(names(data)[vapply(data, is.double, logical(1))], coords)
      values <- lapply(stats::setNames(doubles, doubles), function(col) as.vector(data[[col]]))

      to_clip <- split(which(cut), factor(ring[cut], levels = unique(ring[cut])))
      pieces <- lapply(to_clip, function(rows) {
            P <- xyz[rows, , drop = FALSE]
            if (any(!is.finite(P))) return(list(rows = rows, P = P, values = lapply(values, `[`, rows)))
            clipped <- if (prim[rows[1]] == "segment" && length(rows) == 2) {
                  clip_segment(P, planes)
            } else {
                  clip_ring(P, planes)
            }
            if (is.null(clipped)) return(NULL)
            interpolate_ring(rows, clipped, values)
      })
      pieces <- Filter(Negate(is.null), pieces)

      # Clipped rings take the place of the originals in the row order
      untouched <- which(!cut)
      new_rows <- unlist(lapply(pieces, `[[`, "rows"), use.names = FALSE)
      out <- data[c(untouched, new_rows), , drop = FALSE]
      at <- length(untouched) + seq_along(new_rows)
      if (length(new_rows) > 0) {
            P <- do.call(rbind, lapply(pieces, `[[`, "P"))

            # Rounding can leave new vertices a hair beyond a limit, where ggplot2
            # would censor them
            for (plane in planes) {
                  v <- P[, plane$axis]
                  P[, plane$axis] <- if (plane$sign > 0) pmin(v, plane$bound) else pmax(v, plane$bound)
            }
            for (axis in coords) out[[axis]][at] <- P[, axis]
            for (col in doubles) {
                  out[[col]][at] <- unlist(lapply(pieces, function(p) p$values[[col]]), use.names = FALSE)
            }
      }
      key <- c(untouched, rep(vapply(pieces, function(p) p$rows[1], integer(1)),
                              vapply(pieces, function(p) length(p$rows), integer(1))))
      out <- out[order(key), , drop = FALSE]
      rownames(out) <- NULL
      out
}

#' Clip a polygon ring to the limit planes
#'
#' Sutherland-Hodgman clipping against each plane in turn. Since the region
#' within the limits is convex, this is exact for any simple polygon, and
#' rings of polygons with holes can be clipped one by one.
#'
#' @param P Numeric matrix of vertex coordinates, one row per vertex, with
#'   columns named for the axes.
#' @param planes List of planes from `limit_planes()`.
#' @return `NULL` if fewer than three vertices remain, otherwise a list with
#'   `P`, the clipped coordinates, and `W`, a matrix of weights giving each
#'   new vertex as a combination of the original ones.
#' @keywords internal
#' @noRd
clip_ring <- function(P, planes) {
      W <- diag(nrow(P))
      for (plane in planes) {
            d <- plane$sign * (P[, plane$axis] - plane$bound)
            inside <- d <= 0
            if (all(inside)) next
            if (!any(inside)) return(NULL)
            n <- nrow(P)
            nxt <- c(seq_len(n)[-1], 1L)
            crosses <- inside != inside[nxt]

            # Each vertex is kept if inside, followed by any crossing on the edge it starts
            from <- rep(seq_len(n), each = 2)
            crossing <- rep(c(FALSE, TRUE), n)
            emit <- ifelse(crossing, crosses[from], inside[from])
            from <- from[emit]
            crossing <- crossing[emit]
            to <- ifelse(crossing, nxt[from], from)
            t <- ifelse(crossing, d[from] / (d[from] - d[to]), 0)

            P <- P[from, , drop = FALSE] * (1 - t) + P[to, , drop = FALSE] * t
            W <- W[from, , drop = FALSE] * (1 - t) + W[to, , drop = FALSE] * t
      }
      if (nrow(P) < 3) return(NULL)
      list(P = P, W = W)
}

#' Clip a segment to the limit planes
#'
#' @param P Two-row numeric matrix of endpoint coordinates.
#' @param planes List of planes from `limit_planes()`.
#' @return `NULL` if no part of the segment is within the limits, otherwise a
#'   list like that from `clip_ring()`.
#' @keywords internal
#' @noRd
clip_segment <- function(P, planes) {
      t0 <- 0
      t1 <- 1
      for (plane in planes) {
            d <- plane$sign * (P[, plane$axis] - plane$bound)
            if (d[1] > 0 && d[2] > 0) return(NULL)
            if (d[1] > 0) t0 <- max(t0, d[1] / (d[1] - d[2]))
            if (d[2] > 0) t1 <- min(t1, d[1] / (d[1] - d[2]))
      }
      if (t0 >= t1) return(NULL)
      W <- rbind(c(1 - t0, t0), c(1 - t1, t1))
      list(P = W %*% P, W = W)
}

#' Vertices of a clipped ring
#'
#' @param rows Indices of the ring's original rows in the layer data.
#' @param clipped List from `clip_ring()` or `clip_segment()`.
#' @param values Named list of the layer's numeric columns other than the
#'   coordinates.
#' @return List with `rows`, the original row nearest each new vertex, from
#'   which its other columns are taken; `P`, the new vertex coordinates; and
#'   `values`, the numeric columns at the new vertices, interpolated where
#'   they vary around the ring.
#' @keywords internal
#' @noRd
interpolate_ring <- function(rows, clipped, values) {
      W <- clipped$W
      nearest <- max.col(W, ties.method = "first")
      values <- lapply(values, function(v) {
            v <- v[rows]
            if (anyNA(v) || all(v == v[1])) v[nearest] else as.vector(W %*% v)
      })
      list(rows = rows[nearest], P = clipped$P, values = values)
}


# Removing partly censored polygons -----------------------------------------------

#' Remove polygons with missing vertices
#'
#' The `handle_na` method of the polygon geoms. Where ggplot2 has replaced a
#' coordinate with `NA`, e.g. because it lies beyond scale limits that
#' `clip_to_limits()` doesn't handle, the whole polygon (or segment) is
#' removed, since drawing its remaining vertices would give a different shape.
#'
#' @param data Layer data.
#' @param na.rm If `FALSE`, removal is reported with a warning.
#' @param name Name of the geom, for the warning.
#' @return `data`, without the rows of polygons with missing coordinates.
#' @keywords internal
#' @noRd
remove_partial_polygons <- function(data, na.rm = FALSE, name = "geom_polygon_3d") {
      coords <- intersect(c("x", "y", "z"), names(data))
      if (nrow(data) == 0 || length(coords) == 0) return(data)
      missing <- !stats::complete.cases(data[coords])
      if (!any(missing)) return(data)
      panel <- if ("PANEL" %in% names(data)) as.character(data$PANEL) else ""
      polygon <- paste(panel, data$group, sep = "\r")
      drop <- polygon %in% polygon[missing]
      if (!isTRUE(na.rm)) {
            n <- sum(drop)
            rlang::warn(sprintf(
                  "Removed %d row%s containing missing values or values outside the scale range (`%s()`).",
                  n, if (n == 1) "" else "s", name))
      }
      data[!drop, , drop = FALSE]
}

#' snake_case name of a ggproto object's class, as ggplot2 names layers in
#' messages
#' @keywords internal
#' @noRd
snake_class_name <- function(x) {
      name <- class(x)[1]
      name <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", name)
      name <- gsub(".", "_", name, fixed = TRUE)
      name <- gsub("([a-z])([A-Z])", "\\1_\\2", name)
      tolower(name)
}
