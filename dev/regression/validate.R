# Ground-truth ordering validator
#
# For each panel, casts exact lines of sight through a regular grid of screen
# samples and compares, at every sample, the primitive that is truly nearest
# the viewer in 3D with the primitive drawn last (i.e. visible) at that
# sample. A sample where they differ by more than a tolerance is an ordering
# error. The result does not depend on how sorting is implemented, only on
# the final draw order and each primitive's 3D geometry.
#
# Geometry:
#   polygons - their screen footprint (even-odd over all rings), with depth
#              from the ray's intersection with the polygon's best-fit plane
#              in cube space; errors within a polygon's warp are tolerated
#   segments - samples within half the drawn line width of the screen
#              segment, with depth at the ray's closest approach to it
#   points   - samples within the marker radius, at the depth of the marker's
#              centre (markers face the viewer)
#   text     - not validated (label extents are unknown before drawing)
#
# Marker and line sizes are as drawn when the installed version provides the
# validation hook; otherwise they are approximated from the layer data before
# depth scaling (strength 1 is assumed).
#
# Draw order and 3D coordinates come from `drawn_primitives()`, which uses an
# internal hook in ggcube if the installed version provides one
# (`ggcube:::validation_scene()`), and otherwise traces `sort_by_depth()`.

validate_version <- function(name, cases = NULL, n = 400, tol = 1e-4,
                             width = 6, height = 5, res = 100) {
      lib <- out_dir("libs", name)
      vdir <- out_dir("validation", name)
      cases_file <- file.path(harness_dir(), "cases.R")
      self_file <- file.path(harness_dir(), "validate.R")

      out <- callr::r(function(lib, vdir, cases_file, self_file, cases, n, tol, width, height, res) {
            .libPaths(c(lib, .libPaths()))
            suppressPackageStartupMessages({
                  library(ggplot2)
                  library(ggcube)
            })
            source(cases_file, local = TRUE)
            source(self_file, local = TRUE)
            all <- regression_cases()
            if (!is.null(cases)) all <- all[intersect(cases, names(all))]
            rows <- lapply(names(all), function(nm) {
                  res_case <- tryCatch(
                        validate_plot(all[[nm]]$plot(), n = n, tol = tol, width = width,
                                      height = height, res = res,
                                      map_file = file.path(vdir, paste0(nm, ".png"))),
                        error = function(e) data.frame(covered = NA, errors = NA, error_frac = NA,
                                                       worst = paste("ERROR:", conditionMessage(e))))
                  cbind(case = nm, res_case, stringsAsFactors = FALSE)
            })
            out <- do.call(rbind, rows)
            utils::write.csv(out, file.path(vdir, "validation.csv"), row.names = FALSE)
            out
      }, args = list(lib = lib, vdir = vdir, cases_file = cases_file, self_file = self_file,
                     cases = cases, n = n, tol = tol, width = width, height = height, res = res),
      show = FALSE)
      out
}


#' Validate the draw order of one plot
#'
#' @param p A ggplot.
#' @param n Samples per side of the screen grid in each panel.
#' @param tol Depth tolerance in cube units.
#' @param map_file Optional PNG path for an error map of the first panel
#'   (white: empty, grey: correct, red: error).
#' @return One-row data frame: covered samples, error samples, error
#'   fraction, and the most frequent misordered primitive pairs.
validate_plot <- function(p, n = 400, tol = 1e-4, width = 6, height = 5, res = 100,
                          map_file = NULL) {
      scene <- drawn_primitives(p, width = width, height = height, res = res)
      tot_cov <- 0; tot_err <- 0
      pair_counts <- list()
      for (pi in seq_along(scene$panels)) {
            pn <- scene$panels[[pi]]
            r <- validate_panel(pn$rows, pn$proj, pn$plot_bounds, scene$panel_pt, n = n, tol = tol,
                                sizes_final = isTRUE(scene$sizes_final))
            tot_cov <- tot_cov + r$covered
            tot_err <- tot_err + r$errors
            if (length(r$pairs)) pair_counts <- c(pair_counts, list(r$pairs))
            if (pi == 1 && !is.null(map_file)) write_error_map(r$map, map_file)
      }
      worst <- ""
      if (length(pair_counts)) {
            pc <- do.call(rbind, pair_counts)
            pc <- stats::aggregate(count ~ pair, pc, sum)
            pc <- pc[order(-pc$count), ]
            worst <- paste(utils::head(sprintf("%s (%d)", pc$pair, pc$count), 4), collapse = "; ")
      }
      data.frame(covered = tot_cov, errors = tot_err,
                 error_frac = if (tot_cov > 0) round(tot_err / tot_cov, 5) else NA_real_,
                 worst = worst, stringsAsFactors = FALSE)
}


# Draw order capture ---------------------------------------------------------------

#' Primitives of a plot in final draw order, with 3D coordinates
#'
#' @return List with `panels` (each with `rows`, `proj`, `plot_bounds`) and
#'   `panel_pt` (panel width and height in points at the given device size).
drawn_primitives <- function(p, width = 6, height = 5, res = 100) {
      ns <- asNamespace("ggcube")
      if (exists("validation_scene", envir = ns, inherits = FALSE)) {
            return(get("validation_scene", envir = ns)(p, width = width, height = height, res = res))
      }
      trace_sorted_rows(p, width = width, height = height, res = res)
}

# Captures the output of each layer's outermost sort_by_depth() call, with the
# cube-space coordinates that sort_by_depth() drops copied into .kx3d/.ky3d/.kz3d
trace_sorted_rows <- function(p, width, height, res) {
      ns <- asNamespace("ggcube")
      orig_sort <- get("sort_by_depth", envir = ns)
      orig_proj <- get("project_new_vertices", envir = ns)
      captured <- list()
      layers_seen <- list()
      depth <- 0L

      find_layer <- function() {
            for (f in rev(sys.frames())) {
                  if (exists("self", envir = f, inherits = FALSE)) {
                        s <- get("self", envir = f)
                        if (inherits(s, "Layer")) return(s)
                  }
            }
            NULL
      }
      layer_index <- function(layer) {
            if (is.null(layer)) return(NA_integer_)
            for (i in seq_along(layers_seen)) if (identical(layers_seen[[i]], layer)) return(i)
            layers_seen[[length(layers_seen) + 1]] <<- layer
            length(layers_seen)
      }

      keep3d <- function(d) {
            if (all(c(".x3d", ".y3d", ".z3d") %in% names(d))) {
                  d$.kx3d <- d$.x3d; d$.ky3d <- d$.y3d; d$.kz3d <- d$.z3d
            }
            d
      }
      sort_tracer <- function(data, proj = NULL, plot_bounds = NULL) {
            outer <- depth == 0L
            depth <<- depth + 1L
            on.exit(depth <<- depth - 1L)
            out <- orig_sort(keep3d(data), proj, plot_bounds)
            if (outer) {
                  captured[[length(captured) + 1]] <<- list(
                        rows = out, proj = proj, plot_bounds = plot_bounds,
                        layer = layer_index(find_layer()),
                        panel = as.character(out$PANEL[1] %||% "1"))
            }
            out
      }
      proj_tracer <- function(rows, proj, plot_bounds) keep3d(orig_proj(rows, proj, plot_bounds))

      replace_binding <- function(name, value) {
            unlockBinding(name, ns)
            assign(name, value, envir = ns)
            lockBinding(name, ns)
      }
      replace_binding("sort_by_depth", sort_tracer)
      replace_binding("project_new_vertices", proj_tracer)
      on.exit({
            replace_binding("sort_by_depth", orig_sort)
            replace_binding("project_new_vertices", orig_proj)
      })

      panel_pt <- draw_and_measure(p, width, height, res)

      panels <- split(captured, vapply(captured, `[[`, character(1), "panel"))
      panels <- lapply(panels, function(caps) {
            caps <- caps[order(vapply(caps, `[[`, integer(1), "layer"))]
            rows <- do.call(rbind_fill, lapply(caps, function(cp) {
                  r <- cp$rows
                  r$.layer <- cp$layer
                  r$.x3d <- r$.kx3d; r$.y3d <- r$.ky3d; r$.z3d <- r$.kz3d
                  r
            }))
            list(rows = rows, proj = caps[[1]]$proj, plot_bounds = caps[[1]]$plot_bounds)
      })
      list(panels = panels, panel_pt = panel_pt)
}

# Draws the plot on an off-screen device of the render size and returns the
# first panel's size in points
draw_and_measure <- function(p, width, height, res) {
      f <- tempfile(fileext = ".png")
      ragg::agg_png(f, width = width, height = height, units = "in", res = res)
      on.exit({ grDevices::dev.off(); unlink(f) })
      grid::grid.newpage()
      grid::grid.draw(ggplotGrob(p))
      vps <- grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name
      pv <- grep("^panel\\.", vps, value = TRUE)
      if (length(pv) == 0) return(c(width, height) * 72.27)
      grid::seekViewport(pv[1])
      c(grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE),
        grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE))
}

rbind_fill <- function(...) as.data.frame(dplyr::bind_rows(...))


# Per-panel validation ---------------------------------------------------------------

validate_panel <- function(rows, proj, pb, panel_pt, n = 400, tol = 1e-4, sizes_final = FALSE) {
      pt <- 72.27 / 25.4
      prims <- primitive_list(rows, sizes_final)

      # Sample grid (cell centres) and lines of sight
      g <- (seq_len(n) - 0.5) / n
      sx <- rep(g, times = n)
      sy <- rep(g, each = n)
      rays <- screen_rays(sx, sy, proj, pb)
      dlen <- sqrt(rowSums(rays$d^2))
      spacing <- 1 / n

      best <- rep(Inf, n * n); best_k <- integer(n * n); best_w <- numeric(n * n)
      drawn_k <- integer(n * n); drawn_l <- rep(NA_real_, n * n); drawn_w <- numeric(n * n)

      to_npc_x <- 1 / panel_pt[1]
      to_npc_y <- 1 / panel_pt[2]

      # Markers and lines have extent in depth too: treat them as spheres and
      # tubes, so a polygon passing within their radius isn't counted as an
      # error (pt -> npc -> projected units -> cube units at their depth)
      units_per_pt <- to_npc_x * (pb[2] - pb[1])
      for (k in seq_along(prims)) {
            ext <- switch(prims[[k]]$type, point = prims[[k]]$radius_pt,
                          segment = prims[[k]]$halfwidth_pt, 0)
            prims[[k]]$warp <- max(prims[[k]]$warp, ext * units_per_pt / prims[[k]]$ds)
      }

      for (k in seq_along(prims)) {
            pr <- prims[[k]]
            if (!pr$type %in% c("polygon", "segment", "point")) next
            ext <- switch(pr$type,
                          point = pr$radius_pt,
                          segment = pr$halfwidth_pt,
                          0)
            ex <- ext * to_npc_x
            ey <- ext * to_npc_y
            xr <- range(pr$sx) + c(-ex, ex)
            yr <- range(pr$sy) + c(-ey, ey)
            ix <- seq(max(1, ceiling(xr[1] * n + 0.5)), min(n, floor(xr[2] * n + 0.5)))
            iy <- seq(max(1, ceiling(yr[1] * n + 0.5)), min(n, floor(yr[2] * n + 0.5)))
            if (xr[2] * n + 0.5 < 1 || yr[2] * n + 0.5 < 1 || xr[1] > 1 || yr[1] > 1) next
            if (length(ix) == 0 || length(iy) == 0 || ix[1] > ix[length(ix)] || iy[1] > iy[length(iy)]) next
            idx <- as.vector(outer(ix, (iy - 1) * n, "+"))
            px <- sx[idx]; py <- sy[idx]

            cov <- switch(pr$type,
                          polygon = pip_evenodd(px, py, pr$sx, pr$sy, pr$ring),
                          segment = seg_dist(px, py, pr$sx, pr$sy, to_npc_x, to_npc_y) <= pr$halfwidth_pt,
                          point = sqrt(((px - pr$sx) / to_npc_x)^2 + ((py - pr$sy) / to_npc_y)^2) <= pr$radius_pt)
            idx <- idx[cov]
            if (length(idx) == 0) next

            o <- rays$o[idx, , drop = FALSE]
            d <- rays$d[idx, , drop = FALSE]
            lam <- switch(pr$type,
                          polygon = {
                                den <- as.vector(d %*% pr$normal)
                                l <- (pr$offset - as.vector(o %*% pr$normal)) / den
                                l[abs(den) < 1e-12] <- NA
                                l
                          },
                          segment = ray_segment_lambda(o, d, pr$p3[1, ], pr$p3[2, ]),
                          point = as.vector(((pr$p3[rep(1, length(idx)), , drop = FALSE] - o) * d) %*% c(1, 1, 1)) /
                                rowSums(d * d))
            ok <- is.finite(lam)
            idx <- idx[ok]; lam <- lam[ok]
            if (length(idx) == 0) next
            ldist <- lam * dlen[idx]

            better <- ldist < best[idx]
            best[idx[better]] <- ldist[better]
            best_k[idx[better]] <- k
            best_w[idx[better]] <- pr$warp

            drawn_k[idx] <- k
            drawn_l[idx] <- ldist
            drawn_w[idx] <- pr$warp
      }

      covered <- drawn_k > 0
      err <- covered & best_k != drawn_k & (drawn_l - best) > tol + drawn_w + best_w
      err[is.na(err)] <- FALSE

      pairs <- NULL
      if (any(err)) {
            lab <- vapply(prims, function(pr) sprintf("L%s:%s:%s", pr$layer, pr$type, pr$group), character(1))
            key <- paste(lab[drawn_k[err]], "over", lab[best_k[err]])
            tb <- table(key)
            pairs <- data.frame(pair = names(tb), count = as.integer(tb), stringsAsFactors = FALSE)
      }
      map <- matrix(0L, n, n)
      map[covered] <- 1L
      map[err] <- 2L
      list(covered = sum(covered), errors = sum(err), pairs = pairs, map = map)
}


# Builds per-group primitive descriptions in draw order. Unless
# `sizes_final`, marker and line sizes are taken as before depth scaling and
# reference-element styling, and those are approximated.
primitive_list <- function(rows, sizes_final = FALSE) {
      pt <- 72.27 / 25.4
      key <- paste(rows$.layer, rows$group, sep = "|")
      ukey <- unique(key)
      idx <- split(seq_len(nrow(rows)), factor(key, levels = ukey))
      sg_col <- intersect(c(".subgroup", "subgroup"), names(rows))

      lapply(seq_along(idx), function(k) {
            ix <- idx[[k]]
            r <- rows[ix, , drop = FALSE]
            type <- prim_type(r)
            p3 <- cbind(r$.x3d, r$.y3d, r$.z3d)
            ds <- if ("depth_scale" %in% names(r)) mean(r$depth_scale, na.rm = TRUE) else 1
            if (!is.finite(ds)) ds <- 1
            ds_size <- if (sizes_final) 1 else ds
            out <- list(type = type, group = r$group[1], layer = r$.layer[1],
                        sx = r$x, sy = r$y, p3 = p3, warp = 0, ds = ds)
            if (type == "polygon") {
                  out$ring <- if (length(sg_col)) as.integer(factor(r[[sg_col[1]]])) else rep(1L, length(ix))
                  pl <- ggcube_polygon_plane(p3)
                  if (is.null(pl)) {
                        out$type <- "degenerate"
                  } else {
                        out$normal <- pl$normal; out$offset <- pl$offset; out$warp <- pl$warp
                  }
            } else if (type == "segment") {
                  if ("point_type" %in% names(r) && all(c("start", "end") %in% r$point_type)) {
                        o <- c(which(r$point_type == "start")[1], which(r$point_type == "end")[1])
                        out$sx <- r$x[o]; out$sy <- r$y[o]; out$p3 <- p3[o, , drop = FALSE]
                  } else {
                        out$sx <- r$x[1:2]; out$sy <- r$y[1:2]; out$p3 <- p3[1:2, , drop = FALSE]
                  }
                  lw <- if ("linewidth" %in% names(r) && !is.na(r$linewidth[1])) r$linewidth[1] else 0.5
                  if (!sizes_final && identical(r$element_type[1], "ref_line")) lw <- 0.25
                  out$halfwidth_pt <- lw * ds_size * pt / 2
            } else if (type == "point") {
                  size <- if ("size" %in% names(r) && !is.na(r$size[1])) r$size[1] else 1.5
                  if (!sizes_final && identical(r$element_type[1], "ref_point")) size <- size / 3
                  shape <- if ("shape" %in% names(r)) r$shape[1] else 19
                  stroke <- if ("stroke" %in% names(r) && !is.na(r$stroke[1])) r$stroke[1] else 0.5
                  fs <- size * ds_size * pt + if (is.numeric(shape) && shape %in% 21:25) stroke * ds_size * pt / 2 else 0
                  out$radius_pt <- 0.375 * fs
                  out$sx <- r$x[1]; out$sy <- r$y[1]; out$p3 <- p3[1, , drop = FALSE]
            }
            out
      })
}

prim_type <- function(r) {
      if (".prim" %in% names(r) && !is.na(r$.prim[1])) return(r$.prim[1])
      if ("element_type" %in% names(r) && !is.na(r$element_type[1])) {
            return(switch(r$element_type[1], raw_point = "point", ref_point = "point",
                          ref_circle = "polygon", ref_line = "segment", "polygon"))
      }
      switch(as.character(min(nrow(r), 3)), "1" = "point", "2" = "segment", "polygon")
}

ggcube_polygon_plane <- function(p) {
      if (nrow(p) < 3 || anyNA(p)) return(NULL)
      nxt <- c(seq_len(nrow(p))[-1], 1L)
      x <- p[, 1]; y <- p[, 2]; z <- p[, 3]
      nrm <- c(sum((y - y[nxt]) * (z + z[nxt])), sum((z - z[nxt]) * (x + x[nxt])),
               sum((x - x[nxt]) * (y + y[nxt])))
      len <- sqrt(sum(nrm^2))
      if (len < 1e-12) return(NULL)
      nrm <- nrm / len
      offset <- sum(colMeans(p) * nrm)
      list(normal = nrm, offset = offset, warp = max(abs(p %*% nrm - offset)))
}


# Geometry helpers ------------------------------------------------------------------

# Lines of sight through screen points, in cube space (as ggcube's screen_ray(),
# vectorized): points are o + lambda * d, larger lambda farther from the viewer
screen_rays <- function(sx, sy, proj, pb) {
      xt <- pb[1] + sx * (pb[2] - pb[1])
      yt <- pb[3] + sy * (pb[4] - pb[3])
      rot <- asNamespace("ggcube")$rotate_3d(diag(3), proj$pitch, proj$roll, proj$yaw)
      to_cube <- function(v) {
            f <- v %*% t(rot)
            f[, 3] <- -f[, 3]
            f
      }
      p0 <- to_cube(cbind(xt, yt, 0))
      if (isTRUE(proj$persp)) {
            eye <- as.vector(to_cube(matrix(c(0, 0, -proj$dist), 1)))
            list(o = matrix(eye, nrow(p0), 3, byrow = TRUE), d = sweep(p0, 2, eye))
      } else {
            dir <- as.vector(to_cube(matrix(c(0, 0, 1), 1)))
            list(o = p0, d = matrix(dir, nrow(p0), 3, byrow = TRUE))
      }
}

# Even-odd point-in-polygon over all rings
pip_evenodd <- function(px, py, x, y, ring) {
      inside <- logical(length(px))
      for (rg in unique(ring)) {
            sel <- which(ring == rg)
            rx <- x[sel]; ry <- y[sel]
            m <- length(rx)
            j <- m
            for (i in seq_len(m)) {
                  yi <- ry[i]; yj <- ry[j]
                  if (yi != yj) {
                        cross <- ((yi > py) != (yj > py)) &
                              (px < (rx[j] - rx[i]) * (py - yi) / (yj - yi) + rx[i])
                        inside <- xor(inside, cross)
                  }
                  j <- i
            }
      }
      inside
}

# Distance in points from screen samples to a screen segment
seg_dist <- function(px, py, sx, sy, to_npc_x, to_npc_y) {
      ax <- sx[1] / to_npc_x; ay <- sy[1] / to_npc_y
      bx <- sx[2] / to_npc_x; by <- sy[2] / to_npc_y
      qx <- px / to_npc_x; qy <- py / to_npc_y
      dx <- bx - ax; dy <- by - ay
      l2 <- dx^2 + dy^2
      t <- if (l2 < 1e-20) 0 else pmin(1, pmax(0, ((qx - ax) * dx + (qy - ay) * dy) / l2))
      sqrt((qx - ax - t * dx)^2 + (qy - ay - t * dy)^2)
}

# Ray parameter at the closest approach of each ray to a segment a-b
ray_segment_lambda <- function(o, d, a, b) {
      e <- b - a
      w <- -sweep(o, 2, a)
      dd <- rowSums(d * d); de <- as.vector(d %*% e); ee <- sum(e * e)
      den <- dd * ee - de^2
      lam <- (rowSums(d * w) * ee - de * as.vector(w %*% e)) / den
      # Clamp to the segment: parameter along the segment
      s <- (de * lam - as.vector(w %*% e)) / ee
      s <- pmin(1, pmax(0, s))
      pnt <- matrix(a, nrow(o), 3, byrow = TRUE) + outer(s, e)
      lam2 <- rowSums((pnt - o) * d) / dd
      lam2[abs(den) < 1e-14] <- NA
      lam2
}

write_error_map <- function(map, file) {
      n <- nrow(map)
      img <- array(1, dim = c(n, n, 3))
      m <- t(map)[n:1, ]                     # rows = y from top
      for (ch in 1:3) {
            layer <- img[, , ch]
            layer[m == 1] <- 0.8
            layer[m == 2] <- c(0.85, 0.1, 0.1)[ch]
            img[, , ch] <- layer
      }
      png::writePNG(img, file)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
