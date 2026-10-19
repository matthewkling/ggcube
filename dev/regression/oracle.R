# Pairwise sorting oracle: compares a new sorter against a baseline version on
# random scenes
#
#   Rscript dev/regression/oracle.R <base-name> <current-name> [n_scenes] [seed]
#
# Generates random sort-stage scenes (polygons, including non-convex and
# warped ones, segments, points, and text, under random perspective and
# orthographic views), then in each version (in separate R sessions):
#   - splits the scene and derives its occlusion constraints, and
#   - sorts it with sort_by_depth().
# Reports, per scene, constraints present in only one version or with
# opposite signs, and whether the final draw orders match.
#
# The baseline's constraints come from replaying its pw_render_order() loop;
# the current version's from pw_scene()/pw_constraints(), where available.

if (!exists("out_dir")) source(file.path("dev", "regression", "harness.R"))

random_scene <- function(seed) {
      set.seed(seed)
      proj <- list(pitch = runif(1, -30, 30), roll = runif(1, -80, -20), yaw = runif(1, -180, 180),
                   persp = runif(1) < 0.6, dist = runif(1, 1.2, 3))
      pb <- c(-1, 1, -1, 1)
      rows <- list()
      add <- function(m, group, prim) {
            rows[[length(rows) + 1]] <<- list(m = m, group = group, prim = prim)
      }
      rand_frame <- function() {
            u <- rnorm(3); u <- u / sqrt(sum(u^2))
            v <- rnorm(3); v <- v - sum(v * u) * u; v <- v / sqrt(sum(v^2))
            list(c = runif(3, -0.4, 0.4), u = u, v = v)
      }
      planar <- function(shape2d, scale) {
            f <- rand_frame()
            t(apply(shape2d * scale, 1, function(p) f$c + p[1] * f$u + p[2] * f$v))
      }
      n_poly <- sample(5:25, 1)
      for (i in seq_len(n_poly)) {
            kind <- sample(c("tri", "quad", "big", "lshape", "star", "warped"), 1,
                           prob = c(3, 3, 1, 1, 1, 2))
            m <- switch(kind,
                        tri = planar(matrix(runif(6, -1, 1), 3), runif(1, 0.05, 0.3)),
                        quad = planar(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1)), runif(1, 0.05, 0.25)),
                        big = planar(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1)), 0.6),
                        lshape = planar(rbind(c(0, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 2), c(0, 2)) - 1,
                                        runif(1, 0.05, 0.2)),
                        star = planar(t(sapply(0:9, function(k) {
                              r <- if (k %% 2) 0.4 else 1
                              r * c(cos(k * pi / 5), sin(k * pi / 5))
                        })), runif(1, 0.05, 0.2)),
                        warped = {
                              q <- planar(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1)), runif(1, 0.05, 0.2))
                              q[3, ] <- q[3, ] + rnorm(3, 0, 0.02)
                              q
                        })
            add(m, paste0("poly", i), "polygon")
      }
      for (i in seq_len(sample(0:15, 1))) {
            a <- runif(3, -0.45, 0.45)
            add(rbind(a, a + rnorm(3, 0, 0.25)), paste0("seg", i), "segment")
      }
      for (i in seq_len(sample(0:20, 1))) add(matrix(runif(3, -0.45, 0.45), 1), paste0("pt", i), "point")
      for (i in seq_len(sample(0:3, 1))) add(matrix(runif(3, -0.45, 0.45), 1), paste0("txt", i), "text")

      d <- do.call(rbind, lapply(rows, function(r) {
            n <- nrow(r$m)
            data.frame(x = r$m[, 1], y = r$m[, 2], z = r$m[, 3], group = r$group, .prim = r$prim,
                       point_type = if (r$prim == "segment") c("start", "end") else NA_character_,
                       segment_id = if (r$prim == "segment") 1 else NA_real_,
                       stringsAsFactors = FALSE)
      }))
      list(data = d, proj = proj, pb = pb)
}

# Projects a scene's cube-space rows into sort-stage data (run inside a
# version's session, using its projection functions)
project_scene <- function(sc) {
      ns <- asNamespace("ggcube")
      tr <- ns$scale_to_npc_coordinates(ns$transform_3d_standard(sc$data[c("x", "y", "z")], sc$proj),
                                         plot_bounds = sc$pb)
      out <- cbind(tr[c("x", "y", "z", "depth", "depth_scale")],
                   sc$data[setdiff(names(sc$data), c("x", "y", "z"))])
      out$.x3d <- sc$data$x; out$.y3d <- sc$data$y; out$.z3d <- sc$data$z
      out$.sort_method <- "pairwise"
      out
}

# Constraints of a (split) scene in the baseline: replays the loop of its
# dense pw_render_order()
baseline_constraints <- function(data, proj, pb) {
      ns <- asNamespace("ggcube")
      grp <- data$group; x <- data$x; y <- data$y; z <- data$depth
      ugrp <- unique(grp)
      grp_idx <- split(seq_along(grp), grp)[ugrp]
      prim_lookup <- vapply(grp_idx, function(idx) data$.prim[idx[1]], character(1))
      grp_data <- lapply(grp_idx, function(idx) {
            gx <- x[idx]; gy <- y[idx]; gz <- z[idx]
            bb <- c(min(gx), max(gx), min(gy), max(gy), min(gz), max(gz))
            cbind(cbind(x = gx, y = gy, z = gz), matrix(bb, nrow = length(idx), ncol = 6, byrow = TRUE))
      })
      betas <- ns$compute_poly_betas(grp_data, prim_lookup)
      k <- length(grp_data); groups <- names(grp_data)
      cut_parent <- ifelse(grepl("__cut[0-9]+$", groups), sub("__cut[0-9]+$", "", groups), NA_character_)
      eye <- ns$eye_in_cube_space(proj)
      g3 <- ns$group_geometry_3d(data, groups, prim_lookup)
      out <- list()
      for (i in seq_len(k)) for (j in seq_len(k)) {
            if (i >= j) next
            if (!is.na(cut_parent[i]) && identical(cut_parent[i], cut_parent[j])) next
            ti <- prim_lookup[i]; tj <- prim_lookup[j]
            dlt <- ns$pw_compare_pair(grp_data[[i]], grp_data[[j]], ti, tj, betas[[i]], betas[[j]])
            if (is.na(dlt)) next
            locked <- FALSE
            if (!is.null(eye) && !is.null(g3[[i]]) && !is.null(g3[[j]])) {
                  ex <- ns$plane_side_order(g3[[i]], g3[[j]], eye)
                  if (is.na(ex)) ex <- ns$coplanar_order(g3[[i]], g3[[j]], ti, tj)
                  if (is.na(ex)) ex <- ns$ray_sample_order(g3[[i]], g3[[j]], grp_data[[i]], grp_data[[j]],
                                                            ti, tj, proj, pb)
                  if (!is.na(ex)) { dlt <- ex * max(abs(dlt), 1e-8); locked <- TRUE }
            }
            if (dlt == 0) next
            out[[length(out) + 1]] <- if (dlt > 0) c(i, j, abs(dlt), locked) else c(j, i, abs(dlt), locked)
      }
      e <- if (length(out)) as.data.frame(do.call(rbind, out)) else data.frame(V1 = numeric(), V2 = numeric(), V3 = numeric(), V4 = numeric())
      names(e) <- c("from", "to", "strength", "locked")
      list(groups = groups, edges = e)
}

current_constraints <- function(data, proj, pb) {
      ns <- asNamespace("ggcube")
      sc <- ns$pw_scene(data, "depth", proj, pb)
      cons <- ns$pw_constraints(sc)
      list(groups = as.character(sc$groups), edges = cons$edges)
}

run_version <- function(name, scenes, validate = TRUE) {
      lib <- out_dir("libs", name)
      callr::r(function(lib, scenes, fns, validate_file, validate) {
            .libPaths(c(lib, .libPaths()))
            suppressPackageStartupMessages(library(ggcube))
            for (nm in names(fns)) assign(nm, fns[[nm]])
            source(validate_file, local = TRUE)
            ns <- asNamespace("ggcube")

            # Keep cube-space coordinates through sorting (as in validate.R)
            orig_proj <- get("project_new_vertices", envir = ns)
            keep3d <- function(d) { d$.kx3d <- d$.x3d; d$.ky3d <- d$.y3d; d$.kz3d <- d$.z3d; d }
            unlockBinding("project_new_vertices", ns)
            assign("project_new_vertices", function(rows, proj, plot_bounds) keep3d(orig_proj(rows, proj, plot_bounds)), envir = ns)
            on.exit({ assign("project_new_vertices", orig_proj, envir = ns); lockBinding("project_new_vertices", ns) })

            lapply(scenes, function(sc) {
                  d <- keep3d(project_scene(sc))
                  split <- ns$split_interpenetrating_segments(
                        ns$split_intersecting_polygons(d, sc$proj, sc$pb), sc$proj, sc$pb)
                  cons <- if (exists("pw_scene", ns, inherits = FALSE)) {
                        current_constraints(split, sc$proj, sc$pb)
                  } else {
                        baseline_constraints(split, sc$proj, sc$pb)
                  }
                  t <- system.time(sorted <- withCallingHandlers(ns$sort_by_depth(d, sc$proj, sc$pb),
                                                                 warning = function(w) invokeRestart("muffleWarning")))
                  errors <- NA_integer_
                  if (validate) {
                        rows <- sorted
                        rows$.x3d <- rows$.kx3d; rows$.y3d <- rows$.ky3d; rows$.z3d <- rows$.kz3d
                        rows$.layer <- 1L
                        errors <- validate_panel(rows, sc$proj, sc$pb, c(400, 400), n = 200)$errors
                  }
                  list(cons = cons, order = unique(as.character(sorted$group)), seconds = t[["elapsed"]],
                       errors = errors)
            })
      }, args = list(lib = lib, scenes = scenes,
                     fns = list(project_scene = project_scene, baseline_constraints = baseline_constraints,
                                current_constraints = current_constraints),
                     validate_file = file.path(harness_dir(), "validate.R"), validate = validate))
}

edge_keys <- function(cons) {
      e <- cons$edges
      if (nrow(e) == 0) return(data.frame(key = character(), dir = integer(), locked = logical()))
      a <- cons$groups[e$from]; b <- cons$groups[e$to]
      lo <- pmin(a, b); hi <- pmax(a, b)
      data.frame(key = paste(lo, hi, sep = " | "), dir = ifelse(a == lo, 1L, -1L),
                 locked = as.logical(e$locked), stringsAsFactors = FALSE)
}

compare_oracle <- function(base, current, n = 200, seed = 1) {
      scenes <- lapply(seed + seq_len(n) - 1, random_scene)
      rb <- run_version(base, scenes)
      rc <- run_version(current, scenes)
      res <- do.call(rbind, lapply(seq_len(n), function(s) {
            kb <- edge_keys(rb[[s]]$cons); kc <- edge_keys(rc[[s]]$cons)
            m <- merge(kb, kc, by = "key", all = TRUE, suffixes = c(".b", ".c"))
            data.frame(scene = seed + s - 1,
                       groups = length(rb[[s]]$order),
                       edges_base = nrow(kb), edges_current = nrow(kc),
                       only_base = sum(is.na(m$dir.c)), only_current = sum(is.na(m$dir.b)),
                       flipped = sum(!is.na(m$dir.b) & !is.na(m$dir.c) & m$dir.b != m$dir.c),
                       lock_differs = sum(!is.na(m$dir.b) & !is.na(m$dir.c) & m$locked.b != m$locked.c),
                       same_order = identical(rb[[s]]$order, rc[[s]]$order),
                       errors_base = rb[[s]]$errors, errors_current = rc[[s]]$errors,
                       base_s = rb[[s]]$seconds, current_s = rc[[s]]$seconds)
      }))
      attr(res, "details") <- list(base = rb, current = rc, scenes = scenes)
      res
}

if (sys.nframe() == 0) {
      args <- commandArgs(trailingOnly = TRUE)
      n <- if (length(args) > 2) as.integer(args[3]) else 200
      seed <- if (length(args) > 3) as.integer(args[4]) else 1
      res <- compare_oracle(args[1], args[2], n, seed)
      saveRDS(res, file.path(out_dir("oracle"), paste0(args[1], "_vs_", args[2], ".rds")))
      cat("Scenes:", nrow(res), "\n")
      cat("Identical draw order:", sum(res$same_order), "\n")
      cat("Scenes with any constraint difference:",
          sum(res$only_base + res$only_current + res$flipped > 0), "\n")
      cat("Totals: edges", sum(res$edges_base), "vs", sum(res$edges_current),
          "| only base", sum(res$only_base), "| only current", sum(res$only_current),
          "| flipped", sum(res$flipped), "| locked differs", sum(res$lock_differs), "\n")
      cat("Ground-truth ordering errors (samples), all scenes: base", sum(res$errors_base),
          "| current", sum(res$errors_current),
          "| scenes worse:", sum(res$errors_current > res$errors_base),
          "better:", sum(res$errors_current < res$errors_base), "\n")
      bad <- res[res$only_base + res$only_current + res$flipped > 0 | !res$same_order |
                       res$errors_current != res$errors_base, ]
      if (nrow(bad)) print(utils::head(bad, 30), row.names = FALSE)
}
