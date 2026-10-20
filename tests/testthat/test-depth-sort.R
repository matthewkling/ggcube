# Tests for pairwise comparison modules in depth sorting.

# Screen-space group matrix in the layout pw_render_order() compares
# (x, y, depth, then the six bounding-box values repeated per row)
screen_group <- function(x, y, depth) {
      bb <- c(min(x), max(x), min(y), max(y), min(depth), max(depth))
      cbind(x = x, y = y, z = depth, matrix(bb, nrow = length(x), ncol = 6, byrow = TRUE))
}

# Constraint edge list: `from` is drawn before (behind) `to`
edges <- function(from, to, strength = 1, locked = FALSE) {
      data.frame(from = as.integer(from), to = as.integer(to), strength = strength, locked = locked)
}

# Which side of a polygon's plane the viewer is on
eye_side <- function(plane, eye) {
      if (eye$persp) sign(sum(plane$normal * eye$v) - plane$offset) else sign(sum(plane$normal * eye$v))
}

test_that("polygon-segment comparison constrains a segment overlapping a polygon on screen", {
      square <- screen_group(c(0, 1, 1, 0), c(0, 0, 1, 1), c(1, 1, 1, 1))
      near <- screen_group(c(0.2, 0.8), c(0.5, 0.5), c(0, 0))
      far <- screen_group(c(0.2, 0.8), c(0.5, 0.5), c(2, 2))
      crossing <- screen_group(c(-0.5, 0.5), c(0.5, 0.5), c(0, 0))

      # positive delta = i (polygon) behind j (segment)
      expect_gt(ggcube:::pw_compare_pair(square, near, "polygon", "segment"), 0)
      expect_lt(ggcube:::pw_compare_pair(square, far, "polygon", "segment"), 0)
      expect_gt(ggcube:::pw_compare_pair(square, crossing, "polygon", "segment"), 0)
      expect_lt(ggcube:::pw_compare_pair(near, square, "segment", "polygon"), 0)

      outside <- screen_group(c(1.5, 2), c(0.5, 0.5), c(0, 0))
      expect_true(is.na(ggcube:::pw_compare_pair(square, outside, "polygon", "segment")))
})


# 3D scenes ---------------------------------------------------------------------
# Sort-stage data built from cube-space geometry, mirroring the columns
# coord_3d()$transform hands to sort_by_depth().

proj_p <- list(pitch = 0, roll = -60, yaw = -30, persp = TRUE, dist = 2)
proj_o <- list(pitch = 0, roll = -60, yaw = -30, persp = FALSE, dist = 2)
pb <- c(-1, 1, -1, 1)

project_cube <- function(m, proj) {
      ggcube:::scale_to_npc_coordinates(
            ggcube:::transform_3d_standard(data.frame(x = m[, 1], y = m[, 2], z = m[, 3]), proj),
            plot_bounds = pb)
}

scene_rows <- function(m, group, prim, proj) {
      tr <- project_cube(m, proj)
      data.frame(x = tr$x, y = tr$y, z = tr$z, depth = tr$depth, depth_scale = tr$depth_scale,
                 group = group, .prim = prim,
                 point_type = if (prim == "segment") c("start", "end") else NA_character_,
                 segment_id = if (prim == "segment") 1 else NA_real_,
                 .x3d = m[, 1], .y3d = m[, 2], .z3d = m[, 3], stringsAsFactors = FALSE)
}

tilted_plane <- cbind(c(-0.5, 0.5, 0.5, -0.5), c(-0.5, -0.5, 0.5, 0.5), 0.42 * c(-0.5, -0.5, 0.5, 0.5))

plane_dist <- function(m, pl) as.vector(m %*% pl$normal - pl$offset)


test_that("segments are split exactly where they pass through a polygon", {
      pl <- ggcube:::polygon_plane(tilted_plane)
      hit <- c(0.1, -0.2, 0.42 * -0.2)
      a <- hit - 0.15 * c(0.3, 0.2, 0.9)
      b <- hit + 0.25 * c(0.3, 0.2, 0.9)
      for (proj in list(proj_p, proj_o)) {
            d <- rbind(scene_rows(tilted_plane, "plane", "polygon", proj),
                       scene_rows(rbind(a, b), "seg", "segment", proj))
            out <- ggcube:::split_interpenetrating_segments(d, proj, pb)

            expect_setequal(unique(out$group), c("plane", "seg__split1", "seg__split2"))
            p1 <- out[out$group == "seg__split1", ]
            p2 <- out[out$group == "seg__split2", ]
            cut1 <- unlist(p1[p1$point_type == "end", c(".x3d", ".y3d", ".z3d")])
            expect_equal(unname(cut1), hit, tolerance = 1e-12)

            # New vertex projected exactly as coord_3d would project it
            true_scr <- project_cube(rbind(hit), proj)
            expect_equal(p1$x[p1$point_type == "end"], true_scr$x, tolerance = 1e-12)
            expect_equal(p2$depth[p2$point_type == "start"], true_scr$depth, tolerance = 1e-12)

            # Pieces on opposite sides of the plane
            s1 <- plane_dist(cbind(p1$.x3d, p1$.y3d, p1$.z3d), pl)
            s2 <- plane_dist(cbind(p2$.x3d, p2$.y3d, p2$.z3d), pl)
            expect_true(all(s1 <= 1e-12) && all(s2 >= -1e-12) || all(s1 >= -1e-12) && all(s2 <= 1e-12))
      }
})


test_that("segments are not split when they cross a plane outside the polygon or merely touch it", {
      # Crosses the tilted plane's extension beyond the polygon
      outside <- rbind(c(0.8, 0, -0.2), c(0.8, 0, 0.2))
      # Starts within the touch tolerance of the plane (like a residual line)
      on_plane <- c(-0.1, 0.1, 0.42 * 0.1)
      residual <- rbind(on_plane - c(0, 0, 0.005), on_plane + c(0, 0, 0.3))
      d <- rbind(scene_rows(tilted_plane, "plane", "polygon", proj_p),
                 scene_rows(outside, "out", "segment", proj_p),
                 scene_rows(residual, "res", "segment", proj_p))
      expect_identical(ggcube:::split_interpenetrating_segments(d, proj_p, pb), d)
})


test_that("pairwise sorting orders segments and points against a plane exactly", {
      set.seed(3)
      pl <- ggcube:::polygon_plane(tilted_plane)
      n <- 20
      hit <- cbind(runif(n, -0.4, 0.4), runif(n, -0.4, 0.4)); hit <- cbind(hit, 0.42 * hit[, 2])
      dirs <- matrix(rnorm(n * 3), ncol = 3); dirs <- dirs / sqrt(rowSums(dirs^2))
      dirs <- dirs[abs(dirs %*% pl$normal) > 0.3, , drop = FALSE]
      hit <- hit[seq_len(nrow(dirs)), , drop = FALSE]
      pts <- cbind(runif(n, -0.4, 0.4), runif(n, -0.4, 0.4))
      pts <- cbind(pts, 0.42 * pts[, 2]) + outer(sample(c(-1, 1), n, TRUE) * runif(n, 0.005, 0.05), pl$normal)

      for (proj in list(proj_p, proj_o)) {
            d <- scene_rows(tilted_plane, "plane", "polygon", proj)
            for (i in seq_len(nrow(dirs))) {
                  d <- rbind(d, scene_rows(rbind(hit[i, ] - 0.1 * dirs[i, ], hit[i, ] + 0.15 * dirs[i, ]),
                                           paste0("s", i), "segment", proj))
            }
            for (i in seq_len(n)) d <- rbind(d, scene_rows(pts[i, , drop = FALSE], paste0("p", i), "point", proj))

            res <- ggcube:::pw_render_order(d, "depth", proj, pb)
            out <- res$data
            ord <- unique(out$group[order(res$render_order)])
            expect_equal(sum(grepl("__split", ord)), 2 * nrow(dirs))
            pos <- setNames(seq_along(ord), ord)
            side_eye <- eye_side(pl, ggcube:::eye_in_cube_space(proj))
            plane_scr <- out[out$group == "plane", ]

            n_checked <- 0
            for (g in setdiff(ord, "plane")) {
                  r <- out[out$group == g, ]
                  if (!ggcube:::point_in_polygon(mean(r$x), mean(r$y), plane_scr$x, plane_scr$y)) next
                  dist <- plane_dist(cbind(r$.x3d, r$.y3d, r$.z3d), pl)
                  side <- sign(dist[which.max(abs(dist))])
                  n_checked <- n_checked + 1
                  expect_equal(pos[[g]] > pos[["plane"]], side == side_eye, info = paste(g, proj$persp))
            }
            expect_gt(n_checked, 30)
      }
})


test_that("screen_ray inverts the coord_3d projection", {
      set.seed(1)
      P <- matrix(runif(30, -0.5, 0.5), ncol = 3)
      for (proj in list(proj_p, proj_o)) {
            scr <- project_cube(P, proj)
            lam <- numeric(nrow(P))
            for (k in seq_len(nrow(P))) {
                  r <- ggcube:::screen_ray(scr$x[k], scr$y[k], proj, pb)
                  lam[k] <- sum((P[k, ] - r$o) * r$d) / sum(r$d^2)
                  expect_equal(r$o + lam[k] * r$d, P[k, ], tolerance = 1e-10)
            }
            expect_identical(order(lam), order(scr$depth))
      }
})


test_that("ray_sample_order resolves disjoint primitives that straddle each other's planes", {
      for (proj in list(proj_p, proj_o)) {
            pl <- ggcube:::polygon_plane(tilted_plane)
            # A segment starting directly behind a point inside the polygon and
            # crossing the plane's extension outside the polygon (near x = y = 0.7)
            eye <- ggcube:::eye_in_cube_space(proj)
            q <- c(0.4, 0.4, 0.42 * 0.4)
            away <- if (proj$persp) q - eye$v else -eye$v
            a <- q + 0.3 * away / sqrt(sum(away^2))
            b <- c(0.9, 0.9, 0.42 * 0.9 + 0.3)
            stopifnot(prod(plane_dist(rbind(a, b), pl)) < 0)
            d <- rbind(scene_rows(tilted_plane, "plane", "polygon", proj),
                       scene_rows(rbind(a, b), "seg", "segment", proj))
            expect_identical(ggcube:::split_interpenetrating_segments(d, proj, pb), d)

            g_poly <- list(pts = tilted_plane, plane = pl, touch = 0)
            g_seg <- list(pts = rbind(a, b), plane = NULL, touch = 0.01)
            expect_true(is.na(ggcube:::plane_side_order(g_poly, g_seg, ggcube:::eye_in_cube_space(proj))))

            m <- function(g) { r <- d[d$group == g, ]
                  cbind(x = r$x, y = r$y, z = r$depth,
                        matrix(c(range(r$x), range(r$y), range(r$depth)), nrow(r), 6, byrow = TRUE)) }
            mid <- ggcube:::segment_overlap_midpoint(m("plane"), m("seg"))
            expect_false(is.null(mid))
            ord <- ggcube:::ray_sample_order(g_poly, g_seg, m("plane"), m("seg"), "polygon", "segment", proj, pb)

            # The segment starts directly behind the polygon, so the polygon is in
            # front wherever they overlap (i = polygon in front of j: -1)
            expect_equal(ord, -1)
            expect_equal(ggcube:::ray_sample_order(g_seg, g_poly, m("seg"), m("plane"), "segment", "polygon", proj, pb), -ord)
      }
})


test_that("cycle breaking only removes constraints that lie on a cycle", {
      # Locked 3-cycle among 1, 2, 3, plus a weak unlocked constraint 4 -> 5
      e <- rbind(edges(c(1, 2, 3), c(2, 3, 1), c(0.3, 0.2, 0.5), locked = TRUE),
                 edges(4, 5, 0.001, locked = FALSE))

      pruned <- ggcube:::break_cycles(e, 5)
      expect_true(any(pruned$from == 4 & pruned$to == 5))
      expect_false(any(pruned$from == 2 & pruned$to == 3))
      expect_true(any(pruned$from == 1 & pruned$to == 2))
      expect_equal(nrow(pruned), 3)
      expect_true(ggcube:::is_acyclic(5, pruned))
      expect_false(ggcube:::is_acyclic(5, e))
})


test_that("cycle breaking handles separate cycles independently", {
      # Two disjoint 3-cycles; each loses only its own weakest edge
      e <- rbind(edges(c(1, 2, 3), c(2, 3, 1), c(0.3, 0.1, 0.5)),
                 edges(c(4, 5, 6), c(5, 6, 4), c(0.05, 0.4, 0.2)))
      pruned <- ggcube:::break_cycles(e, 6)
      removed <- e[!do.call(paste, e[1:2]) %in% do.call(paste, pruned[1:2]), ]
      expect_equal(removed$from, c(2, 4))
      expect_equal(removed$to, c(3, 5))
})


test_that("strongly_connected_components groups mutually reachable nodes", {
      e <- edges(c(1, 2, 3, 3, 4, 5), c(2, 3, 1, 4, 5, 4))   # cycles {1,2,3} and {4,5}
      comp <- ggcube:::strongly_connected_components(6, e)
      expect_length(unique(comp[1:3]), 1)
      expect_length(unique(comp[4:5]), 1)
      expect_length(unique(comp), 3)
})


test_that("topological sort draws the farthest available node first, ties in arrival order", {
      # 1 must precede 2; otherwise farthest first. Nodes 3 and 4 tie in depth.
      e <- edges(1, 2)
      expect_equal(ggcube:::topological_sort(4, e, c(1, 5, 3, 3)), c(3, 4, 1, 2))
      # A node freed later goes after an equally deep node already waiting
      e <- edges(c(1, 1), c(2, 3))
      expect_equal(ggcube:::topological_sort(4, e, c(9, 2, 2, 2)), c(1, 4, 2, 3))
})


# Arrowheads on split segments --------------------------------------------------

test_that("split segments keep arrowheads only on the pieces holding the original ends", {
      pl <- ggcube:::polygon_plane(tilted_plane)
      hit <- c(0.1, -0.2, 0.42 * -0.2)
      a <- hit - 0.15 * c(0.3, 0.2, 0.9)
      b <- hit + 0.25 * c(0.3, 0.2, 0.9)
      d <- rbind(scene_rows(tilted_plane, "plane", "polygon", proj_p),
                 scene_rows(rbind(a, b), "seg", "segment", proj_p))
      d$colour <- "black"; d$linewidth <- 0.5; d$alpha <- NA
      out <- ggcube:::split_interpenetrating_segments(d, proj_p, pb)
      segs <- out[out$.prim == "segment", ]
      starts <- segs[segs$point_type == "start", ]
      starts <- starts[order(starts$group), ]
      expect_equal(starts$.seg_first, c(TRUE, FALSE))
      expect_equal(starts$.seg_last, c(FALSE, TRUE))

      arrow_grobs <- function(g) {
            kids <- if (inherits(g, "gTree")) g$children else list(g)
            Filter(function(k) !is.null(k$arrow), kids)
      }
      end_xy <- function(piece) unlist(segs[segs$group == piece & segs$point_type == "end", c("x", "y")])
      start_xy <- function(piece) unlist(segs[segs$group == piece & segs$point_type == "start", c("x", "y")])

      # ends = "last": one arrowhead, at the end of the last piece
      g <- arrow_grobs(ggcube:::render_segments(segs, arrow = grid::arrow()))
      expect_length(g, 1)
      expect_equal(g[[1]]$arrow$ends, 2L)
      expect_equal(c(as.numeric(g[[1]]$x1), as.numeric(g[[1]]$y1)), unname(end_xy("seg__split2")))

      # ends = "both": a start arrowhead on the first piece, an end arrowhead on the last
      g <- arrow_grobs(ggcube:::render_segments(segs, arrow = grid::arrow(ends = "both")))
      expect_length(g, 2)
      by_end <- setNames(g, vapply(g, function(k) k$arrow$ends, 1L))
      expect_setequal(names(by_end), c("1", "2"))
      expect_equal(c(as.numeric(by_end[["1"]]$x0), as.numeric(by_end[["1"]]$y0)), unname(start_xy("seg__split1")))
      expect_equal(c(as.numeric(by_end[["2"]]$x1), as.numeric(by_end[["2"]]$y1)), unname(end_xy("seg__split2")))
})


test_that("unsplit segments are drawn as a single grob with the arrow unchanged", {
      d <- data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1), group = c(1, 1, 2, 2),
                      point_type = c("start", "end", "start", "end"),
                      colour = "black", linewidth = 0.5, alpha = NA)
      arr <- grid::arrow(ends = "both", type = "closed")
      g <- ggcube:::render_segments(d, arrow = arr)
      expect_s3_class(g, "segments")
      expect_identical(g$arrow, arr)
      expect_null(ggcube:::render_segments(d)$arrow)
})


test_that("per-piece arrow runs preserve drawing order", {
      # Draw order: a (unsplit), p1 (first piece), p2 (last piece), b (unsplit)
      d <- data.frame(x = c(0, 1, 0, 0.5, 0.5, 1, 0, 1), y = rep(1:4, each = 2),
                      group = rep(c("a", "p__split1", "p__split2", "b"), each = 2),
                      point_type = rep(c("start", "end"), 4),
                      .seg_first = rep(c(NA, TRUE, FALSE, NA), each = 2),
                      .seg_last = rep(c(NA, FALSE, TRUE, NA), each = 2),
                      colour = "black", linewidth = 0.5, alpha = NA)
      g <- ggcube:::render_segments(d, arrow = grid::arrow())
      ys <- unname(unlist(lapply(g$children, function(k) as.numeric(k$y0))))
      expect_equal(ys, 1:4)
      has_arrow <- unname(unlist(lapply(g$children, function(k) rep(!is.null(k$arrow), length(k$y0)))))
      expect_equal(has_arrow, c(TRUE, FALSE, TRUE, TRUE))
})


test_that("pairwise sorting types untyped groups by vertex count", {
      # Point layers assign primitive types only when rendering, so their
      # groups reach the sort untyped, alongside other layers' typed rows
      p <- ggplot(data.frame(x = 1:3, y = 1:3, z = 1:3), aes(x, y, z)) +
            suppressWarnings(geom_point_3d(ref_lines = TRUE)) +
            annotate_3d("segment", x = 2, y = 2, z = 2.5, xend = 3, yend = 2, zend = 2.5) +
            coord_3d()
      expect_no_error(drawn_grob(p))

      d <- data.frame(x = c(0, 0, 1, 0, 1, 1), y = c(0, 0, 0, 1, 0, 1), depth = 1,
                      group = c("p", "s", "s", "g", "g", "g"))
      res <- pw_render_order(d, "depth")
      expect_setequal(unique(res$data$group), c("p", "s", "g"))
})


# Segments through surfaces ---------------------------------------------------

tile_scene <- function(seg, persp = TRUE) {
      g <- expand.grid(i = 1:10, j = 1:10)
      tiles <- do.call(rbind, lapply(seq_len(nrow(g)), function(k) {
            x0 <- -1 + (g$i[k] - 1) * 0.2; y0 <- -1 + (g$j[k] - 1) * 0.2
            data.frame(x = x0 + c(0, 0.2, 0.2, 0), y = y0 + c(0, 0, 0.2, 0.2), z = 0, tile = k)
      }))
      p <- ggplot(tiles, aes(x, y, z, group = tile)) +
            geom_polygon_3d() + seg +
            zlim(-1, 1) + coord_3d(persp = persp)
      out <- ggcube:::validation_scene(p)$panels[[1]]$rows
      out$group <- paste(out$.layer, out$group)      # groups are numbered per layer
      out
}


test_that("segments above a surface stay in front of it and those below behind", {
      for (persp in c(TRUE, FALSE)) {
            for (zz in c(0.3, -0.3)) {
                  seg <- annotate_3d("segment", x = -0.9, y = -0.6, z = zz, xend = 0.9, yend = 0.7, zend = zz)
                  out <- tile_scene(seg, persp)
                  tiles <- unique(out$group[out$.prim %in% "polygon"])
                  for (id in unique(out$group[out$.prim %in% "segment"])) {
                        r <- which(out$group == id)
                        f <- seq(0.1, 0.9, by = 0.1)
                        sx <- out$x[r[1]] + f * diff(out$x[r])
                        sy <- out$y[r[1]] + f * diff(out$y[r])
                        for (t in tiles) {
                              tr <- which(out$group == t)
                              covered <- any(mapply(point_in_polygon, sx, sy,
                                                    MoreArgs = list(poly_x = out$x[tr], poly_y = out$y[tr])))
                              if (!covered) next
                              if (zz > 0) expect_gt(min(r), max(tr)) else expect_lt(max(r), min(tr))
                        }
                  }
            }
      }
})


# Scaling ------------------------------------------------------------------------

test_that("pairwise sorting compares only primitives that overlap on screen", {
      # A grid of small separated squares facing the viewer, plus one large
      # polygon behind part of them
      sq <- function(cx, cy, h = 0.01) cbind(cx + c(-h, h, h, -h), cy + c(-h, -h, h, h))
      ctr <- expand.grid(x = seq(0.05, 0.95, length.out = 20), y = seq(0.05, 0.95, length.out = 20))
      polys <- lapply(seq_len(nrow(ctr)), function(k) sq(ctr$x[k], ctr$y[k]))
      polys <- c(polys, list(cbind(c(0, 0.5, 0.5, 0), c(0, 0, 0.5, 0.5))))
      d <- do.call(rbind, lapply(seq_along(polys), function(k) {
            data.frame(x = polys[[k]][, 1], y = polys[[k]][, 2],
                       depth = if (k == length(polys)) 2 else 1,
                       group = paste0("g", k), .prim = "polygon")
      }))
      scene <- ggcube:::pw_scene(d, "depth")
      cons <- ggcube:::pw_constraints(scene)

      # Only squares overlapping the large polygon are candidates (100 of 400)
      expect_equal(cons$n_candidates, 100)
      expect_equal(nrow(cons$edges), 100)
      expect_true(all(cons$edges$from == length(polys)))
})


test_that("pairwise sorting handles large layers", {
      skip_on_cran()
      set.seed(1)
      n <- 20000
      P <- matrix(runif(3 * n, -0.45, 0.45), ncol = 3)
      plane <- cbind(c(-0.5, 0.5, 0.5, -0.5), c(-0.5, -0.5, 0.5, 0.5), 0)
      scr <- project_cube(P, proj_p)
      pts <- data.frame(x = scr$x, y = scr$y, z = scr$z, depth = scr$depth, depth_scale = scr$depth_scale,
                        group = paste0("p", seq_len(n)), .prim = "point", point_type = NA_character_,
                        segment_id = NA_real_, .x3d = P[, 1], .y3d = P[, 2], .z3d = P[, 3])
      d <- rbind(scene_rows(plane, "plane", "polygon", proj_p), pts)

      t <- system.time(res <- ggcube:::pw_render_order(d, "depth", proj_p, pb))[["elapsed"]]
      expect_lt(t, 30)

      # Points over the plane on screen are drawn after it exactly when they
      # are on the viewer's side of it
      ord <- unique(res$data$group[order(res$render_order)])
      after <- match(pts$group, ord) > match("plane", ord)
      pl <- ggcube:::polygon_plane(plane)
      viewer_side <- sign(P[, 3]) == eye_side(pl, ggcube:::eye_in_cube_space(proj_p))
      plane_scr <- d[d$group == "plane", ]
      over <- ggcube:::points_in_polygons_cpp(pts$x, pts$y, rep(0L, n), plane_scr$x, plane_scr$y, c(0L, 4L))
      expect_gt(sum(over), n / 2)
      expect_equal(after[over], viewer_side[over])
})


test_that("overlaps with non-convex polygons are sampled inside the overlap", {
      # A U-shaped polygon and a segment crossing both of its arms: the
      # segment's inside pieces are separated by the notch, so the mean of
      # their end points falls in the notch, outside the polygon
      u <- cbind(c(0, 1, 1, 0.7, 0.7, 0.3, 0.3, 0), c(0, 0, 1, 1, 0.2, 0.2, 1, 1))
      seg <- cbind(c(-0.1, 1.1), c(0.6, 0.6))
      mid <- ggcube:::segment_overlap_midpoint(u, seg)
      expect_true(ggcube:::point_in_polygon(mid[1], mid[2], u[, 1], u[, 2]))

      # The same for two polygons: a bar across both arms of the U
      bar <- cbind(c(-0.1, 1.1, 1.1, -0.1), c(0.5, 0.5, 0.7, 0.7))
      ctr <- ggcube:::interior_point_cpp(c(0, 0.3, 0.3, 0), c(0.5, 0.5, 0.7, 0.7))
      expect_true(ggcube:::point_in_polygon(ctr[1], ctr[2], u[, 1], u[, 2]))
      xy <- ggcube:::intersection_centroid(cbind(x = u[, 1], y = u[, 2]), cbind(x = bar[, 1], y = bar[, 2]))
      expect_true(ggcube:::point_in_polygon(xy[1], xy[2], u[, 1], u[, 2]))
      expect_true(ggcube:::point_in_polygon(xy[1], xy[2], bar[, 1], bar[, 2]))
})


# Marker and line extents --------------------------------------------------------

# Screen-only sort data: a polygon Q spanning x = 0.1 to 0.5 whose depth
# varies linearly in x, plus points with given positions, depths, and radii
marker_scene <- function(q_depth, pts) {
      q <- data.frame(x = c(0.1, 0.5, 0.5, 0.1), y = c(0.4, 0.4, 0.6, 0.6),
                      depth = c(q_depth[1], q_depth[2], q_depth[2], q_depth[1]),
                      group = "Q", .prim = "polygon", .radius_pt = 0)
      p <- data.frame(x = pts$x, y = 0.5, depth = pts$depth, group = pts$group,
                      .prim = "point", .radius_pt = pts$r)
      rbind(q, p)
}
drawn_groups <- function(d, panel_pt) unique(ggcube:::order_by_depth(d, panel_pt = panel_pt)$group)

test_that("overlapping markers are drawn farther first, even when a constraint delays the farther", {
      # F (far) is over Q, which is behind it there, so F must wait for Q,
      # whose mean depth is nearer than N's; N (near) overlaps F but not Q
      d <- marker_scene(c(-2, 3), data.frame(x = c(0.49, 0.505), depth = c(2, 1),
                                                group = c("F", "N"), r = c(5, 1.5)))
      expect_equal(drawn_groups(d, c(400, 400)), c("Q", "F", "N"))
      # Without the panel size, markers are treated as points and N comes first
      expect_equal(drawn_groups(d, NULL), c("N", "Q", "F"))
})

test_that("markers overlapping a polygon's edge are ordered against it", {
      # P's centre is just outside Q, but its marker reaches over Q's edge,
      # where Q is in front of it
      d <- marker_scene(c(5, 1), data.frame(x = 0.505, depth = 2, group = "P", r = 5))
      expect_equal(drawn_groups(d, c(400, 400)), c("P", "Q"))
      expect_equal(drawn_groups(d, NULL), c("Q", "P"))
})

test_that("coplanar primitives of different layers are drawn in layer order", {
      sq <- function(x0) data.frame(x = x0 + c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = 0)
      drawn_fill <- function(first, second) {
            p <- ggplot() +
                  geom_polygon_3d(data = sq(0), aes(x, y, z), fill = first) +
                  geom_polygon_3d(data = sq(0.5), aes(x, y, z), fill = second) +
                  coord_3d(light = "none")
            polys <- find_grobs(drawn_grob(p), "polygon")
            fills <- unname(unlist(lapply(polys, function(g) g$gp$fill)))
            fills[fills %in% c(first, second)]
      }
      expect_equal(drawn_fill("red", "blue"), c("red", "blue"))
      expect_equal(drawn_fill("blue", "red"), c("blue", "red"))
})
