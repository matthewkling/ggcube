# Tests for pairwise comparison modules in depth sorting.

# Screen-space group matrix in the layout pw_render_order() compares
# (x, y, depth, then the six bounding-box values repeated per row)
screen_group <- function(x, y, depth) {
      bb <- c(min(x), max(x), min(y), max(y), min(depth), max(depth))
      cbind(x = x, y = y, z = depth, matrix(bb, nrow = length(x), ncol = 6, byrow = TRUE))
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
            eye_side <- ggcube:::eye_side_of_plane(pl, ggcube:::eye_in_cube_space(proj))
            plane_scr <- out[out$group == "plane", ]

            n_checked <- 0
            for (g in setdiff(ord, "plane")) {
                  r <- out[out$group == g, ]
                  if (!ggcube:::point_in_polygon(mean(r$x), mean(r$y), plane_scr$x, plane_scr$y)) next
                  dist <- plane_dist(cbind(r$.x3d, r$.y3d, r$.z3d), pl)
                  side <- sign(dist[which.max(abs(dist))])
                  n_checked <- n_checked + 1
                  expect_equal(pos[[g]] > pos[["plane"]], side == eye_side, info = paste(g, proj$persp))
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
      delta <- matrix(NA_real_, 5, 5)
      set_edge <- function(i, j, v) { delta[i, j] <<- v; delta[j, i] <<- -v }
      set_edge(1, 2, 0.3); set_edge(2, 3, 0.2); set_edge(3, 1, 0.5)
      set_edge(4, 5, 0.001)
      locked <- !is.na(delta); locked[4:5, 4:5] <- FALSE

      pruned <- ggcube:::break_cycles(delta, locked)
      expect_equal(pruned[4, 5], 0.001)
      expect_true(is.na(pruned[2, 3]))
      expect_equal(pruned[1, 2], 0.3)
      expect_true(ggcube:::is_acyclic((pruned > 0) & !is.na(pruned)))
})


test_that("strongly_connected_components groups mutually reachable nodes", {
      adj <- matrix(FALSE, 6, 6)
      adj[1, 2] <- adj[2, 3] <- adj[3, 1] <- TRUE        # cycle {1,2,3}
      adj[3, 4] <- TRUE
      adj[4, 5] <- adj[5, 4] <- TRUE                     # cycle {4,5}
      comp <- ggcube:::strongly_connected_components(adj)
      expect_length(unique(comp[1:3]), 1)
      expect_length(unique(comp[4:5]), 1)
      expect_length(unique(comp), 3)
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
      # groups reach the sort untyped, alongside typed annotation rows
      p <- ggplot(data.frame(x = 1:3, y = 1:3, z = 1:3), aes(x, y, z)) +
            geom_point_3d(sort_method = "pairwise", ref_lines = TRUE,
                          annotate = annotate_3d("point", x = 2, y = 2, z = 2.5)) +
            coord_3d()
      expect_no_error(ggplotGrob(p))

      d <- data.frame(x = c(0, 0, 1, 0, 1, 1), y = c(0, 0, 0, 1, 0, 1), depth = 1,
                      group = c("p", "s", "s", "g", "g", "g"))
      res <- pw_render_order(d, "depth")
      expect_setequal(unique(res$data$group), c("p", "s", "g"))
})
