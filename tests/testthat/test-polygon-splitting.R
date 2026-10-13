# Tests for splitting interpenetrating polygons during pairwise depth sorting,
# exact plane-side ordering, and seam-free rendering of split pieces.
#
# Ground truth comes from the projection pipeline itself (transform_3d_standard
# depths) or from exact 3D geometry, not from rendered pixels.

proj_persp <- list(pitch = 0, roll = -60, yaw = -30, persp = TRUE, dist = 2)
proj_ortho <- list(pitch = 0, roll = -60, yaw = -30, persp = FALSE, dist = 2)
bounds <- c(-1, 1, -1, 1)

# Build sort-stage data from a named list of 3D vertex matrices (cube space),
# mirroring the columns coord_3d()$transform hands to sort_by_depth()
poly_data <- function(polys, proj = proj_persp) {
      d <- do.call(rbind, lapply(names(polys), function(nm) {
            m <- polys[[nm]]
            data.frame(x = m[, 1], y = m[, 2], z = m[, 3], group = nm,
                       .prim = "polygon", fill = "grey", colour = NA,
                       linewidth = 0.5, alpha = 1, stringsAsFactors = FALSE)
      }))
      tr <- ggcube:::transform_3d_standard(d[c("x", "y", "z")], proj)
      tr <- ggcube:::scale_to_npc_coordinates(tr, plot_bounds = bounds)
      out <- cbind(tr[c("x", "y", "z", "depth", "depth_scale")], d[setdiff(names(d), c("x", "y", "z"))])
      out$.x3d <- d$x; out$.y3d <- d$y; out$.z3d <- d$z
      out
}

square <- function(center, u, v, half = 0.2) {
      corners <- rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1)) * half
      t(apply(corners, 1, function(cc) center + cc[1] * u + cc[2] * v))
}

piece_pts <- function(d, g) {
      r <- d$group == g
      cbind(d$.x3d[r], d$.y3d[r], d$.z3d[r])
}

poly_area <- function(pts) ggcube:::polygon_plane(pts)$area

# Screen-space group matrix in the layout pw_render_order() compares
# (x, y, depth, then the six bounding-box values repeated per row)
grp_mat <- function(d, g) {
      r <- d$group == g
      gx <- d$x[r]; gy <- d$y[r]; gz <- d$depth[r]
      bb <- c(min(gx), max(gx), min(gy), max(gy), min(gz), max(gz))
      cbind(x = gx, y = gy, z = gz, matrix(bb, nrow = sum(r), ncol = 6, byrow = TRUE))
}


test_that("crossing polygons: the smaller is cut into pieces on either side of the larger's plane", {
      big <- square(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), half = 0.3)            # z = 0 plane
      small <- square(c(0, 0, 0), c(1, 0, 0), c(0, 0.6, 0.8), half = 0.2)      # tilted through it
      d <- poly_data(list(big = big, small = small))

      out <- ggcube:::split_intersecting_polygons(d, proj_persp, bounds)

      expect_setequal(unique(out$group), c("big", "small__cut1", "small__cut2"))

      pl <- ggcube:::polygon_plane(big)
      sides <- vapply(c("small__cut1", "small__cut2"), function(g) {
            dist <- piece_pts(out, g) %*% pl$normal - pl$offset
            expect_true(all(dist >= -1e-9) || all(dist <= 1e-9))
            sign(sum(dist))
      }, numeric(1))
      expect_setequal(sides, c(-1, 1))

      # Pieces tile the original polygon
      expect_equal(poly_area(piece_pts(out, "small__cut1")) +
                         poly_area(piece_pts(out, "small__cut2")),
                   poly_area(small), tolerance = 1e-9)

      # Exactly one cut edge per piece, along the larger polygon's plane
      for (g in c("small__cut1", "small__cut2")) {
            expect_equal(sum(out$.cut_edge[out$group == g]), 1)
      }
      expect_false(any(out$.cut_edge[out$group == "big"]))
})


test_that("new vertices are projected exactly as coord_3d would project them", {
      big <- square(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), half = 0.3)
      small <- square(c(0.05, 0, 0), c(1, 0, 0), c(0, 0.6, 0.8), half = 0.2)
      d <- poly_data(list(big = big, small = small))
      out <- ggcube:::split_intersecting_polygons(d, proj_persp, bounds)

      expected <- poly_data(list(all = cbind(out$.x3d, out$.y3d, out$.z3d)))
      expect_equal(out$x, expected$x, tolerance = 1e-12)
      expect_equal(out$y, expected$y, tolerance = 1e-12)
      expect_equal(out$depth, expected$depth, tolerance = 1e-12)
      expect_equal(out$depth_scale, expected$depth_scale, tolerance = 1e-12)
})


test_that("adjacent tiles sharing an edge are not split, even when one straddles the other's plane", {
      # Saddle-like neighbours: the warped quad b has vertices on both sides of
      # a's plane, but a only touches b along the shared edge
      a <- rbind(c(0, 0, 0), c(0.2, 0, 0), c(0, 0.2, 0))
      b <- rbind(c(0.2, 0, 0), c(0.3, 0.1, 0.1), c(0.1, 0.3, -0.1), c(0, 0.2, 0))
      pa <- ggcube:::polygon_plane(a)
      expect_true(any(b %*% pa$normal - pa$offset > 0.05) &&
                        any(b %*% pa$normal - pa$offset < -0.05))

      # A triangle touching a square's plane at a single vertex
      c1 <- square(c(0.5, 0.5, 0), c(1, 0, 0), c(0, 1, 0), half = 0.1)
      c2 <- rbind(c(0.55, 0.55, 0), c(0.7, 0.6, 0.1), c(0.6, 0.7, 0.1))
      d <- poly_data(list(a = a, b = b, c1 = c1, c2 = c2))

      expect_identical(ggcube:::split_intersecting_polygons(d, proj_persp, bounds), d)
})


test_that("polygons that straddle each other's planes but miss each other are not split", {
      p1 <- square(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), half = 0.1)            # z = 0, x in [-0.1, 0.1]
      p2 <- rbind(c(-0.05, 0, 0.2), c(0.3, 0, -0.1), c(0.4, 0, 0.1))           # y = 0, crosses z = 0 at x in [0.18, 0.35]
      pl1 <- ggcube:::polygon_plane(p1)
      pl2 <- ggcube:::polygon_plane(p2)
      expect_true(all(range(p2 %*% pl1$normal - pl1$offset) * c(-1, 1) > 0))
      expect_true(all(range(p1 %*% pl2$normal - pl2$offset) * c(-1, 1) > 0))
      expect_false(ggcube:::polygons_interpenetrate(p1, pl1, p2, pl2, 1e-6))

      d <- poly_data(list(p1 = p1, p2 = p2))
      expect_identical(ggcube:::split_intersecting_polygons(d, proj_persp, bounds), d)
})


test_that("a warped polygon is cut by a planar one, but two warped polygons are left alone", {
      warped <- rbind(c(-0.2, -0.2, -0.05), c(0.2, -0.2, 0.05), c(0.2, 0.2, -0.05), c(-0.2, 0.2, 0.05))
      expect_false(ggcube:::polygon_plane(warped)$planar)

      # Planar polygon tilted through the warped quad: only the planar one can
      # act as a cutter, so the warped one is cut
      flat <- square(c(0, 0, 0), c(1, 0, 0), c(0, 0.6, 0.8), half = 0.4)
      d <- poly_data(list(warped = warped, flat = flat))
      out <- ggcube:::split_intersecting_polygons(d, proj_persp, bounds)
      expect_setequal(unique(out$group), c("flat", "warped__cut1", "warped__cut2"))

      warped2 <- warped
      warped2[, 3] <- warped2[, 3] + c(0.1, -0.1, 0.1, -0.1)
      warped2 <- warped2[, c(1, 3, 2)]
      expect_false(ggcube:::polygon_plane(warped2)$planar)
      d2 <- poly_data(list(warped = warped, warped2 = warped2))
      expect_identical(ggcube:::split_intersecting_polygons(d2, proj_persp, bounds), d2)
})


test_that("eye_in_cube_space locates the viewer at zero perspective depth", {
      eye <- ggcube:::eye_in_cube_space(proj_persp)
      tr <- ggcube:::transform_3d_standard(data.frame(x = eye$v[1], y = eye$v[2], z = eye$v[3]),
                                           proj_persp)
      expect_equal(tr$depth, 0, tolerance = 1e-12)

      dir <- ggcube:::eye_in_cube_space(proj_ortho)$v
      tr0 <- ggcube:::transform_3d_standard(data.frame(x = 0, y = 0, z = 0), proj_ortho)
      tr1 <- ggcube:::transform_3d_standard(data.frame(x = dir[1], y = dir[2], z = dir[3]), proj_ortho)
      expect_lt(tr1$depth, tr0$depth)
})


test_that("plane_side_order matches projected depths along shared lines of sight", {
      for (proj in list(proj_persp, proj_ortho)) {
            eye <- ggcube:::eye_in_cube_space(proj)
            base <- square(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), half = 0.4)
            for (offset in c(-0.15, 0.15)) {
                  small <- square(c(0.05, -0.05, offset), c(1, 0, 0), c(0, 0.8, 0.6), half = 0.08)
                  gi <- list(pts = base, plane = ggcube:::polygon_plane(base))
                  gj <- list(pts = small, plane = ggcube:::polygon_plane(small))
                  ord <- ggcube:::plane_side_order(gi, gj, eye)

                  # Ground truth: along the ray from the eye through the small
                  # polygon's centre, which surface is reached first?
                  ctr <- colMeans(small)
                  dirn <- if (proj$persp) ctr - eye$v else -eye$v
                  t_plane <- (gi$plane$offset - sum(gi$plane$normal * ctr)) / sum(gi$plane$normal * dirn)
                  hit <- ctr + t_plane * dirn
                  d_small <- ggcube:::transform_3d_standard(data.frame(x = ctr[1], y = ctr[2], z = ctr[3]), proj)$depth
                  d_plane <- ggcube:::transform_3d_standard(data.frame(x = hit[1], y = hit[2], z = hit[3]), proj)$depth
                  truth <- if (d_plane > d_small) 1 else -1     # 1 = base (i) behind small (j)

                  expect_equal(ord, truth)
                  expect_equal(ggcube:::plane_side_order(gj, gi, eye), -truth)
            }
      }
})


test_that("plane_side_order is undecided for coplanar polygons", {
      eye <- ggcube:::eye_in_cube_space(proj_persp)
      a <- square(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), half = 0.2)
      b <- square(c(0.1, 0.1, 0), c(1, 0, 0), c(0, 1, 0), half = 0.2)
      expect_true(is.na(ggcube:::plane_side_order(
            list(pts = a, plane = ggcube:::polygon_plane(a)),
            list(pts = b, plane = ggcube:::polygon_plane(b)), eye)))
})


test_that("pairwise ordering of a plane crossing a curved surface is correct under perspective", {
      # Saddle surface and a tilted plane spanning the cube. Under perspective,
      # the sampled screen-space depth fit is too coarse to order the plane
      # against thin pieces cut from the tiles; the exact plane-side test is not.
      n <- 8; s <- seq(-0.5, 0.5, length.out = n + 1)
      g <- expand.grid(i = seq_len(n), j = seq_len(n))
      tiles <- lapply(seq_len(nrow(g)), function(k) {
            x <- s[g$i[k] + c(0, 1, 1, 0)]; y <- s[g$j[k] + c(0, 0, 1, 1)]
            cbind(x, y, 2 * x^2 - 2 * y^2)
      })
      names(tiles) <- paste0("t", seq_along(tiles))
      px <- c(-0.5, 0.5, 0.5, -0.5); py <- c(-0.5, -0.5, 0.5, 0.5)
      plane <- cbind(px, py, 0.42 * py)
      d <- poly_data(c(list(plane = plane), tiles))
      d$.vertex_order <- ave(seq_len(nrow(d)), d$group, FUN = seq_along)

      res <- ggcube:::pw_render_order(d, "depth", proj_persp, bounds)
      out <- res$data
      ord <- unique(out$group[order(res$render_order)])
      expect_gt(length(ord), length(tiles) + 1)                           # something was cut

      eye <- ggcube:::eye_in_cube_space(proj_persp)
      pl <- ggcube:::polygon_plane(plane)
      eye_side <- sign(sum(pl$normal * eye$v) - pl$offset)
      plane_groups <- unique(out$group[startsWith(out$group, "plane")])
      expect_length(plane_groups, 1)
      pos <- setNames(seq_along(ord), ord)
      plane_m <- grp_mat(out, plane_groups)

      n_checked <- 0
      for (g in setdiff(ord, plane_groups)) {
            dist <- piece_pts(out, g) %*% pl$normal - pl$offset
            side <- if (all(dist >= -1e-9)) 1 else if (all(dist <= 1e-9)) -1 else NA
            expect_false(is.na(side))
            # Only pairs overlapping on screen are constrained
            ov <- ggcube:::pw_compare_pair(plane_m, grp_mat(out, g), "polygon", "polygon")
            if (is.na(ov)) next
            n_checked <- n_checked + 1
            in_front <- side == eye_side
            expect_equal(pos[[g]] > pos[[plane_groups]], in_front, info = g)
      }
      expect_gt(n_checked, 20)
})


test_that("break_cycles does not warn for a single group", {
      expect_no_warning(ggcube:::break_cycles(matrix(NA_real_, 1, 1)))
})


test_that("break_cycles removes unlocked constraints before locked ones", {
      # 3-cycle: 1 behind 2, 2 behind 3, 3 behind 1. The locked edge is the
      # weakest, so it would be removed first without locking.
      delta <- matrix(NA_real_, 3, 3)
      delta[1, 2] <- 0.01; delta[2, 1] <- -0.01
      delta[2, 3] <- 0.5;  delta[3, 2] <- -0.5
      delta[3, 1] <- 0.2;  delta[1, 3] <- -0.2
      locked <- matrix(FALSE, 3, 3)
      locked[1, 2] <- locked[2, 1] <- TRUE

      pruned <- ggcube:::break_cycles(delta, locked)
      expect_equal(pruned[1, 2], 0.01)
      expect_true(is.na(pruned[3, 1]))
})


test_that("cut pieces are outlined along original edges only", {
      ch <- ggcube:::edge_chains(c(0, 1, 1, 0), c(0, 0, 1, 1), keep = c(TRUE, TRUE, FALSE, TRUE))
      # Edges 4 (v4->v1), 1 (v1->v2), 2 (v2->v3) form one chain; edge 3 is cut
      expect_equal(ch$x, c(0, 0, 1, 1))
      expect_equal(ch$y, c(1, 0, 0, 1))
      expect_equal(unique(ch$id), 1L)

      closed <- ggcube:::edge_chains(c(0, 1, 1), c(0, 0, 1), keep = rep(TRUE, 3))
      expect_equal(closed$x, c(0, 1, 1, 0))

      d <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), group = 1, fill = "red",
                      colour = "black", linewidth = 0.5, alpha = 1, linetype = 1)
      g <- ggcube:::cut_polygon_grob(d, flag = c(FALSE, FALSE, TRUE, FALSE))
      outline <- Filter(function(ch) inherits(ch, "polyline") && identical(ch$gp$col, "black"),
                        g$children)
      expect_length(outline, 1)
      expect_equal(as.numeric(outline[[1]]$x), c(0, 0, 1, 1))
      expect_true(is.na(Filter(function(ch) inherits(ch, "polygon"), g$children)[[1]]$gp$col))
})


test_that("intersecting planes in one layer split and sort without warnings", {
      planes <- list(P1 = c(0.5, 0.5, 0), P2 = c(0, 0, -0.5), P3 = c(0.25, 0.4, 0.4))
      d <- do.call(rbind, Map(function(b, id) {
            x <- c(-2, 2, 2, -2); y <- c(-2, -2, 2, 2)
            data.frame(x = x, y = y, z = b[1] + b[2] * x + b[3] * y, group = id)
      }, planes, names(planes)))
      p <- ggplot(d, aes(x, y, z = z, group = group, fill = group)) +
            geom_polygon_3d(sort_method = "pairwise", colour = "black") +
            coord_3d(scales = "fixed")

      b <- ggplot_build(p)
      ld <- b$data[[1]]
      ld$.sort_method <- "pairwise"
      ld$.prim <- "polygon"
      expect_no_warning(tr <- b$plot$coordinates$transform(ld, b$layout$panel_params[[1]]))
      expect_gt(length(unique(tr$group)), 3)
      expect_true(any(tr$.cut_edge))
      expect_false(any(c(".x3d", ".y3d", ".z3d") %in% names(tr)))
      expect_no_warning(ggplotGrob(p))
})
