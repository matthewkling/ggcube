# Tests for isosurfaces, checked analytically against fields whose isosurfaces
# are known spheres.

# A field on a grid, as a data frame
field_grid <- function(f, x, y = x, z = x) {
      g <- expand.grid(x = x, y = y, z = z)
      g$value <- f(g$x, g$y, g$z)
      g
}

radius <- function(x, y, z) sqrt(x^2 + y^2 + z^2)

iso_data <- function(data, ...) {
      p <- ggplot(data, aes(x, y, z, value = value)) +
            geom_isosurface_3d(...) +
            coord_3d()
      layer_data(p)
}

# The stat's output for one group, without building a plot, so that its errors
# and warnings reach the caller directly
iso_stat <- function(data, levels = NULL, ...) {
      data$group <- 1
      StatIsosurface3D$compute_group(data, scales = list(), levels = levels, ...)
}

# Triangle vertices, as one three-row data frame per triangle
triangles <- function(d) {
      split(d[, c("x", "y", "z")], d$group)
}


test_that("isosurfaces of a radial field are spheres of the right radius", {
      skip_if_not_installed("rmarchingcubes")
      d <- iso_data(field_grid(radius, seq(-1.5, 1.5, length.out = 25)), levels = 1)
      expect_true(all(table(d$group) == 3))
      expect_equal(radius(d$x, d$y, d$z), rep(1, nrow(d)), tolerance = 0.01)
})


test_that("isosurfaces are placed correctly on unevenly spaced grids", {
      skip_if_not_installed("rmarchingcubes")
      # Spacing coarse near the edges and fine near the center
      s <- sign(seq(-1, 1, length.out = 25)) * abs(seq(-1, 1, length.out = 25))^1.5 * 1.5
      d <- iso_data(field_grid(radius, s, s * 0.9, s * 1.1), levels = 1)
      expect_equal(radius(d$x, d$y, d$z), rep(1, nrow(d)), tolerance = 0.02)
})


test_that("normals and winding face away from the region above the level", {
      skip_if_not_installed("rmarchingcubes")
      s <- seq(-1.5, 1.5, length.out = 20)
      check <- function(d, outward) {
            tris <- triangles(d)
            centers <- t(vapply(tris, colMeans, numeric(3)))
            normals <- as.matrix(d[match(names(tris), d$group),
                                   c("normal_x", "normal_y", "normal_z")])
            expect_equal(sqrt(rowSums(normals^2)), rep(1, nrow(normals)), ignore_attr = TRUE)
            facing <- rowSums(normals * centers) > 0
            expect_true(all(facing == outward))

            # Vertices run counterclockwise seen from the side the normal faces,
            # as back-face detection assumes
            winding <- t(vapply(tris, function(v) {
                  v <- as.matrix(v)
                  e1 <- v[2, ] - v[1, ]
                  e2 <- v[3, ] - v[1, ]
                  c(e1[2] * e2[3] - e1[3] * e2[2],
                    e1[3] * e2[1] - e1[1] * e2[3],
                    e1[1] * e2[2] - e1[2] * e2[1])
            }, numeric(3)))
            expect_true(all(rowSums(winding * normals) > 0))
      }

      # High values inside the sphere: surfaces face outward
      blob <- field_grid(function(x, y, z) exp(-radius(x, y, z)^2), s)
      check(iso_data(blob, levels = exp(-1)), outward = TRUE)

      # High values outside the sphere: surfaces face inward
      bowl <- field_grid(radius, s)
      check(iso_data(bowl, levels = 1), outward = FALSE)
})


test_that("each level gives its own surface, and the default is the midpoint", {
      skip_if_not_installed("rmarchingcubes")
      g <- field_grid(radius, seq(-1.5, 1.5, length.out = 20))

      d <- iso_data(g, levels = c(0.5, 1))
      expect_setequal(unique(d$level), c(0.5, 1))
      for (lv in c(0.5, 1)) {
            at <- d[d$level == lv, ]
            expect_equal(radius(at$x, at$y, at$z), rep(lv, nrow(at)), tolerance = 0.02)
      }
      # Triangles of different levels are never grouped together
      expect_true(all(tapply(d$level, d$group, function(v) length(unique(v))) == 1))

      d <- iso_data(g)
      expect_equal(unique(d$level), mean(range(g$value)))
})


test_that("data that aren't a complete finite grid are an error", {
      skip_if_not_installed("rmarchingcubes")
      g <- field_grid(radius, seq(-1.5, 1.5, length.out = 8))

      expect_error(iso_stat(g[-5, ]), "complete grid")
      expect_error(iso_stat(g[-5, ]), "missing values are removed")
      expect_error(iso_stat(rbind(g, g[5, ])), "more than once")

      g_inf <- g
      g_inf$value[5] <- Inf
      expect_error(iso_stat(g_inf), "finite `value`")

      expect_error(iso_stat(g[g$z == g$z[1], ]), "two distinct values")
      expect_error(iso_stat(g, levels = NA), "`levels`")
})


test_that("levels with no surface give a warning", {
      skip_if_not_installed("rmarchingcubes")
      g <- field_grid(radius, seq(-1.5, 1.5, length.out = 8))
      expect_warning(d <- iso_stat(g, levels = 10), "No isosurface")
      expect_equal(nrow(d), 0)
})


test_that("a missing rmarchingcubes is reported", {
      local_mocked_bindings(isosurface_engine_available = function() FALSE)
      g <- field_grid(radius, seq(-1.5, 1.5, length.out = 8))
      expect_error(iso_stat(g), "rmarchingcubes")
})


test_that("outlines take the fill color unless a color is given or the fill is translucent", {
      skip_if_not_installed("rmarchingcubes")
      g <- field_grid(radius, seq(-1.5, 1.5, length.out = 8))
      gp <- function(...) {
            p <- ggplot(g, aes(x, y, z, value = value)) +
                  geom_isosurface_3d(levels = 1, light = "none", ...) +
                  coord_3d()
            poly <- find_grobs(find_grobs(drawn_grob(p), "ggcube_layer")[[1]], "polygon")[[1]]
            poly$gp
      }

      fixed <- gp(fill = "red")
      expect_true(all(fixed$col == fixed$fill))

      mapped <- gp(aes(fill = after_stat(level)))
      expect_true(all(mapped$col == mapped$fill))

      expect_true(all(gp(fill = "red", color = "black")$col == "black"))

      translucent <- gp(fill = "red", alpha = 0.5)
      expect_true(all(is.na(translucent$col) | translucent$col == "transparent"))
})


test_that("isosurfaces are computed separately for each panel", {
      skip_if_not_installed("rmarchingcubes")
      s <- seq(-1.5, 1.5, length.out = 12)
      g <- rbind(cbind(field_grid(radius, s), f = "a"),
                 cbind(field_grid(function(x, y, z) radius(x, y, z) * 2, s), f = "b"))
      p <- ggplot(g, aes(x, y, z, value = value)) +
            geom_isosurface_3d(levels = 1) +
            facet_wrap(~f) +
            coord_3d()
      d <- layer_data(p)
      r <- radius(d$x, d$y, d$z)
      expect_equal(as.vector(tapply(r, d$PANEL, mean)), c(1, 0.5), tolerance = 0.03)
})


test_that("scale limits cut surfaces at the last grid points within them", {
      skip_if_not_installed("rmarchingcubes")
      s <- seq(-1.5, 1.5, length.out = 13)
      g <- field_grid(radius, s)
      base <- ggplot(g, aes(x, y, z, value = value)) + geom_isosurface_3d() + coord_3d()
      free <- layer_data(base)

      d <- layer_data(base + zlim(-0.6, NA))
      expect_false(anyNA(d$z))
      expect_equal(min(d$z), min(s[s >= -0.6]))
      expect_equal(max(d$z), max(free$z), tolerance = 1e-6)

      # The default level comes from the whole grid
      expect_equal(unique(d$level), unique(free$level))

      # Rows outside x limits are removed by ggplot2 before the stat
      d <- suppressWarnings(layer_data(base + xlim(-0.6, NA)))
      expect_false(anyNA(d$x))
      expect_equal(min(d$x), min(s[s >= -0.6]))
})


# Vertex and edge keys, rounded so that vertices computed by marching cubes and
# by marching squares at the same point match
vertex_key <- function(x, y, z) paste(signif(x, 8), signif(y, 8), signif(z, 8), sep = ",")
edge_key <- function(a, b) ifelse(a < b, paste(a, b, sep = "|"), paste(b, a, sep = "|"))

# Edges of surface triangles that belong to only one of them: the rims the
# surfaces leave where they're cut
open_edges <- function(d) {
      tri <- d[!d$cap, ]
      v <- matrix(vertex_key(tri$x, tri$y, tri$z), ncol = 3, byrow = TRUE)
      edges <- table(c(edge_key(v[, 1], v[, 2]), edge_key(v[, 2], v[, 3]), edge_key(v[, 1], v[, 3])))
      names(edges)[edges == 1]
}

# Edges of cap rings
ring_edges <- function(d) {
      caps <- d[d$cap, ]
      unlist(lapply(split(caps, caps$group), function(r) {
            v <- vertex_key(r$x, r$y, r$z)
            edge_key(v, c(v[-1], v[1]))
      }), use.names = FALSE)
}

# Number of polygons, given by `group`, that contain each point
polygons_containing <- function(px, py, polys, u, v) {
      counts <- rep(0, length(px))
      for (poly in split(polys, polys$group)) {
            x <- poly[[u]]
            y <- poly[[v]]
            x2 <- c(x[-1], x[1])
            y2 <- c(y[-1], y[1])
            inside <- rep(FALSE, length(px))
            for (i in seq_along(x)) {
                  crosses <- (y[i] > py) != (y2[i] > py) &
                        px < (x2[i] - x[i]) * (py - y[i]) / (y2[i] - y[i]) + x[i]
                  inside <- xor(inside, crosses)
            }
            counts <- counts + inside
      }
      counts
}

blob_field <- function(x, y, z) exp(-radius(x, y, z)^2)


test_that("caps close surfaces cut by limits, in the plane of the cut", {
      skip_if_not_installed("rmarchingcubes")
      s <- seq(-1.5, 1.5, length.out = 13)
      base <- ggplot(field_grid(blob_field, s), aes(x, y, z, value = value)) + coord_3d()
      lim <- min(s[s >= -0.6])

      open <- layer_data(base + geom_isosurface_3d(levels = exp(-1)) + zlim(-0.6, NA))
      expect_false(any(open$cap))
      expect_gt(length(open_edges(open)), 0)

      d <- layer_data(base + geom_isosurface_3d(levels = exp(-1), cap = TRUE) + zlim(-0.6, NA))

      # The surface is unchanged, and its rim is closed by the cap
      expect_equal(d[!d$cap, c("x", "y", "z")], open[, c("x", "y", "z")], ignore_attr = TRUE)
      expect_true(all(open_edges(d) %in% ring_edges(d)))

      # The cap lies in the plane of the cut, facing away from the enclosed region
      caps <- d[d$cap, ]
      expect_true(all(caps$z == lim))
      expect_equal(unique(caps$normal_z), -1)
      for (ring in split(caps, caps$group)) {
            expect_lt(newell_normal(as.matrix(ring[, c("x", "y", "z")]))[3], 0)
      }
})


test_that("caps of nested surfaces tile the cut, each covering its own band", {
      skip_if_not_installed("rmarchingcubes")
      s <- seq(-1.5, 1.5, length.out = 17)
      levels <- c(0.2, 0.5, 0.8)
      d <- layer_data(ggplot(field_grid(blob_field, s), aes(x, y, z, value = value)) +
                            geom_isosurface_3d(levels = levels, cap = TRUE) +
                            zlim(0, NA) + coord_3d())
      caps <- d[d$cap, ]
      expect_setequal(unique(caps$level), levels)

      # At points on the cut, the containing cap is that of the highest level
      # below the field there, and no point is covered twice
      pts <- expand.grid(x = seq(-1.4, 1.4, length.out = 41), y = seq(-1.4, 1.4, length.out = 41))
      field <- blob_field(pts$x, pts$y, 0)
      coverage <- sapply(levels, function(lv) {
            polygons_containing(pts$x, pts$y, caps[caps$level == lv, ], "x", "y")
      })
      expected <- vapply(field, function(f) if (f < min(levels)) NA else max(levels[levels <= f]), numeric(1))

      # Away from contours, where cut and grid can disagree within a cell
      clear <- vapply(field, function(f) min(abs(f - levels)) > 0.05, logical(1))
      expect_true(all(rowSums(coverage) <= 1))
      covered_level <- apply(coverage, 1, function(r) if (any(r > 0)) levels[r > 0] else NA)
      expect_identical(covered_level[clear], expected[clear])
})


test_that("caps close surfaces cut by the edge of the grid", {
      skip_if_not_installed("rmarchingcubes")
      s <- seq(-1.5, 1.5, length.out = 13)
      half <- field_grid(blob_field, s[s >= 0], s, s)
      d <- iso_data(half, levels = exp(-1), cap = TRUE)
      expect_true(all(open_edges(d) %in% ring_edges(d)))
      expect_true(all(d$x[d$cap] == 0))
      expect_equal(unique(d$normal_x[d$cap]), -1)

      # Surfaces that don't reach the edges get no caps
      whole <- field_grid(blob_field, s)
      expect_false(any(iso_data(whole, levels = exp(-1), cap = TRUE)$cap))

      expect_error(iso_stat(whole, cap = NA), "`cap`")
})


test_that("surfaces don't depend on the scale of the field", {
      skip_if_not_installed("rmarchingcubes")
      # Marching cubes tolerances are absolute, so values as small as densities
      # once misplaced surfaces
      g <- field_grid(blob_field, seq(-1.5, 1.5, length.out = 15))
      tiny <- transform(g, value = value * 1e-9)
      d <- iso_data(g, levels = c(0.3, 0.6))
      d_tiny <- iso_data(tiny, levels = c(0.3, 0.6) * 1e-9)
      expect_equal(d_tiny[, c("x", "y", "z")], d[, c("x", "y", "z")], tolerance = 1e-8)
})


test_that("`inside` chooses which side of each level surfaces enclose", {
      skip_if_not_installed("rmarchingcubes")
      s <- seq(-1.5, 1.5, length.out = 13)
      g <- field_grid(radius, s)
      half <- ggplot(g, aes(x, y, z, value = value)) + zlim(0, NA) + coord_3d()
      outward <- function(d) {
            tri <- d[!duplicated(d$group) & !d$cap, ]
            sign(tri$normal_x * tri$x + tri$normal_y * tri$y + tri$normal_z * tri$z)
      }

      # Radius below 1 is a ball: surfaces face outward, and it's capped at
      # the cut, facing down
      d <- layer_data(half + geom_isosurface_3d(levels = 1, inside = "below", cap = TRUE))
      expect_true(all(outward(d) == 1))
      expect_equal(unique(d$level), 1)
      expect_true(all(d$z[d$cap] == 0))
      expect_equal(unique(d$normal_z[d$cap]), -1)

      # Radius above 1 is the space around it, so surfaces face inward
      d <- layer_data(half + geom_isosurface_3d(levels = 1, cap = TRUE))
      expect_true(all(outward(d) == -1))

      # Enclosing below a level is enclosing above it in the negated field
      neg <- transform(g, value = -value)
      below <- iso_data(g, levels = c(0.5, 1), inside = "below", cap = TRUE)
      above <- iso_data(neg, levels = c(-0.5, -1), cap = TRUE)
      cols <- c("x", "y", "z", "normal_x", "normal_y", "normal_z", "cap")
      expect_equal(below[, cols], above[, cols], ignore_attr = TRUE)
      expect_equal(below$level, -above$level)

      expect_error(iso_stat(g, inside = "out"), "`inside`")
})


test_that("caps are split into polygons without holes", {
      skip_if_not_installed("rmarchingcubes")
      # Nested caps are rings around one another; each must be cut up
      s <- seq(-1.5, 1.5, length.out = 17)
      d <- layer_data(ggplot(field_grid(blob_field, s), aes(x, y, z, value = value)) +
                            geom_isosurface_3d(levels = c(0.2, 0.5, 0.8), cap = TRUE) +
                            zlim(0, NA) + coord_3d())
      caps <- d[d$cap, ]
      expect_false(".subgroup" %in% names(d))

      # A polygon with a hole would need a seam back to itself: some vertex
      # visited twice. Simple polygons visit each vertex once.
      repeats <- vapply(split(caps, caps$group), function(r) {
            anyDuplicated(paste(signif(r$x, 10), signif(r$y, 10)))
      }, numeric(1))
      expect_true(all(repeats == 0))

      # And every piece winds the same way, as seen from outside the cut
      for (r in split(caps, caps$group)) {
            expect_lt(newell_normal(as.matrix(r[, c("x", "y", "z")]))[3], 0)
      }
})
