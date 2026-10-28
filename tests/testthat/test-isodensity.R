# Tests for isodensity surfaces. A kernel density estimate of a standard
# trivariate normal sample is close to a normal density with variance 1 + s^2
# on each axis, where s is the kernel standard deviation, so the surface
# enclosing a fraction p of its mass is a sphere of radius
# sqrt((1 + s^2) * qchisq(p, 3)).

radius <- function(x, y, z) sqrt(x^2 + y^2 + z^2)

normal_cloud <- function(n = 4000, seed = 1) {
      withr::with_seed(seed, data.frame(x = rnorm(n), y = rnorm(n), z = rnorm(n)))
}

isodensity_data <- function(data, mapping = aes(x, y, z), ...) {
      p <- ggplot(data, mapping) +
            geom_isodensity_3d(...) +
            coord_3d()
      layer_data(p)
}

# The stat's output for one group, without building a plot, so that its errors
# and warnings reach the caller directly
isodensity_stat <- function(data, ...) {
      data$group <- 1
      StatIsodensity3D$compute_group(data, scales = list(), ...)
}


test_that("surfaces of a normal sample's density are spheres of the expected radius", {
      skip_if_not_installed("rmarchingcubes")
      cloud <- normal_cloud()
      probs <- c(0.25, 0.5, 0.8)

      # Bandwidths are four kernel standard deviations
      for (h in c(2, 4)) {
            s <- h / 4
            d <- isodensity_data(cloud, probs = probs, h = h)
            for (p in probs) {
                  at <- d[d$prob == p, ]
                  expect_equal(median(radius(at$x, at$y, at$z)),
                               sqrt((1 + s^2) * qchisq(p, 3)),
                               tolerance = 0.04, info = paste("h =", h, "p =", p))
            }
      }
})


test_that("each fraction gives its own surface, at a lower density for a larger fraction", {
      skip_if_not_installed("rmarchingcubes")
      d <- isodensity_data(normal_cloud(1000), probs = c(0.2, 0.6, 0.9))
      expect_setequal(unique(d$prob), c(0.2, 0.6, 0.9))
      levels <- tapply(d$level, d$prob, unique)
      expect_true(all(lengths(levels) == 1))
      expect_true(all(diff(unlist(levels)) < 0))
      expect_true(all(tapply(d$prob, d$group, function(v) length(unique(v))) == 1))

      expect_equal(unique(isodensity_data(normal_cloud(1000))$prob), 0.5)
})


test_that("surfaces close, rather than being cut by the edge of the grid", {
      skip_if_not_installed("rmarchingcubes")
      # The outermost surface reaches beyond the data in the direction of a
      # point far from the rest, but closes nonetheless
      cloud <- rbind(normal_cloud(300), data.frame(x = 4, y = 0, z = 0))
      d <- isodensity_data(cloud, probs = 0.99)

      # On a closed surface, every edge is shared by exactly two triangles
      key <- function(v) paste(round(v, 8), collapse = ",")
      edges <- unlist(lapply(split(d[, c("x", "y", "z")], d$group), function(tri) {
            v <- vapply(1:3, function(i) key(unlist(tri[i, ])), character(1))
            c(paste(sort(v[1:2]), collapse = "|"), paste(sort(v[2:3]), collapse = "|"),
              paste(sort(v[c(1, 3)]), collapse = "|"))
      }))
      expect_true(all(table(edges) == 2))
})


test_that("density levels enclose the given fractions of mass", {
      values <- array(c(4, 3, 2, 1), c(2, 2, 1))
      # Cumulative mass, densest first: 0.4, 0.7, 0.9, 1
      expect_equal(isodensity_levels(values, c(0.3, 0.4, 0.5, 0.95)), c(4, 4, 3, 1))
})


test_that("default bandwidths follow the normal reference rule for three dimensions", {
      cloud <- normal_cloud(500)
      s <- vapply(cloud, function(v) min(sd(v), IQR(v) / 1.34), numeric(1))
      expect_equal(isodensity_bandwidth(cloud),
                   unname(4 * (4 / 5)^(1 / 7) * s * 500^(-1 / 7)))

      # `adjust` multiplies bandwidths, given or estimated
      expect_equal(isodensity_bandwidth(cloud, adjust = c(1, 2, 3)),
                   isodensity_bandwidth(cloud) * c(1, 2, 3))
      expect_equal(isodensity_bandwidth(cloud, h = 1, adjust = 2), c(2, 2, 2))
})


test_that("weights shift the density toward heavier points", {
      skip_if_not_installed("rmarchingcubes")
      left <- normal_cloud(300, seed = 1)
      left$x <- left$x / 2 - 3
      right <- normal_cloud(300, seed = 2)
      right$x <- right$x / 2 + 3
      two <- rbind(left, right)

      # Equally weighted, the densest half spans both clusters
      d <- isodensity_data(two)
      expect_true(any(d$x < 0) && any(d$x > 0))

      # Weighted toward the left cluster, the densest half lies within it
      two$w <- rep(c(9, 1), each = 300)
      d <- isodensity_data(two, aes(x, y, z, weight = w))
      expect_true(all(d$x < 0))
})


test_that("each group gets its own density", {
      skip_if_not_installed("rmarchingcubes")
      a <- normal_cloud(500, seed = 1)
      b <- normal_cloud(500, seed = 2)
      b$x <- b$x + 10
      two <- rbind(cbind(a, g = "a"), cbind(b, g = "b"))

      d <- isodensity_data(two, aes(x, y, z, fill = g))
      centers <- tapply(d$x, d$fill, mean)
      expect_length(centers, 2)
      expect_equal(sort(as.vector(centers)), c(0, 10), tolerance = 0.1)
})


test_that("invalid parameters and data are reported", {
      skip_if_not_installed("rmarchingcubes")
      cloud <- normal_cloud(100)

      expect_error(isodensity_stat(cloud, probs = 1), "`probs`")
      expect_error(isodensity_stat(cloud, probs = c(0.5, NA)), "`probs`")
      expect_error(isodensity_stat(cloud, n = 1), "`n`")
      expect_error(isodensity_stat(cloud, h = -1), "`h`")
      expect_error(isodensity_stat(cloud, h = c(1, 2)), "`h`")
      expect_error(isodensity_stat(cloud, adjust = 0), "`adjust`")

      flat <- cloud
      flat$z <- 0
      expect_error(isodensity_stat(flat), "variation")

      expect_warning(d <- isodensity_stat(cloud[1, ]), "at least two points")
      expect_equal(nrow(d), 0)
})


test_that("a missing rmarchingcubes is reported", {
      local_mocked_bindings(isosurface_engine_available = function() FALSE)
      expect_error(isodensity_stat(normal_cloud(100)), "rmarchingcubes")
})


test_that("scale limits cut surfaces without changing the mass they enclose", {
      skip_if_not_installed("rmarchingcubes")
      cloud <- normal_cloud(2000)
      # The surface around 99% of the mass reaches beyond the data, so limits at
      # the data's range cut it without removing any points
      base <- ggplot(cloud, aes(x, y, z)) + geom_isodensity_3d(probs = 0.99) + coord_3d()
      free <- layer_data(base)

      for (axis in c("x", "y", "z")) {
            lim <- range(cloud[[axis]])
            limit <- switch(axis, x = xlim(lim), y = ylim(lim), z = zlim(lim[1], lim[2]))
            d <- layer_data(base + limit)
            v <- d[[axis]]
            expect_false(anyNA(v), info = axis)
            expect_true(all(v >= lim[1] - 1e-9 & v <= lim[2] + 1e-9), info = axis)
            expect_true(any(abs(v - lim[1]) < 1e-9) && any(abs(v - lim[2]) < 1e-9), info = axis)
            expect_equal(unique(d$level), unique(free$level), info = axis)
      }

      # A limit on one side only removes the points beyond it, as x and y limits
      # do, and cuts the surface there. The density is evaluated on a grid over
      # the remaining range, so the far side is nearly unchanged.
      expect_warning(d <- layer_data(base + scale_z_continuous(limits = c(0, NA))),
                     "Removed \\d+ rows")
      expect_equal(min(d$z), 0)
      expect_equal(max(d$z), max(free$z), tolerance = 0.05)
})


test_that("caps close surfaces at limits without changing the mass they enclose", {
      skip_if_not_installed("rmarchingcubes")
      cloud <- normal_cloud(2000)
      lim <- range(cloud$z)
      base <- ggplot(cloud, aes(x, y, z)) + coord_3d() + zlim(lim[1], lim[2])
      open <- layer_data(base + geom_isodensity_3d(probs = 0.99))
      d <- layer_data(base + geom_isodensity_3d(probs = 0.99, cap = TRUE))

      expect_equal(d[!d$cap, c("x", "y", "z")], open[, c("x", "y", "z")], ignore_attr = TRUE)
      expect_equal(unique(d$level), unique(open$level))

      # A cap at each limit, in its plane, facing outward
      caps <- d[d$cap, ]
      expect_true(all(caps$z == lim[1] | caps$z == lim[2]))
      expect_true(all(caps$normal_z[caps$z == lim[1]] == -1))
      expect_true(all(caps$normal_z[caps$z == lim[2]] == 1))
      expect_true(any(caps$z == lim[1]) && any(caps$z == lim[2]))

      # No caps where nothing is cut
      expect_false(any(layer_data(ggplot(cloud, aes(x, y, z)) + coord_3d() +
                                        geom_isodensity_3d(probs = 0.99, cap = TRUE))$cap))
})


test_that("every triangle lies within a single grid cell", {
      skip_if_not_installed("rmarchingcubes")
      # Densities this small once led marching cubes to place vertices on edges
      # the surface doesn't cross, drawing shards
      q <- data.frame(x = quakes$long, y = quakes$lat, z = -quakes$depth)
      d <- layer_data(ggplot(q, aes(x, y, z)) +
                            geom_isodensity_3d(probs = c(0.1, 0.5, 0.9)) +
                            coord_3d())
      ranges <- isodensity_ranges(q, isodensity_bandwidth(q))
      cell <- vapply(ranges, diff, numeric(1)) / 29
      extent <- t(vapply(split(d[, c("x", "y", "z")], d$group),
                         function(v) vapply(v, function(a) diff(range(a)), numeric(1)),
                         numeric(3)))
      expect_true(all(sweep(extent, 2, cell * (1 + 1e-9), "<=")))
})
