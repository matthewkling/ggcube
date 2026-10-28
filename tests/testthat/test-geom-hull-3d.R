# Merging coplanar triangles ----------------------------------------------------

test_that("coplanar hull triangles merge into faces with outward winding", {
      skip_if_not_installed("geometry")
      corners <- as.matrix(expand.grid(x = 0:1, y = 0:1, z = 0:1))
      A <- matrix(c(1, 0, 1, 0, 2, 0, 1, 0, 2), 3, 3)
      box <- corners %*% t(A)
      tri <- geometry::convhulln(box)
      oriented <- orient_triangles(box, tri, NULL)
      faces <- merge_coplanar_triangles(box, oriented$tri, oriented$normals)

      expect_length(faces$vertices, 6)
      expect_true(all(lengths(faces$vertices) == 4))
      expect_false(any(faces$singular))
      for (k in seq_along(faces$vertices)) {
            v <- box[faces$vertices[[k]], ]
            n <- faces$normals[k, ]
            # Planar, and wound counter-clockwise seen from outside (Newell normal agrees)
            expect_equal(as.vector(v %*% n), rep(sum(v[1, ] * n), 4), tolerance = 1e-10)
            nxt <- c(2:4, 1)
            newell <- c(sum((v[, 2] - v[nxt, 2]) * (v[, 3] + v[nxt, 3])),
                        sum((v[, 3] - v[nxt, 3]) * (v[, 1] + v[nxt, 1])),
                        sum((v[, 1] - v[nxt, 1]) * (v[, 2] + v[nxt, 2])))
            expect_gt(sum(newell * n), 0)
      }

      # Triangles that may not be merged stay separate
      kept <- merge_coplanar_triangles(box, oriented$tri, oriented$normals,
                                       mergeable = rep(FALSE, nrow(tri)))
      expect_length(kept$vertices, nrow(tri))
      expect_true(all(kept$singular))
})


test_that("hulls without coplanar triangles are unchanged", {
      skip_if_not_installed("geometry")
      set.seed(2)
      pts <- matrix(rnorm(60), ncol = 3)
      tri <- geometry::convhulln(pts)
      oriented <- orient_triangles(pts, tri, NULL)
      faces <- merge_coplanar_triangles(pts, oriented$tri, oriented$normals)
      expect_length(faces$vertices, nrow(tri))
      expect_equal(do.call(rbind, faces$vertices), oriented$tri, ignore_attr = TRUE)
})


test_that("a coplanar region with a hole isn't merged", {
      xy <- rbind(c(0, 0), c(3, 0), c(3, 3), c(0, 3), c(1, 1), c(2, 1), c(2, 2), c(1, 2))
      tri <- rbind(c(1, 2, 6), c(1, 6, 5), c(2, 3, 7), c(2, 7, 6),
                   c(3, 4, 8), c(3, 8, 7), c(4, 1, 5), c(4, 5, 8))
      expect_null(boundary_loop(tri))
      coords <- cbind(xy, 0)
      normals <- matrix(c(0, 0, 1), nrow(tri), 3, byrow = TRUE)
      faces <- merge_coplanar_triangles(coords, tri, normals)
      expect_length(faces$vertices, nrow(tri))
})


test_that("the hull of a box's corners is drawn with square faces", {
      skip_if_not_installed("geometry")
      d <- expand.grid(x = 0:1, y = c(0, 2), z = c(0, 5))
      b <- ggplot_build(ggplot(d, aes(x, y, z)) + geom_hull_3d() + coord_3d())
      sizes <- table(b$data[[1]]$group)
      expect_length(sizes, 6)
      expect_true(all(sizes == 4))
})


# Groups that can't enclose a volume ---------------------------------------------

test_that("groups that can't enclose a volume are dropped, not the whole layer", {
      skip_if_not_installed("geometry")

      # A z limit leaves virginica with a single point
      p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, Petal.Length, group = Species)) +
            geom_hull_3d() + zlim(NA, 4.5) + coord_3d()
      warnings <- character(0)
      d <- withCallingHandlers(layer_data(p), warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
      })
      expect_true(any(grepl("Dropped 1 group with too few points", warnings)))
      expect_false(any(grepl("Computation failed", warnings)))

      # The other two groups are hulled as without the dropped group
      kept <- iris[iris$Species != "virginica" & iris$Petal.Length <= 4.5, ]
      alone <- suppressWarnings(layer_data(
            ggplot(kept, aes(Sepal.Length, Sepal.Width, Petal.Length, group = Species)) +
                  geom_hull_3d() + zlim(NA, 4.5) + coord_3d()))
      expect_equal(nrow(d), nrow(alone))
      expect_equal(sort(d$z), sort(alone$z))
})

test_that("a group needs four points not all in one plane", {
      skip_if_not_installed("geometry")
      tetra <- data.frame(x = c(0, 1, 0, 0), y = c(0, 0, 1, 0), z = c(0, 0, 0, 1), g = "tetra")
      flat <- data.frame(x = c(0, 1, 1, 0, 0.5), y = c(0, 0, 1, 1, 0.5), z = 0, g = "flat")
      few <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 1, 2), g = "few")

      p <- ggplot(rbind(tetra, flat, few), aes(x, y, z, group = g)) + geom_hull_3d() + coord_3d()
      expect_warning(d <- layer_data(p), "Dropped 2 groups")
      expect_equal(nrow(d), 12)
      expect_equal(length(unique(d$group)), 4)

      # Nothing is dropped, and no warning given, when every group can be hulled
      expect_no_warning(layer_data(ggplot(tetra, aes(x, y, z)) + geom_hull_3d() + coord_3d()))
})
