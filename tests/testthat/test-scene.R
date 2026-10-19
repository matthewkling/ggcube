# Tests for sorting the primitives of several layers together as one scene.
#
# Orders are read from the drawn scene (ggcube:::validation_scene() captures
# each scene's rows in drawing order, with their layer and cube-space
# coordinates) and checked against exact 3D geometry.

scene_of <- function(p) {
      sc <- ggcube:::validation_scene(p)
      expect_gte(length(sc$panels), 1)
      sc$panels[[1]]
}

matlib_diagram <- function() {
      vecs <- data.frame(x = 0, y = 0, z = 0, xend = c(1, 0, 0.6), yend = c(0, 1, 0.6),
                         zend = c(0, 0, 1), v = c("a", "b", "c"))
      tri <- data.frame(x = c(1, 0, 0.6), y = c(0, 1, 0.6), z = c(0, 0, 1))
      labs <- transform(vecs, x = xend * 1.1, y = yend * 1.1, z = zend * 1.1)
      ggplot() +
            geom_polygon_3d(data = tri, aes(x, y, z), fill = "gold",
                            annotate = annotate_3d("plane", zintercept = 0.4, alpha = 0.5)) +
            geom_segment_3d(data = vecs, aes(x, y, z, xend = xend, yend = yend, zend = zend),
                            arrow = grid::arrow(type = "closed")) +
            geom_text_3d(data = labs, aes(x, y, z, label = v)) +
            coord_3d(light = "none", xlim = c(-0.1, 1.2), ylim = c(-0.1, 1.2), zlim = c(-0.1, 1.2))
}


test_that("primitives of different layers are depth-sorted together", {
      sc <- scene_of(matlib_diagram())
      rows <- sc$rows
      expect_setequal(unique(rows$.layer), 1:3)

      # The plane (an annotation in layer 1) cuts vector c (layer 2) where it
      # passes through z = 0.4: the piece below is drawn before the plane, the
      # piece above after it, as the viewer looks down on the plane
      plane_pos <- range(which(rows$.layer == 1 & rows$.ann %in% TRUE))
      # Vector c is the one segment along x = y that rises in z
      seg <- which(rows$.layer == 2)
      by_group <- split(seg, rows$group[seg])
      is_c <- vapply(by_group, function(i) all(abs(rows$.x3d[i] - rows$.y3d[i]) < 1e-9) &&
                           diff(range(rows$.z3d[i])) > 0, logical(1))
      pieces <- by_group[is_c]
      expect_length(pieces, 2)
      below <- pieces[[which.min(vapply(pieces, function(i) mean(rows$.z3d[i]), 1))]]
      above <- pieces[[which.max(vapply(pieces, function(i) mean(rows$.z3d[i]), 1))]]
      expect_lt(max(below), plane_pos[1])
      expect_gt(min(above), plane_pos[2])

      # Labels (layer 3) are interleaved with the other layers, not all drawn last
      expect_false(all(which(rows$.layer == 3) > max(which(rows$.layer != 3))) &&
                         all(diff(rows$.layer) >= 0))
})


test_that("each layer's primitives keep their own rendering settings", {
      g <- drawn_grob(matlib_diagram())
      scene <- find_grobs(g, "ggcube_scene")
      expect_length(scene, 1)
      segs <- find_grobs(scene[[1]], "segments")
      # The vector layer's arrow is drawn on the end pieces only
      with_arrow <- Filter(function(s) !is.null(s$arrow), segs)
      expect_gte(length(with_arrow), 1)
      expect_true(all(vapply(with_arrow, function(s) s$gp$col[1] != "transparent", logical(1))))
      # The polygon layer keeps its fill
      polys <- find_grobs(scene[[1]], "polygon")
      fills <- unlist(lapply(polys, function(p) p$gp$fill))
      expect_true(any(fills == "gold"))
})


test_that("runs of ggcube layers are merged into scenes, split by other grobs", {
      layer <- function(id) grid::gTree(id = id, cl = "ggcube_layer")
      grobs <- list(grid::rectGrob(), layer(1), grid::nullGrob(), layer(2),
                    grid::textGrob("x"), layer(3), grid::rectGrob())
      out <- ggcube:::group_scene_layers(grobs)
      cls <- vapply(out, function(g) class(g)[1], character(1))
      expect_equal(cls, c("rect", "ggcube_scene", "text", "ggcube_layer", "rect"))
      expect_equal(vapply(out[[2]]$layers, `[[`, 1, "id"), c(1, 2))
      expect_equal(out[[4]]$id, 3)
})


test_that("a layer with no data in a facet panel doesn't disturb that panel's scene", {
      set.seed(1)
      pts <- data.frame(x = runif(20), y = runif(20), z = runif(20), f = "a")
      sq <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 0, 1, 1))
      sq <- rbind(transform(sq, f = "a"), transform(sq, f = "b"))
      p <- ggplot(sq, aes(x, y, z)) +
            geom_polygon_3d(fill = "grey70") +
            geom_point_3d(data = pts, colour = "red") +
            facet_wrap(~f) + coord_3d()
      sc <- ggcube:::validation_scene(p)
      expect_length(sc$panels, 2)
      layers <- lapply(sc$panels, function(pn) sort(unique(pn$rows$.layer)))
      expect_true(any(vapply(layers, identical, logical(1), 1:2)))
      expect_true(any(vapply(layers, identical, logical(1), 1L)))
})


test_that("a plane layer occludes another layer's points by depth", {
      set.seed(2)
      pts <- data.frame(x = runif(200), y = runif(200), z = runif(200))
      plane <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = 0.5)
      p <- ggplot(pts, aes(x, y, z)) +
            geom_point_3d() +
            geom_polygon_3d(data = plane) +
            coord_3d()
      rows <- scene_of(p)$rows
      plane <- rows[rows$.layer == 2, ]
      plane_pos <- which(rows$.layer == 2)[1]
      # Points over the plane on screen, seen from above, are drawn after it
      # exactly when they're above it
      pt <- which(rows$.layer == 1)
      over <- ggcube:::points_in_polygons_cpp(rows$x[pt], rows$y[pt], rep(0L, length(pt)),
                                              plane$x, plane$y, c(0L, nrow(plane)))
      expect_gt(sum(over), 50)
      expect_equal(pt[over] > plane_pos, rows$.z3d[pt[over]] > 0)
})


test_that("layers' sort_method is deprecated", {
      lifecycle::expect_deprecated(geom_point_3d(sort_method = "pairwise"))
      lifecycle::expect_deprecated(geom_surface_3d(sort_method = "painter"))
      lifecycle::expect_deprecated(stat_density_3d(sort_method = "auto"))
      expect_silent(geom_point_3d())
})
