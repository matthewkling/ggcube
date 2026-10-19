# Annotations are supported by every ggcube layer function.

annotation_layer_plots <- function(ann) {
      set.seed(1)
      pts <- data.frame(x = rnorm(60), y = rnorm(60), z = rnorm(60), g = rep(1:3, 20))
      grid_df <- expand.grid(x = seq(-1, 1, length.out = 6), y = seq(-1, 1, length.out = 6))
      grid_df$z <- with(grid_df, x^2 - y^2)
      segs <- data.frame(x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1)
      poly <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 0, 1, 1), group = 1)
      txt <- data.frame(x = 0, y = 0, z = 0, label = "a")

      list(
            geom_point_3d = ggplot(pts, aes(x, y, z)) + geom_point_3d(annotate = ann),
            geom_segment_3d = ggplot(segs, aes(x, y, z, xend = xend, yend = yend, zend = zend)) +
                  geom_segment_3d(annotate = ann),
            geom_path_3d = ggplot(pts[1:10, ], aes(x, y, z)) + geom_path_3d(annotate = ann),
            geom_polygon_3d = ggplot(poly, aes(x, y, z, group = group)) + geom_polygon_3d(annotate = ann),
            geom_surface_3d = ggplot(grid_df, aes(x, y, z)) + geom_surface_3d(annotate = ann),
            geom_ridgeline_3d = ggplot(grid_df, aes(x, y, z)) + geom_ridgeline_3d(annotate = ann),
            geom_contour_3d = ggplot(grid_df, aes(x, y, z)) + geom_contour_3d(annotate = ann),
            geom_smooth_3d = ggplot(pts, aes(x, y, z)) + geom_smooth_3d(annotate = ann),
            geom_density_3d = ggplot(pts, aes(x, y)) + geom_density_3d(annotate = ann),
            geom_function_3d = ggplot() + geom_function_3d(fun = function(x, y) x * y, xlim = c(-1, 1),
                                                           ylim = c(-1, 1), annotate = ann),
            geom_distributions_3d = ggplot(pts, aes(x = x, y = g)) + geom_distributions_3d(annotate = ann),
            geom_hull_3d = ggplot(pts, aes(x, y, z)) + geom_hull_3d(annotate = ann),
            geom_voxel_3d = ggplot(data.frame(x = 1:3, y = 1:3, z = 1:3), aes(x, y, z)) + geom_voxel_3d(annotate = ann),
            geom_bar_3d = ggplot(data.frame(x = c(1, 1, 2, 2, 3), y = c(1, 2, 1, 2, 1)), aes(x, y)) +
                  geom_bar_3d(annotate = ann),
            geom_col_3d = ggplot(data.frame(x = c(1, 2, 1, 2), y = c(1, 1, 2, 2), z = 1:4), aes(x, y, z)) +
                  geom_col_3d(annotate = ann),
            geom_text_3d = ggplot(txt, aes(x, y, z, label = label)) + geom_text_3d(annotate = ann),
            geom_text_3d_polygon = ggplot(txt, aes(x, y, z, label = label)) +
                  geom_text_3d(method = "polygon", annotate = ann)
      )
}

# Does any grob in the tree use this colour?
grob_uses_colour <- function(g, colour) {
      # Compare RGB only, since text colours carry an alpha channel ("#FF00FFFF")
      if (inherits(g, "grob") && !is.null(g$gp$col) &&
          any(substr(toupper(g$gp$col), 1, 7) %in% toupper(colour))) {
            return(TRUE)
      }
      any(vapply(grob_kids(g), grob_uses_colour, logical(1), colour = colour))
}


test_that("every geom function exposes a documented annotate argument", {
      geoms <- grep("^geom_.*_3d$", getNamespaceExports("ggcube"), value = TRUE)
      expect_gte(length(geoms), 16)
      for (fn in geoms) {
            expect_true("annotate" %in% names(formals(get(fn, envir = asNamespace("ggcube")))), info = fn)
      }
})


test_that("every layer draws annotations of each type", {
      anns <- list(
            point = annotate_3d("point", x = 0.5, y = 0.5, z = 0.5, colour = "#FF00FF", size = 4),
            segment = annotate_3d("segment", x = 0, y = 0, z = 0, xend = 0.5, yend = 0.5, zend = 0.5,
                                  colour = "#FF00FF"),
            text = annotate_3d("text", x = 0.5, y = 0.5, z = 0.5, label = "ann", colour = "#FF00FF"),
            axes = annotate_3d("axes", colour = "#FF00FF"),
            plane = NULL,
            line = NULL,
            polygon = NULL
      )
      for (type in names(anns)) {
            plots <- annotation_layer_plots(anns[[type]])
            for (nm in names(plots)) {
                  p <- plots[[nm]] + coord_3d()
                  if (type %in% c("plane", "line", "polygon")) {
                        # Placed through the middle of this panel
                        info <- ggplot_build(p)$layout$panel_params[[1]]$scale_info
                        lim <- lapply(info[c("x", "y", "z")], function(s) range(s$limits))
                        mid <- vapply(lim, mean, numeric(1))
                        step <- vapply(lim, diff, numeric(1)) / 10
                        p$layers[[1]]$geom_params$annotate <- switch(type,
                              plane = annotate_3d("plane", zintercept = mid[["z"]], colour = "#FF00FF"),
                              line = annotate_3d("line", x = mid[["x"]], y = mid[["y"]], z = mid[["z"]],
                                                 dx = 1, dy = 1, dz = 1, colour = "#FF00FF"),
                              polygon = annotate_3d("polygon", x = mid[["x"]] + c(0, step[["x"]], 0),
                                                    y = mid[["y"]] + c(0, 0, step[["y"]]),
                                                    z = mid[["z"]] + c(0, step[["z"]], step[["z"]]),
                                                    colour = "#FF00FF"))
                  }
                  warnings <- character(0)
                  g <- withCallingHandlers(
                        drawn_grob(p),
                        warning = function(w) {
                              warnings <<- c(warnings, conditionMessage(w))
                              invokeRestart("muffleWarning")
                        })
                  expect_false(any(grepl("unknown param|Removed .* row", warnings, ignore.case = TRUE) &
                                         !grepl("^Removed 78 rows", warnings)),
                               info = paste(nm, type))
                  expect_true(grob_uses_colour(g, "#FF00FF"), info = paste(nm, type))
            }
      }
})


test_that("billboard text labels are sized by their own depth", {
      set.seed(4)
      d <- data.frame(x = rnorm(25), y = rnorm(25), z = rnorm(25), label = "a")
      p <- ggplot(d, aes(x, y, z, label = label)) + geom_text_3d() + coord_3d()
      b <- ggplot_build(p)
      pp <- b$layout$panel_params[[1]]
      g <- b$plot$layers[[1]]$geom$draw_panel(b$data[[1]], pp, b$plot$coordinates)
      kids <- find_grobs(drawn_grob(g), "text")
      drawn <- data.frame(x = vapply(kids, function(k) as.numeric(k$x), 1),
                          y = vapply(kids, function(k) as.numeric(k$y), 1),
                          fontsize = vapply(kids, function(k) k$gp$fontsize, 1))

      truth <- b$plot$coordinates$transform(transform(b$data[[1]], group = seq_len(nrow(d))), pp)
      truth$fontsize <- 3.88 * truth$depth_scale * .pt
      m <- merge(round(drawn, 8), round(truth[c("x", "y", "fontsize")], 8), by = c("x", "y"))
      expect_equal(nrow(m), nrow(d))
      expect_equal(m$fontsize.x, m$fontsize.y)
})


test_that("billboard text still drops data rows with missing labels", {
      d <- data.frame(x = 1:3, y = 1:3, z = 1:3, label = c("a", NA, "c"))
      p <- ggplot(d, aes(x, y, z, label = label)) +
            geom_text_3d(annotate = annotate_3d("point", x = 2, y = 2, z = 2)) + coord_3d()
      expect_warning(ggplotGrob(p), "Removed 1 row")
})


test_that("named and numeric font faces can be mixed across a layer and its annotations", {
      expect_equal(as_fontface(c(1, 2, 3, 4)), c("plain", "bold", "italic", "bold.italic"))
      expect_equal(as_fontface(c("1", "bold", "italic")), c("plain", "bold", "italic"))

      d <- data.frame(x = 1:2, y = 1:2, z = 1:2, label = c("a", "b"))
      p <- ggplot(d, aes(x, y, z, label = label)) +
            geom_text_3d(annotate = list(
                  annotate_3d("text", x = 1.5, y = 1.5, z = 1.5, label = "c", fontface = "bold"),
                  annotate_3d("axes"))) +
            coord_3d()
      expect_no_error(drawn_grob(p))
})
