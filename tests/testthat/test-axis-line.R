# Tests for axis.line rendering along each axis's labelled cube edge.

axis_line_plot <- function(...) {
      ggplot(mtcars, aes(mpg, wt, qsec)) + geom_point_3d() + coord_3d(...)
}

# Axis line grobs for a built plot, named by axis
axis_lines <- function(p) {
      b <- ggplot_build(p)
      pp <- b$layout$panel_params[[1]]
      th <- complete_theme(b$plot$theme)
      lines <- render_axis_text(b$plot$coordinates, pp, th, 400, 400)$lines
      lines <- Filter(Negate(is.null), lines)
      setNames(lines, sub("^axis\\.line\\.(.)\\.3d$", "\\1", vapply(lines, function(g) g$name, "")))
}

# Screen direction (npc) of increasing data along one axis
data_direction <- function(p, axis) {
      b <- ggplot_build(p)
      pp <- b$layout$panel_params[[1]]
      lim <- list(x = range(mtcars$mpg), y = range(mtcars$wt), z = range(mtcars$qsec))
      lo <- hi <- vapply(lim, mean, numeric(1))
      lo[axis] <- lim[[axis]][1]
      hi[axis] <- lim[[axis]][2]
      for (a in c("x", "y", "z")) {
            sc <- b$plot$scales$get_scales(a)
            lo[a] <- sc$transform(lo[a])
            hi[a] <- sc$transform(hi[a])
      }
      d <- data.frame(x = c(lo["x"], hi["x"]), y = c(lo["y"], hi["y"]), z = c(lo["z"], hi["z"]),
                      group = 1:2, PANEL = 1, .prim = "point", .sort_method = "painter")
      tr <- b$plot$coordinates$transform(d, pp)
      tr <- tr[order(tr$group), ]
      c(diff(tr$x), diff(tr$y))
}

line_vector <- function(g) {
      c(as.numeric(g$x1) - as.numeric(g$x0), as.numeric(g$y1) - as.numeric(g$y0))
}


test_that("axis lines follow the theme: none by default, one per axis with theme_classic()", {
      expect_length(axis_lines(axis_line_plot()), 0)

      lines <- axis_lines(axis_line_plot() + theme_classic())
      expect_setequal(names(lines), c("x", "y", "z"))
})


test_that("per-axis axis.line elements are honoured, including axis.line.z", {
      lines <- axis_lines(axis_line_plot() + theme(axis.line.x = element_line(colour = "red")))
      expect_named(lines, "x")
      expect_equal(lines$x$gp$col, "red")

      lines <- axis_lines(axis_line_plot() + theme_classic() +
                                theme(axis.line.y = element_blank(),
                                      axis.line.z = element_line(colour = "blue")))
      expect_setequal(names(lines), c("x", "z"))
      expect_equal(lines$z$gp$col, "blue")
      expect_equal(lines$x$gp$col, calc_element("axis.line", theme_classic())$colour)
})


test_that("axis lines run along the edge carrying each axis's ticks and labels", {
      for (labels in list("auto", c("ymax", "zmin"))) {
            p <- axis_line_plot(xlabels = labels) + theme_classic()
            b <- ggplot_build(p)
            pp <- b$layout$panel_params[[1]]
            ratios <- compute_effective_ratios(
                  list(x = pp$scale_info$x$limits, y = pp$scale_info$y$limits,
                       z = pp$scale_info$z$limits), pp$scales, pp$ratio)
            sel <- get_axis_selection("x", b$plot$coordinates, pp, ratios)
            if (length(labels) == 2) expect_true(all(sel$edge$touching_faces %in% labels))

            ends <- rbind(sel$edge$p1[c("x", "y", "z")], sel$edge$p2[c("x", "y", "z")])
            ends <- sweep(ends, 2, ratios, `*`)
            expected <- transform_3d_standard(
                  data.frame(x = ends[, 1], y = ends[, 2], z = ends[, 3]), pp$proj)
            expected <- scale_to_npc_coordinates(expected, plot_bounds = pp$plot_bounds)

            g <- axis_lines(p)$x
            got <- cbind(c(as.numeric(g$x0), as.numeric(g$x1)), c(as.numeric(g$y0), as.numeric(g$y1)))
            want <- cbind(expected$x, expected$y)
            # Same segment, in either direction
            expect_true(isTRUE(all.equal(got, want, tolerance = 1e-9)) ||
                              isTRUE(all.equal(got, want[2:1, ], tolerance = 1e-9)))
      }
})


test_that("axis lines run toward increasing data values, including on reversed scales", {
      base <- axis_line_plot() + theme_classic()
      variants <- list(base, base + scale_x_reverse(),
                       base + scale_z_continuous(transform = "reverse"))
      for (p in variants) {
            lines <- axis_lines(p)
            for (axis in c("x", "y", "z")) {
                  expect_gt(sum(line_vector(lines[[axis]]) * data_direction(p, axis)), 0)
            }
      }
})


test_that("axis line arrows come from element_line(arrow = )", {
      lines <- axis_lines(axis_line_plot() + theme_classic())
      expect_null(lines$x$arrow)

      lines <- axis_lines(axis_line_plot() + theme_classic() +
                                theme(axis.line = element_line(arrow = grid::arrow())))
      expect_s3_class(lines$x$arrow, "arrow")
})


test_that("axis lines are drawn only alongside rendered panels", {
      expect_length(axis_lines(axis_line_plot(panels = "none") + theme_classic()), 0)
})


test_that("axis lines are drawn when they are the only axis furniture", {
      p <- axis_line_plot() + theme_classic() +
            theme(axis.text = element_blank(), axis.title = element_blank(),
                  axis.ticks = element_blank())
      expect_setequal(names(axis_lines(p)), c("x", "y", "z"))

      grob <- ggplotGrob(p)
      find_furniture <- function(g) {
            if (inherits(g, "ggcube_axis_furniture")) return(list(g))
            kids <- if (inherits(g, "gtable")) g$grobs else if (inherits(g, "gTree")) g$children else list()
            unlist(lapply(kids, find_furniture), recursive = FALSE)
      }
      furniture <- find_furniture(grob)
      expect_length(furniture, 1)
      expect_true(furniture[[1]]$show_lines)
})
