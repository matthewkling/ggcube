test_that("find_legend_key_cells matches both naming schemes and rejects non-key cells", {
      # Synthetic gtable whose layout names mimic the cells ggplot2 produces.
      # ggplot2 >= 4.0 uses "key-1-1-bg"; ggplot2 3.5.x uses "key-1-1-1".
      gt <- gtable::gtable(widths = grid::unit(1, "null"),
                           heights = grid::unit(1, "null"))
      gt$layout <- data.frame(
            t = c(1, 2, 3, 4, 5, 6),
            l = c(1, 1, 1, 1, 1, 1),
            b = c(1, 2, 3, 4, 5, 6),
            r = c(1, 1, 1, 1, 1, 1),
            z = c(1, 2, 3, 4, 5, 6),
            clip = "off",
            name = c("key-1-1-bg",   # 4.0.x key cell
                     "key-3-1-bg",   # 4.0.x key cell
                     "key-1-1-1",    # 3.5.x key cell
                     "label-1-2",    # not a key
                     "title",        # not a key
                     "background"),  # not a key
            stringsAsFactors = FALSE
      )

      idx <- find_legend_key_cells(gt)

      expect_setequal(gt$layout$name[idx],
                      c("key-1-1-bg", "key-3-1-bg", "key-1-1-1"))
})

test_that("replace_key_glyph replaces a bare rect cell with a raster grob", {
      cell <- grid::rectGrob(
            x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
            width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
            gp = grid::gpar(fill = "red")
      )

      out <- replace_key_glyph(cell, c("#000000", "#888888", "#ffffff"))

      expect_true(inherits(out, "rastergrob"))
})

test_that("replace_key_glyph replaces only the layer glyph inside a gTree cell", {
      # Mimic the ggplot2 >= 4.0 key cell: a background rect plus the layer's
      # drawn glyph rect. childrenOrder names the background "" and the glyph
      # after its layer.
      bg <- grid::rectGrob(gp = grid::gpar(fill = "grey95"),
                           name = "legend.key.rect")
      glyph <- grid::rectGrob(gp = grid::gpar(fill = "red"),
                              name = "layer.glyph")
      cell <- grid::gTree(children = grid::gList(bg, glyph),
                          childrenOrder = stats::setNames(
                                c("legend.key.rect", "layer.glyph"),
                                c("", "stat_surface_3d")))

      out <- replace_key_glyph(cell, c("#000000", "#888888", "#ffffff"))

      # The layer glyph becomes a raster; the background rect is preserved.
      expect_true(inherits(out$children[["layer.glyph"]], "rastergrob"))
      expect_true(inherits(out$children[["legend.key.rect"]], "rect"))
})

test_that("replace_legend_colors converts key glyphs to rasters in a rendered legend", {
      library(ggplot2)

      p <- ggplot(mountain, aes(x, y, z, fill = x > .5, group = 1)) +
            stat_surface_3d(light = light(mode = "hsl", direction = c(1, 0, 0))) +
            guides(fill = guide_legend_3d()) +
            coord_3d(ratio = c(2, 3, 1.5))

      gt <- ggplotGrob(p)
      box_idx <- which(grepl("guide-box", gt$layout$name))
      expect_gt(length(box_idx), 0)

      # Use the first non-empty guide-box (legend position varies by version).
      legend <- NULL
      for (i in box_idx) {
            g <- gt$grobs[[i]]
            if (inherits(g, "gtable")) {
                  legend <- g
                  break
            }
      }
      expect_false(is.null(legend))

      # Supply a gradient directly, bypassing extract_light_from_plot() (which
      # only resolves during a live render).
      grad <- matrix(
            c("#000000", "#444444", "#888888", "#bbbbbb", "#ffffff"),
            nrow = 2, ncol = 5, byrow = TRUE
      )

      shaded <- replace_legend_colors(legend, grad)

      # Recursively collect grob class names and assert at least one raster
      # appeared where the key glyphs were.
      collect_classes <- function(g) {
            classes <- class(g)
            if (inherits(g, "gtable") || inherits(g, "gTree")) {
                  kids <- g$grobs %||% g$children
                  for (k in kids) classes <- c(classes, collect_classes(k))
            }
            classes
      }

      expect_true("rastergrob" %in% collect_classes(shaded))
})

test_that("replace_colorbar_colors shades the raster in a rendered colorbar guide", {
      library(ggplot2)

      p <- ggplot(mountain, aes(x, y, z, fill = z)) +
            stat_surface_3d(light = light(mode = "hsl", direction = c(1, 0, 0))) +
            guides(fill = guide_colorbar_3d()) +
            scale_fill_gradientn(colors = c("tomato", "dodgerblue")) +
            coord_3d()

      gt <- ggplotGrob(p)
      box_idx <- which(grepl("guide-box", gt$layout$name))
      expect_gt(length(box_idx), 0)

      # Locate the colorbar gtable that replace_colorbar_colors actually
      # operates on: the one whose direct children include the rastergrob.
      # This mirrors the function's own (non-recursive) search depth, so the
      # test fails if the raster ever nests deeper than the function reaches.
      find_colorbar_gtable <- function(g) {
            if (inherits(g, "gtable")) {
                  for (sub in g$grobs) {
                        if (inherits(sub, "rastergrob")) return(g)
                  }
                  for (sub in g$grobs) {
                        hit <- find_colorbar_gtable(sub)
                        if (!is.null(hit)) return(hit)
                  }
            }
            NULL
      }

      colorbar <- NULL
      for (i in box_idx) {
            colorbar <- find_colorbar_gtable(gt$grobs[[i]])
            if (!is.null(colorbar)) break
      }
      expect_false(is.null(colorbar))

      # Capture the original raster width so we can confirm it changed.
      orig_raster <- NULL
      for (g in colorbar$grobs) {
            if (inherits(g, "rastergrob")) {
                  orig_raster <- g$raster
                  break
            }
      }
      expect_false(is.null(orig_raster))

      n_colors <- 6
      n_light_steps <- 8
      ramp <- grDevices::colorRampPalette(c("tomato", "dodgerblue"))(n_colors)
      grad <- matrix(rep(ramp, times = n_light_steps),
                     nrow = n_colors, ncol = n_light_steps)

      shaded <- replace_colorbar_colors(colorbar, grad)

      shaded_raster <- NULL
      for (g in shaded$grobs) {
            if (inherits(g, "rastergrob")) {
                  shaded_raster <- g$raster
                  break
            }
      }
      expect_false(is.null(shaded_raster))

      # A vertical colorbar gains lighting columns; a horizontal one gains rows.
      # Either way the raster's dimensions must change from the original.
      expect_false(identical(dim(shaded_raster), dim(orig_raster)))
      expect_true(n_light_steps %in% dim(shaded_raster))
})

test_that("replace_colorbar_colors widens the raster to the lighting steps", {
      # A vertical colorbar raster is a single column of colors (n rows x 1 col).
      n_colors <- 6
      ramp <- grDevices::colorRampPalette(c("tomato", "dodgerblue"))(n_colors)
      original_raster <- as.raster(matrix(ramp, ncol = 1))

      cbar <- grid::rasterGrob(original_raster)
      gt <- gtable::gtable(widths = grid::unit(1, "null"),
                           heights = grid::unit(1, "null"))
      gt <- gtable::gtable_add_grob(gt, cbar, t = 1, l = 1, name = "bar")

      # Gradient with a known number of lighting steps; columns drive new width.
      n_light_steps <- 8
      grad <- matrix(
            rep(ramp, times = n_light_steps),
            nrow = n_colors, ncol = n_light_steps
      )

      shaded <- replace_colorbar_colors(gt, grad)

      # Find the (modified) rastergrob and confirm it gained lighting columns.
      raster_grob <- NULL
      for (g in shaded$grobs) {
            if (inherits(g, "rastergrob")) {
                  raster_grob <- g
                  break
            }
      }
      expect_false(is.null(raster_grob))

      # Original was 1 column wide; shaded raster should span n_light_steps.
      expect_equal(ncol(raster_grob$raster), n_light_steps)
      expect_equal(nrow(raster_grob$raster), n_colors)
})
