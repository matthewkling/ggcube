# Tests for the lighting pipeline, focused on camera-anchor normal orientation.
#
# These verify the *analytic* invariant that a face's computed light equals the dot
# product of its correctly-oriented normal with the light direction, rather than
# snapshotting rendered pixels. The bugs these guard against all produced
# plausible-looking output, so the value is in checking orientation against ground
# truth, not against a frozen image.

# Extract per-face light and metadata from a built plot, after the full coord
# transform (projection, npc scaling, and backface handling) has run.
built_faces <- function(p) {
      b <- ggplot_build(p)
      d <- b$data[[1]]
      coord <- b$plot$coordinates
      pp <- b$layout$panel_params[[1]]
      tr <- coord$transform(d, pp)
      list(tr = tr, proj = pp$proj)
}

# Ground-truth light for an axis-aligned face under camera-anchor directional
# lighting: rotate the scene-space normal into camera space (z-flip + projection
# rotation), then dot with the normalised direction.
axis_normal <- function(face_type) {
      switch(face_type,
             xmin = c(-1, 0, 0), xmax = c(1, 0, 0),
             ymin = c(0, -1, 0), ymax = c(0, 1, 0),
             zmin = c(0, 0, -1), zmax = c(0, 0, 1),
             c(0, 0, 1))
}


test_that("rotate_normals_to_camera matches the coordinate transform convention", {
      proj <- list(pitch = 10, roll = -25, yaw = 60)
      n <- matrix(c(1, 0, 0,
                    0, 1, 0,
                    0, 0, 1,
                    -1, 0, 0), ncol = 3, byrow = TRUE)

      rotated <- rotate_normals_to_camera(n, proj)

      # Expected: z-flip then rotate_3d, identical to how coordinates enter camera space.
      expected <- n
      expected[, 3] <- -expected[, 3]
      expected <- rotate_3d(expected, proj$pitch, proj$roll, proj$yaw)

      expect_equal(rotated, expected)

      # Rotation preserves unit length.
      expect_equal(sqrt(rowSums(rotated^2)), rep(1, nrow(rotated)))
})


test_that("voxel camera-anchor lighting matches camera-space ground truth", {
      dir <- c(1, 1, 0)
      dirn <- dir / sqrt(sum(dir^2))

      for (cfg in list(c(60, -25, 10), c(30, 0, 0), c(-45, 20, -15))) {
            p <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
                  geom_bar_3d(bins = 20, width = c(.5, 1)) +
                  coord_3d(scales = "fixed", ratio = c(1, 3, .25),
                           yaw = cfg[1], roll = cfg[2], pitch = cfg[3],
                           light = light(mode = "hsl", anchor = "camera",
                                         direction = dir)) +
                  scale_z_continuous(expand = c(0, 0))

            bf <- built_faces(p)
            faces <- dplyr::summarise(dplyr::group_by(bf$tr, group),
                                      face_type = face_type[1],
                                      light = as.numeric(light)[1],
                                      .groups = "drop")

            N <- t(vapply(faces$face_type, axis_normal, numeric(3)))
            N[, 3] <- -N[, 3]
            Ncam <- rotate_3d(N, bf$proj$pitch, bf$proj$roll, bf$proj$yaw)
            truth <- as.numeric(Ncam %*% dirn)

            expect_equal(faces$light, truth,
                         tolerance = 1e-6,
                         info = paste("yaw/roll/pitch =",
                                      paste(cfg, collapse = "/")))
      }
})


test_that("voxel scene-anchor lighting is unchanged by the camera path", {
      dir <- c(1, 1, 0)
      dirn <- dir / sqrt(sum(dir^2))

      p <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
            geom_bar_3d(bins = 20, width = c(.5, 1)) +
            coord_3d(scales = "fixed", ratio = c(1, 3, .25),
                     yaw = 60, roll = -25, pitch = 10,
                     light = light(mode = "hsl", anchor = "scene",
                                   direction = dir)) +
            scale_z_continuous(expand = c(0, 0))

      bf <- built_faces(p)
      faces <- dplyr::summarise(dplyr::group_by(bf$tr, group),
                                face_type = face_type[1],
                                light = as.numeric(light)[1],
                                .groups = "drop")

      # Scene anchor: normals stay in scene space, direction applied directly.
      N <- t(vapply(faces$face_type, axis_normal, numeric(3)))
      truth <- as.numeric(N %*% dirn)

      expect_equal(faces$light, truth, tolerance = 1e-6)
})


# Note on hulls: geom_hull_3d also routes through rotate_normals_to_camera for
# camera-anchor lighting. Before the fix its triangle normals were computed via a
# coordinate z-flip (a reflection, which negates the cross-product normal) rather
# than a normal z-flip, leaving hull normals inverted. For a closed convex hull this
# inversion is fully masked by backface handling -- every visible face's polarity is
# flipped back -- so the final rendered light is identical with or without the fix.
# There is therefore no observable behaviour to pin with a regression test; the fix
# is an internal-consistency improvement so that hull normals share the single
# camera-space convention. The helper it relies on is covered by the
# rotate_normals_to_camera test above.


test_that("surface camera-anchor normals are oriented, not sign-forced", {
      # Reproduces the original white-patch report. Root cause: compute_surface_normals
      # builds (-dzdx, -dzdy, +1), forcing the camera-space normal z-component positive
      # for every face. That made faces curving past perpendicular to the sight line
      # receive frontface lighting, producing a bright patch where they should be dark.
      #
      # Once normals are computed in scene space and rotated into camera space, faces
      # whose top points away from the viewer have negative camera-space normal_z. The
      # pre-fix code could never produce this; its normal_z was clamped non-negative.
      p <- ggplot() +
            geom_function_3d(
                  fun = function(x, y) sin(x) * cos(y),
                  xlim = c(-pi, pi), ylim = c(-pi, pi),
                  fill = "steelblue", color = "steelblue") +
            coord_3d(light = light(mode = "hsl", anchor = "camera",
                                   direction = c(1, 1, 0)),
                     yaw = -30, roll = -40, pitch = 0)

      b <- ggplot_build(p)
      d <- b$data[[1]]
      coord <- b$plot$coordinates
      pp <- b$layout$panel_params[[1]]
      tr <- coord$transform(d, pp)

      faces <- dplyr::summarise(dplyr::group_by(tr, group),
                                normal_z = normal_z[1],
                                light = as.numeric(light)[1],
                                .groups = "drop")

      # Directly catches the +1 forcing: a rotated surface at this angle must have
      # faces whose camera-space normal points away from the viewer.
      expect_true(any(faces$normal_z < 0),
                  info = "no away-facing faces; normal_z appears sign-forced")

      # Backface handling engaged: some faces were flipped to negative (dark) light.
      # (backface_scale defaults to -1, so a face whose bottom faces the camera reads
      # dark.) This guards the polarity-flip half of the pipeline.
      expect_true(any(faces$light < 0))
})


# Plot-level lighting via `+ light()` -------------------------------------------
#
# `+ light()` sets lighting for a whole plot. It resolves through two different
# routes depending on where it appears: added *after* `coord_3d()` it writes
# straight to the coord, while added *before* it is stashed on the plot and
# retrieved during the build by `find_pending_light()`. Both routes are exercised
# here, since only one of them is covered by any given ordering.

# The light actually handed to the layers, read from the built panel params.
resolved_light <- function(p) {
      ggplot_build(p)$layout$panel_params[[1]]$light
}

# Minimal 3D plot used as a base throughout.
light_test_plot <- function() {
      ggplot(mtcars, aes(mpg, wt, disp)) + geom_point_3d()
}


test_that("`+ light()` applies whether added before or after coord_3d()", {
      p_after  <- light_test_plot() + coord_3d() + light(direction = c(1, 0, 0))
      p_before <- light_test_plot() + light(direction = c(1, 0, 0)) + coord_3d()

      expect_equal(resolved_light(p_after)$direction, c(1, 0, 0))

      # The pre-coord ordering is the one that depends on the stash-and-retrieve
      # path, so it is asserted separately rather than folded into the above.
      expect_equal(resolved_light(p_before)$direction, c(1, 0, 0))

      # Both orderings must agree, not merely each work.
      expect_equal(resolved_light(p_after), resolved_light(p_before))
})


test_that("plot-level lighting falls back to the default when unspecified", {
      p <- light_test_plot() + coord_3d()

      expect_s3_class(resolved_light(p), "light")
      expect_equal(resolved_light(p)$direction, light()$direction)
})


test_that("coord_3d(light = NULL) disables lighting and is not overwritten by the default", {
      p <- light_test_plot() + coord_3d(light = NULL)

      # NULL is a deliberate request for no lighting, distinct from the `waiver()`
      # that signals "unspecified". Resolving NULL to the default would silently
      # light plots that asked not to be.
      expect_null(resolved_light(p))
})


test_that("specifying lighting in both coord_3d() and `+ light()` is an error", {
      # Both orderings hit a different branch: the first errors in ggplot_add,
      # the second during the build in setup_panel_params.
      expect_error(
            print(light_test_plot() + coord_3d(light = light()) + light()),
            "specified twice"
      )
      expect_error(
            print(light_test_plot() + light() + coord_3d(light = light())),
            "specified twice"
      )
})


test_that("coord_3d(light = ) alone is unaffected by the double-specification check", {
      p <- light_test_plot() + coord_3d(light = light(direction = c(0, 1, 0)))
      expect_equal(resolved_light(p)$direction, c(0, 1, 0))
})


test_that("replacing plot-level lighting is reported", {
      # Mirrors ggplot2's behaviour for coords and scales: a component discarded
      # wholesale is reported. Both routes are checked, since a light added after
      # `coord_3d()` is replaced on the coord while one added before is replaced
      # in the stash.
      expect_message(
            light_test_plot() + coord_3d() + light() + light(),
            "already present"
      )
      expect_message(
            light_test_plot() + light() + light() + coord_3d(),
            "already present"
      )

      # A single light is not a replacement, by either route.
      expect_silent(light_test_plot() + coord_3d() + light())
      expect_silent(light_test_plot() + light() + coord_3d())
})


test_that("the last `+ light()` wins", {
      # Matches ggplot2's convention for repeated components (coords, labs, scales).
      p <- light_test_plot() + coord_3d() +
            light(direction = c(1, 0, 0)) +
            light(direction = c(0, 1, 0))

      expect_equal(resolved_light(p)$direction, c(0, 1, 0))
})


test_that("light('none') matches the bare string and the fill/color form", {
      expect_identical(light("none"), light(fill = FALSE, color = FALSE))
      expect_equal(light("none")$shade, "neither")

      d <- data.frame(x = 1)
      expect_identical(
            attach_light(d, "none")$lighting_spec[[1]],
            attach_light(d, light("none"))$lighting_spec[[1]]
      )

      # `"none"` short-circuits before validation, so parameters that would
      # otherwise error are ignored rather than checked.
      expect_silent(light("none", direction = c(0, 0, 0)))

      # The error message advertises "none" as a valid method.
      expect_error(light("bogus"), "none")
})


test_that("adding `+ light()` does not leak into other plots sharing a coord", {
      # Coord ggproto objects have reference semantics and are not deep-copied by
      # ggplot2's plot_clone(), so mutating one in place would affect every plot
      # built from the same coord object.
      shared <- coord_3d()

      p1 <- light_test_plot() + shared + light(direction = c(1, 0, 0))
      p2 <- light_test_plot() + shared

      expect_equal(resolved_light(p1)$direction, c(1, 0, 0))
      expect_equal(resolved_light(p2)$direction, light()$direction)
})


test_that("two plots sharing a coord can carry different plot-level lights", {
      shared <- coord_3d()

      p1 <- light_test_plot() + shared + light(direction = c(1, 0, 0))
      p2 <- light_test_plot() + shared + light(direction = c(0, 1, 0))

      # Order of evaluation matters here: building p1 first must not fix the
      # light that p2 subsequently resolves.
      l1 <- resolved_light(p1)
      l2 <- resolved_light(p2)

      expect_equal(l1$direction, c(1, 0, 0))
      expect_equal(l2$direction, c(0, 1, 0))
})


test_that("repeated `+ light()` does not build a self-referential coord", {
      # `ggproto(NULL, parent)` captures `parent` lazily. Rebinding the same name
      # (`co <- ggproto(NULL, co)`) makes the clone its own parent, and any
      # inherited field lookup then recurses until the C stack overflows. The
      # coord is cloned once and mutated in place thereafter to avoid this.
      p <- light_test_plot() + coord_3d()
      for (i in 1:25) p <- p + light(direction = c(i, 0, 1))

      co <- p$coordinates
      parent <- get("super", envir = as.environment(co), inherits = FALSE)
      if (is.function(parent)) parent <- parent()

      expect_false(identical(as.environment(parent), as.environment(co)))

      # An inherited-field read is what actually triggers the overflow, so read
      # one rather than only inspecting the chain.
      expect_false(isTRUE(co$light_explicit))
      expect_equal(resolved_light(p)$direction, c(25, 0, 1))
})


test_that("plot-level lighting survives faceting and the print path", {
      # setup_panel_params() runs once per unique scale combination, so faceting
      # exercises the retrieval path repeatedly within one build.
      p <- light_test_plot() + facet_wrap(~cyl) +
            light(direction = c(1, 0, 0)) + coord_3d()

      pp <- ggplot_build(p)$layout$panel_params
      expect_gt(length(pp), 1)
      for (panel in pp) expect_equal(panel$light$direction, c(1, 0, 0))

      # Printing takes a different route into the build than ggplot_build().
      tmp <- tempfile(fileext = ".png")
      grDevices::png(tmp)
      on.exit({grDevices::dev.off(); unlink(tmp)}, add = TRUE)
      expect_silent(print(p))
})


test_that("3D guides see the resolved light, not the unresolved waiver", {
      # extract_light_from_plot() reads the coord's `light` field directly. That
      # field holds a `waiver()` until the build resolves it, and `waiver()$shade`
      # is NULL, so an unguarded `$shade == "neither"` comparison yields NA and
      # errors when used as a condition.
      extract <- extract_light_from_plot
      probe <- function(plot) extract()

      # Unspecified lighting must resolve to the default, so guides shade the key.
      bare <- probe(light_test_plot() + coord_3d())
      expect_s3_class(bare, "light")
      expect_equal(bare$shade, light()$shade)

      # `+ light()` is visible whether it lands on the coord or is still stashed.
      after  <- probe(light_test_plot() + coord_3d() + light(direction = c(1, 0, 0)))
      before <- probe(light_test_plot() + light(direction = c(1, 0, 0)) + coord_3d())
      expect_equal(after$direction, c(1, 0, 0))
      expect_equal(before$direction, c(1, 0, 0))

      # Disabled lighting stays NULL so guides skip shading entirely.
      expect_null(probe(light_test_plot() + coord_3d(light = NULL)))
})


test_that("layer-level light overrides plot-level light", {
      # Layer-level lighting is genuine specificity rather than redundancy, so it
      # wins silently instead of erroring the way two plot-level sources do.
      p <- ggplot(mountain, aes(x, y, z)) +
            geom_surface_3d(light = light(direction = c(0, 0, 1))) +
            coord_3d() +
            light(direction = c(1, 0, 0))

      b <- ggplot_build(p)
      spec <- b$data[[1]]$lighting_spec[[1]]

      expect_equal(spec$direction, c(0, 0, 1))
})
