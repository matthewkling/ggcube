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
