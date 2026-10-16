# Tests for the light-animation machinery shared by animate_3d() and
# orbit_3d(): resolving which light to move, building per-frame parameter
# sequences, and putting each frame's light on its coord. These check built
# plots rather than rendered pixels.

lit_plot <- function() {
      ggplot(sphere_points, aes(x, y, z)) +
            geom_hull_3d()
}

# Build frame `i` the way render_3d_frames() does, and return its resolved
# panel light.
built_frame_light <- function(plot, light, param_seqs, i) {
      prepared <- prepare_frame_lights(plot, light, param_seqs)
      static_params <- extract_static_coord_params(plot$coordinates)
      frame_plot <- build_frame_plot(prepared$plot, param_seqs, static_params,
                                     global_bounds = NULL, i = i,
                                     frame_light = prepared$frame_lights[[i]])
      ggplot_build(frame_plot)$layout$panel_params[[1]]$light
}

# --- resolve_animated_light ---------------------------------------------------

test_that("the animated light comes from + light(), coord_3d(light =), or the default", {
      added <- resolve_animated_light(lit_plot() + light("direct", direction = c(1, 0, 0)) +
                                            coord_3d())
      expect_equal(added$method, "direct")
      expect_equal(added$direction, c(1, 0, 0))

      on_coord <- resolve_animated_light(lit_plot() +
                                               coord_3d(light = light(direction = c(0, 1, 0))))
      expect_equal(on_coord$direction, c(0, 1, 0))

      default <- resolve_animated_light(lit_plot() + coord_3d())
      expect_equal(default$direction, light()$direction)
})

test_that("animating a disabled light is an error", {
      expect_error(resolve_animated_light(lit_plot() + coord_3d(light = NULL)),
                   "lighting is disabled")
      expect_error(resolve_animated_light(lit_plot() + coord_3d(light = "none")),
                   "lighting is disabled")
      expect_error(resolve_animated_light(lit_plot() + light("none") + coord_3d()),
                   "lighting is disabled")
})

test_that("animating a positional light is an error", {
      p <- lit_plot() + light(position = c(1, 1, 1)) + coord_3d()
      expect_error(resolve_animated_light(p), "directional")
})

test_that("animating the light when a layer has its own light is an error", {
      p <- lit_plot() +
            geom_hull_3d(aes(x = x + 3), light = light("direct")) +
            coord_3d()
      expect_error(resolve_animated_light(p), "layer 2 sets its own")

      # Layers that inherit the plot light are fine.
      expect_no_error(resolve_animated_light(lit_plot() + geom_point_3d() +
                                                   coord_3d()))
})

test_that("light_elevation outside [-90, 90] is an error", {
      expect_error(validate_light_elevation(c(0, 100)), "between -90 and 90")
      expect_error(validate_light_elevation(-91), "between -90 and 90")
      expect_no_error(validate_light_elevation(c(-90, 90)))
      expect_no_error(validate_light_elevation(NULL))
})

# --- build_param_sequences ----------------------------------------------------

test_that("animation sequences gain light columns only when the light moves", {
      coord <- list(pitch = 0, roll = -60, yaw = -30)

      seqs <- build_param_sequences(coord, NULL, NULL, c(0, 90), nframes = 4)
      expect_named(seqs, c("pitch", "roll", "yaw"))

      seqs <- build_param_sequences(coord, NULL, NULL, NULL, nframes = 4,
                                    light_azimuth = c(0, 90, 0),
                                    light_angles = list(azimuth = 270,
                                                        elevation = 60))
      expect_equal(seqs$light_azimuth,
                   stats::approx(c(1, 2.5, 4), c(0, 90, 0), xout = 1:4)$y)
      expect_true(all(seqs$light_elevation == 60))
      expect_true(all(seqs$yaw == -30))

      seqs <- build_param_sequences(coord, NULL, NULL, NULL, nframes = 3,
                                    light_elevation = c(0, 90),
                                    light_angles = list(azimuth = 270,
                                                        elevation = 60))
      expect_equal(seqs$light_elevation, c(0, 45, 90))
      expect_true(all(seqs$light_azimuth == 270))
})

test_that("pause and rewind carry the light columns along", {
      seqs <- data.frame(pitch = 0, roll = 0, yaw = 0,
                         light_azimuth = c(0, 90, 180), light_elevation = 30)
      out <- apply_pause_and_rewind(seqs, start_pause = 1, end_pause = 0,
                                    rewind = TRUE)
      expect_equal(out$light_azimuth, c(0, 0, 90, 180, 90))
})

test_that("light angles don't change the global bounds", {
      static_params <- list(persp = TRUE, dist = 2)
      camera <- data.frame(pitch = 0, roll = -60, yaw = -30)
      with_light <- data.frame(pitch = 0, roll = -60, yaw = -30,
                               light_azimuth = c(0, 90, 180),
                               light_elevation = 30)
      expect_equal(compute_global_bounds(with_light, static_params, c(1, 1, 1)),
                   compute_global_bounds(camera, static_params, c(1, 1, 1)))
})

# --- Per-frame lights on built plots ---------------------------------------------

test_that("each frame's built plot carries that frame's light direction", {
      p <- lit_plot() + light("direct") + coord_3d()
      base <- resolve_animated_light(p)
      seqs <- data.frame(pitch = 0, roll = -60, yaw = -30,
                         light_azimuth = c(0, 90), light_elevation = c(0, 45))

      first <- built_frame_light(p, base, seqs, 1)
      second <- built_frame_light(p, base, seqs, 2)

      expect_equal(first$direction, direction(0, 0))
      expect_equal(second$direction, direction(90, 45))

      # Everything but the direction is carried over from the base light.
      expect_equal(second$method, "direct")
      expect_equal(second$anchor, base$anchor)
})

test_that("a light added with + light() doesn't collide with the frame's light", {
      # Without clearing the stashed `+ light()`, the frame coord's explicit
      # light would trigger the double-light error at build time.
      p <- lit_plot() + light("direct", direction = c(1, 0, 0)) + coord_3d()
      base <- resolve_animated_light(p)
      seqs <- data.frame(pitch = 0, roll = -60, yaw = -30,
                         light_azimuth = 180, light_elevation = 0)

      expect_no_error(light <- built_frame_light(p, base, seqs, 1))
      expect_equal(light$direction, direction(180, 0))
})

test_that("frames are left alone when the light isn't animated", {
      p <- lit_plot() + light("direct", direction = c(1, 0, 0)) + coord_3d()
      seqs <- data.frame(pitch = 0, roll = -60, yaw = -30)

      prepared <- prepare_frame_lights(p, NULL, seqs)
      expect_null(prepared$frame_lights)
      expect_identical(prepared$plot, p)

      light <- built_frame_light(p, NULL, seqs, 1)
      expect_equal(light$direction, c(1, 0, 0))
})
