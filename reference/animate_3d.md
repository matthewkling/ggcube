# Animate a 3D plot

Renders an animated GIF or MP4 of a rotating ggplot with
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md),
smoothly interpolating rotation angles across frames.

## Usage

``` r
animate_3d(
  plot,
  pitch = NULL,
  roll = NULL,
  yaw = NULL,
  nframes = NULL,
  fps = NULL,
  duration = NULL,
  width = 480,
  height = 480,
  res = 96,
  renderer = gifski_renderer_3d(),
  start_pause = 0,
  end_pause = 0,
  rewind = FALSE,
  cores = 1,
  device = "png",
  progress = interactive()
)
```

## Arguments

- plot:

  A ggplot object that uses
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md).

- pitch, roll, yaw:

  Rotation parameters to animate. Each can be:

  - `NULL` (default): Hold at the value specified in the plot's
    [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
    call.

  - A single numeric value: Hold at this value for all frames
    (overriding the plot's coord).

  - A numeric vector of length 2+: Keyframe values that are interpolated
    linearly across the animation. Keyframes are evenly spaced, so
    `c(0, 180, 180)` with 100 frames means: start at 0, reach 180 at
    frame 50, hold at 180 until frame 100.

- nframes, fps, duration:

  Control animation length and speed. Specify any two of three; the
  third is computed as `duration = nframes / fps`. Defaults:
  `nframes = 100`, `fps = 10`.

- width, height:

  Dimensions of the animation in pixels. Defaults to 480 x 480.

- res:

  Resolution in ppi for the rendered frames. Default is 96.

- renderer:

  A renderer function that combines image frames into an animation file.
  Built-in options:

  - [`gifski_renderer_3d()`](https://matthewkling.github.io/ggcube/reference/renderers_3d.md)
    (default): Renders a GIF using the gifski package.

  - [`av_renderer_3d()`](https://matthewkling.github.io/ggcube/reference/renderers_3d.md):
    Renders a video using the av package.

  - [`file_renderer_3d()`](https://matthewkling.github.io/ggcube/reference/renderers_3d.md):
    Returns the frame image files without combining them.

  Any function with signature `function(frames, fps)` can be used.

- start_pause, end_pause:

  Number of frames to hold at the beginning and end of the animation.
  Default is 0.

- rewind:

  Logical. If `TRUE`, the animation plays in reverse after completing,
  creating a seamless loop. Default is `FALSE`.

- cores:

  Number of CPU cores for parallel frame rendering. Default is 1
  (sequential). Values greater than 1 use
  [`parallel::parLapply()`](https://rdrr.io/r/parallel/clusterApply.html)
  to render frames in parallel, which can significantly speed up
  animation of complex plots. Note that progress reporting is not
  available during parallel rendering.

- device:

  The graphics device to use for rendering frames. Default is `"png"`.
  Other options include `"ragg_png"` (requires the ragg package).

- progress:

  Whether to print a progress bar. Ignored when `cores > 1`.

## Value

The return value of the renderer function. For
[`gifski_renderer_3d()`](https://matthewkling.github.io/ggcube/reference/renderers_3d.md),
a `gif_3d` object (a file path with class attributes for display in
RStudio and knitr). For
[`file_renderer_3d()`](https://matthewkling.github.io/ggcube/reference/renderers_3d.md),
a character vector of file paths.

## See also

[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
for the 3D coordinate system,
[`gifski_renderer_3d()`](https://matthewkling.github.io/ggcube/reference/renderers_3d.md)
and
[`file_renderer_3d()`](https://matthewkling.github.io/ggcube/reference/renderers_3d.md)
for renderer options.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- ggplot() +
  geom_function_3d(
    fun = function(x, y) sin(x) * cos(y),
    xlim = c(-pi, pi), ylim = c(-pi, pi),
    fill = "steelblue", color = "steelblue", linewidth = .5) +
  coord_3d(light = light(mode = "hsl", anchor = "camera",
                         direction = c(1, 1, 0))) +
  theme_void()

# Simple turntable rotation
animate_3d(p, yaw = c(-30, 330))

# Multi-segment orbit with pitch change
animate_3d(p, yaw = c(0, 720), roll = c(-90, 0, -90),
           nframes = 120, fps = 15, cores = 10)

# Save to file
anim <- animate_3d(p, yaw = c(0, 360))
anim_save_3d(anim, "rotating_surface.gif")
} # }
```
