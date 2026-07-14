# Create an interactive drag-to-rotate ggcube

Creates a ggcube HTML object that you can rotate interactively by
dragging. Pre-renders a set of ggcube plots across a 1D or 2D grid of
rotation angles, and packages them as a self-contained HTML file that
can be viewed in the RStudio viewer or a web browser. When viewing the
HTML, you can navigate through the views by dragging the image or using
keyboard arrows.

## Usage

``` r
flipbook_3d(
  plot,
  pitch = NULL,
  roll = NULL,
  yaw = NULL,
  n = 36,
  loop = NULL,
  start = NULL,
  max_frames = 500,
  width = 480,
  height = 480,
  res = 96,
  cores = 1,
  device = "png",
  progress = interactive(),
  file = NULL,
  viewer = interactive()
)
```

## Arguments

- plot:

  A ggplot object that uses
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md).

- pitch, roll, yaw:

  Rotation angles in degrees. Each is either a single value (held fixed)
  or a length-2 range `c(from, to)` that becomes a draggable axis. For
  `yaw`, using a larger value for `from` than `to` gives more natural
  results. Values specified here replace the ones specified in
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md).
  See Details.

- n:

  Grid resolution. For a 1D flipbook, a single number giving the frame
  count. For a 2D flipbook, a length-2 vector giving the counts for the
  horizontal and vertical screen axes respectively (in that order, x
  then y). Its length must match the number of ranged parameters.
  Default is 36.

- loop:

  Controls seamless wraparound. `NULL` (default) auto-detects looping
  independently for each axis by checking whether that axis returns to
  its starting view. `TRUE` or `FALSE` force wrapping on or off for all
  axes.

- start:

  Initial viewing angle(s) the flipbook opens at, given in degrees as a
  named vector matching the variable axes, e.g. `start = c(yaw = 30)` or
  `start = c(yaw = 30, roll = 45)`. For a 1D flipbook a bare scalar
  (e.g. `start = 30`) is also accepted. The flipbook opens at the frame
  nearest each given angle. Any variable axis not named in `start` opens
  at the midpoint of its range, as does every axis when `start = NULL`
  (the default). Naming an axis that is fixed rather than variable
  throas an error.

- max_frames:

  Safety cap on the total number of rendered frames (`prod(n)`). A 2D
  grid can become very large; if the requested grid exceeds this cap the
  function errors rather than rendering. Raise it to allow larger grids.
  Default is 500.

- width, height:

  Dimensions of the animation in pixels. Defaults to 480 x 480.

- res:

  Resolution in ppi for the rendered frames. Default is 96. Controls the
  size of point-based text/lines/point elements relative to the plot.
  See [`?grDevices::png`](https://rdrr.io/r/grDevices/png.html) for
  details.

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

- file:

  Output path for the HTML file. If `NULL` (default), a temporary file
  is used.

- viewer:

  Logical. If `TRUE`, open the flipbook in the RStudio Viewer pane
  (falling back to the system browser when RStudio is not available).
  Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

A `flipbook_3d` object: the path to the written HTML file, with class
attributes for display in RStudio and knitr. Returned invisibly.

## Specifying rotation

Each of `pitch`, `roll`, and `yaw` is given either as a single value
(held fixed) or as a length-2 range `c(from, to)` that is a variable,
draggable axis. The number of ranges determines the interaction:

- One range gives a 1D flipbook: horizontal drag (or left/right keyboard
  arrows) vary that one parameter across its range.

- Two ranges give a 2D flipbook: horizontal drag (or left/right keyboard
  arrows) vary one parameter while vertical drag/arrows vary the other.

At least one range is required. Supplying three ranges is an error.

For a 2D flipbook, the horizontal (x) and vertical (y) axes are assigned
from whichever parameters are ranged, by priority. The x axis takes the
highest-priority ranged parameter in the order `yaw`, `pitch`, `roll`;
the y axis takes the highest-priority remaining ranged parameter in the
order `roll`, `pitch`. In the common case
`yaw = c(0, 360), roll = c(...)` this puts yaw on horizontal drag and
roll on vertical drag.

The range direction follows the order the values are written: for
example `yaw = c(360, 0)` rotates the opposite way from
`yaw = c(0, 360)`.

## Looping

When a variable axis returns to its starting view (for example a full
`yaw = c(0, 360)` turn), that axis loops seamlessly: dragging past
either end wraps around. This is detected per axis by comparing the
first and last angle modulo 360, so a full-turn axis wraps while a
partial range (such as `roll = c(-90, 90)`) clamps. `loop` can force
this on or off for all axes at once.

## Keyboard

Once the widget has focus, the arrow keys step one frame: left/right
move the horizontal axis, and (in a 2D flipbook) up/down move the
vertical axis.

## See also

[`animate_3d()`](https://matthewkling.github.io/ggcube/reference/animate_3d.md)
for GIF/MP4 output. See `vignette("animation")` for live, interactive
examples.

## Examples

``` r
# \donttest{
p <- ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
  geom_surface_3d(linewidth = .25) +
  coord_3d(light = light(anchor = "camera", direction = c(1, 1, 0)),
           ratio = c(1.5, 2, 1)) +
  scale_fill_viridis_c() + scale_color_viridis_c() +
  theme_void()

# 1D drag-to-rotate turntable (small n to minimize runtime)
flipbook_3d(p, yaw = c(360, 0), n = 12)
#> Rendering 12 frames...
#> Assembling flipbook...

# 2D flipbook: drag horizontally to spin (yaw), vertically to tilt (roll)
# (note you could increase `cores` to speed up compute time)
flipbook_3d(p, yaw = c(360, 0), roll = c(-90, 0), n = c(20, 10),
            width = 1000, height = 1000, cores = 1)
#> Rendering 200 frames...
#> Assembling flipbook...

# Save to a chosen path
flipbook_3d(p, yaw = c(360, 0), n = 12,
            file = file.path(tempdir(), "surface.html"))
#> Rendering 12 frames...
#> Assembling flipbook...
# }
```
