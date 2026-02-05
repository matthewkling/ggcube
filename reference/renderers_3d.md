# Animation renderers for animate_3d

Renderer functions that combine individual frame images into a final
animation file. These are factory functions that return the actual
rendering function.

## Usage

``` r
gifski_renderer_3d(file = NULL, loop = TRUE)

av_renderer_3d(file = NULL, vfilter = "null", codec = NULL)

file_renderer_3d(dir = ".", prefix = "ggcube_frame", overwrite = FALSE)
```

## Arguments

- file:

  Output file path. If `NULL`, a temporary file is used.

- loop:

  Logical. Should the GIF loop? Default is `TRUE`.

- vfilter:

  A video filter string for ffmpeg (passed to av).

- codec:

  Video codec name. Default lets av choose.

- dir:

  Directory to copy frame files into.

- prefix:

  Filename prefix for copied frame files.

- overwrite:

  Logical. Overwrite existing files? Default is `FALSE`.

## Value

A function with signature `function(frames, fps, width, height)` that
produces the animation output.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- ggplot() +
  geom_function_3d(
    fun = function(x, y) sin(x) * cos(y),
    xlim = c(-pi, pi), ylim = c(-pi, pi),
    fill = "steelblue", color = "steelblue") +
  coord_3d()

# GIF output (default)
animate_3d(p, yaw = c(0, 360), renderer = gifski_renderer_3d())

# Video output
animate_3d(p, yaw = c(0, 360), renderer = av_renderer_3d())

# Keep individual frame files
animate_3d(p, yaw = c(0, 360),
           renderer = file_renderer_3d(dir = "my_frames"))
} # }
```
