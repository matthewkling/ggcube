# Animation renderers for animate_3d

Renderer functions that combine individual frame images into a final
animation file. These are factory functions that return the actual
rendering function.

## Usage

``` r
gifski_renderer_3d(file = NULL, loop = TRUE)

av_renderer_3d(file = NULL, vfilter = "null", codec = NULL)

file_renderer_3d(dir = tempdir(), prefix = "ggcube_frame", overwrite = FALSE)
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

  Directory to copy frame files into. Defaults to a session temporary
  directory ([`tempdir()`](https://rdrr.io/r/base/tempfile.html)). For
  animations you want to keep, pass an explicit directory.

- prefix:

  Filename prefix for copied frame files.

- overwrite:

  Logical. Overwrite existing files? Default is `FALSE`.

## Value

A function with signature `function(frames, fps, width, height)` that
produces the animation output.

## Examples

``` r
# \donttest{
p <- ggplot() +
  geom_function_3d(
    fun = function(x, y) sin(x) * cos(y),
    xlim = c(-pi, pi), ylim = c(-pi, pi),
    fill = "steelblue", color = "steelblue") +
  coord_3d()

# GIF output (default)
animate_3d(p, yaw = c(0, 360), renderer = gifski_renderer_3d())
#> Rendering 100 frames...
#> Assembling animation...

# Video output
animate_3d(p, yaw = c(0, 360), renderer = av_renderer_3d())
#> Rendering 100 frames...
#> Assembling animation...

# Keep individual frame files in a chosen directory
animate_3d(p, yaw = c(0, 360),
    renderer = file_renderer_3d(dir = file.path(tempdir(), "my_frames")))
#> Rendering 100 frames...
#> Assembling animation...
#>   [1] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0001.png"
#>   [2] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0002.png"
#>   [3] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0003.png"
#>   [4] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0004.png"
#>   [5] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0005.png"
#>   [6] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0006.png"
#>   [7] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0007.png"
#>   [8] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0008.png"
#>   [9] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0009.png"
#>  [10] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0010.png"
#>  [11] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0011.png"
#>  [12] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0012.png"
#>  [13] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0013.png"
#>  [14] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0014.png"
#>  [15] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0015.png"
#>  [16] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0016.png"
#>  [17] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0017.png"
#>  [18] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0018.png"
#>  [19] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0019.png"
#>  [20] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0020.png"
#>  [21] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0021.png"
#>  [22] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0022.png"
#>  [23] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0023.png"
#>  [24] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0024.png"
#>  [25] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0025.png"
#>  [26] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0026.png"
#>  [27] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0027.png"
#>  [28] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0028.png"
#>  [29] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0029.png"
#>  [30] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0030.png"
#>  [31] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0031.png"
#>  [32] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0032.png"
#>  [33] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0033.png"
#>  [34] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0034.png"
#>  [35] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0035.png"
#>  [36] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0036.png"
#>  [37] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0037.png"
#>  [38] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0038.png"
#>  [39] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0039.png"
#>  [40] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0040.png"
#>  [41] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0041.png"
#>  [42] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0042.png"
#>  [43] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0043.png"
#>  [44] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0044.png"
#>  [45] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0045.png"
#>  [46] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0046.png"
#>  [47] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0047.png"
#>  [48] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0048.png"
#>  [49] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0049.png"
#>  [50] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0050.png"
#>  [51] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0051.png"
#>  [52] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0052.png"
#>  [53] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0053.png"
#>  [54] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0054.png"
#>  [55] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0055.png"
#>  [56] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0056.png"
#>  [57] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0057.png"
#>  [58] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0058.png"
#>  [59] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0059.png"
#>  [60] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0060.png"
#>  [61] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0061.png"
#>  [62] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0062.png"
#>  [63] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0063.png"
#>  [64] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0064.png"
#>  [65] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0065.png"
#>  [66] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0066.png"
#>  [67] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0067.png"
#>  [68] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0068.png"
#>  [69] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0069.png"
#>  [70] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0070.png"
#>  [71] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0071.png"
#>  [72] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0072.png"
#>  [73] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0073.png"
#>  [74] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0074.png"
#>  [75] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0075.png"
#>  [76] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0076.png"
#>  [77] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0077.png"
#>  [78] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0078.png"
#>  [79] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0079.png"
#>  [80] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0080.png"
#>  [81] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0081.png"
#>  [82] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0082.png"
#>  [83] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0083.png"
#>  [84] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0084.png"
#>  [85] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0085.png"
#>  [86] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0086.png"
#>  [87] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0087.png"
#>  [88] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0088.png"
#>  [89] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0089.png"
#>  [90] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0090.png"
#>  [91] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0091.png"
#>  [92] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0092.png"
#>  [93] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0093.png"
#>  [94] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0094.png"
#>  [95] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0095.png"
#>  [96] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0096.png"
#>  [97] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0097.png"
#>  [98] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0098.png"
#>  [99] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0099.png"
#> [100] "/tmp/RtmpmPf5qs/my_frames/ggcube_frame_0100.png"
#> attr(,"class")
#> [1] "frame_files_3d" "character"     
# }
```
