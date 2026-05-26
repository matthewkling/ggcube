# Save a 3D animation to a file

Saves an animation object produced by
[`animate_3d()`](https://matthewkling.github.io/ggcube/reference/animate_3d.md)
to a file. If no animation is provided, saves the most recently rendered
animation.

## Usage

``` r
anim_save_3d(animation = NULL, filename)
```

## Arguments

- animation:

  An animation object from
  [`animate_3d()`](https://matthewkling.github.io/ggcube/reference/animate_3d.md),
  or `NULL` to use the last rendered animation.

- filename:

  Output file path.

## Value

Invisibly returns the output file path (a character string). Called
primarily for its side effect of copying the animation to `filename`.

## Examples

``` r
# \donttest{
p <- ggplot() +
  geom_function_3d(
    fun = function(x, y) sin(x) * cos(y),
    xlim = c(-pi, pi), ylim = c(-pi, pi),
    fill = "steelblue", color = "steelblue") +
  coord_3d()

animate_3d(p, yaw = c(0, 360))
#> Rendering 100 frames...
#> Assembling animation...
anim_save_3d(filename = file.path(tempdir(), "my_animation.gif"))
#> Animation saved to /tmp/RtmpL5sZEA/my_animation.gif
# }
```
