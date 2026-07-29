# Light param

Light param

## Arguments

- light:

  A lighting specification object created by
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md),
  `light("none")` or the string `"none"` to disable lighting, or `NULL`
  to inherit plot-level lighting. Set plot-level lighting by adding
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
  to the plot, and layer-specific lighting via the `light` argument of
  `geom_*_3d()` functions, which takes precedence.
