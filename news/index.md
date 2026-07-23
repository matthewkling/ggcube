# Changelog

## ggcube (development version)

- `geom_hull_3d(method = "alpha")` has bug fixes related to face
  orientation and the `radius` parameter

## ggcube 0.2.0

CRAN release: 2026-07-21

- The new
  [`orbit_3d()`](https://matthewkling.github.io/ggcube/reference/orbit_3d.md)
  function builds HTML widgets that let you interactively rotate ggcube
  plots.
- Plotmath is now supported in axis text and titles; e.g.,
  `xlab(expression(italic(alpha[1])))` now behaves as expected.
- Fixed several bugs, including issues with
  [`element_rect()`](https://matthewkling.github.io/ggcube/reference/element_rect.md),
  [`guide_legend_3d()`](https://matthewkling.github.io/ggcube/reference/guide_3d.md),
  and camera-anchored lighting.

## ggcube 0.1.0

CRAN release: 2026-05-27

- Initial CRAN submission.
