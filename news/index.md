# Changelog

## ggcube (development version)

- The new
  [`flipbook_3d()`](https://matthewkling.github.io/ggcube/reference/flipbook_3d.md)
  function builds HTML widgets that let you interactively rotate ggcube
  figures.
- Plotmath is now supported in axis text and titles; e.g.,
  `xlab(expression(italic(alpha[1])))` now behaves as expected.
- Bugfix:
  [`element_rect()`](https://matthewkling.github.io/ggcube/reference/element_rect.md)
  is now a proper ggplot2 theme element with expected behavior.
- Bugfix:
  [`guide_legend_3d()`](https://matthewkling.github.io/ggcube/reference/guide_3d.md)
  now works with a wider range of ggplot2 versions.

## ggcube 0.1.0

CRAN release: 2026-05-27

- Initial CRAN submission.
