# Changelog

## ggcube (development version)

- [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  now draws axis lines from the `axis.line` theme elements, plus a new
  `axis.line.z`. Each line runs along the cube edge carrying its axis’s
  ticks and labels, so it appears only alongside rendered panels. Axis
  lines are blank in most complete themes, but plots using
  [`theme_classic()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  now show them. Lines run toward increasing data values, so arrows set
  with `element_line(arrow = )` point up each axis. They are
  depth-scaled with the cube edges, under the `"border"` key of
  `coord_3d(scale_depth = )`.
- Pairwise depth sorting (`sort_method = "pairwise"`, and `"auto"` for
  small layers) now splits polygons that pass through one another, so
  intersecting planes and surfaces render with correct occlusion along
  their line of intersection. Outlines are drawn only along original
  polygon edges, not along the cuts.
- Pairwise sorting now determines the order of a polygon and any element
  overlapping it on screen (polygon, segment, point, or text) exactly
  from their 3D geometry where possible, rather than from sampled
  screen-space depths. This fixes misordering under perspective
  projection, which was common for elements close to large polygons.
- Segments that pass through polygons are now split at the exact 3D
  crossing point. Previously, under perspective projection, most such
  crossings went undetected and the rest were split at visibly wrong
  positions.
- When pairwise sorting finds cyclic overlaps, it now drops only
  constraints that are part of a cycle. Previously it could discard many
  unrelated constraints, and pairwise sorting of large layers is now
  considerably faster when cycles occur.
- Fixed a spurious “Cycle breaking exceeded maximum iterations” warning
  for pairwise-sorted layers containing a single element.
- Fixed a bug where pairwise sorting never constrained the order of a
  segment relative to a polygon it overlapped on screen, so segments
  (e.g. residual lines and
  [`annotate_3d()`](https://matthewkling.github.io/ggcube/reference/annotate_3d.md)
  segments) could be drawn on the wrong side of polygons.

## ggcube 0.3.0

CRAN release: 2026-09-05

Changes to default behaviors, which slightly alter styling relative to
prior version:

- [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  now draws axis ticks by default, whereas previously axis ticks were
  not implemented. They can be removed or styled with the standard
  `axis.ticks` and `axis.ticks.length` theme elements plus a new
  `axis.ticks.z`.
- Axis text now gets depth scaling, i.e. perspective-based sizing, by
  default. (Axis titles remain unscaled.) You can use
  `coord_3d(scale_depth = c(text = FALSE))` to opt out.
- Axis text and title placement is now computed at draw time from
  measured text dimensions. This fixes inexact size estimates that
  previously sometimes led to text overlapping titles and/or axes on
  small devices.

New options:

- Lighting can now be set for a whole plot by adding
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
  to it,
  e.g. `ggplot(...) + coord_3d() + light(direction = c(1, 0, 0))`. This
  is the recommended way to specify plot-level lighting, and works
  regardless of where it appears in the plot expression. The `light`
  argument of
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  continues to work; supplying both is an error.
- `light("none")` is a new way to disable lighting, equivalent to the
  string `"none"` accepted by `light` arguments.
- `scale_depth` arguments are now more customizable: in 3D layer
  functions you can now pass continuous values to reduce or exaggerate
  the effect (rather than just a logical flag), and in
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  you have more granular control over scaling for gridlines, ticks, and
  axis text.

Bug fixes:

- Fixed a bug where
  [`scale_z_continuous()`](https://matthewkling.github.io/ggcube/reference/scale_z_continuous.md)
  and
  [`scale_z_discrete()`](https://matthewkling.github.io/ggcube/reference/scale_z_discrete.md)
  stored z scales in a package-level cache, causing plots to sometimes
  contaminate each other’s z-axis ranges. Now, the z scale is stored
  directly in the plot being built, preventing cross-contamination.
- [`geom_hull_3d()`](https://matthewkling.github.io/ggcube/reference/geom_hull_3d.md)’s
  `"alpha"` method got some bug fixes related to face orientation and
  the `radius` parameter.

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
