# ggcube 0.3.0

Changes to default behaviors, which slightly alter styling relative to prior version:

* `coord_3d()` now draws axis ticks by default, whereas previously axis ticks were not implemented. They can be removed or styled with the standard `axis.ticks` and `axis.ticks.length` theme elements plus a new `axis.ticks.z`.
* Axis text now gets depth scaling, i.e. perspective-based sizing, by default. (Axis titles remain unscaled.) You can use `coord_3d(scale_depth = c(text = FALSE))` to opt out.
* Axis text and title placement is now computed at draw time from measured text dimensions. This fixes inexact size estimates that previously sometimes led to text overlapping titles and/or axes on small devices.

New options:

* Lighting can now be set for a whole plot by adding `light()` to it, e.g. `ggplot(...) + coord_3d() + light(direction = c(1, 0, 0))`. This is the recommended way to specify plot-level lighting, and works regardless of where it appears in the plot expression. The `light` argument of `coord_3d()` continues to work; supplying both is an error.
* `light("none")` is a new way to disable lighting, equivalent to the string `"none"` accepted by `light` arguments.
* `scale_depth` arguments are now more customizable: in 3D layer functions you can now pass continuous values to reduce or exaggerate the effect (rather than just a logical flag), and in `coord_3d()` you have more granular control over scaling for gridlines, ticks, and axis text.

Bug fixes:

* Fixed a bug where `scale_z_continuous()` and `scale_z_discrete()` stored z scales in a package-level cache, causing plots to sometimes contaminate each other's z-axis ranges. Now, the z scale is stored directly in the plot being built, preventing cross-contamination.
* `geom_hull_3d()`'s `"alpha"` method got some bug fixes related to face orientation and the `radius` parameter.

# ggcube 0.2.0

* The new `orbit_3d()` function builds HTML widgets that let you interactively rotate ggcube plots.
* Plotmath is now supported in axis text and titles; e.g., `xlab(expression(italic(alpha[1])))` now behaves as expected.
* Fixed several bugs, including issues with `element_rect()`, `guide_legend_3d()`, and camera-anchored lighting.

# ggcube 0.1.0

* Initial CRAN submission.
