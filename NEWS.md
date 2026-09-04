# ggcube (development version)

* Lighting can now be set for a whole plot by adding `light()` to it, e.g. `ggplot(...) + coord_3d() + light(direction = c(1, 0, 0))`. This is the recommended way to specify plot-level lighting, and works regardless of where it appears in the plot expression. The `light` argument of `coord_3d()` continues to work; supplying both is an error.
* `light("none")` is a new way to disable lighting, equivalent to the string `"none"` accepted by `light` arguments.
* `geom_hull_3d()`'s `"alpha"` method got some bug fixes related to face orientation and the `radius` parameter.
* Fixed a bug where `scale_z_continuous()` and `scale_z_discrete()` stored the scale they built in a package-level cache, so that building several 3D plots before rendering them could make a plot pick up another plot's z-axis range. The z scale is now read from the plot being built, like every other scale.
* Axis label and title placement is now computed at draw time from measured text dimensions rather than estimated at build time. Spacing no longer varies with the size of the rendered device, and labels no longer crowd the cube on small devices. Label positions will shift slightly compared with previous versions.
* `coord_3d()` gains a `scale_depth` argument controlling how strongly gridlines and axis text respond to viewing distance under perspective projection.
* Axis tick labels are now depth-scaled under perspective projection by default. (Axis titles remain unscaled.) Use `coord_3d(scale_depth = c(text = 0))` to opt out.

# ggcube 0.2.0

* The new `orbit_3d()` function builds HTML widgets that let you interactively rotate ggcube plots.
* Plotmath is now supported in axis text and titles; e.g., `xlab(expression(italic(alpha[1])))` now behaves as expected.
* Fixed several bugs, including issues with `element_rect()`, `guide_legend_3d()`, and camera-anchored lighting.

# ggcube 0.1.0

* Initial CRAN submission.
