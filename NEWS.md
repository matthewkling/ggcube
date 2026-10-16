# ggcube (development version)

* New `direction()` builds the length-3 direction vectors used by `coord_3d()` and `light()` from an azimuth and an elevation in degrees, for cases where angles are easier to reason about than components. Azimuth is a compass bearing measured clockwise from the positive y axis, matching terrain shading conventions.
* `coord_3d()` gains a `direction` argument: an alternative to `pitch`, `roll`, and `yaw` that specifies the direction the scene is viewed from, so `direction = c(1, 1, 1)` views the cube from its xmax-ymax-zmax corner. The z axis is kept pointing up the plot, and supplying `direction` alongside any of the Euler angles is an error. The default rotation corresponds to `direction(210, 30)`.
* `print.light()` now reports a directional light's azimuth and elevation alongside its direction vector.
* `orbit_3d()` and `animate_3d()` gain `light_azimuth` and `light_elevation` arguments for moving the light, alone or together with the camera. They follow the same conventions as `direction()` and take the same forms as the rotation arguments, so `orbit_3d(p, yaw = c(360, 0), light_elevation = c(0, 90))` spins the camera with horizontal drag and raises the light with vertical drag. They move the plot-level light, and error if lighting is disabled, positional, or overridden by a layer's own `light`.

* Every `geom_*_3d()` function now has a documented `annotate` argument for embedding `annotate_3d()` annotations in the layer. `geom_text_3d()` with its default `method = "billboard"` now supports annotations; previously it ignored them with an "unknown parameter" warning.
* Fixed a bug where `geom_text_3d()` (billboard method) could draw labels at each other's depth-scaled sizes under perspective projection.
* Text annotations gain an `on_top` argument: `annotate_3d("text", ..., on_top = TRUE)` draws labels after everything else in the layer, instead of depth-sorting them, so geometry never hides or clips them.
* New `annotate_3d("polygon")` draws filled polygons from their vertices, with `group` to draw several in one call. Like planes, they are lit from the side facing the viewer and never removed by back-face culling.
* New `annotate_3d("line")` draws infinite lines through a point (`x`, `y`, `z`) in a direction (`dx`, `dy`, `dz`), clipped to the panel. Like planes, lines don't expand the plot's axis ranges.
* Annotation segments no longer take the arrowheads of the layer they're in (e.g. `geom_segment_3d(arrow = )`); they're drawn with an arrow only when given their own via `annotate_3d(arrow = )`.
* `geom_segment_3d()` and `geom_path_3d()` gain an `arrow.fill` argument. Closed arrowheads are now filled with the line colour by default, as in ggplot2; previously they were drawn hollow.
* Pairwise sorting now always draws points, segments, and text lying in the plane of a planar polygon (e.g. a line drawn along a plane) on top of it. Previously, especially under perspective projection, they were often hidden behind it.
* New `annotate_3d("plane")` draws planes clipped to the panel. Planes can be given as `xintercept`, `yintercept`, or `zintercept` (perpendicular to an axis); as `intercept`, `slope_x`, and `slope_y` (`z = intercept + slope_x * x + slope_y * y`, as from a linear model); or as general coefficients `a * x + b * y + c * z = d`. All forms are vectorized, so e.g. a linear system can be drawn in one call. Planes are depth-sorted (and, with pairwise sorting, split where they intersect) with the layer's geometry, lit from the side facing the viewer, never removed by back-face culling, and don't expand the plot's axis ranges.
* Fixed an error when a `geom_point_3d()` layer with pairwise sorting contained annotations. Pairwise sorting now also treats the reference lines of point layers as segments rather than degenerate polygons.
* New `annotate_3d("axes")` draws x, y, and z axis lines through an origin (default `0`), each spanning the panel's full range, with arrowheads and labels at their high ends. Axes are depth-sorted with the layer's geometry, and are built from the panel's limits at draw time, so they don't expand the plot's axis ranges.
* `annotate_3d("segment")` gains an `arrow` argument, taking a `grid::arrow()` specification. Closed arrowheads are filled with the segment colour, and when a segment is split where it passes through a polygon, its arrowheads stay at the original ends.
* `coord_3d()` now draws axis lines from the `axis.line` theme elements, plus a new `axis.line.z`. Each line runs along the cube edge carrying its axis's ticks and labels, so it appears only alongside rendered panels. Axis lines are blank in most complete themes, but plots using `theme_classic()` now show them. Lines run toward increasing data values, so arrows set with `element_line(arrow = )` point up each axis. They are depth-scaled with the cube edges, under the `"border"` key of `coord_3d(scale_depth = )`.
* Pairwise depth sorting (`sort_method = "pairwise"`, and `"auto"` for small layers) now splits polygons that pass through one another, so intersecting planes and surfaces render with correct occlusion along their line of intersection. Outlines are drawn only along original polygon edges, not along the cuts.
* Pairwise sorting now determines the order of a polygon and any element overlapping it on screen (polygon, segment, point, or text) exactly from their 3D geometry where possible, rather than from sampled screen-space depths. This fixes misordering under perspective projection, which was common for elements close to large polygons.
* Segments that pass through polygons are now split at the exact 3D crossing point. Previously, under perspective projection, most such crossings went undetected and the rest were split at visibly wrong positions.
* When pairwise sorting finds cyclic overlaps, it now drops only constraints that are part of a cycle. Previously it could discard many unrelated constraints, and pairwise sorting of large layers is now considerably faster when cycles occur.
* Fixed a spurious "Cycle breaking exceeded maximum iterations" warning for pairwise-sorted layers containing a single element.
* Fixed a bug where pairwise sorting never constrained the order of a segment relative to a polygon it overlapped on screen, so segments (e.g. residual lines and `annotate_3d()` segments) could be drawn on the wrong side of polygons.

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
