# ggcube (development version)

## Depth sorting

Changes to default behaviors, which alter the rendering of some plots relative to the prior version:

* All ggcube layers in a panel are now depth-sorted together, as one scene, so the geometry of one layer can pass in front of or behind that of another: points behind a surface drawn by another layer are hidden by it, a plane drawn by one layer cuts through segments drawn by another, and so on. Previously each layer was sorted on its own and drawn on top of the layers before it. Layers from other packages are drawn as before, and separate the ggcube layers on either side of them into separate scenes. Coplanar polygons from different layers are drawn in layer order.
* Depth sorting is now always pairwise: every pair of elements that overlap on screen is ordered from their 3D geometry. The faster but approximate `"painter"` method, and the `"auto"` choice between the two, are gone, along with the `sort_method` argument of layer functions, which is deprecated and has no effect. Plots of layers that used painter sorting by default, such as `geom_point_3d()`, `geom_path_3d()`, `geom_segment_3d()`, and large surfaces, are now drawn correctly where their elements overlap.
* Point markers and lines are now ordered using their drawn size: overlapping markers are drawn farther first, a marker overlapping the edge of a polygon is ordered against it, and lines are ordered where their drawn widths overlap. Previously markers were treated as points, so two overlapping markers could be drawn in either order. Elements are now ordered when the plot is drawn, since their drawn size depends on the size of the panel.

Performance:

* Pairwise sorting has been rewritten in C++, and compares only elements that overlap on screen rather than every pair, so its time and memory grow with the number of overlapping pairs rather than the square of the number of elements. Sorting is now about as fast as painter sorting was: a scatterplot of 10,000 points, which previously took minutes to sort pairwise, now renders in about two seconds, and a 100 x 100 surface, which previously ran out of memory, in about six. ggcube now contains compiled code.

## Annotations

* `annotate_3d()` now returns a layer, added to a plot with `+` like ggplot2's `annotate()`, e.g. `p + annotate_3d("point", x = 0, y = 0, z = 0)`. Since all 3D layers in a panel are now depth-sorted together, annotations no longer need to be embedded in another layer. Annotations now appear in every facet panel, rather than only the first.
* The `annotate` argument of layer functions is deprecated. Annotations given through it are now added to the plot as layers of their own.
* New `geom_plane_3d()` draws planes clipped to the panel. Planes can be given as `xintercept`, `yintercept`, or `zintercept` (perpendicular to an axis); as `intercept`, `slope_x`, and `slope_y` (`z = intercept + slope_x * x + slope_y * y`, as from a linear model); or as general coefficients `a * x + b * y + c * z = d`. Given as parameters, all forms are vectorized, so e.g. a linear system can be drawn in one call; the regression and general forms can also be mapped from data. Planes are split where they intersect other polygons, lit from the side facing the viewer, never removed by back-face culling, and don't expand the plot's axis ranges.
* New `geom_refline_3d()` draws infinite lines through a point (`x`, `y`, `z`) in a direction (`dx`, `dy`, `dz`), clipped to the panel, such as the line spanned by a vector. Directions can be given as parameters or mapped from data. Like planes, lines don't expand the plot's axis ranges.
* New `geom_axes_3d()` draws x, y, and z axis lines through an origin (default `0`), each spanning the panel's full range, with arrowheads and labels at their high ends. Unlike the axes on the edges of the panel, they are part of the 3D scene, and are depth-sorted with it.
* `annotate_3d()` gains the types `"polygon"` (filled polygons from their vertices, with `group` to draw several in one call), and `"axes"`, `"refline"`, and `"plane"`, which build layers with the new geoms above. Segment annotations take an `arrow`, like `geom_segment_3d()`.
* `geom_point_3d()`, `geom_segment_3d()`, `geom_path_3d()`, `geom_polygon_3d()`, `geom_text_3d()`, and the new geoms gain an `on_top` argument, which draws the layer on top of the rest of the scene instead of depth-sorting it with it, so other geometry never hides or cuts it. This keeps labels legible wherever they fall, as in `annotate_3d("text", ..., on_top = TRUE)`.

## Reference elements

* New `geom_dropline_3d()` draws lines from points to their projections on faces of the panel, given by `faces` (default `"zmin"`, the floor).
* `geom_point_3d()` gains a `flat` argument: with `flat = TRUE`, points placed on a face of the panel by `position_on_face()` are drawn as markers lying flat in that face, like shadows, rather than as symbols facing the viewer. Flat markers can be open, solid, or filled circles, squares, diamonds, or triangles, match the size of ordinary markers at the centre of the panel, and are drawn in true perspective, outlines included, regardless of `scale_depth`. With `position_on_face(c("3D", "zmin"))`, one layer draws the points and their shadows.
* The reference element arguments of `geom_point_3d()` and `stat_point_3d()` (`raw_points`, and those starting `ref_`) are deprecated in favour of separate layers: drop lines with `geom_dropline_3d()`, and reference points with `geom_point_3d(position = position_on_face())`, adding `flat = TRUE` for circles. They still work, with a warning.
* The `points` argument of `geom_smooth_3d()` and `stat_smooth_3d()`, and the `point_*` arguments that style the points, are deprecated. Draw the data points with a separate layer, e.g. `+ geom_point_3d()`; points requested through `points = TRUE` are now added as such a layer.

## Other changes

* New `geom_isosurface_3d()` and `stat_isosurface_3d()` draw isosurfaces of a scalar field sampled on a 3D grid, the 3D analog of contour lines: map the field to the new `value` aesthetic, and give the field values at which to draw surfaces as `levels`. Grids may be unevenly spaced. Surfaces face outward from the regions they enclose, by default where `value` exceeds the level (`inside = "above"`), or with `inside = "below"` where it's less, and are triangulated by marching cubes using the rmarchingcubes package, which must be installed.
* New `geom_isodensity_3d()` and `stat_isodensity_3d()` estimate the density of a 3D point cloud, and draw surfaces enclosing given fractions of it (`probs`, by default `0.5`), the 3D analog of `geom_density_2d()`. Each group gets its own density, and points can be weighted. Bandwidths `h` follow the convention of `stat_density_3d()`, and default to the normal reference rule for three dimensions. Like isosurfaces, these require the rmarchingcubes package. Both kinds of surface are cut cleanly where they cross limits set on the plot's scales, e.g. with `xlim()` or `zlim()`, and with `cap = TRUE` are closed there with flat caps showing their cross sections. Isosurfaces are also capped where they meet the edges of their grid.
* New `geom_isofunction_3d()` and `stat_isofunction_3d()` draw isosurfaces of a function of three variables, evaluated over a domain given by `xlim`, `ylim`, and `zlim`: by default the implicit surface `fun(x, y, z) = 0`, so that e.g. `function(x, y, z) x^2 + y^2 + z^2 - 1` draws a unit ball. They are to `geom_isosurface_3d()` what `geom_function_3d()` is to `geom_surface_3d()`.
* ggcube now requires ggplot2 4.0.0 or later.
* `geom_polygon_3d()` now lights each polygon using the normal of its own plane, oriented by its vertex winding. Lighting previously treated every polygon as a tile of a surface `z = f(x, y)`, so vertical polygons were lit as if horizontal, and polygons wound clockwise were lit as their reverse side. With the default lighting, polygons are now lit the same way whatever their winding order.
* Plots in which no layer has data now draw an empty cube, as ggplot2 draws an empty panel. They previously failed with an unrelated error, which also hid the warning from any layer whose stat failed, or that found nothing to draw.
* `light("direct")` now lights back faces that face the light. Light values were clamped to be non-negative before back faces had their sign flipped, so every visible back face, such as the underside of a surface seen from below, was drawn black regardless of where the light came from. A nonzero `backface_offset` now shifts direct light values before they are clamped, as it does for diffuse lighting.
* `geom_contour_3d()` and `geom_ridgeline_3d()` now work with facets. They processed every panel's data together, so faceted plots mixed panels' contours and could leave panels empty.
* ggcube now requires systemfonts 1.2.0 or later. Earlier versions lack the glyph outline functions ggcube uses, so loading ggcube against them failed.
* New `direction()` builds the length-3 direction vectors used by `coord_3d()` and `light()` from an azimuth and an elevation in degrees, for cases where angles are easier to reason about than components. Azimuth is a compass bearing measured clockwise from the positive y axis, matching terrain shading conventions.
* `coord_3d()` gains a `direction` argument: an alternative to `pitch`, `roll`, and `yaw` that specifies the direction the scene is viewed from, so `direction = c(1, 1, 1)` views the cube from its xmax-ymax-zmax corner. The z axis is kept pointing up the plot, and supplying `direction` alongside any of the Euler angles is an error. The default rotation corresponds to `direction(210, 30)`.
* `print.light()` now reports a directional light's azimuth and elevation alongside its direction vector.
* `orbit_3d()` and `animate_3d()` gain `light_azimuth` and `light_elevation` arguments for moving the light, alone or together with the camera. They follow the same conventions as `direction()` and take the same forms as the rotation arguments, so `orbit_3d(p, yaw = c(360, 0), light_elevation = c(0, 90))` spins the camera with horizontal drag and raises the light with vertical drag. They move the plot-level light, and error if lighting is disabled, positional, or overridden by a layer's own `light`.
* New `facet_wrap_views()` and `facet_grid_views()` show a plot from several angles at once, with one panel per view. They vary viewing angles the way `facet_wrap()` and `facet_grid()` vary data variables: `p + facet_wrap_views(~yaw)` shows views around the vertical axis, and `p + facet_grid_views(roll ~ yaw)` varies roll down the rows and yaw across the columns. Light angles can be faceted too. Each angle in the formula gets a sensible default range, which its argument can override, and every panel is framed identically.
* `orbit_3d()` can now show the angles of the current view, so a view found by dragging can be copied into `coord_3d()`. Press `r` on any orbit to show or hide the readout, or use `readout = TRUE` to have it showing when the orbit opens. When the orbit moves the light, the readout also shows the light's azimuth and elevation, for use with `direction()`. Angles are shown in the terms the orbit was given, so `yaw = c(0, 360)` reports values from 0 to 360.

* Fixed a bug where `geom_text_3d()` (billboard method) could draw labels at each other's depth-scaled sizes under perspective projection.
* `geom_segment_3d()` and `geom_path_3d()` gain an `arrow.fill` argument. Closed arrowheads are now filled with the line colour by default, as in ggplot2; previously they were drawn hollow.
* Points, segments, and text lying in the plane of a planar polygon (e.g. a line drawn along a plane) are now always drawn on top of it. Previously, especially under perspective projection, they were often hidden behind it.
* Depth sorting now treats the reference lines of point layers as segments rather than degenerate polygons.
* `coord_3d()` now draws axis lines from the `axis.line` theme elements, plus a new `axis.line.z`. Each line runs along the cube edge carrying its axis's ticks and labels, so it appears only alongside rendered panels. Axis lines are blank in most complete themes, but plots using `theme_classic()` now show them. Lines run toward increasing data values, so arrows set with `element_line(arrow = )` point up each axis. They are depth-scaled with the cube edges, under the `"border"` key of `coord_3d(scale_depth = )`.
* Depth sorting now splits polygons that pass through one another, so intersecting planes and surfaces render with correct occlusion along their line of intersection. Outlines are drawn only along original polygon edges, not along the cuts.
* Depth sorting now determines the order of a polygon and any element overlapping it on screen (polygon, segment, point, or text) exactly from their 3D geometry where possible, rather than from sampled screen-space depths. This fixes misordering under perspective projection, which was common for elements close to large polygons.
* Segments that pass through polygons are now split at the exact 3D crossing point. Previously, under perspective projection, most such crossings went undetected and the rest were split at visibly wrong positions.
* When depth sorting finds cyclic overlaps, it now drops only constraints that are part of a cycle. Previously it could discard many unrelated constraints, and pairwise sorting of large layers is now considerably faster when cycles occur.
* Fixed a spurious "Cycle breaking exceeded maximum iterations" warning for layers containing a single element.
* Fixed a bug where depth sorting never constrained the order of a segment relative to a polygon it overlapped on screen, so segments (e.g. residual lines and annotation segments) could be drawn on the wrong side of polygons.

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
