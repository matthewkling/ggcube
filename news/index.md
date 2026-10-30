# Changelog

## ggcube (development version)

### Depth sorting

Changes to default behaviors, which alter the rendering of some plots
relative to the prior version:

- All ggcube layers in a panel are now depth-sorted together, as one
  scene, so the geometry of one layer can pass in front of or behind
  that of another: points behind a surface drawn by another layer are
  hidden by it, a plane drawn by one layer cuts through segments drawn
  by another, and so on. Previously each layer was sorted on its own and
  drawn on top of the layers before it. Layers from other packages are
  drawn as before, and separate the ggcube layers on either side of them
  into separate scenes. Coplanar polygons from different layers are
  drawn in layer order.
- Depth sorting is now always pairwise: every pair of elements that
  overlap on screen is ordered from their 3D geometry. The faster but
  approximate `"painter"` method, and the `"auto"` choice between the
  two, are gone, along with the `sort_method` argument of layer
  functions, which is deprecated and has no effect. Plots of layers that
  used painter sorting by default, such as
  [`geom_point_3d()`](https://matthewkling.github.io/ggcube/reference/geom_point_3d.md),
  [`geom_path_3d()`](https://matthewkling.github.io/ggcube/reference/geom_path_3d.md),
  [`geom_segment_3d()`](https://matthewkling.github.io/ggcube/reference/geom_segment_3d.md),
  and large surfaces, are now drawn correctly where their elements
  overlap.
- Point markers and lines are now ordered using their drawn size:
  overlapping markers are drawn farther first, a marker overlapping the
  edge of a polygon is ordered against it, and lines are ordered where
  their drawn widths overlap. Previously markers were treated as points,
  so two overlapping markers could be drawn in either order. Elements
  are now ordered when the plot is drawn, since their drawn size depends
  on the size of the panel.

Performance:

- Pairwise sorting has been rewritten in C++, and compares only elements
  that overlap on screen rather than every pair, so its time and memory
  grow with the number of overlapping pairs rather than the square of
  the number of elements. Sorting is now about as fast as painter
  sorting was: a scatterplot of 10,000 points, which previously took
  minutes to sort pairwise, now renders in about two seconds, and a 100
  x 100 surface, which previously ran out of memory, in about six.
  ggcube now contains compiled code.

### Annotations

- [`annotate_3d()`](https://matthewkling.github.io/ggcube/reference/annotate_3d.md)
  now returns a layer, added to a plot with `+` like ggplot2’s
  [`annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html),
  e.g. `p + annotate_3d("point", x = 0, y = 0, z = 0)`. Since all 3D
  layers in a panel are now depth-sorted together, annotations no longer
  need to be embedded in another layer. Annotations now appear in every
  facet panel, rather than only the first.
- The `annotate` argument of layer functions is deprecated. Annotations
  given through it are now added to the plot as layers of their own.
- New
  [`geom_plane_3d()`](https://matthewkling.github.io/ggcube/reference/geom_plane_3d.md)
  draws planes clipped to the panel. Planes can be given as
  `xintercept`, `yintercept`, or `zintercept` (perpendicular to an
  axis); as `intercept`, `slope_x`, and `slope_y`
  (`z = intercept + slope_x * x + slope_y * y`, as from a linear model);
  or as general coefficients `a * x + b * y + c * z = d`. Given as
  parameters, all forms are vectorized, so e.g. a linear system can be
  drawn in one call; the regression and general forms can also be mapped
  from data. Planes are split where they intersect other polygons, lit
  from the side facing the viewer, never removed by back-face culling,
  and don’t expand the plot’s axis ranges.
- New
  [`geom_refline_3d()`](https://matthewkling.github.io/ggcube/reference/geom_refline_3d.md)
  draws infinite lines through a point (`x`, `y`, `z`) in a direction
  (`dx`, `dy`, `dz`), clipped to the panel, such as the line spanned by
  a vector. Directions can be given as parameters or mapped from data.
  Like planes, lines don’t expand the plot’s axis ranges.
- New
  [`geom_axes_3d()`](https://matthewkling.github.io/ggcube/reference/geom_axes_3d.md)
  draws x, y, and z axis lines through an origin (default `0`), each
  spanning the panel’s full range, with arrowheads and labels at their
  high ends. Unlike the axes on the edges of the panel, they are part of
  the 3D scene, and are depth-sorted with it.
- [`annotate_3d()`](https://matthewkling.github.io/ggcube/reference/annotate_3d.md)
  gains the types `"polygon"` (filled polygons from their vertices, with
  `group` to draw several in one call), and `"axes"`, `"refline"`, and
  `"plane"`, which build layers with the new geoms above. Segment
  annotations take an `arrow`, like
  [`geom_segment_3d()`](https://matthewkling.github.io/ggcube/reference/geom_segment_3d.md).
- [`geom_point_3d()`](https://matthewkling.github.io/ggcube/reference/geom_point_3d.md),
  [`geom_segment_3d()`](https://matthewkling.github.io/ggcube/reference/geom_segment_3d.md),
  [`geom_path_3d()`](https://matthewkling.github.io/ggcube/reference/geom_path_3d.md),
  [`geom_polygon_3d()`](https://matthewkling.github.io/ggcube/reference/geom_polygon_3d.md),
  [`geom_text_3d()`](https://matthewkling.github.io/ggcube/reference/geom_text_3d.md),
  and the new geoms gain an `on_top` argument, which draws the layer on
  top of the rest of the scene instead of depth-sorting it with it, so
  other geometry never hides or cuts it. This keeps labels legible
  wherever they fall, as in `annotate_3d("text", ..., on_top = TRUE)`.

### Reference elements

- New
  [`geom_dropline_3d()`](https://matthewkling.github.io/ggcube/reference/geom_dropline_3d.md)
  draws lines from points to their projections on faces of the panel,
  given by `faces` (default `"zmin"`, the floor).
- [`geom_point_3d()`](https://matthewkling.github.io/ggcube/reference/geom_point_3d.md)
  gains a `flat` argument: with `flat = TRUE`, points placed on a face
  of the panel by
  [`position_on_face()`](https://matthewkling.github.io/ggcube/reference/position_on_face.md)
  are drawn as markers lying flat in that face, like shadows, rather
  than as symbols facing the viewer. Flat markers can be open, solid, or
  filled circles, squares, diamonds, or triangles, match the size of
  ordinary markers at the centre of the panel, and are drawn in true
  perspective, outlines included, regardless of `scale_depth`. With
  `position_on_face(c("3D", "zmin"))`, one layer draws the points and
  their shadows.
- The reference element arguments of
  [`geom_point_3d()`](https://matthewkling.github.io/ggcube/reference/geom_point_3d.md)
  and
  [`stat_point_3d()`](https://matthewkling.github.io/ggcube/reference/geom_point_3d.md)
  (`raw_points`, and those starting `ref_`) are deprecated in favour of
  separate layers: drop lines with
  [`geom_dropline_3d()`](https://matthewkling.github.io/ggcube/reference/geom_dropline_3d.md),
  and reference points with
  `geom_point_3d(position = position_on_face())`, adding `flat = TRUE`
  for circles. They still work, with a warning.
- The `points` argument of
  [`geom_smooth_3d()`](https://matthewkling.github.io/ggcube/reference/geom_smooth_3d.md)
  and
  [`stat_smooth_3d()`](https://matthewkling.github.io/ggcube/reference/geom_smooth_3d.md),
  and the `point_*` arguments that style the points, are deprecated.
  Draw the data points with a separate layer, e.g. `+ geom_point_3d()`;
  points requested through `points = TRUE` are now added as such a
  layer.

### Scale limits

- Limits on the z scale, e.g. from
  [`zlim()`](https://matthewkling.github.io/ggcube/reference/zlim.md),
  now apply before a layer’s stat is computed, as ggplot2 applies limits
  on x and y: stats that summarize observations, such as hulls, smooths,
  isodensity surfaces, voxels, columns, points, segments, paths, and
  text, now use only the data within the limits, and remove the rest
  with a warning. Previously z limits were applied only afterward, so
  that e.g. a hull or smooth was computed from all the data and then
  drawn with its vertices beyond the limits missing. The z scale’s `oob`
  function is respected, e.g. `oob = scales::squish` moves values to the
  limits instead.
- Surfaces and other polygons are now cut cleanly where they cross
  limits on any axis, with new vertices on the limits, rather than drawn
  with their vertices beyond the limits dropped. This applies to
  [`geom_polygon_3d()`](https://matthewkling.github.io/ggcube/reference/geom_polygon_3d.md),
  [`geom_surface_3d()`](https://matthewkling.github.io/ggcube/reference/stat_surface_3d.md),
  [`geom_contour_3d()`](https://matthewkling.github.io/ggcube/reference/geom_contour_3d.md),
  [`geom_ridgeline_3d()`](https://matthewkling.github.io/ggcube/reference/geom_ridgeline_3d.md),
  and the layers drawn with them, such as
  [`geom_function_3d()`](https://matthewkling.github.io/ggcube/reference/stat_function_3d.md),
  [`geom_smooth_3d()`](https://matthewkling.github.io/ggcube/reference/geom_smooth_3d.md),
  [`geom_density_3d()`](https://matthewkling.github.io/ggcube/reference/stat_density_3d.md),
  and
  [`geom_distributions_3d()`](https://matthewkling.github.io/ggcube/reference/stat_distributions_3d.md),
  and to text drawn with `method = "polygon"`. Since rows of gridded
  surface data are the surface’s vertices, z limits cut those surfaces
  rather than removing rows; x and y limits still trim the grid before
  the stat. Columns, bars, and voxels that extend beyond the limits are
  removed whole, as ggplot2 removes bars, rather than cut.
- Polygons with missing vertices, such as tiles of a smooth that can’t
  be predicted at all their corners, are now removed whole rather than
  drawn with their remaining vertices.
- [`geom_path_3d()`](https://matthewkling.github.io/ggcube/reference/geom_path_3d.md)
  now breaks paths at points that are missing or outside the scale
  limits, as
  [`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  does, rather than joining the points on either side.
- [`geom_point_3d()`](https://matthewkling.github.io/ggcube/reference/geom_point_3d.md)
  no longer fails when points lie outside limits set on the z scale.

### Lighting

Changes to default behaviors, which alter the rendering of most lit
plots relative to the prior version:

- Lighting now changes the lightness of colors in the perceptual OKLab
  color space, keeping their hue and as much of their chroma as
  possible. Colors already at full brightness, such as `"darkorange"`,
  can now be highlighted, where they previously stayed flat on lit
  faces, and dark saturated colors, such as `"darkgreen"`, are lightened
  to lighter versions of themselves rather than to neon. Highlights no
  longer wash out to white.
- Faces at 45 degrees to the light now keep their base color exactly,
  and faces turned further toward or away from it are highlighted or
  shadowed. Previously faces perpendicular to the light kept their base
  color, so nearly every upward-facing face was strongly highlighted.
- Shadows are now stronger than highlights by default (`shadow = 0.8`,
  `highlight = 0.6`), since fading to black looks more natural than
  fading to white.
- The default light now comes from 60 degrees above the horizon,
  `direction(270, 60)`, rather than about 63 degrees (`c(-.5, 0, 1)`).
  The tops and two visible sides of columns and voxels now get three
  distinct shades.
- [`geom_contour_3d()`](https://matthewkling.github.io/ggcube/reference/geom_contour_3d.md)
  and
  [`geom_ridgeline_3d()`](https://matthewkling.github.io/ggcube/reference/geom_ridgeline_3d.md)
  are no longer lit by default. Their faces all point the same way, so
  lighting could only shift all their colors away from those specified.
  Lighting set explicitly, with `+ light()`, `coord_3d(light = )`, or a
  layer’s `light` argument, still applies.
- Polygon layers now outline opaque polygons in their fill color by
  default, as
  [`geom_isosurface_3d()`](https://matthewkling.github.io/ggcube/reference/geom_isosurface_3d.md)
  already did, which hides the seams that antialiasing leaves between
  adjacent polygons, and don’t outline translucent polygons. These
  outlines are lit along with the fill, so `light(color = FALSE)`
  affects only border colors that are mapped or set. Previously surfaces
  had no outlines, and hulls, columns, voxels, and other polygons had
  grey outlines. Setting or mapping `colour` works as before. Contours
  and ridgelines keep their outlines.
- The default fill of surfaces, planes, and polygon annotations is now
  `"grey50"`, matching other polygon layers, rather than `"grey60"`.

New options:

- [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
  gains `ramp`, which decides how brightness changes as a surface turns
  away from the light, and `tone`, which decides what lighter and darker
  look like for a given color. Both take names, or objects from the new
  experimental functions
  [`ramp_smooth()`](https://matthewkling.github.io/ggcube/reference/ramps.md),
  [`ramp_hard()`](https://matthewkling.github.io/ggcube/reference/ramps.md),
  and
  [`ramp_steps()`](https://matthewkling.github.io/ggcube/reference/ramps.md)
  (gradual change, a sharp edge between lit and unlit sides, or flat
  bands, divided evenly or at chosen angles from the light), and
  [`tone_lightness()`](https://matthewkling.github.io/ggcube/reference/tones.md),
  [`tone_tint()`](https://matthewkling.github.io/ggcube/reference/tones.md),
  and
  [`tone_normals()`](https://matthewkling.github.io/ggcube/reference/tones.md)
  (lighter and darker versions of a hue, mixing toward chosen highlight
  and shadow colors for colored light, or coloring faces by their
  direction).
- [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
  gains `shadow` and `highlight`, which set the strength of shadows and
  highlights separately, from 0 to 1.
- The `method`, `mode`, and `contrast` arguments of
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
  are deprecated. `light("direct")` is now `light("hard")`,
  `light(method = "rgb")` is now `light(tone = "normals")`, and
  `contrast` is replaced by `shadow` and `highlight`. `mode = "hsv"` and
  `mode = "hsl"` still give the color math of ggcube 0.3 for now.
  `light("none")` is unchanged.
- [`guide_colorbar_3d()`](https://matthewkling.github.io/ggcube/reference/guide_3d.md)
  and
  [`guide_legend_3d()`](https://matthewkling.github.io/ggcube/reference/guide_3d.md)
  are now used by default for `fill` and `colour` scales whose aesthetic
  is mapped by a lit layer, so lit plots show shaded guides without
  asking for them. Guides set with
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html), or
  given to a scale’s `guide` argument as guide objects, are left alone;
  use e.g. `guides(fill = "colourbar")` for an unshaded guide. Guides of
  aesthetics mapped only by unlit layers, such as points, aren’t shaded.
- [`guide_colorbar_3d()`](https://matthewkling.github.io/ggcube/reference/guide_3d.md)
  and
  [`guide_legend_3d()`](https://matthewkling.github.io/ggcube/reference/guide_3d.md)
  now center their shading on the scale’s colors, so the middle of the
  guide shows the colors as specified, and `shade_range` gives exactly
  the range shown, from full shadow (`-1`) to full highlight (`1`).
- [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
  gains `specular` and `specular_size`, for glints where faces reflect
  the light toward the viewer, as on a glossy surface. Glints move
  toward the tone’s full highlight color, so colored light from
  [`tone_tint()`](https://matthewkling.github.io/ggcube/reference/tones.md)
  gives colored glints, and with
  [`ramp_steps()`](https://matthewkling.github.io/ggcube/reference/ramps.md)
  they’re flat spots. They’re off by default.
- Fixed
  [`guide_legend_3d()`](https://matthewkling.github.io/ggcube/reference/guide_3d.md)
  shading keys from the wrong colors: each key was shaded from the color
  of the key in the mirrored position, and keys of legends with several
  columns were also mismatched. Each key is now shaded from its own
  color.

### Other changes

- New
  [`geom_isosurface_3d()`](https://matthewkling.github.io/ggcube/reference/geom_isosurface_3d.md)
  and
  [`stat_isosurface_3d()`](https://matthewkling.github.io/ggcube/reference/geom_isosurface_3d.md)
  draw isosurfaces of a scalar field sampled on a 3D grid, the 3D analog
  of contour lines: map the field to the new `value` aesthetic, and give
  the field values at which to draw surfaces as `levels`. Grids may be
  unevenly spaced. Surfaces face outward from the regions they enclose,
  by default where `value` exceeds the level (`inside = "above"`), or
  with `inside = "below"` where it’s less, and are triangulated by
  marching cubes using the rmarchingcubes package, which must be
  installed.

- New
  [`geom_isodensity_3d()`](https://matthewkling.github.io/ggcube/reference/geom_isodensity_3d.md)
  and
  [`stat_isodensity_3d()`](https://matthewkling.github.io/ggcube/reference/geom_isodensity_3d.md)
  estimate the density of a 3D point cloud, and draw surfaces enclosing
  given fractions of it (`probs`, by default `0.5`), the 3D analog of
  [`geom_density_2d()`](https://ggplot2.tidyverse.org/reference/geom_density_2d.html).
  Each group gets its own density, and points can be weighted.
  Bandwidths `h` follow the convention of
  [`stat_density_3d()`](https://matthewkling.github.io/ggcube/reference/stat_density_3d.md),
  and default to the normal reference rule for three dimensions. Like
  isosurfaces, these require the rmarchingcubes package. Both kinds of
  surface are cut cleanly where they cross limits set on the plot’s
  scales, e.g. with
  [`xlim()`](https://ggplot2.tidyverse.org/reference/lims.html) or
  [`zlim()`](https://matthewkling.github.io/ggcube/reference/zlim.md),
  and with `cap = TRUE` are closed there with flat caps showing their
  cross sections. Isosurfaces are also capped where they meet the edges
  of their grid.

- New
  [`geom_isofunction_3d()`](https://matthewkling.github.io/ggcube/reference/geom_isofunction_3d.md)
  and
  [`stat_isofunction_3d()`](https://matthewkling.github.io/ggcube/reference/geom_isofunction_3d.md)
  draw isosurfaces of a function of three variables, evaluated over a
  domain given by `xlim`, `ylim`, and `zlim`: by default the implicit
  surface `fun(x, y, z) = 0`, so that
  e.g. `function(x, y, z) x^2 + y^2 + z^2 - 1` draws a unit ball. They
  are to
  [`geom_isosurface_3d()`](https://matthewkling.github.io/ggcube/reference/geom_isosurface_3d.md)
  what
  [`geom_function_3d()`](https://matthewkling.github.io/ggcube/reference/stat_function_3d.md)
  is to
  [`geom_surface_3d()`](https://matthewkling.github.io/ggcube/reference/stat_surface_3d.md).

- ggcube now requires ggplot2 4.0.0 or later.

- [`geom_polygon_3d()`](https://matthewkling.github.io/ggcube/reference/geom_polygon_3d.md)
  now lights each polygon using the normal of its own plane, oriented by
  its vertex winding. Lighting previously treated every polygon as a
  tile of a surface `z = f(x, y)`, so vertical polygons were lit as if
  horizontal, and polygons wound clockwise were lit as their reverse
  side. With the default lighting, polygons are now lit the same way
  whatever their winding order.

- Plots in which no layer has data now draw an empty cube, as ggplot2
  draws an empty panel. They previously failed with an unrelated error,
  which also hid the warning from any layer whose stat failed, or that
  found nothing to draw.

- Hard lighting, `light("hard")` (formerly `light("direct")`), now
  lights back faces that face the light. Light values were clamped to be
  non-negative before back faces had their sign flipped, so every
  visible back face, such as the underside of a surface seen from below,
  was drawn black regardless of where the light came from. A nonzero
  `backface_offset` now shifts direct light values before they are
  clamped, as it does for diffuse lighting.

- [`geom_contour_3d()`](https://matthewkling.github.io/ggcube/reference/geom_contour_3d.md)
  and
  [`geom_ridgeline_3d()`](https://matthewkling.github.io/ggcube/reference/geom_ridgeline_3d.md)
  now work with facets. They processed every panel’s data together, so
  faceted plots mixed panels’ contours and could leave panels empty.

- Lighting now keeps the alpha of colors given with an alpha channel,
  such as `"#FF000080"`. It previously made them opaque.

- With `light(method = "rgb")`, the points and lines of a lit layer,
  such as the residuals of
  [`geom_smooth_3d()`](https://matthewkling.github.io/ggcube/reference/geom_smooth_3d.md),
  are now drawn. Their colors were previously set to `NA`, so they
  disappeared.

- [`geom_ridgeline_3d()`](https://matthewkling.github.io/ggcube/reference/geom_ridgeline_3d.md)
  with `direction = "x"`, the default, now lights each ridge by the side
  that faces the viewer. Ridges were wound to face +x but lit as if they
  faced -x, so the side facing the light was shaded as a back face: with
  the default view and light, ridgelines were drawn darker than their
  fill.

- ggcube now requires systemfonts 1.2.0 or later. Earlier versions lack
  the glyph outline functions ggcube uses, so loading ggcube against
  them failed.

- New
  [`direction()`](https://matthewkling.github.io/ggcube/reference/direction.md)
  builds the length-3 direction vectors used by
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  and
  [`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
  from an azimuth and an elevation in degrees, for cases where angles
  are easier to reason about than components. Azimuth is a compass
  bearing measured clockwise from the positive y axis, matching terrain
  shading conventions.

- [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
  gains a `direction` argument: an alternative to `pitch`, `roll`, and
  `yaw` that specifies the direction the scene is viewed from, so
  `direction = c(1, 1, 1)` views the cube from its xmax-ymax-zmax
  corner. The z axis is kept pointing up the plot, and supplying
  `direction` alongside any of the Euler angles is an error. The default
  rotation corresponds to `direction(210, 30)`.

- `print.light()` now reports a directional light’s azimuth and
  elevation alongside its direction vector.

- [`orbit_3d()`](https://matthewkling.github.io/ggcube/reference/orbit_3d.md)
  and
  [`animate_3d()`](https://matthewkling.github.io/ggcube/reference/animate_3d.md)
  gain `light_azimuth` and `light_elevation` arguments for moving the
  light, alone or together with the camera. They follow the same
  conventions as
  [`direction()`](https://matthewkling.github.io/ggcube/reference/direction.md)
  and take the same forms as the rotation arguments, so
  `orbit_3d(p, yaw = c(360, 0), light_elevation = c(0, 90))` spins the
  camera with horizontal drag and raises the light with vertical drag.
  They move the plot-level light, and error if lighting is disabled,
  positional, or overridden by a layer’s own `light`.

- New
  [`facet_wrap_views()`](https://matthewkling.github.io/ggcube/reference/facet_wrap_views.md)
  and
  [`facet_grid_views()`](https://matthewkling.github.io/ggcube/reference/facet_wrap_views.md)
  show a plot from several angles at once, with one panel per view. They
  vary viewing angles the way
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  and
  [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  vary data variables: `p + facet_wrap_views(~yaw)` shows views around
  the vertical axis, and `p + facet_grid_views(roll ~ yaw)` varies roll
  down the rows and yaw across the columns. Light angles can be faceted
  too. Each angle in the formula gets a sensible default range, which
  its argument can override, and every panel is framed identically.

- [`orbit_3d()`](https://matthewkling.github.io/ggcube/reference/orbit_3d.md)
  can now show the angles of the current view, so a view found by
  dragging can be copied into
  [`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md).
  Press `r` on any orbit to show or hide the readout, or use
  `readout = TRUE` to have it showing when the orbit opens. When the
  orbit moves the light, the readout also shows the light’s azimuth and
  elevation, for use with
  [`direction()`](https://matthewkling.github.io/ggcube/reference/direction.md).
  Angles are shown in the terms the orbit was given, so
  `yaw = c(0, 360)` reports values from 0 to 360.

- Fixed a bug where
  [`geom_text_3d()`](https://matthewkling.github.io/ggcube/reference/geom_text_3d.md)
  (billboard method) could draw labels at each other’s depth-scaled
  sizes under perspective projection.

- [`geom_segment_3d()`](https://matthewkling.github.io/ggcube/reference/geom_segment_3d.md)
  and
  [`geom_path_3d()`](https://matthewkling.github.io/ggcube/reference/geom_path_3d.md)
  gain an `arrow.fill` argument. Closed arrowheads are now filled with
  the line colour by default, as in ggplot2; previously they were drawn
  hollow.

- Points, segments, and text lying in the plane of a planar polygon
  (e.g. a line drawn along a plane) are now always drawn on top of it.
  Previously, especially under perspective projection, they were often
  hidden behind it.

- Depth sorting now treats the reference lines of point layers as
  segments rather than degenerate polygons.

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

- Depth sorting now splits polygons that pass through one another, so
  intersecting planes and surfaces render with correct occlusion along
  their line of intersection. Outlines are drawn only along original
  polygon edges, not along the cuts.

- Depth sorting now determines the order of a polygon and any element
  overlapping it on screen (polygon, segment, point, or text) exactly
  from their 3D geometry where possible, rather than from sampled
  screen-space depths. This fixes misordering under perspective
  projection, which was common for elements close to large polygons.

- Segments that pass through polygons are now split at the exact 3D
  crossing point. Previously, under perspective projection, most such
  crossings went undetected and the rest were split at visibly wrong
  positions.

- When depth sorting finds cyclic overlaps, it now drops only
  constraints that are part of a cycle. Previously it could discard many
  unrelated constraints, and pairwise sorting of large layers is now
  considerably faster when cycles occur.

- Fixed a spurious “Cycle breaking exceeded maximum iterations” warning
  for layers containing a single element.

- Fixed a bug where depth sorting never constrained the order of a
  segment relative to a polygon it overlapped on screen, so segments
  (e.g. residual lines and annotation segments) could be drawn on the
  wrong side of polygons.

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
