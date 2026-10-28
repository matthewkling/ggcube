# Geom -------------------------------------------------------------------------

GeomPoint3D <- ggproto("GeomPoint3D", GeomPoint,

                       non_missing_aes = c(GeomPoint$non_missing_aes, "z"),

                       draw_panel = function(data, panel_params, coord, na.rm = FALSE,
                                             scale_depth = TRUE, on_top = FALSE, flat = FALSE,
                                             ref_line_colour = NULL, ref_line_linewidth = 0.25, ref_line_linetype = NULL, ref_line_alpha = NULL,
                                             ref_point_colour = NULL, ref_point_fill = NULL, ref_point_alpha = NULL, ref_point_size = NULL, ref_point_stroke = NULL, ref_point_shape = NULL) {

                             validate_coord3d(coord)

                             # Markers projected onto a face are drawn lying in it
                             if (isTRUE(flat)) {
                                   face <- if ("project_to_face" %in% names(data)) data$project_to_face else NA
                                   raw <- if ("element_type" %in% names(data)) {
                                         is.na(data$element_type) | data$element_type == "raw_point"
                                   } else TRUE
                                   data$.flat <- raw & !is.na(face)
                                   if (any(data$.flat)) check_flat_shapes(data$shape[data$.flat])
                             }

                             # Point-specific preprocessing
                             data$ref_circle_radius <- data$ref_circle_radius / 100
                             coords <- coord$transform(data, panel_params, sort = FALSE)

                             finish <- function(coords) {
                                   # Scale points, strokes, linewidths by depth
                                   if(!"linewidth" %in% names(coords)) coords$linewidth <- 0.5
                                   coords <- apply_depth_scaling(coords, scale_depth)

                                   # Resolve ref_* styling and assign .prim types
                                   coords <- resolve_point_aesthetics(
                                         coords,
                                         ref_line_colour = ref_line_colour,
                                         ref_line_linewidth = ref_line_linewidth,
                                         ref_line_linetype = ref_line_linetype,
                                         ref_line_alpha = ref_line_alpha,
                                         ref_point_colour = ref_point_colour,
                                         ref_point_fill = ref_point_fill,
                                         ref_point_alpha = ref_point_alpha,
                                         ref_point_size = ref_point_size,
                                         ref_point_stroke = ref_point_stroke,
                                         ref_point_shape = ref_point_shape
                                   )
                                   style_flat_markers(coords)
                             }

                             prepare <- if (isTRUE(flat)) {
                                   function(rows, panel_pt) {
                                         expand_flat_markers(rows, panel_pt, panel_params$proj,
                                                             panel_params$plot_bounds)
                                   }
                             }

                             layer_scene_grob(coords, panel_params, finish, on_top = on_top,
                                              prepare = prepare)
                       }
)

# Ref element aesthetic resolution ---------------------------------------------

#' Resolve reference element aesthetics and assign .prim types
#'
#' Walks through the data and applies ref_* styling inheritance logic,
#' writing final visual property values into standard aesthetic columns.
#' Also maps element_type to .prim for the shared renderer.
#'
#' @param coords Transformed coordinate data with element_type column.
#' @param ref_line_colour,ref_line_linewidth,ref_line_linetype,ref_line_alpha
#'   Overrides for reference line styling.
#' @param ref_point_colour,ref_point_fill,ref_point_alpha,ref_point_size,ref_point_stroke,ref_point_shape
#'   Overrides for reference point/circle styling.
#' @return Data frame with resolved aesthetics and .prim column.
#' @keywords internal
#' @noRd
resolve_point_aesthetics <- function(coords,
                                     ref_line_colour = NULL,
                                     ref_line_linewidth = 0.25,
                                     ref_line_linetype = NULL,
                                     ref_line_alpha = NULL,
                                     ref_point_colour = NULL,
                                     ref_point_fill = NULL,
                                     ref_point_alpha = NULL,
                                     ref_point_size = NULL,
                                     ref_point_stroke = NULL,
                                     ref_point_shape = NULL) {

      if (!"element_type" %in% names(coords)) {
            # No element_type column — treat everything as raw points
            coords$.prim <- "point"
            return(coords)
      }

      # Extract base object IDs for grouping (everything before "__")
      coords$object_id <- sub("__.*", "", coords$group)

      # Detect shape complexity and alpha mapping per object
      obj_info <- coords[!duplicated(coords$object_id), ]
      is_complex <- setNames(
            is.numeric(obj_info$shape) & obj_info$shape %in% 21:25,
            obj_info$object_id
      )
      alpha_mapped <- setNames(
            !sapply(split(obj_info$alpha, obj_info$object_id),
                    function(a) all(a == 1, na.rm = TRUE)),
            obj_info$object_id
      )

      # Initialize .prim column
      coords$.prim <- NA_character_

      # Ensure linetype column exists (point data may not have it)
      if (!"linetype" %in% names(coords)) coords$linetype <- 1

      # Process each element type
      types <- unique(coords$element_type)

      for (et in types) {
            mask <- coords$element_type == et
            et_data <- coords[mask, ]
            oids <- et_data$object_id

            if (et == "raw_point") {
                  # Raw points keep their aesthetics as-is; no changes needed
                  coords$.prim[mask] <- "point"

            } else if (et == "ref_circle") {
                  # Ref circles are polygons — apply ref_point inheritance
                  complex <- is_complex[oids]
                  amapped <- alpha_mapped[oids]

                  coords$fill[mask] <- resolve_ref_fill(
                        ref_point_fill, et_data$fill, et_data$colour, complex)
                  coords$colour[mask] <- resolve_ref_colour(
                        ref_point_colour, et_data$colour, complex)
                  coords$alpha[mask] <- resolve_ref_alpha(
                        ref_point_alpha, et_data$alpha, amapped, default = 0.5)
                  coords$linetype[mask] <- 1

                  # Stroke/linewidth for circle border
                  avg_ds <- ave(et_data$depth_scale, et_data$object_id, FUN = mean)
                  coords$linewidth[mask] <- resolve_ref_circle_linewidth(
                        ref_point_stroke, et_data$stroke, complex, avg_ds)

                  coords$.prim[mask] <- "polygon"

            } else if (et == "ref_line") {
                  # Ref lines are segments — apply ref_line inheritance
                  amapped <- alpha_mapped[oids]

                  coords$colour[mask] <- ref_line_colour %||% et_data$colour
                  coords$linetype[mask] <- ref_line_linetype %||% 1
                  coords$alpha[mask] <- resolve_ref_alpha(
                        ref_line_alpha, et_data$alpha, amapped, default = 0.5)

                  # Linewidth with depth scaling
                  avg_ds <- ave(et_data$depth_scale, et_data$object_id, FUN = mean)
                  coords$linewidth[mask] <- ref_line_linewidth * avg_ds

                  coords$.prim[mask] <- "segment"

            } else if (et == "ref_point") {
                  # Ref points are points — apply ref_point inheritance
                  complex <- is_complex[oids]
                  amapped <- alpha_mapped[oids]

                  coords$colour[mask] <- ref_point_colour %||% et_data$colour
                  coords$fill[mask] <- resolve_ref_fill(
                        ref_point_fill, et_data$fill, et_data$colour, complex)
                  coords$alpha[mask] <- resolve_ref_alpha(
                        ref_point_alpha, et_data$alpha, amapped, default = 0.5)
                  coords$shape[mask] <- ref_point_shape %||% et_data$shape

                  # Size: ref_point_size scales by depth, otherwise 1/3 of original
                  if (!is.null(ref_point_size)) {
                        coords$size[mask] <- ref_point_size * et_data$depth_scale
                  } else {
                        coords$size[mask] <- et_data$size * (1/3)
                  }

                  coords$stroke[mask] <- ref_point_stroke %||% et_data$stroke

                  coords$.prim[mask] <- "point"
            }
      }

      coords$object_id <- NULL
      return(coords)
}


# Ref aesthetic helpers --------------------------------------------------------

resolve_ref_fill <- function(override, fill, colour, is_complex) {
      if (!is.null(override)) return(rep(override, length(fill)))
      ifelse(is_complex, fill, colour)
}

resolve_ref_colour <- function(override, colour, is_complex) {
      if (!is.null(override)) return(rep(override, length(colour)))
      ifelse(is_complex, colour, NA_character_)
}

resolve_ref_alpha <- function(override, alpha, alpha_mapped, default = 0.5) {
      if (!is.null(override)) return(rep(override, length(alpha)))
      ifelse(alpha_mapped, alpha, default)
}

resolve_ref_circle_linewidth <- function(override_stroke, stroke, is_complex, avg_depth_scale) {
      if (!is.null(override_stroke)) {
            return(override_stroke * avg_depth_scale)
      }
      ifelse(is_complex, (stroke %||% 0.5) * avg_depth_scale, 0)
}


# Deprecated reference elements -------------------------------------------------

.point_ref_defaults <- list(
      raw_points = TRUE, ref_lines = FALSE, ref_points = FALSE, ref_faces = "zmin",
      ref_circle_radius = 1.5, ref_circle_vertices = 16,
      ref_line_color = NULL, ref_line_colour = NULL, ref_line_linewidth = 0.25,
      ref_line_linetype = NULL, ref_line_alpha = NULL,
      ref_point_color = NULL, ref_point_colour = NULL, ref_point_fill = NULL,
      ref_point_size = NULL, ref_point_alpha = NULL, ref_point_stroke = NULL,
      ref_point_shape = NULL
)

#' Resolve a point layer's deprecated reference element arguments
#'
#' Reference lines, points, and circles are now drawn by separate layers
#' ([geom_dropline_3d()], and [geom_point_3d()] with [position_on_face()]).
#' The arguments still work, with a deprecation warning for each one given.
#'
#' @param env Environment of the call to the layer function.
#' @param fn Name of the layer function.
#' @param user_env Environment of the user's call to the layer function.
#' @return Named list of the arguments' values, with defaults for those not
#'   given and colour spellings resolved.
#' @keywords internal
#' @noRd
point_ref_args <- function(env, fn, user_env = rlang::caller_env(2)) {
      out <- .point_ref_defaults
      details <- function(nm) {
            if (nm == "raw_points") {
                  "Reference elements are now drawn by separate layers, so leave out the point layer instead."
            } else if (nm == "ref_lines") {
                  "Draw drop lines with `geom_dropline_3d()` instead."
            } else if (nm %in% c("ref_points", "ref_circle_radius", "ref_circle_vertices")) {
                  paste("Draw reference points with a layer such as",
                        "`geom_point_3d(position = position_on_face(), flat = TRUE)` instead.")
            } else if (nm == "ref_faces") {
                  "Give the faces to `geom_dropline_3d()` and `position_on_face()` instead."
            } else if (startsWith(nm, "ref_line_")) {
                  "Style a `geom_dropline_3d()` layer instead."
            } else {
                  "Style a `geom_point_3d(position = position_on_face())` layer instead."
            }
      }
      for (nm in names(out)) {
            if (eval(call("missing", as.name(nm)), env)) next
            out[nm] <- list(get(nm, envir = env))
            lifecycle::deprecate_warn("0.4.0", paste0(fn, "(", nm, ")"), details = details(nm),
                                      user_env = user_env)
      }
      out$ref_line_color <- out$ref_line_color %||% out$ref_line_colour
      out$ref_point_color <- out$ref_point_color %||% out$ref_point_colour
      out
}


#' Check a point layer's `flat` argument
#'
#' @param flat The `flat` argument.
#' @param position The layer's position adjustment.
#' @keywords internal
#' @noRd
check_flat <- function(flat, position, call = rlang::caller_env()) {
      if (!is.logical(flat) || length(flat) != 1 || is.na(flat)) {
            rlang::abort("`flat` must be `TRUE` or `FALSE`.", call = call)
      }
      if (!flat) return(invisible())
      faces <- if (inherits(position, "PositionOnFace")) setdiff(position$faces, "3D") else character()
      if (length(faces) == 0) {
            rlang::abort(c("`flat = TRUE` draws markers lying flat on a face of the panel, so it needs a face.",
                           i = "Place the points on one or more faces with `position = position_on_face()`, e.g. `position_on_face(\"zmin\")`."),
                         call = call)
      }
      invisible()
}


# Stat -------------------------------------------------------------------------

StatPoint3D <- ggproto("StatPoint3D", Stat,
                       required_aes = c("x", "y", "z"),

                       setup_data = function(data, params) {
                             censor_z_limits(data)
                       },

                       compute_panel = function(data, scales, na.rm = FALSE,
                                                raw_points = TRUE,
                                                ref_lines = FALSE,
                                                ref_points = "circles",
                                                ref_faces = "zmin",
                                                ref_circle_radius = 0.02,
                                                ref_circle_vertices = 16) {

                             # Validate parameters
                             if (!is.logical(raw_points)) raw_points <- TRUE
                             if (!is.logical(ref_lines)) ref_lines <- FALSE

                             if (is.logical(ref_points)) {
                                   ref_points <- if(ref_points) "circles" else FALSE
                             }

                             # Handle empty data
                             if (nrow(data) == 0) return(data)

                             # Handle discrete scale conversion
                             if ("z" %in% names(data)) {
                                   data$z_raw <- data$z
                                   if (is.factor(data$z) || is.character(data$z)) {
                                         data$z <- as.numeric(as.factor(data$z))
                                   }
                             }

                             if ("x" %in% names(data)) {
                                   data$x_raw <- data$x
                                   if (is.factor(data$x) || is.character(data$x)) {
                                         data$x <- as.numeric(as.factor(data$x))
                                   }
                             }

                             if ("y" %in% names(data)) {
                                   data$y_raw <- data$y
                                   if (is.factor(data$y) || is.character(data$y)) {
                                         data$y <- as.numeric(as.factor(data$y))
                                   }
                             }

                             # Determine which faces to use for projections
                             valid_faces <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
                             ref_faces <- intersect(ref_faces, valid_faces)

                             result <- generate_point_elements(data, raw_points,
                                                               ref_faces, ref_points, ref_lines,
                                                               ref_circle_radius, ref_circle_vertices)

                             return(result)
                       }
)


# Ref element generation -------------------------------------------------------

#' Generate reference elements with simplified approach
#'
#' @param data Original point data
#' @param raw_points Whether to include raw points
#' @param ref_faces Character vector of face names
#' @param ref_points Type of reference points: FALSE, "circles", or "points"
#' @param ref_lines Whether to include reference lines
#' @param ref_circle_radius Radius for circular reference points
#' @param ref_circle_vertices Number of vertices for circular reference points
#' @return Data frame with reference elements
#' @keywords internal
#' @noRd
generate_point_elements <- function(data, raw_points,
                                    ref_faces, ref_points, ref_lines,
                                    ref_circle_radius, ref_circle_vertices) {

      # Build result data frame
      result <- data.frame()
      id <- 1:nrow(data)

      # Add primary points if requested
      if(raw_points) {
            result <- data %>%
                  mutate(element_type = "raw_point",
                         group = paste0("point_raw_", id, "__grouped"),
                         project_to_face = NA_character_,
                         ref_circle_radius = NA_real_,
                         ref_circle_vertices = NA_integer_) %>%
                  bind_rows(result)
      }

      for(face in ref_faces){
            if(ref_points == "circles"){
                  result <- data %>%
                        mutate(element_type = "ref_circle",
                               group = paste0("circle_", face, "_", id, "__grouped"),
                               project_to_face = face,
                               ref_circle_radius = ref_circle_radius,
                               ref_circle_vertices = as.integer(ref_circle_vertices)) %>%
                        bind_rows(result)
            } else if(ref_points == "points") {
                  result <- data %>%
                        mutate(element_type = "ref_point",
                               group = paste0("point_", face, "_", id, "__grouped"),
                               project_to_face = face,
                               ref_circle_radius = NA_real_,
                               ref_circle_vertices = NA_integer_) %>%
                        bind_rows(result)
            }

            if(ref_lines){
                  result <- data %>%
                        mutate(element_type = "ref_line",
                               group = paste0("segment_", face, "_", id, "__grouped"),
                               project_to_face = NA_character_,
                               ref_circle_radius = NA_real_,
                               ref_circle_vertices = NA_integer_) %>%
                        bind_rows(mutate(., project_to_face = face)) %>%
                        bind_rows(result)
            }
      }

      return(result)
}



# User-facing functions --------------------------------------------------------

#' 3D scatter plots
#'
#' `geom_point_3d()` creates scatter plots in 3D space with automatic depth-based
#' size scaling. Points closer to the viewer appear larger, while points farther
#' away appear smaller, creating realistic perspective effects.
#'
#' StatPoint3D performs identity transformation (passes data through unchanged) while
#' properly handling discrete scales for 3D coordinate systems.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. In addition to
#'   the standard point aesthetics, `geom_point_3d()` requires x, y, and z coordinates.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to `"point_3d"`.
#' @param geom The geometric object used to display the data. Defaults to `"point_3d"`.
#' @param position Position adjustment, defaults to "identity". Use
#'   [position_on_face()] to place points on faces of the panel.
#' @param ... Other arguments passed on to [ggplot2::layer()].
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param flat If `TRUE`, points placed on a face of the panel by
#'   [position_on_face()] are drawn as markers lying flat in that face, like
#'   shadows, rather than as symbols facing the viewer. Flat markers match the
#'   size of ordinary markers at the centre of the panel, and are drawn in
#'   true perspective, outlines included, whatever `scale_depth` is. They can be open, solid, or filled circles, squares,
#'   diamonds, or triangles (shapes 0, 1, 2, 5, 6, and 15 to 25). Points not
#'   on a face (e.g. with `position_on_face(c("3D", "zmin"))`) are drawn as
#'   usual. Default `FALSE`.
#' @inheritParams sort_params
#' @inheritParams depth_params
#' @inheritParams on_top_param
#' @inheritParams annotate_params
#' @param raw_points,ref_lines,ref_points,ref_faces,ref_circle_radius,ref_circle_vertices
#'   Deprecated. Reference elements are now drawn by separate layers: drop
#'   lines by [geom_dropline_3d()], and reference points by `geom_point_3d()`
#'   with `position = position_on_face()` (and `flat = TRUE` for circles).
#' @param ref_line_color,ref_line_colour,ref_line_linewidth,ref_line_linetype,ref_line_alpha
#'   Deprecated. Style a [geom_dropline_3d()] layer instead.
#' @param ref_point_color,ref_point_colour,ref_point_fill,ref_point_size,ref_point_alpha,ref_point_stroke,ref_point_shape
#'   Deprecated. Style a `geom_point_3d(position = position_on_face())` layer instead.
#'
#' @section Aesthetics:
#' `geom_point_3d()` supports all the same aesthetics as [ggplot2::geom_point()], plus `z`:
#' - **x**: X coordinate (required)
#' - **y**: Y coordinate (required)
#' - **z**: Z coordinate (required)
#' - `alpha`: Transparency
#' - `colour`: Point border color
#' - `fill`: Point fill color (for certain shapes)
#' - `shape`: Point shape
#' - `size`: Point size (gets depth-scaled when `scale_depth = TRUE`)
#' - `stroke`: Border width for shapes with borders
#'
#' @section StatPoint3D returns the following computed variables:
#' - `x_raw`, `y_raw`, `z_raw`: Original values before discrete-to-numeric conversion
#'
#' @section Depth Scaling:
#' The depth scaling uses an inverse relationship with distance, following the
#' mathematical relationship: `apparent_size = base_size * reference_distance / actual_distance`
#'
#' This creates realistic perspective where:
#' - Objects twice as far appear half as big
#' - Objects twice as close appear twice as big
#' - The center of the plot volume renders at exactly the user-specified size
#'
#' @section Point Rendering:
#' ggcube uses shape-aware rendering for improved stroke behavior:
#' - **Simple shapes (0-20, characters):** Use `size` for fontsize, `stroke` for border width, no fill
#' - **Complex shapes (21-25):** Use ggplot2's approach: `size + 0.5*stroke` for fontsize to prevent gaps
#' - Both size and stroke are depth-scaled when `scale_depth = TRUE`
#' - All shapes preserve stroke depth scaling and parameter control
#'
#' @section Reference elements:
#' To show where points lie in 3D, combine a point layer with layers that
#' project the points onto faces of the panel:
#' - [geom_dropline_3d()] draws lines from the points to faces.
#' - `geom_point_3d(position = position_on_face())` draws the points on
#'   faces, as upright markers, or with `flat = TRUE`, as markers lying in
#'   the faces.
#'
#' All of these layers are depth-sorted together.
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic 3D scatter plot with depth scaling
#' ggplot(expand.grid(x = 1:5, y = 1:5, z = 1:5),
#'        aes(x, y, z, fill = z)) +
#'   geom_point_3d(size = 10, shape = 21, color = "white", stroke = .1) +
#'   coord_3d(pitch = 40, roll = 5, yaw = 0, dist = 1.5) +
#'   scale_fill_viridis_c()
#'
#' # Points with flat markers on two faces, and drop lines to them
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_dropline_3d(faces = c("ymax", "xmax")) +
#'   geom_point_3d(position = position_on_face(c("ymax", "xmax")),
#'                 flat = TRUE, alpha = .5, size = 3) +
#'   geom_point_3d(size = 3) +
#'   coord_3d()
#'
#' # Points and their shadows on the floor, in one layer
#' ggplot(mtcars, aes(mpg, wt, qsec, fill = factor(cyl))) +
#'   geom_point_3d(position = position_on_face(c("3D", "zmin")), flat = TRUE,
#'                 shape = 21, size = 3) +
#'   coord_3d()
#'
#' @seealso [geom_dropline_3d()] for drop lines, [position_on_face()] for
#'   placing points on faces, [geom_segment_3d()] for 3D segments,
#'   [geom_path_3d()] for 3D paths, [coord_3d()] for 3D coordinate systems.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_point_3d
#' @export
geom_point_3d <- function(mapping = NULL, data = NULL,
                          stat = StatPoint3D, position = "identity",
                          ...,
                          annotate = deprecated(),
                          na.rm = FALSE,
                          sort_method = deprecated(),
                          scale_depth = TRUE, on_top = FALSE, flat = FALSE,
                          raw_points = deprecated(),
                          ref_lines = deprecated(),
                          ref_points = deprecated(),
                          ref_faces = deprecated(),
                          ref_circle_radius = deprecated(),
                          ref_circle_vertices = deprecated(),
                          ref_line_color = deprecated(), ref_line_colour = deprecated(),
                          ref_line_linewidth = deprecated(),
                          ref_line_linetype = deprecated(),
                          ref_line_alpha = deprecated(),
                          ref_point_color = deprecated(), ref_point_colour = deprecated(),
                          ref_point_fill = deprecated(),
                          ref_point_size = deprecated(),
                          ref_point_alpha = deprecated(),
                          ref_point_stroke = deprecated(),
                          ref_point_shape = deprecated(),
                          inherit.aes = TRUE,
                          show.legend = NA) {
      check_sort_method(sort_method, "geom_point_3d")
      check_on_top(on_top)

      ref <- point_ref_args(rlang::current_env(), "geom_point_3d")
      check_flat(flat, position)

      out <- layer(
            data = data, mapping = mapping, stat = get_proto(stat, "stat"), geom = GeomPoint3D,
            position = position,  show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  scale_depth = scale_depth, on_top = on_top, flat = flat,
                  raw_points = ref$raw_points,
                  ref_lines = ref$ref_lines,
                  ref_points = ref$ref_points,
                  ref_faces = ref$ref_faces,
                  ref_circle_radius = ref$ref_circle_radius,
                  ref_circle_vertices = ref$ref_circle_vertices,
                  ref_line_color = ref$ref_line_color,
                  ref_line_linewidth = ref$ref_line_linewidth,
                  ref_line_linetype = ref$ref_line_linetype,
                  ref_line_alpha = ref$ref_line_alpha,
                  ref_point_color = ref$ref_point_color,
                  ref_point_fill = ref$ref_point_fill,
                  ref_point_size = ref$ref_point_size,
                  ref_point_alpha = ref$ref_point_alpha,
                  ref_point_stroke = ref$ref_point_stroke,
                  ref_point_shape = ref$ref_point_shape,
                  ...
            )
      )
      with_annotations(out, annotate, "geom_point_3d")
}

#' @rdname geom_point_3d
#' @export
stat_point_3d <- function(mapping = NULL, data = NULL,
                          geom = GeomPoint3D, position = "identity",
                          ...,
                          na.rm = FALSE,
                          sort_method = deprecated(),
                          scale_depth = TRUE,
                          raw_points = deprecated(),
                          ref_lines = deprecated(),
                          ref_points = deprecated(),
                          ref_faces = deprecated(),
                          ref_circle_radius = deprecated(),
                          ref_circle_vertices = deprecated(),
                          ref_line_color = deprecated(), ref_line_colour = deprecated(),
                          ref_line_linewidth = deprecated(),
                          ref_line_linetype = deprecated(),
                          ref_line_alpha = deprecated(),
                          ref_point_color = deprecated(), ref_point_colour = deprecated(),
                          ref_point_fill = deprecated(),
                          ref_point_size = deprecated(),
                          ref_point_alpha = deprecated(),
                          ref_point_stroke = deprecated(),
                          ref_point_shape = deprecated(),
                          inherit.aes = TRUE,
                          show.legend = NA) {
      check_sort_method(sort_method, "stat_point_3d")

      ref <- point_ref_args(rlang::current_env(), "stat_point_3d")

      layer(
            data = data, mapping = mapping, stat = StatPoint3D, geom = get_proto(geom, "geom"),
            position = position,  show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  scale_depth = scale_depth,
                  raw_points = ref$raw_points,
                  ref_lines = ref$ref_lines,
                  ref_points = ref$ref_points,
                  ref_faces = ref$ref_faces,
                  ref_circle_radius = ref$ref_circle_radius,
                  ref_circle_vertices = ref$ref_circle_vertices,
                  ref_line_color = ref$ref_line_color,
                  ref_line_linewidth = ref$ref_line_linewidth,
                  ref_line_linetype = ref$ref_line_linetype,
                  ref_line_alpha = ref$ref_line_alpha,
                  ref_point_color = ref$ref_point_color,
                  ref_point_fill = ref$ref_point_fill,
                  ref_point_size = ref$ref_point_size,
                  ref_point_alpha = ref$ref_point_alpha,
                  ref_point_stroke = ref$ref_point_stroke,
                  ref_point_shape = ref$ref_point_shape,
                  ...
            )
      )
}
