# Depth scaling utility --------------------------------------------------------

# Apply depth scaling to point sizes, strokes, and linewidths if enabled
scale_depth <- function(coords, scale_depth){
      if (scale_depth && "depth_scale" %in% names(coords)) {
            if("size" %in% names(coords)) {
                  coords$size <- coords$size * coords$depth_scale
            }
            if("stroke" %in% names(coords)) {
                  coords$stroke <- coords$stroke * coords$depth_scale
            }
            if("linewidth" %in% names(coords)) {
                  coords$linewidth <- coords$linewidth * coords$depth_scale
            }
      }
      return(coords)
}


# Geom -------------------------------------------------------------------------

GeomPoint3D <- ggproto("GeomPoint3D", GeomPoint,

                       draw_panel = function(data, panel_params, coord, na.rm = FALSE,
                                             sort_method = "painter",
                                             scale_depth = TRUE,
                                             ref_line_colour = NULL, ref_line_linewidth = 0.25, ref_line_linetype = NULL, ref_line_alpha = NULL,
                                             ref_point_colour = NULL, ref_point_fill = NULL, ref_point_alpha = NULL, ref_point_size = NULL, ref_point_stroke = NULL, ref_point_shape = NULL) {

                             # Transform data
                             validate_coord3d(coord)
                             data$ref_circle_radius <- data$ref_circle_radius / 100
                             sort_method <- match.arg(sort_method, c("auto", "pairwise", "painter"))
                             data$.sort_method <- sort_method
                             coords <- coord$transform(data, panel_params)

                             # Scale points, strokes, linewidths by depth
                             if(!"linewidth" %in% names(coords)) coords$linewidth <- 0.5
                             coords <- scale_depth(coords, scale_depth)

                             if (nrow(coords) == 0) {
                                   return(grid::nullGrob())
                             }

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

                             render_mixed_grobs(coords)
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


# Stat -------------------------------------------------------------------------

StatPoint3D <- ggproto("StatPoint3D", Stat,
                       required_aes = c("x", "y", "z"),

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

#' 3D scatter plot with 2D reference elements
#'
#' `geom_point_3d()` creates scatter plots in 3D space with automatic depth-based
#' size scaling. Points closer to the viewer appear larger, while points farther
#' away appear smaller, creating realistic perspective effects. Optionally adds
#' reference lines and points projecting to cube faces, to illustrate point locations
#' in 2D.
#'
#' StatPoint3D performs identity transformation (passes data through unchanged) while
#' properly handling discrete scales for 3D coordinate systems. It can optionally
#' generate reference lines and points projecting to cube faces.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. In addition to
#'   the standard point aesthetics, `geom_point_3d()` requires x, y, and z coordinates.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to [StatPoint3D].
#' @param geom The geometric object used to display the data. Defaults to [GeomPoint3D].
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed on to [layer()].
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param scale_depth Logical indicating whether to apply depth-based scaling
#'   to point sizes, point stroke widths, and reference line widths.
#'   When `TRUE` (default), points/lines closer to the viewer appear larger/wider, and
#'   points farther away appear smaller. When `FALSE`, all points/lines have uniform size/width.
#' @inheritParams sort_method_param
#' @param raw_points Logical indicating whether to show the original 3D points.
#'   Default is `TRUE`.
#' @param ref_lines Logical indicating whether to show reference lines projecting
#'   from points to cube faces. Default is `FALSE`.
#' @param ref_points Type of reference points to create. Options:
#'   \itemize{
#'     \item \code{FALSE}: No reference points (default)
#'     \item \code{TRUE or "circles"}: Circular reference points that project properly
#'     \item \code{"points"}: Single-point references (renders faster and supports non-circular shapes,
#'     but point shape is not 3D-transformed)
#'   }
#' @param ref_faces Character vector specifying which cube faces to project to.
#'   Valid face names are: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax".
#'   Default is "zmin" (bottom face). Multiple faces can be specified.
#' @param ref_circle_radius Radius for circular reference points as percentage of standardized coordinate space.
#' @param ref_circle_vertices Number of vertices for circular reference points (higher = smoother).
#' @param ref_line_color,ref_line_colour,ref_line_linewidth,ref_line_linetype,ref_line_alpha
#'   Styling overrides for reference lines. When NULL, inherits from point aesthetics or uses defaults.
#'   Both American (`ref_line_color`) and British (`ref_line_colour`) spellings are accepted.
#' @param ref_point_color,ref_point_colour,ref_point_fill,ref_point_size,ref_point_alpha,ref_point_stroke,ref_point_shape
#'   Styling overrides for reference points and circles. When NULL, inherits from raw point aesthetics
#'   with shape-aware logic (complex shapes 21-25 vs simple shapes 0-20/characters).
#'   Both American (`ref_point_color`) and British (`ref_point_colour`) spellings are accepted.
#'
#' @section Aesthetics:
#' `geom_point_3d()` supports all the same aesthetics as [geom_point()], plus `z`:
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
#' - `element_type`: Type of element ("raw_point", "ref_point", "ref_line", "ref_circle")
#' - `segment_id`: ID linking the two endpoints of reference lines
#' - `ref_face`: Which face reference elements project to
#' - `ref_circle_radius`: Radius for circular reference points
#' - `ref_circle_vertices`: Number of vertices for circular reference points
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
#' @section Reference Features:
#' Reference lines and points help visualize the 3D relationships by projecting
#' data points onto cube faces:
#' - **Reference lines**: Connect each 3D point to its projection on specified faces
#' - **Reference points**: Show the projected location on the faces
#' - **Reference circles**: Circular projections that appear as realistic 3D shadows
#' - **Depth sorting**: All elements (original points, reference lines, reference points)
#'   are automatically depth-sorted for proper 3D rendering
#'
#' @section Aesthetic Inheritance:
#' Reference elements intelligently inherit styling from raw points:
#' - **Shape-aware**: Complex shapes (21-25) with fill/colour are handled differently from simple shapes (0-20/characters)
#' - **Alpha detection**: If all points have alpha=1, uses 0.5 default for ref elements; otherwise inherits mapped alpha
#' - **Priority**: Explicit ref_* parameters > mapped aesthetics > smart defaults
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
#' # Add circular reference points on 2D face panel
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point_3d(size = 3,
#'     ref_points = TRUE, ref_faces = c("ymax", "xmax")) +
#'   coord_3d()
#'
#' @seealso [geom_segment_3d()] for 3D segments, [geom_path_3d()] for 3D paths,
#'   [coord_3d()] for 3D coordinate systems.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_point_3d
#' @export
geom_point_3d <- function(mapping = NULL, data = NULL,
                          stat = StatPoint3D, position = "identity",
                          ...,
                          na.rm = FALSE,
                          sort_method = "painter",
                          scale_depth = TRUE,
                          raw_points = TRUE,
                          ref_lines = FALSE,
                          ref_points = FALSE,
                          ref_faces = "zmin",
                          ref_circle_radius = 1.5,
                          ref_circle_vertices = 16,
                          ref_line_color = NULL, ref_line_colour = NULL,
                          ref_line_linewidth = 0.25,
                          ref_line_linetype = NULL,
                          ref_line_alpha = NULL,
                          ref_point_color = NULL, ref_point_colour = NULL,
                          ref_point_fill = NULL,
                          ref_point_size = NULL,
                          ref_point_alpha = NULL,
                          ref_point_stroke = NULL,
                          ref_point_shape = NULL,
                          inherit.aes = TRUE,
                          show.legend = NA) {

      # Handle both American and British spellings
      if (!is.null(ref_line_colour) && is.null(ref_line_color)) {
            ref_line_color <- ref_line_colour
      }
      if (!is.null(ref_point_colour) && is.null(ref_point_color)) {
            ref_point_color <- ref_point_colour
      }

      layer(
            data = data, mapping = mapping, stat = get_proto(stat, "stat"), geom = GeomPoint3D,
            position = position,  show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  sort_method = sort_method,
                  scale_depth = scale_depth,
                  raw_points = raw_points,
                  ref_lines = ref_lines,
                  ref_points = ref_points,
                  ref_faces = ref_faces,
                  ref_circle_radius = ref_circle_radius,
                  ref_circle_vertices = ref_circle_vertices,
                  ref_line_color = ref_line_color,
                  ref_line_linewidth = ref_line_linewidth,
                  ref_line_linetype = ref_line_linetype,
                  ref_line_alpha = ref_line_alpha,
                  ref_point_color = ref_point_color,
                  ref_point_fill = ref_point_fill,
                  ref_point_size = ref_point_size,
                  ref_point_alpha = ref_point_alpha,
                  ref_point_stroke = ref_point_stroke,
                  ref_point_shape = ref_point_shape,
                  ...
            )
      )
}

#' @rdname geom_point_3d
#' @export
stat_point_3d <- function(mapping = NULL, data = NULL,
                          geom = GeomPoint3D, position = "identity",
                          ...,
                          na.rm = FALSE,
                          sort_method = "painter",
                          scale_depth = TRUE,
                          raw_points = TRUE,
                          ref_lines = FALSE,
                          ref_points = FALSE,
                          ref_faces = "zmin",
                          ref_circle_radius = 1.5,
                          ref_circle_vertices = 16,
                          ref_line_color = NULL, ref_line_colour = NULL,
                          ref_line_linewidth = 0.25,
                          ref_line_linetype = NULL,
                          ref_line_alpha = NULL,
                          ref_point_color = NULL, ref_point_colour = NULL,
                          ref_point_fill = NULL,
                          ref_point_size = NULL,
                          ref_point_alpha = NULL,
                          ref_point_stroke = NULL,
                          ref_point_shape = NULL,
                          inherit.aes = TRUE,
                          show.legend = NA) {

      # Handle both American and British spellings
      if (!is.null(ref_line_colour) && is.null(ref_line_color)) {
            ref_line_color <- ref_line_colour
      }
      if (!is.null(ref_point_colour) && is.null(ref_point_color)) {
            ref_point_color <- ref_point_colour
      }

      layer(
            data = data, mapping = mapping, stat = StatPoint3D, geom = get_proto(geom, "geom"),
            position = position,  show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(
                  na.rm = na.rm,
                  sort_method = sort_method,
                  scale_depth = scale_depth,
                  raw_points = raw_points,
                  ref_lines = ref_lines,
                  ref_points = ref_points,
                  ref_faces = ref_faces,
                  ref_circle_radius = ref_circle_radius,
                  ref_circle_vertices = ref_circle_vertices,
                  ref_line_color = ref_line_color,
                  ref_line_linewidth = ref_line_linewidth,
                  ref_line_linetype = ref_line_linetype,
                  ref_line_alpha = ref_line_alpha,
                  ref_point_color = ref_point_color,
                  ref_point_fill = ref_point_fill,
                  ref_point_size = ref_point_size,
                  ref_point_alpha = ref_point_alpha,
                  ref_point_stroke = ref_point_stroke,
                  ref_point_shape = ref_point_shape,
                  ...
            )
      )
}
