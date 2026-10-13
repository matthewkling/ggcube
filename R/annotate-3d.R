# annotate-3d.R
# In-layer 3D annotation system
#
# Provides annotate_3d() for embedding annotations (points, text labels,
# segments, axes, planes) within a 3D layer so they participate in the same
# depth sorting pass as the primary geometry.


# Constructor ------------------------------------------------------------------

#' Create a 3D annotation specification
#'
#' Defines fixed-position annotations to be embedded within a 3D layer.
#' Annotations are depth-sorted together with the primary geometry, unlike
#' ggplot2's `annotate()` which creates a separate layer.
#'
#' Parameters can be vectors to create multiple annotations of the same type
#' in one call, with scalar values recycled as needed (matching
#' `ggplot2::annotate()` behaviour).
#'
#' @param type Character string specifying the annotation type. One of
#'   `"point"`, `"text"`, `"segment"`, `"axes"`, or `"plane"`.
#' @param ... Type-specific parameters. See Details.
#'
#' @details
#' ## Point annotations
#' Requires `x`, `y`, `z`. Optional styling: `colour`/`color`, `fill`, `size`,
#' `shape`, `alpha`, `stroke`.
#'
#' ## Text annotations
#' Requires `x`, `y`, `z`, `label`. Optional styling: `colour`/`color`, `size`,
#' `alpha`, `family`, `fontface`, `hjust`, `vjust`, `angle`, `lineheight`.
#'
#' ## Segment annotations
#' Requires `x`, `y`, `z`, `xend`, `yend`, `zend`. Optional styling:
#' `colour`/`color`, `linewidth`, `linetype`, `alpha`, and `arrow`, an arrow
#' specification created by [grid::arrow()] that applies to every segment in
#' the call. Closed arrowheads are filled with the segment colour. When a
#' segment passes through a polygon and is split, its arrowheads stay at the
#' segment's original ends.
#'
#' ## Axes annotations
#' Draws a line along each of the x, y, and z axes through an origin given by
#' `x`, `y`, `z` (default `0`), each spanning the panel's full range along its
#' axis, with an arrowhead and a label at its high end. Optional: `labels`
#' (a length-3 vector, default `c("x", "y", "z")`, or `NULL` for none),
#' `arrow` (default a closed 6pt arrowhead, or `NULL` for none), line styling
#' `colour`/`color`, `linewidth`, `linetype`, `alpha`, and label styling
#' `size`, `family`, `fontface`. Styling values can have length 3 to style
#' each axis separately.
#'
#' Axes are built from the panel's limits when the plot is drawn, so unlike
#' other annotations they don't expand the plot's axis ranges.
#'
#' ## Plane annotations
#' Draws the plane `a * x + b * y + c * z = d`, clipped to the panel. Requires
#' `a`, `b`, `c` (not all zero); `d` defaults to `0`. All four can be vectors
#' to draw several planes, matching the rows of a linear system `A x = d`.
#' Optional styling: `fill` (default `"grey60"`), `alpha` (default `0.5`),
#' `colour`/`color` for the outline (default none), `linewidth`, `linetype`.
#'
#' Like axes, planes are built from the panel's limits and don't expand the
#' plot's axis ranges. They are lit like the layer's other polygons, from
#' whichever side faces the viewer, and are never removed by back-face
#' culling. A plane that misses the panel entirely is skipped with a warning.
#'
#' ## Coordinates
#' Annotation coordinates are placed directly in the plot's (transformed)
#' scale space: with `scale_x_log10()`, for example, give x positions as
#' log10 values.
#'
#' @return An S3 object of class `"annotate_3d"`.
#'
#' @examples
#' p <- ggplot(mountain, aes(x, y, z)) +
#'   coord_3d(ratio = c(2, 3, 1),
#'            light = light(mode = "hsl", direction = c(-1, 0, 0)))
#'
#' # Basic point annotation
#' p + geom_surface_3d(
#'   annotate = annotate_3d("point", x = .5, y = .25, z = 100,
#'                          color = "red", size = 3))
#'
#' # Vectorized: multiple points in one call
#' p + geom_surface_3d(
#'   annotate = annotate_3d("point", x = .5, y = .25, z = seq(50, 100, 5),
#'                          color = "red", size = 3))
#'
#' # Multiple annotation types
#' p + geom_surface_3d(
#'   annotate = list(
#'     annotate_3d("point", x = .5, y = .25, z = 100,
#'                 color = "red", size = 3),
#'     annotate_3d("text", x = .5, y = .25, z = 100,
#'                 color = "red", label = "Look here!",
#'                 vjust = -1, fontface = "bold"),
#'     annotate_3d("segment", x = .5, y = .25, z = 100,
#'                 xend = .5, yend = .25, zend = 50,
#'                 color = "red", size = 3)))
#'
#' # Planes of a linear system, meeting at its solution
#' A <- rbind(c(1, 1, 1), c(1, -1, 0), c(0, 1, -1))
#' ggplot(data.frame(x = 1/3, y = 1/3, z = 1/3), aes(x, y, z)) +
#'   geom_point_3d(size = 3, sort_method = "pairwise",
#'                 annotate = annotate_3d("plane", a = A[, 1], b = A[, 2],
#'                                        c = A[, 3], d = c(1, 0, 0),
#'                                        fill = c("tomato", "seagreen", "steelblue"))) +
#'   lims(x = c(-1, 1), y = c(-1, 1)) + zlim(-1, 1) +
#'   coord_3d()
#'
#' # Axes through the origin, depth-sorted with a surface
#' ggplot() +
#'   geom_function_3d(fun = function(x, y) (x^2 - y^2) / 2,
#'                    xlim = c(-1, 1), ylim = c(-1, 1),
#'                    annotate = annotate_3d("axes", linewidth = 0.8)) +
#'   coord_3d(panels = "none") +
#'   theme_void()
#'
#' @export
annotate_3d <- function(type, ...) {

      valid_types <- c("point", "text", "segment", "axes", "plane")
      if (!type %in% valid_types) {
            stop("annotate_3d() type must be one of: ",
                 paste(valid_types, collapse = ", "),
                 call. = FALSE)
      }

      params <- list(...)

      # Arrows are a single specification per call, not a per-row value
      arrow_given <- "arrow" %in% names(params)
      arrow <- params$arrow
      params$arrow <- NULL
      if (arrow_given && !type %in% c("segment", "axes")) {
            stop("annotate_3d(): `arrow` applies only to segment and axes annotations.",
                 call. = FALSE)
      }
      if (!arrow_given && type == "axes") {
            arrow <- grid::arrow(length = grid::unit(6, "pt"), type = "closed")
      }
      if (!is.null(arrow) && !inherits(arrow, "arrow")) {
            stop("annotate_3d(): `arrow` must be created by grid::arrow().",
                 call. = FALSE)
      }

      # Normalize color -> colour (American -> British, matching ggplot2)
      if ("color" %in% names(params)) {
            params$colour <- params$colour %||% params$color
            params$color <- NULL
      }

      if (type == "axes") return(new_axes_spec(params, arrow))

      # Validate required parameters
      required <- switch(type,
                         point   = c("x", "y", "z"),
                         text    = c("x", "y", "z", "label"),
                         segment = c("x", "y", "z", "xend", "yend", "zend"),
                         plane   = c("a", "b", "c")
      )
      missing <- setdiff(required, names(params))
      if (length(missing) > 0) {
            stop("annotate_3d('", type, "') requires: ",
                 paste(missing, collapse = ", "),
                 call. = FALSE)
      }

      # Expand and recycle all params via data.frame(). This handles
      # vectorized inputs (e.g. x = 1:5, colour = "red") with standard
      # R recycling rules and throws clear errors for incompatible lengths.
      params_df <- tryCatch(
            data.frame(params, stringsAsFactors = FALSE),
            error = function(e) {
                  stop("annotate_3d(): incompatible parameter lengths.\n",
                       conditionMessage(e), call. = FALSE)
            }
      )

      if (type == "plane") {
            if (!"d" %in% names(params_df)) params_df$d <- 0
            coefs <- as.matrix(params_df[c("a", "b", "c", "d")])
            if (!is.numeric(coefs) || any(!is.finite(coefs))) {
                  stop("annotate_3d('plane'): `a`, `b`, `c`, and `d` must be finite numbers.",
                       call. = FALSE)
            }
            if (any(rowSums(coefs[, 1:3, drop = FALSE]^2) == 0)) {
                  stop("annotate_3d('plane'): `a`, `b`, and `c` can't all be zero.",
                       call. = FALSE)
            }
      }

      structure(
            list(type = type, data = params_df, arrow = arrow),
            class = "annotate_3d"
      )
}


#' Build the specification for an axes annotation
#'
#' One row per axis (x, y, z). The origin must be a single point; styling
#' values are recycled across the three axes, so each axis can be styled
#' individually.
#'
#' @param params Named list of user parameters.
#' @param arrow Arrow specification, or NULL.
#' @return An `annotate_3d` object of type `"axes"`.
#' @keywords internal
#' @noRd
new_axes_spec <- function(params, arrow) {
      labels <- if ("labels" %in% names(params)) params$labels else c("x", "y", "z")
      params$labels <- NULL
      if (!is.null(labels) && length(labels) != 3) {
            stop("annotate_3d('axes'): `labels` must have length 3 (one per axis), or be NULL.",
                 call. = FALSE)
      }

      origin <- vapply(c("x", "y", "z"), function(a) {
            v <- params[[a]] %||% 0
            if (!is.numeric(v) || length(v) != 1 || !is.finite(v)) {
                  stop("annotate_3d('axes'): `", a, "` must be a single finite number.",
                       call. = FALSE)
            }
            v
      }, numeric(1))
      params[c("x", "y", "z")] <- NULL

      bad <- vapply(params, function(v) !length(v) %in% c(1, 3), logical(1))
      if (any(bad)) {
            stop("annotate_3d('axes'): styling values must have length 1 or 3 (one per axis): ",
                 paste(names(params)[bad], collapse = ", "), call. = FALSE)
      }

      d <- data.frame(axis = c("x", "y", "z"), stringsAsFactors = FALSE)
      for (nm in names(params)) d[[nm]] <- rep_len(params[[nm]], 3)

      structure(
            list(type = "axes", data = d, origin = origin,
                 labels = if (is.null(labels)) NULL else as.character(labels),
                 arrow = arrow),
            class = "annotate_3d"
      )
}


# Annotation types built at draw time from the panel's limits, rather than in
# setup_data. They never take part in scale training.
.panel_annotation_types <- c("axes", "plane")


# Build annotations ------------------------------------------------------------

#' Build annotation rows from annotation specs
#'
#' Takes annotation specifications (single or list) and generates data-space
#' rows tagged with `.prim` and dot-prefixed style columns, ready to be
#' bound onto primary data in `setup_data` so that annotation positions
#' participate in scale training.
#'
#' Annotation types that depend on the panel's limits (see
#' `.panel_annotation_types`) are skipped here and built at draw time by
#' `prepare_annotations()`.
#'
#' @param annotate A single `annotate_3d` object, or a list of them.
#' @param max_group Numeric. The maximum existing group number, used to
#'   generate non-colliding group IDs for annotation rows.
#' @return A data frame of annotation rows, or `NULL` if `annotate` is NULL.
#' @keywords internal
#' @noRd
build_annotations <- function(annotate, max_group = 0) {

      if (is.null(annotate)) return(NULL)

      # Normalize to a flat list of specs
      specs <- normalize_annotation_specs(annotate)

      if (length(specs) == 0) return(NULL)

      # Generate rows for each spec
      all_rows <- list()
      group_offset <- max_group

      for (spec in specs) {
            if (spec$type %in% .panel_annotation_types) next
            result <- switch(spec$type,
                             point   = make_annotation_point_rows(spec, group_offset),
                             text    = make_annotation_text_rows(spec, group_offset),
                             segment = make_annotation_segment_rows(spec, group_offset),
                             stop("Unknown annotation type: ", spec$type, call. = FALSE)
            )
            all_rows <- c(all_rows, list(result$data))
            group_offset <- result$next_offset
      }

      if (length(all_rows) == 0) return(NULL)
      dplyr::bind_rows(all_rows)
}


#' Normalize annotation input to a flat list of specs
#'
#' Accepts a single annotate_3d object, a list of them, or nested
#' combinations, and returns a flat list of annotate_3d objects.
#'
#' @param annotate Input annotation spec(s).
#' @return A flat list of `annotate_3d` objects.
#' @keywords internal
#' @noRd
normalize_annotation_specs <- function(annotate) {

      if (inherits(annotate, "annotate_3d")) {
            return(list(annotate))
      }

      if (is.list(annotate)) {
            # Flatten and validate
            specs <- list()
            for (item in annotate) {
                  if (inherits(item, "annotate_3d")) {
                        specs <- c(specs, list(item))
                  } else if (is.list(item)) {
                        # Recurse for nested lists
                        specs <- c(specs, normalize_annotation_specs(item))
                  } else {
                        warning("Ignoring non-annotate_3d element in annotation list",
                                call. = FALSE)
                  }
            }
            return(specs)
      }

      warning("annotate must be an annotate_3d object or list of them",
              call. = FALSE)
      return(list())
}


# Geom integration helpers -----------------------------------------------------

#' Build and bind annotation rows in setup_data
#'
#' One-liner helper for geom `setup_data` methods. Checks for annotation
#' specs in params, builds annotation rows, handles PANEL assignment and
#' type coercion, and binds onto the primary data. Returns data unchanged
#' if no annotations are present.
#'
#' @param data Primary data frame from the layer.
#' @param params Layer params list (checked for `$annotate`).
#' @return Data frame with annotation rows appended, or original data if
#'   no annotations.
#' @keywords internal
#' @noRd
setup_annotations <- function(data, params) {
      annotate <- params$annotate
      if (is.null(annotate)) return(data)

      max_group <- max(as.numeric(factor(data$group)), na.rm = TRUE)
      ann_rows <- build_annotations(annotate, max_group = max_group)
      if (is.null(ann_rows) || nrow(ann_rows) == 0) return(data)

      bind_annotation_rows(data, ann_rows)
}


#' Bind annotation rows onto layer data
#'
#' Assigns the layer's PANEL and coerces overlapping columns to compatible
#' types so bind_rows doesn't choke on mismatches (e.g. factor or integer vs
#' character group). bind_rows fills missing columns with NA.
#'
#' @param data Layer data.
#' @param ann_rows Annotation rows.
#' @return Combined data frame.
#' @keywords internal
#' @noRd
bind_annotation_rows <- function(data, ann_rows) {
      ann_rows$PANEL <- data$PANEL[1]

      shared_cols <- intersect(names(data), names(ann_rows))
      for (col in shared_cols) {
            d_chr <- is.factor(data[[col]]) || is.character(data[[col]])
            a_chr <- is.factor(ann_rows[[col]]) || is.character(ann_rows[[col]])
            if (d_chr != a_chr || is.factor(data[[col]]) != is.factor(ann_rows[[col]])) {
                  ann_rows[[col]] <- as.character(ann_rows[[col]])
                  data[[col]] <- as.character(data[[col]])
            }
      }

      dplyr::bind_rows(data, ann_rows)
}


#' Build panel-dependent annotations in draw_panel
#'
#' Helper for geom `draw_panel` methods, called before coord transformation.
#' Builds the rows for annotation types that span the panel (see
#' `.panel_annotation_types`) from the panel's limits, and binds them onto the
#' layer data. Because this happens at draw time, these annotations never
#' affect scale training.
#'
#' @param data Layer data in `draw_panel`.
#' @param panel_params Panel parameters from the coord.
#' @param annotate The layer's `annotate` parameter.
#' @return Data frame with panel annotation rows appended.
#' @keywords internal
#' @noRd
prepare_annotations <- function(data, panel_params, annotate = NULL) {
      if (is.null(annotate)) return(data)
      specs <- normalize_annotation_specs(annotate)
      specs <- Filter(function(sp) sp$type %in% .panel_annotation_types, specs)
      if (length(specs) == 0) return(data)

      limits <- list(x = panel_params$scale_info$x$limits,
                     y = panel_params$scale_info$y$limits,
                     z = panel_params$scale_info$z$limits)

      rows <- lapply(seq_along(specs), function(i) {
            switch(specs[[i]]$type,
                   axes = make_annotation_axes_rows(specs[[i]], limits, i),
                   plane = make_annotation_plane_rows(specs[[i]], limits, i))
      })
      rows <- dplyr::bind_rows(rows)
      if (nrow(rows) == 0) return(data)

      bind_annotation_rows(data, rows)
}


# Apply annotation styles ------------------------------------------------------

#' Apply annotation styles from dot-prefixed columns
#'
#' After coord transformation, copies `.ann_*` style columns to the standard
#' aesthetic columns for annotation rows only (identified by `.ann` marker).
#' This allows annotation styling to survive scale training without
#' contaminating colour/fill/etc. scales.
#'
#' @param data Transformed data frame potentially containing annotation rows.
#' @return Data frame with annotation styles applied.
#' @keywords internal
#' @noRd
apply_annotation_styles <- function(data) {

      if (!".ann" %in% names(data)) return(data)

      ann_mask <- !is.na(data$.ann) & data$.ann
      if (!any(ann_mask)) return(data)

      # Find all .ann_ prefixed columns
      ann_cols <- grep("^\\.ann_", names(data), value = TRUE)

      for (ann_col in ann_cols) {
            # Map .ann_colour -> colour, .ann_fill -> fill, etc.
            target_col <- sub("^\\.ann_", "", ann_col)

            # Ensure target column exists
            if (!target_col %in% names(data)) {
                  data[[target_col]] <- NA
            }

            # Apply annotation values only to annotation rows
            ann_values <- data[[ann_col]][ann_mask]
            non_na <- !is.na(ann_values)
            if (any(non_na)) {
                  idx <- which(ann_mask)[non_na]
                  data[[target_col]][idx] <- ann_values[non_na]
            }
      }

      data
}


# Row generators ---------------------------------------------------------------

#' Generate point annotation rows
#' @keywords internal
#' @noRd
make_annotation_point_rows <- function(spec, group_offset) {

      d <- spec$data
      n <- nrow(d)
      ids <- seq_len(n) + group_offset

      rows <- data.frame(
            x = d$x,
            y = d$y,
            z = d$z,
            group = paste0("ann__pt", ids),
            .prim = "point",
            .ann = TRUE,
            stringsAsFactors = FALSE
      )

      # Point style defaults
      style_defaults <- list(
            colour = "black",
            fill   = NA_character_,
            size   = 2,
            shape  = 19,
            alpha  = 1,
            stroke = 0.5
      )

      # Apply user values (already recycled in spec$data) or defaults
      for (col in names(style_defaults)) {
            val <- if (col %in% names(d)) d[[col]] else style_defaults[[col]]
            rows[[paste0(".ann_", col)]] <- val
      }

      list(data = rows, next_offset = group_offset + n)
}


#' Generate text annotation rows
#' @keywords internal
#' @noRd
make_annotation_text_rows <- function(spec, group_offset) {

      d <- spec$data
      n <- nrow(d)
      ids <- seq_len(n) + group_offset

      rows <- data.frame(
            x = d$x,
            y = d$y,
            z = d$z,
            group = paste0("ann__text", ids),
            label = d$label,
            .prim = "text",
            .ann = TRUE,
            stringsAsFactors = FALSE
      )

      # Text style defaults
      style_defaults <- list(
            colour     = "black",
            size       = 3.88,
            alpha      = 1,
            family     = "",
            fontface   = 1,
            hjust      = 0.5,
            vjust      = 0.5,
            angle      = 0,
            lineheight = 1.2
      )

      for (col in names(style_defaults)) {
            val <- if (col %in% names(d)) d[[col]] else style_defaults[[col]]
            rows[[paste0(".ann_", col)]] <- val
      }

      list(data = rows, next_offset = group_offset + n)
}


#' Generate segment annotation rows
#' @keywords internal
#' @noRd
make_annotation_segment_rows <- function(spec, group_offset) {

      d <- spec$data
      n <- nrow(d)
      ids <- seq_len(n) + group_offset
      seg_groups <- paste0("ann__seg", ids)

      start_rows <- data.frame(
            x = d$x,
            y = d$y,
            z = d$z,
            group = seg_groups,
            point_type = "start",
            segment_id = ids,
            .prim = "segment",
            .ann = TRUE,
            stringsAsFactors = FALSE
      )

      end_rows <- data.frame(
            x = d$xend,
            y = d$yend,
            z = d$zend,
            group = seg_groups,
            point_type = "end",
            segment_id = ids,
            .prim = "segment",
            .ann = TRUE,
            stringsAsFactors = FALSE
      )

      rows <- rbind(start_rows, end_rows)

      # Segment style defaults
      style_defaults <- list(
            colour    = "black",
            linewidth = 0.5,
            linetype  = 1,
            alpha     = 1
      )

      for (col in names(style_defaults)) {
            val <- if (col %in% names(d)) rep(d[[col]], 2) else style_defaults[[col]]
            rows[[paste0(".ann_", col)]] <- val
      }

      rows <- add_arrow_columns(rows, spec$arrow)

      list(data = rows, next_offset = group_offset + n)
}


#' Record an arrow specification on segment rows
#'
#' Arrow objects can't travel through the ggplot2 pipeline in data frame
#' columns, so their components are stored as plain columns and rebuilt at
#' render time by `segment_arrows()`.
#'
#' @param rows Segment rows.
#' @param arrow Arrow from [grid::arrow()], or NULL.
#' @return `rows`, with `.arrow_*` columns when `arrow` is supplied.
#' @keywords internal
#' @noRd
add_arrow_columns <- function(rows, arrow) {
      if (is.null(arrow)) return(rows)
      rows$.arrow_angle <- arrow$angle[1]
      rows$.arrow_length <- as.numeric(arrow$length)[1]
      rows$.arrow_unit <- grid::unitType(arrow$length)[1]
      rows$.arrow_ends <- arrow$ends[1]
      rows$.arrow_type <- arrow$type[1]
      rows
}


#' Generate axes annotation rows
#'
#' Each axis is a segment through the origin spanning the panel's full range
#' along that axis, with an optional label just beyond its high end.
#'
#' @param spec Axes specification from `new_axes_spec()`.
#' @param limits Named list of panel limits for x, y, z.
#' @param index Index of this spec, used to build unique group names.
#' @return Data frame of segment and text rows.
#' @keywords internal
#' @noRd
make_annotation_axes_rows <- function(spec, limits, index) {
      d <- spec$data
      origin <- spec$origin

      outside <- vapply(c("x", "y", "z"), function(a) {
            origin[[a]] < min(limits[[a]]) || origin[[a]] > max(limits[[a]])
      }, logical(1))
      if (any(outside)) {
            warning("annotate_3d('axes'): the origin lies outside the panel range on ",
                    paste(names(outside)[outside], collapse = ", "),
                    ", so some axes are drawn outside the panel.", call. = FALSE)
      }

      line_defaults <- list(colour = "grey30", linewidth = 0.5, linetype = 1, alpha = 1)
      text_defaults <- list(colour = "grey30", size = 3.88, alpha = 1, family = "",
                            fontface = 1, hjust = 0.5, vjust = 0.5, angle = 0,
                            lineheight = 1.2)
      style <- function(defaults, k) {
            lapply(setNames(names(defaults), names(defaults)), function(nm) {
                  if (nm %in% names(d)) d[[nm]][k] else defaults[[nm]]
            })
      }

      rows <- list()
      for (k in 1:3) {
            a <- d$axis[k]
            lim <- range(limits[[a]])
            start <- origin; start[[a]] <- lim[1]
            end <- origin; end[[a]] <- lim[2]
            grp <- paste0("ann__axes", index, "_", a)

            seg <- data.frame(x = c(start[["x"]], end[["x"]]),
                              y = c(start[["y"]], end[["y"]]),
                              z = c(start[["z"]], end[["z"]]),
                              group = grp, point_type = c("start", "end"),
                              segment_id = NA_real_, .prim = "segment", .ann = TRUE,
                              stringsAsFactors = FALSE)
            for (nm in names(line_defaults)) seg[[paste0(".ann_", nm)]] <- style(line_defaults, k)[[nm]]
            seg <- add_arrow_columns(seg, spec$arrow)
            rows <- c(rows, list(seg))

            label <- spec$labels[k]
            if (!is.null(spec$labels) && !is.na(label) && nzchar(label)) {
                  pos <- end; pos[[a]] <- lim[2] + 0.06 * diff(lim)
                  txt <- data.frame(x = pos[["x"]], y = pos[["y"]], z = pos[["z"]],
                                    group = paste0(grp, "_label"), label = label,
                                    .prim = "text", .ann = TRUE, stringsAsFactors = FALSE)
                  for (nm in names(text_defaults)) txt[[paste0(".ann_", nm)]] <- style(text_defaults, k)[[nm]]
                  rows <- c(rows, list(txt))
            }
      }
      dplyr::bind_rows(rows)
}



#' Generate plane annotation rows
#'
#' Each plane `a x + b y + c z = d` is clipped to the panel box, giving one
#' convex polygon with 3 to 6 vertices.
#'
#' @param spec Plane specification.
#' @param limits Named list of panel limits for x, y, z.
#' @param index Index of this spec, used to build unique group names.
#' @return Data frame of polygon rows.
#' @keywords internal
#' @noRd
make_annotation_plane_rows <- function(spec, limits, index) {
      d <- spec$data
      style_defaults <- list(fill = "grey60", alpha = 0.5, colour = NA_character_,
                             linewidth = 0.5, linetype = 1)

      rows <- list()
      for (k in seq_len(nrow(d))) {
            verts <- clip_plane_to_box(c(d$a[k], d$b[k], d$c[k]), d$d[k], limits)
            if (is.null(verts)) {
                  warning("annotate_3d('plane'): the plane ", d$a[k], "x + ", d$b[k], "y + ",
                          d$c[k], "z = ", d$d[k], " doesn't cross the panel, so it is not drawn.",
                          call. = FALSE)
                  next
            }
            poly <- data.frame(x = verts[, 1], y = verts[, 2], z = verts[, 3],
                               group = paste0("ann__plane", index, "_", k),
                               .prim = "polygon", .ann = TRUE, stringsAsFactors = FALSE)
            for (nm in names(style_defaults)) {
                  poly[[paste0(".ann_", nm)]] <- if (nm %in% names(d)) d[[nm]][k] else style_defaults[[nm]]
            }
            rows <- c(rows, list(poly))
      }
      dplyr::bind_rows(rows)
}


#' Clip a plane to an axis-aligned box
#'
#' Finds where the plane `normal . p = offset` crosses the box's edges (and
#' any corners lying on it), and orders those points around the resulting
#' convex polygon.
#'
#' @param normal Length-3 numeric vector `c(a, b, c)`.
#' @param offset Plane offset `d`.
#' @param limits Named list of x, y, z limits.
#' @return Matrix of polygon vertices (n x 3, n >= 3), or NULL if the plane
#'   doesn't cross the box.
#' @keywords internal
#' @noRd
clip_plane_to_box <- function(normal, offset, limits) {
      lo <- vapply(limits[c("x", "y", "z")], min, numeric(1))
      hi <- vapply(limits[c("x", "y", "z")], max, numeric(1))
      corners <- as.matrix(expand.grid(x = c(lo[1], hi[1]), y = c(lo[2], hi[2]),
                                       z = c(lo[3], hi[3])))
      f <- as.vector(corners %*% normal - offset)
      tol <- 1e-10 * sqrt(sum(normal^2)) * sqrt(sum((hi - lo)^2))
      f[abs(f) <= tol] <- 0

      pts <- corners[f == 0, , drop = FALSE]
      for (i in 1:7) {
            for (j in (i + 1):8) {
                  if (sum(corners[i, ] != corners[j, ]) != 1) next    # box edges only
                  if (f[i] * f[j] < 0) {
                        t <- f[i] / (f[i] - f[j])
                        pts <- rbind(pts, corners[i, ] + t * (corners[j, ] - corners[i, ]))
                  }
            }
      }
      if (nrow(pts) < 3) return(NULL)

      # Drop near-duplicates, then order by angle around the centroid
      scale <- hi - lo
      scale[scale == 0] <- 1
      key <- round(sweep(pts, 2, scale, "/"), 9)
      pts <- pts[!duplicated(key), , drop = FALSE]
      if (nrow(pts) < 3) return(NULL)

      n <- normal / sqrt(sum(normal^2))
      helper <- diag(3)[which.min(abs(n)), ]
      u <- cross3(n, helper); u <- u / sqrt(sum(u^2))
      v <- cross3(n, u)
      centred <- sweep(pts, 2, colMeans(pts))
      angle <- atan2(centred %*% v, centred %*% u)
      unname(pts[order(angle), , drop = FALSE])
}
