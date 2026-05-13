# annotate-3d.R
# In-layer 3D annotation system
#
# Provides annotate_3d() for embedding fixed-position annotations (points,
# text labels, segments) within a 3D layer so they participate in the same
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
#'   `"point"`, `"text"`, or `"segment"`.
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
#' `colour`/`color`, `linewidth`, `linetype`, `alpha`.
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
#' @export
annotate_3d <- function(type, ...) {

      valid_types <- c("point", "text", "segment")
      if (!type %in% valid_types) {
            stop("annotate_3d() type must be one of: ",
                 paste(valid_types, collapse = ", "),
                 call. = FALSE)
      }

      params <- list(...)

      # Normalize color -> colour (American -> British, matching ggplot2)
      if ("color" %in% names(params)) {
            params$colour <- params$colour %||% params$color
            params$color <- NULL
      }

      # Validate required parameters
      required <- switch(type,
                         point   = c("x", "y", "z"),
                         text    = c("x", "y", "z", "label"),
                         segment = c("x", "y", "z", "xend", "yend", "zend")
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

      structure(
            list(type = type, data = params_df),
            class = "annotate_3d"
      )
}


# Build annotations ------------------------------------------------------------

#' Build annotation rows from annotation specs
#'
#' Takes annotation specifications (single or list) and generates data-space
#' rows tagged with `.prim` and dot-prefixed style columns, ready to be
#' bound onto primary data in `setup_data` so that annotation positions
#' participate in scale training.
#'
#' For annotation types that need panel range information (e.g. planes),
#' positional columns may contain sentinel values (NA) that are resolved
#' later in `draw_panel` via `resolve_annotations()`.
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
            result <- switch(spec$type,
                             point   = make_annotation_point_rows(spec, group_offset),
                             text    = make_annotation_text_rows(spec, group_offset),
                             segment = make_annotation_segment_rows(spec, group_offset),
                             stop("Unknown annotation type: ", spec$type, call. = FALSE)
            )
            all_rows <- c(all_rows, list(result$data))
            group_offset <- result$next_offset
      }

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

      ann_rows$PANEL <- data$PANEL[1]

      # Coerce overlapping columns to compatible types so bind_rows
      # doesn't choke on mismatches (e.g. factor vs character group).
      # bind_rows fills missing columns with NA automatically.
      shared_cols <- intersect(names(data), names(ann_rows))
      for (col in shared_cols) {
            if (is.factor(data[[col]]) && !is.factor(ann_rows[[col]])) {
                  ann_rows[[col]] <- as.character(ann_rows[[col]])
                  data[[col]] <- as.character(data[[col]])
            }
      }

      dplyr::bind_rows(data, ann_rows)
}


#' Resolve annotation sentinels in draw_panel
#'
#' One-liner helper for geom `draw_panel` methods, called before coord
#' transformation. For annotation types that need panel range information
#' (e.g. planes needing extent on free axes), replaces sentinel NA values
#' with actual ranges from `panel_params`.
#'
#' For v1 annotation types (point, text, segment), this is a no-op since
#' all positions are fully specified at build time.
#'
#' @param data Data frame potentially containing annotation rows with
#'   sentinel values.
#' @param panel_params Panel parameters from the coord, containing scale
#'   ranges.
#' @return Data frame with sentinel values resolved.
#' @keywords internal
#' @noRd
prepare_annotations <- function(data, panel_params) {
      # v1: no-op. Point, text, and segment annotations are fully specified.
      # When plane annotations are added, this function will resolve NA extents
      # using panel_params$scale_info.
      data
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

      list(data = rows, next_offset = group_offset + n)
}
