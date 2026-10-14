# Mixed-geometry rendering pipeline
#
# Shared rendering function that produces grobs from depth-sorted data
# containing multiple primitive types (polygon, point, segment, text).
# Data arrives already transformed to NPC coordinates, depth-sorted, and
# with resolved aesthetic values. This function handles only the final
# grob assembly step.


# Depth scaling ----------------------------------------------------------------

#' Validate a layer's `scale_depth` argument
#'
#' Layers take a single strength rather than the coord's per-element vector.
#' `TRUE` and `FALSE` are accepted as 1 and 0 so that existing code keeps
#' working unchanged.
#'
#' @param scale_depth User-supplied value.
#' @return A single non-negative number.
#' @keywords internal
#' @noRd
resolve_layer_scale_depth <- function(scale_depth) {
      if (is.null(scale_depth)) return(1)

      if (is.logical(scale_depth)) {
            if (length(scale_depth) != 1 || is.na(scale_depth)) {
                  rlang::abort("`scale_depth` must be TRUE, FALSE, or a single number.")
            }
            return(as.numeric(scale_depth))
      }

      if (!is.numeric(scale_depth) || length(scale_depth) != 1) {
            rlang::abort("`scale_depth` must be TRUE, FALSE, or a single number.")
      }

      if (!is.finite(scale_depth)) {
            rlang::abort("`scale_depth` must be finite.")
      }

      if (scale_depth < 0) {
            rlang::abort(c(
                  "`scale_depth` must be non-negative.",
                  i = "Negative values would invert the depth cue, drawing distant elements larger than near ones."
            ))
      }

      scale_depth
}

#' Apply depth scaling to point sizes, strokes, and linewidths
#'
#' Scales visual properties by the depth_scale factor so that closer objects
#' appear larger/thicker and farther objects appear smaller/thinner. The
#' strength exponent is shared with `coord_3d(scale_depth = )`, so `1` is full
#' strength, `0` disables scaling, and intermediate values subdue it.
#'
#' @param coords Data frame with aesthetic columns and optionally `depth_scale`.
#' @param strength Depth scaling strength. `TRUE`/`FALSE` are accepted as 1/0.
#' @return Data frame with scaled aesthetics.
#' @keywords internal
#' @noRd
apply_depth_scaling <- function(coords, strength){
      strength <- resolve_layer_scale_depth(strength)

      if (strength > 0 && "depth_scale" %in% names(coords)) {
            factor <- apply_depth_strength(coords$depth_scale, strength)

            if("size" %in% names(coords)) {
                  coords$size <- coords$size * factor
            }
            if("stroke" %in% names(coords)) {
                  coords$stroke <- coords$stroke * factor
            }
            if("linewidth" %in% names(coords)) {
                  coords$linewidth <- coords$linewidth * factor
            }
      }
      return(coords)
}


# Grob assembly ----------------------------------------------------------------


#' Render mixed-geometry data into a grob tree
#'
#' Takes depth-sorted, transformed data with a `.prim` column indicating
#' primitive type and produces a single grob (or grobTree) for the layer.
#' Consecutive runs of the same primitive type are batched into single
#' vectorized grob calls for efficiency.
#'
#' @param data Data frame with transformed NPC coordinates, depth-sorted.
#'   Must contain a `.prim` column with values in `"polygon"`, `"point"`,
#'   `"segment"`, `"text"`.
#' @param rule Polygon fill rule: `"evenodd"` or `"winding"`. Only used
#'   for polygons with subgroups (holes).
#' @param arrow Arrow specification for segments, created by [grid::arrow()].
#' @param arrow.fill Fill colour for closed arrowheads on the layer's own
#'   segments; `NULL` uses each segment's colour.
#' @param lineend Line end style for segments.
#' @return A grid grob.
#' @keywords internal
#' @noRd
render_mixed_grobs <- function(data, rule = "evenodd",
                               arrow = NULL, arrow.fill = NULL, lineend = "butt") {

      if (nrow(data) == 0) return(grid::nullGrob())

      # If no .prim column, treat everything as polygon (backward compat)
      if (!".prim" %in% names(data)) {
            data$.prim <- "polygon"
      }

      # Identify runs of consecutive same-type primitives
      runs <- compute_prim_runs(data)

      if (nrow(runs) == 1) {
            # Single type: fast path
            return(render_single_type(data, runs$.prim[1], rule, arrow, arrow.fill, lineend))
      }

      # Multiple types: render each run, assemble into grobTree
      grobs <- vector("list", nrow(runs))
      for (i in seq_len(nrow(runs))) {
            run_data <- data[runs$start[i]:runs$end[i], ]
            grobs[[i]] <- render_single_type(run_data, runs$.prim[i],
                                             rule, arrow, arrow.fill, lineend)
      }

      # Drop nullGrobs
      grobs <- grobs[!vapply(grobs, inherits, logical(1), "null")]
      if (length(grobs) == 0) return(grid::nullGrob())
      if (length(grobs) == 1) return(grobs[[1]])
      do.call(grid::grobTree, grobs)
}


#' Compute runs of consecutive same-type primitives
#'
#' Groups are the atomic unit: all rows in a group share a .prim type.
#' We detect run boundaries by looking at where .prim changes across
#' consecutive rows (groups are contiguous after depth sorting).
#'
#' @param data Data frame with `.prim` and `group` columns.
#' @return Data frame with columns `.prim`, `start`, `end` (row indices).
#' @keywords internal
#' @noRd
compute_prim_runs <- function(data) {
      prims <- data$.prim
      # Detect where .prim changes
      changes <- c(TRUE, prims[-1] != prims[-length(prims)])
      run_id <- cumsum(changes)

      # Build run table
      run_starts <- which(changes)
      run_ends <- c(run_starts[-1] - 1L, nrow(data))
      run_types <- prims[run_starts]

      data.frame(.prim = run_types, start = run_starts, end = run_ends,
                 stringsAsFactors = FALSE)
}


# Type-specific renderers ----------------------------------------------------

#' Dispatch to the correct renderer for a single primitive type
#' @keywords internal
#' @noRd
render_single_type <- function(data, prim_type, rule, arrow, arrow.fill, lineend) {
      switch(prim_type,
             polygon = render_polygons(data, rule),
             point   = render_points(data),
             segment = render_segments(data, arrow, arrow.fill, lineend),
             text    = render_texts(data),
             {
                   warning("Unknown .prim type: ", prim_type, "; skipping")
                   grid::nullGrob()
             }
      )
}


#' Render polygon primitives
#'
#' Handles both simple polygons and polygons with holes (subgroups).
#' All polygons in the batch are rendered in a single vectorized grob call.
#'
#' @param data Polygon data with x, y, group, and aesthetic columns.
#' @param rule Fill rule for polygons with holes.
#' @return A polygonGrob or pathGrob.
#' @keywords internal
#' @noRd
render_polygons <- function(data, rule = "evenodd") {
      if (nrow(data) == 0) return(grid::nullGrob())

      if (".cut_edge" %in% names(data) && any(data$.cut_edge, na.rm = TRUE)) {
            return(render_cut_polygons(data, rule))
      }

      # Detect subgroup column
      has_subgroup <- any(c("subgroup", ".subgroup") %in% names(data))
      if (has_subgroup) {
            sg_col <- if (".subgroup" %in% names(data)) ".subgroup" else "subgroup"
      }

      # Map group to integer IDs preserving data order (not alphabetical)
      unique_groups <- unique(data$group)
      group_ids <- match(data$group, unique_groups)

      # Extract per-group aesthetics (one value per group, in data order)
      group_first_idx <- !duplicated(data$group)
      group_col <- data$colour[group_first_idx]
      group_fill <- data$fill[group_first_idx]
      group_lwd <- data$linewidth[group_first_idx] * .pt
      group_lty <- if ("linetype" %in% names(data)) data$linetype[group_first_idx] else 1
      group_lty <- ifelse(is.na(group_lty), 1, group_lty)
      group_alpha <- data$alpha[group_first_idx]
      group_alpha <- ifelse(is.na(group_alpha), 1, group_alpha)

      if (has_subgroup) {
            # Ensure rows are contiguous by group and ordered by subgroup within,
            # while preserving the inter-group depth-sorted order.
            grp_rank <- match(data$group, unique_groups)
            data <- data[order(grp_rank, data[[sg_col]]), ]

            # Recompute after reordering
            unique_groups <- unique(data$group)
            group_ids <- match(data$group, unique_groups)
            unique_subgroups <- unique(data[[sg_col]])
            subgroup_ids <- match(data[[sg_col]], unique_subgroups)

            group_first_idx <- !duplicated(data$group)
            group_col <- data$colour[group_first_idx]
            group_fill <- data$fill[group_first_idx]
            group_lwd <- data$linewidth[group_first_idx] * .pt
            group_lty <- if ("linetype" %in% names(data)) data$linetype[group_first_idx] else 1
            group_lty <- ifelse(is.na(group_lty), 1, group_lty)
            group_alpha <- data$alpha[group_first_idx]
            group_alpha <- ifelse(is.na(group_alpha), 1, group_alpha)

            grid::pathGrob(
                  x = data$x,
                  y = data$y,
                  id = subgroup_ids,
                  pathId = group_ids,
                  rule = rule,
                  default.units = "npc",
                  gp = grid::gpar(
                        col = group_col,
                        fill = group_fill,
                        lwd = group_lwd,
                        lty = group_lty,
                        alpha = group_alpha
                  )
            )
      } else {
            grid::polygonGrob(
                  x = data$x,
                  y = data$y,
                  id = group_ids,
                  default.units = "npc",
                  gp = grid::gpar(
                        col = group_col,
                        fill = group_fill,
                        lwd = group_lwd,
                        lty = group_lty,
                        alpha = group_alpha
                  )
            )
      }
}


#' Render polygons that include pieces of split polygons
#'
#' Pieces produced by `split_intersecting_polygons()` carry a per-vertex
#' `.cut_edge` flag. Stroking their full outlines would draw the cut lines,
#' so each cut piece is drawn as a fill-only polygon followed by a polyline
#' along its original (uncut) edges. Runs of consecutive uncut groups are
#' batched through the standard polygon renderer. Draw order is preserved.
#'
#' @param data Polygon data with a `.cut_edge` column, depth-sorted.
#' @param rule Fill rule passed through for uncut polygons.
#' @return A grobTree.
#' @keywords internal
#' @noRd
render_cut_polygons <- function(data, rule = "evenodd") {
      cut_flag <- !is.na(data$.cut_edge) & data$.cut_edge
      data$.cut_edge <- NULL

      grp <- data$group
      ugrp <- unique(grp)
      grp_has_cut <- unname(tapply(cut_flag, factor(grp, levels = ugrp), any))

      # Runs of consecutive groups sharing cut status
      run_id <- cumsum(c(TRUE, grp_has_cut[-1] != grp_has_cut[-length(grp_has_cut)]))
      row_run <- run_id[match(grp, ugrp)]

      grobs <- list()
      for (r in unique(run_id)) {
            rows <- which(row_run == r)
            if (!grp_has_cut[which(run_id == r)[1]]) {
                  grobs <- c(grobs, list(render_polygons(data[rows, , drop = FALSE], rule)))
                  next
            }
            for (g in ugrp[run_id == r]) {
                  gi <- rows[grp[rows] == g]
                  grobs <- c(grobs, list(cut_polygon_grob(data[gi, , drop = FALSE],
                                                          cut_flag[gi])))
            }
      }
      do.call(grid::grobTree, grobs)
}


#' Grob for a single cut polygon piece: fill plus original-edge outline
#'
#' @param d Rows of one polygon group, in vertex order.
#' @param flag Logical vector; `flag[k]` is TRUE if the edge from vertex k to
#'   vertex k+1 (wrapping) lies along a cut.
#' @return A grob.
#' @keywords internal
#' @noRd
cut_polygon_grob <- function(d, flag) {
      alpha <- d$alpha[1]
      if (is.na(alpha)) alpha <- 1
      lty <- if ("linetype" %in% names(d) && !is.na(d$linetype[1])) d$linetype[1] else 1

      grobs <- list(grid::polygonGrob(
            x = d$x, y = d$y, default.units = "npc",
            gp = grid::gpar(col = NA, fill = d$fill[1], alpha = alpha)
      ))

      # Opaque pieces: a hairline in the fill colour along cut edges hides the
      # anti-aliasing gap between abutting pieces. Skipped for translucent
      # fills, where the overlap would darken the cut line.
      if (alpha >= 1 && !is.na(d$fill[1]) && any(flag)) {
            cut <- edge_chains(d$x, d$y, flag)
            grobs <- c(grobs, list(grid::polylineGrob(
                  x = cut$x, y = cut$y, id = cut$id, default.units = "npc",
                  gp = grid::gpar(col = d$fill[1], lwd = 0.5, lineend = "round")
            )))
      }

      if (!is.na(d$colour[1]) && !all(flag)) {
            orig <- edge_chains(d$x, d$y, !flag)
            grobs <- c(grobs, list(grid::polylineGrob(
                  x = orig$x, y = orig$y, id = orig$id, default.units = "npc",
                  gp = grid::gpar(col = d$colour[1], lwd = d$linewidth[1] * .pt,
                                  lty = lty, alpha = alpha)
            )))
      }

      if (length(grobs) == 1) return(grobs[[1]])
      do.call(grid::grobTree, grobs)
}


#' Chains of consecutive selected edges around a closed ring
#'
#' @param x,y Ring vertex coordinates.
#' @param keep Logical vector; `keep[k]` selects the edge from vertex k to
#'   vertex k+1 (wrapping).
#' @return List with `x`, `y`, and polyline `id` vectors.
#' @keywords internal
#' @noRd
edge_chains <- function(x, y, keep) {
      n <- length(x)
      if (all(keep)) return(list(x = c(x, x[1]), y = c(y, y[1]), id = rep(1L, n + 1)))
      start <- which(!keep)[1] %% n + 1
      ord <- c(start:n, seq_len(start - 1))
      xs <- numeric(0); ys <- numeric(0); ids <- integer(0)
      chain <- 0L
      in_chain <- FALSE
      for (k in ord) {
            if (!keep[k]) {
                  in_chain <- FALSE
                  next
            }
            nk <- if (k == n) 1L else k + 1L
            if (!in_chain) {
                  chain <- chain + 1L
                  xs <- c(xs, x[k]); ys <- c(ys, y[k]); ids <- c(ids, chain)
                  in_chain <- TRUE
            }
            xs <- c(xs, x[nk]); ys <- c(ys, y[nk]); ids <- c(ids, chain)
      }
      list(x = xs, y = ys, id = ids)
}


#' Normalize a font face to a form grid accepts
#'
#' Font faces arrive as numbers (1-4) or names, and binding annotation rows
#' onto layer data can coerce numbers to strings such as `"1"`, which grid
#' rejects. Numeric values, including numeric strings, are mapped to names.
#'
#' @param x Font face value(s).
#' @return Character vector of font face names.
#' @keywords internal
#' @noRd
as_fontface <- function(x) {
      faces <- c("plain", "bold", "italic", "bold.italic")
      num <- suppressWarnings(as.integer(x))
      ifelse(!is.na(num) & num %in% 1:4, faces[pmax(1L, pmin(4L, num))],
             as.character(x))
}


#' Render point primitives
#'
#' Each group is a single point. Points are rendered in a single vectorized
#' pointsGrob call when all share the same shape, or as individual grobs
#' when shapes differ (required by grid's pointsGrob which takes scalar pch).
#'
#' @param data Point data with x, y, and aesthetic columns.
#' @return A pointsGrob or grobTree.
#' @keywords internal
#' @noRd
render_points <- function(data) {
      if (nrow(data) == 0) return(grid::nullGrob())

      # Points: one row per group (take first row of each group)
      pts <- data[!duplicated(data$group), ]

      # Detect shape complexity for fill handling
      shapes <- pts$shape
      is_complex <- is.numeric(shapes) & shapes %in% 21:25

      # Compute fontsize: complex shapes use size + 0.5*stroke, simple use size
      stroke <- if ("stroke" %in% names(pts)) pts$stroke else rep(0.5, nrow(pts))
      fontsize <- ifelse(is_complex,
                         (pts$size + 0.5 * stroke) * .pt,
                         pts$size * .pt)

      # Fill: complex shapes use fill column, simple shapes use NA
      fill <- ifelse(is_complex,
                     ifelse(is.na(pts$fill), NA_character_, pts$fill),
                     NA_character_)

      alpha <- ifelse(is.na(pts$alpha), 1, pts$alpha)

      # If all shapes are the same, single vectorized call
      if (length(unique(shapes)) == 1) {
            grid::pointsGrob(
                  x = pts$x,
                  y = pts$y,
                  pch = shapes[1],
                  default.units = "npc",
                  gp = grid::gpar(
                        col = pts$colour,
                        fill = fill,
                        fontsize = fontsize,
                        lwd = stroke * .pt,
                        alpha = alpha
                  )
            )
      } else {
            # Mixed shapes: individual grobs (grid limitation on pch)
            grobs <- lapply(seq_len(nrow(pts)), function(i) {
                  grid::pointsGrob(
                        x = pts$x[i],
                        y = pts$y[i],
                        pch = shapes[i],
                        default.units = "npc",
                        gp = grid::gpar(
                              col = pts$colour[i],
                              fill = fill[i],
                              fontsize = fontsize[i],
                              lwd = stroke[i] * .pt,
                              alpha = alpha[i]
                        )
                  )
            })
            do.call(grid::grobTree, grobs)
      }
}


#' Render segment primitives
#'
#' Each group is a segment with exactly two rows (start and end points).
#' Segments are rendered in a single vectorized segmentsGrob call.
#'
#' @param data Segment data with x, y, group, and aesthetic columns.
#'   Each group must have exactly 2 rows with `point_type` "start"/"end",
#'   or simply two rows in order.
#' @param arrow Arrow specification.
#' @param arrow.fill Fill colour for closed arrowheads on the layer's own
#'   segments; `NULL` uses each segment's colour.
#' @param lineend Line end style.
#' @return A segmentsGrob.
#' @keywords internal
#' @noRd
render_segments <- function(data, arrow = NULL, arrow.fill = NULL, lineend = "butt") {
      if (nrow(data) == 0) return(grid::nullGrob())

      # Segments are stored as two rows per group: start and end
      # Identify start/end using point_type column if present,
      # otherwise use first/second row of each group
      if ("point_type" %in% names(data)) {
            start_rows <- data[data$point_type == "start", ]
            end_rows <- data[data$point_type == "end", ]
            # Match end rows to start rows by group, preserving start row order
            end_rows <- end_rows[match(start_rows$group, end_rows$group), ]
      } else {
            # Generic: first row = start, second row = end within each group
            groups <- unique(data$group)
            start_idx <- integer(length(groups))
            end_idx <- integer(length(groups))
            for (i in seq_along(groups)) {
                  rows <- which(data$group == groups[i])
                  if (length(rows) >= 2) {
                        start_idx[i] <- rows[1]
                        end_idx[i] <- rows[2]
                  }
            }
            valid <- start_idx > 0 & end_idx > 0
            start_rows <- data[start_idx[valid], ]
            end_rows <- data[end_idx[valid], ]
      }

      if (nrow(start_rows) == 0) return(grid::nullGrob())

      lty <- if ("linetype" %in% names(start_rows)) start_rows$linetype else 1
      lty <- rep_len(lty, nrow(start_rows))

      arrows <- segment_arrows(start_rows, arrow)
      ends <- segment_arrow_ends(start_rows, arrows$arrows)
      fill <- rep_len(arrow.fill %||% start_rows$colour, nrow(start_rows))
      fill[arrows$own] <- start_rows$colour[arrows$own]

      draw <- function(i) {
            k <- i[1]
            arrow_i <- if (ends[k] == "none") NULL else arrow_with_ends(arrows$arrows[[k]], ends[k])
            gp <- grid::gpar(
                  col = start_rows$colour[i],
                  lwd = start_rows$linewidth[i] * .pt,
                  lty = lty[i],
                  lineend = lineend,
                  alpha = ifelse(is.na(start_rows$alpha[i]), 1, start_rows$alpha[i])
            )
            if (!is.null(arrow_i)) gp$fill <- fill[i]
            grid::segmentsGrob(
                  x0 = start_rows$x[i], y0 = start_rows$y[i],
                  x1 = end_rows$x[i], y1 = end_rows$y[i],
                  default.units = "npc",
                  arrow = arrow_i,
                  gp = gp
            )
      }

      # Segments can differ in their arrows (an annotation's own arrow, or
      # pieces of a split segment carrying only some of its arrowheads). Draw
      # each run of consecutive segments sharing an arrow configuration as one
      # grob, so the depth-sorted drawing order is preserved.
      key <- vapply(seq_along(ends), function(k) {
            if (ends[k] == "none") return("none")
            a <- arrows$arrows[[k]]
            paste(ends[k], a$angle, format(a$length), a$type)
      }, character(1))

      if (length(unique(key)) == 1) return(draw(seq_along(key)))

      run <- cumsum(c(TRUE, key[-1] != key[-length(key)]))
      grobs <- lapply(split(seq_along(key), run), draw)
      do.call(grid::grobTree, unname(grobs))
}


#' Arrow for each segment
#'
#' The layer's arrow applies to the layer's own segments. Annotation segments
#' carry their own arrow, if any, in `.arrow_*` columns (see
#' `add_arrow_columns()`), and never take the layer's.
#'
#' @param start_rows Start rows of the segments, one per segment.
#' @param arrow The layer's arrow from [grid::arrow()], or NULL.
#' @return List with `arrows` (an arrow or NULL per segment) and logical
#'   `own`, marking segments that carry their own arrow.
#' @keywords internal
#' @noRd
segment_arrows <- function(start_rows, arrow) {
      n <- nrow(start_rows)
      own <- if (".arrow_type" %in% names(start_rows)) !is.na(start_rows$.arrow_type) else rep(FALSE, n)
      arrows <- rep(list(arrow), n)
      ann <- if (".ann" %in% names(start_rows)) start_rows$.ann %in% TRUE else rep(FALSE, n)
      arrows[ann & !own] <- list(NULL)
      for (k in which(own)) {
            arrows[[k]] <- grid::arrow(
                  angle = start_rows$.arrow_angle[k],
                  length = grid::unit(start_rows$.arrow_length[k], start_rows$.arrow_unit[k]),
                  ends = c("first", "last", "both")[start_rows$.arrow_ends[k]],
                  type = c("open", "closed")[start_rows$.arrow_type[k]]
            )
      }
      list(arrows = arrows, own = own)
}


#' Which ends of each segment should carry an arrowhead
#'
#' A segment split where it passes through polygons keeps the arrowheads of
#' the original segment on the pieces holding its ends: an arrowhead at the
#' start only on the first piece (`.seg_first`), and at the end only on the
#' last piece (`.seg_last`). Unsplit segments keep their arrow as given.
#'
#' @param start_rows Start rows of the segments, one per segment.
#' @param arrows List of arrows (or NULL) per segment, from `segment_arrows()`.
#' @return Character vector of `"first"`, `"last"`, `"both"`, or `"none"`.
#' @keywords internal
#' @noRd
segment_arrow_ends <- function(start_rows, arrows) {
      code <- vapply(arrows, function(a) if (is.null(a)) 0L else as.integer(a$ends[1]), integer(1))
      first <- code %in% c(1L, 3L)                     # 1 = first, 2 = last, 3 = both
      last <- code %in% c(2L, 3L)

      is_first <- if (".seg_first" %in% names(start_rows)) start_rows$.seg_first else NA
      is_last <- if (".seg_last" %in% names(start_rows)) start_rows$.seg_last else NA
      first <- first & (is.na(is_first) | is_first)
      last <- last & (is.na(is_last) | is_last)

      ifelse(first & last, "both", ifelse(first, "first", ifelse(last, "last", "none")))
}


#' Copy of an arrow specification with different ends
#' @keywords internal
#' @noRd
arrow_with_ends <- function(arrow, ends) {
      arrow$ends <- match(ends, c("first", "last", "both"))
      arrow
}


#' Render text primitives (billboard)
#'
#' Each group is a single text label. Rendered as individual textGrobs
#' assembled into a grobTree.
#'
#' @param data Text data with x, y, label, and aesthetic columns.
#' @return A grobTree of textGrobs.
#' @keywords internal
#' @noRd
render_texts <- function(data) {
      if (nrow(data) == 0) return(grid::nullGrob())

      # One row per label (take first row of each group)
      labels <- data[!duplicated(data$group), ]

      fontsize <- labels$size * .pt
      alpha <- ifelse(is.na(labels$alpha), 1, labels$alpha)

      grobs <- lapply(seq_len(nrow(labels)), function(i) {
            grid::textGrob(
                  label = labels$label[i],
                  x = labels$x[i],
                  y = labels$y[i],
                  default.units = "npc",
                  hjust = labels$hjust[i] %||% 0.5,
                  vjust = labels$vjust[i] %||% 0.5,
                  rot = labels$angle[i] %||% 0,
                  gp = grid::gpar(
                        col = alpha(labels$colour[i], alpha[i]),
                        fontsize = fontsize[i],
                        fontfamily = labels$family[i] %||% "",
                        fontface = as_fontface(labels$fontface[i] %||% 1),
                        lineheight = labels$lineheight[i] %||% 1.2
                  )
            )
      })

      if (length(grobs) == 1) return(grobs[[1]])
      do.call(grid::grobTree, grobs)
}
