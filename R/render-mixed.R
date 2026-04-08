# Mixed-geometry rendering pipeline
#
# Shared rendering function that produces grobs from depth-sorted data
# containing multiple primitive types (polygon, point, segment, text).
# Data arrives already transformed to NPC coordinates, depth-sorted, and
# with resolved aesthetic values. This function handles only the final
# grob assembly step.


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
#' @param lineend Line end style for segments.
#' @return A grid grob.
#' @keywords internal
render_mixed_grobs <- function(data, rule = "evenodd",
                               arrow = NULL, lineend = "butt") {

      if (nrow(data) == 0) return(grid::nullGrob())

      # If no .prim column, treat everything as polygon (backward compat)
      if (!".prim" %in% names(data)) {
            data$.prim <- "polygon"
      }

      # Identify runs of consecutive same-type primitives
      runs <- compute_prim_runs(data)

      if (nrow(runs) == 1) {
            # Single type: fast path
            return(render_single_type(data, runs$.prim[1], rule, arrow, lineend))
      }

      # Multiple types: render each run, assemble into grobTree
      grobs <- vector("list", nrow(runs))
      for (i in seq_len(nrow(runs))) {
            run_data <- data[runs$start[i]:runs$end[i], ]
            grobs[[i]] <- render_single_type(run_data, runs$.prim[i],
                                             rule, arrow, lineend)
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
render_single_type <- function(data, prim_type, rule, arrow, lineend) {
      switch(prim_type,
             polygon = render_polygons(data, rule),
             point   = render_points(data),
             segment = render_segments(data, arrow, lineend),
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
render_polygons <- function(data, rule = "evenodd") {
      if (nrow(data) == 0) return(grid::nullGrob())

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
      group_lty <- data$linetype[group_first_idx]
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
            group_lty <- data$linetype[group_first_idx]
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


#' Render point primitives
#'
#' Each group is a single point. Points are rendered in a single vectorized
#' pointsGrob call when all share the same shape, or as individual grobs
#' when shapes differ (required by grid's pointsGrob which takes scalar pch).
#'
#' @param data Point data with x, y, and aesthetic columns.
#' @return A pointsGrob or grobTree.
#' @keywords internal
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
#'   Each group must have exactly 2 rows. The group must contain
#'   `__start` and `__end` suffixed rows, or simply two rows in order.
#' @param arrow Arrow specification.
#' @param lineend Line end style.
#' @return A segmentsGrob.
#' @keywords internal
render_segments <- function(data, arrow = NULL, lineend = "butt") {
      if (nrow(data) == 0) return(grid::nullGrob())

      # Segments are stored as two rows per group: start and end
      # Identify start/end using point_type column if present,
      # otherwise use first/second row of each group
      if ("point_type" %in% names(data)) {
            start_rows <- data[data$point_type == "start", ]
            end_rows <- data[data$point_type == "end", ]
            # Match end rows to start rows by segment_id, preserving start row order
            if ("segment_id" %in% names(data)) {
                  end_rows <- end_rows[match(start_rows$segment_id, end_rows$segment_id), ]
            }
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

      grid::segmentsGrob(
            x0 = start_rows$x, y0 = start_rows$y,
            x1 = end_rows$x, y1 = end_rows$y,
            default.units = "npc",
            arrow = arrow,
            gp = grid::gpar(
                  col = start_rows$colour,
                  lwd = start_rows$linewidth * .pt,
                  lty = lty,
                  lineend = lineend,
                  alpha = ifelse(is.na(start_rows$alpha), 1, start_rows$alpha)
            )
      )
}


#' Render text primitives (billboard)
#'
#' Each group is a single text label. Rendered as individual textGrobs
#' assembled into a grobTree.
#'
#' @param data Text data with x, y, label, and aesthetic columns.
#' @return A grobTree of textGrobs.
#' @keywords internal
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
                        fontface = labels$fontface[i] %||% 1,
                        lineheight = labels$lineheight[i] %||% 1.2
                  )
            )
      })

      if (length(grobs) == 1) return(grobs[[1]])
      do.call(grid::grobTree, grobs)
}
