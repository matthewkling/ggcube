# Depth sorting for 3D rendering
#
# Orders primitives back to front for painter's-algorithm rendering. Supports
# mixed primitive types (polygon, point, segment, text) identified by the
# `.prim` column. Polygons that pass through one another, and segments that
# pass through polygons, are split first; primitives are then ordered from
# pairwise occlusion constraints between those that overlap on screen.
#
# The pairwise comparisons and constraint graph are computed in C++
# (src/pairwise.cpp, src/graph.cpp). Candidate pairs are those whose screen
# bounding boxes overlap, so cost grows with the number of overlapping
# pairs rather than with the square of the number of primitives.
#
# Pairwise comparison coverage:
#   polygon-polygon: separating axis test, depth interpolated at the centroid
#                    of their screen intersection
#   polygon-point:   point-in-polygon test, depth interpolated at the point
#   polygon-segment: segment clipped to the polygon, depth interpolated at the
#                    midpoint of the clipped portion
#   segment-segment: depths interpolated at their screen crossing
#   point-segment:   bbox proximity test, depth at closest point on segment
#   point-point:     no constraint
#
# Where a polygon is involved, the sign of each constraint is settled exactly
# from 3D geometry where possible (plane-side tests, primitives lying in a
# polygon's plane, and exact lines of sight). When primitives are coplanar
# (depth delta ~ 0), smaller primitives render on top: point > segment >
# polygon.


# Screen polygon helpers -----------------------------------------------------------

get_centroid <- function(p) {
      x <- p$x
      y <- p$y

      # Close the loop if it's not already closed
      n <- length(x)
      if (x[1] != x[n] || y[1] != y[n]) {
            x <- c(x, x[1])
            y <- c(y, y[1])
            n <- n + 1
      }

      i <- 1:(n - 1)
      cross_prod <- x[i] * y[i+1] - x[i+1] * y[i]
      area <- sum(cross_prod) / 2
      cx <- sum((x[i] + x[i+1]) * cross_prod) / (6 * area)
      cy <- sum((y[i] + y[i+1]) * cross_prod) / (6 * area)

      return(c(x = cx, y = cy))
}

# A representative point of the screen intersection of two polygons, or NULL
# if they don't intersect: the centroid of its largest part, or if that falls
# outside the part, a point inside it. Used for pairs of non-convex polygons,
# which the C++ comparison hands back to R.
intersection_centroid <- function(poly1, poly2) {
      clipped <- polyclip::polyclip(list(as.data.frame(poly1)),
                                    list(as.data.frame(poly2)))
      if(length(clipped) == 0) return(NULL)

      area <- vapply(clipped, function(p) abs(sum(p$x * c(p$y[-1], p$y[1]) - c(p$x[-1], p$x[1]) * p$y)), numeric(1))
      part <- clipped[[which.max(area)]]
      interior_point_cpp(part$x, part$y)
}


# Point-in-polygon test using ray casting
point_in_polygon <- function(px, py, poly_x, poly_y) {
      n <- length(poly_x)
      inside <- FALSE
      j <- n
      for (i in seq_len(n)) {
            if (((poly_y[i] > py) != (poly_y[j] > py)) &&
                (px < (poly_x[j] - poly_x[i]) * (py - poly_y[i]) /
                 (poly_y[j] - poly_y[i]) + poly_x[i])) {
                  inside <- !inside
            }
            j <- i
      }
      inside
}


# Pairwise render order --------------------------------------------------------------

.prim_codes <- c(polygon = 0L, point = 1L, segment = 2L, text = 3L)

#' Pairwise render order with mixed-geometry support
#'
#' Splits interpenetrating polygons and segments, derives occlusion
#' constraints between primitives that overlap on screen, breaks any cycles,
#' and orders primitives topologically, drawing the farthest available
#' primitive first.
#'
#' @param data Sort-stage data, as handed to `sort_by_depth()`.
#' @param depth_var Name of the depth column.
#' @param proj,plot_bounds Projection parameters and plot bounds; needed for
#'   splitting and for exact ordering, which are skipped without them.
#' @return List with `data` (after splitting) and `render_order`, the draw
#'   position of each row's group.
#' @keywords internal
#' @noRd
pw_render_order <- function(data,
                            depth_var = "depth",
                            proj = NULL,
                            plot_bounds = NULL
){
      data <- split_for_sorting(data, proj, plot_bounds)
      list(data = data, render_order = pw_order(data, depth_var, proj, plot_bounds))
}

#' Pairwise draw order of already-split primitives
#'
#' @inheritParams order_by_depth
#' @return The draw position of each row's group.
#' @keywords internal
#' @noRd
pw_order <- function(data, depth_var = "depth", proj = NULL, plot_bounds = NULL, panel_pt = NULL) {
      scene <- pw_scene(data, depth_var, proj, plot_bounds, panel_pt)
      cons <- pw_constraints(scene)
      edges <- break_cycles(cons$edges, scene$k)
      ord <- topological_sort(scene$k, edges, cons$depth)
      match(data$group, scene$groups[ord])
}


#' Flatten sort-stage data into the arrays the C++ sorter takes
#'
#' Groups are numbered in order of first appearance, with each group's rows
#' contiguous and in their original order.
#'
#' @return List of arguments for `pw_constraints_cpp()`, plus `groups`
#'   (group names), `k` (number of groups), and `order` (row permutation).
#' @keywords internal
#' @noRd
pw_scene <- function(data, depth_var = "depth", proj = NULL, plot_bounds = NULL, panel_pt = NULL) {
      grp <- data$group
      groups <- unique(grp)
      k <- length(groups)
      gi <- match(grp, groups)
      ord <- order(gi)
      offsets <- c(0L, cumsum(tabulate(gi, k)))
      first <- ord[offsets[-(k + 1)] + 1L]

      # Primitive type per group. Groups without one (e.g. in point layers,
      # whose types are assigned only when rendering) are typed by their
      # vertex count: 1 = point, 2 = segment, 3+ = polygon.
      prim <- if (".prim" %in% names(data)) as.character(data$.prim[first]) else rep(NA_character_, k)
      untyped <- is.na(prim)
      if (any(untyped)) {
            n_vert <- diff(offsets)[untyped]
            prim[untyped] <- ifelse(n_vert == 1, "point", ifelse(n_vert == 2, "segment", "polygon"))
      }
      type <- unname(.prim_codes[prim])
      type[is.na(type)] <- 4L

      # Pieces cut from the same polygon are coplanar and only share edges,
      # so they impose no ordering constraint on one another
      gname <- as.character(groups)
      is_cut <- grepl("__cut[0-9]+$", gname)
      parent <- sub("__cut[0-9]+$", "", gname)
      cut_parent <- ifelse(is_cut, match(parent, unique(parent[is_cut])) - 1L, -1L)

      has3d <- all(c(".x3d", ".y3d", ".z3d") %in% names(data))
      num <- function(v) as.numeric(v[ord])

      list(x = num(data$x), y = num(data$y), d = num(data[[depth_var]]),
           x3 = if (has3d) num(data$.x3d) else numeric(0),
           y3 = if (has3d) num(data$.y3d) else numeric(0),
           z3 = if (has3d) num(data$.z3d) else numeric(0),
           offsets = as.integer(offsets), type = as.integer(type),
           cut_parent = as.integer(cut_parent),
           eye = eye_vector(proj), proj = proj_vector(proj, plot_bounds),
           marks = list(panel = as.numeric(panel_pt %||% numeric(0)),
                        radius = group_value(data, ".radius_pt", first),
                        halfwidth = group_value(data, ".halfwidth_pt", first),
                        layer = as.integer(group_value(data, ".layer", first))),
           groups = groups, k = k, order = ord)
}

# A column's value at each group's first row, or empty if absent
group_value <- function(data, col, first) {
      if (col %in% names(data)) as.numeric(data[[col]][first]) else numeric(0)
}

eye_vector <- function(proj) {
      eye <- eye_in_cube_space(proj)
      if (is.null(eye)) numeric(0) else c(as.numeric(eye$persp), eye$v)
}

proj_vector <- function(proj, plot_bounds) {
      if (is.null(proj) || is.null(plot_bounds)) return(numeric(0))
      rot <- rotate_3d(diag(3), proj$pitch, proj$roll, proj$yaw)
      c(as.numeric(isTRUE(proj$persp)), proj$dist %||% 2, as.vector(rot), plot_bounds)
}


#' Occlusion constraints between overlapping primitives
#'
#' @param scene Output of `pw_scene()`.
#' @return List with `edges`, a data frame of constraints (`from` behind
#'   `to`, 1-based group indices, with `strength` and `locked`), and `depth`,
#'   each group's mean depth.
#' @keywords internal
#' @noRd
pw_constraints <- function(scene) {
      args <- scene[c("x", "y", "d", "x3", "y3", "z3", "offsets", "type", "cut_parent", "eye", "proj", "marks")]
      res <- do.call(pw_constraints_cpp, args)
      edges <- data.frame(from = res$from, to = res$to, strength = res$strength, locked = res$locked)

      # Pairs of non-convex polygons whose screen intersection is needed are
      # finished with intersections from polyclip
      if (length(res$deferred_i) > 0) {
            ring <- function(g) {
                  idx <- (scene$offsets[g + 1] + 1L):scene$offsets[g + 2]
                  cbind(x = scene$x[idx], y = scene$y[idx])
            }
            xy <- t(mapply(function(i, j) {
                  ctr <- intersection_centroid(ring(i), ring(j))
                  if (is.null(ctr)) c(NaN, NaN) else unname(ctr)
            }, res$deferred_i, res$deferred_j))
            fin <- do.call(pw_finish_pairs_cpp, c(args, list(pi = res$deferred_i, pj = res$deferred_j,
                                                              cx = xy[, 1], cy = xy[, 2])))
            edges <- rbind(edges, data.frame(from = fin$from, to = fin$to,
                                             strength = fin$strength, locked = fin$locked))
      }
      edges$from <- edges$from + 1L
      edges$to <- edges$to + 1L
      list(edges = edges, depth = res$depth, n_candidates = res$n_candidates)
}


# Constraint graph ---------------------------------------------------------------
# Edges are data frames with 1-based `from` (drawn first) and `to`, plus
# `strength` and `locked` where needed.

#' Remove the weakest constraints that lie on cycles
#'
#' Only constraints joining two members of the same strongly connected
#' component lie on a cycle and are candidates for removal. Unlocked
#' constraints (from sampled depths) are removed before locked ones (exact
#' geometric results), and weaker before stronger, until no cycles remain.
#'
#' @param edges Constraint data frame.
#' @param n Number of nodes.
#' @return The constraints kept.
#' @keywords internal
#' @noRd
break_cycles <- function(edges, n) {
      if (nrow(edges) == 0) return(edges)
      keep <- break_cycles_cpp(as.integer(n), edges$from - 1L, edges$to - 1L,
                               as.numeric(edges$strength), as.logical(edges$locked))
      edges[keep, , drop = FALSE]
}

#' Strongly connected components (integer component id per node)
#' @keywords internal
#' @noRd
strongly_connected_components <- function(n, edges) {
      scc_cpp(as.integer(n), edges$from - 1L, edges$to - 1L)
}

#' Whether a constraint graph has no cycles
#' @keywords internal
#' @noRd
is_acyclic <- function(n, edges) {
      topological_sort_cpp(as.integer(n), edges$from - 1L, edges$to - 1L, numeric(n))$complete
}

#' Topological order, drawing the farthest available node first
#'
#' Ties in depth go to the node that became available first.
#'
#' @param n Number of nodes.
#' @param edges Constraint data frame.
#' @param depths Depth per node (larger is farther).
#' @return 1-based node order.
#' @keywords internal
#' @noRd
topological_sort <- function(n, edges, depths = numeric(n)) {
      res <- topological_sort_cpp(as.integer(n), edges$from - 1L, edges$to - 1L, as.numeric(depths))
      if (!res$complete) warning("Topological sort failed - remaining cycles detected")
      res$order + 1L
}


#' Depth-sort primitives back to front
#'
#' Splits polygons and segments that pass through one another, then orders
#' primitives from pairwise occlusion constraints (see `order_by_depth()`).
#' Rows of each group keep their order (preserving polygon winding).
#'
#' @param data Sort-stage data: projected rows with `group`, `depth`,
#'   optionally `.prim`, and cube-space `.x3d`/`.y3d`/`.z3d`.
#' @param proj,plot_bounds Projection parameters and plot bounds, needed to
#'   split primitives and to order them exactly; both are skipped without
#'   them.
#' @return `data` with rows in drawing order.
#' @keywords internal
#' @noRd
sort_by_depth <- function(data, proj = NULL, plot_bounds = NULL) {
      data <- split_for_sorting(data, proj, plot_bounds)
      order_by_depth(data, proj, plot_bounds)
}

#' Split polygons that pass through one another, then segments that pass
#' through polygons, so that every pair has a consistent occlusion order
#' @keywords internal
#' @noRd
split_for_sorting <- function(data, proj, plot_bounds) {
      data <- split_intersecting_polygons(data, proj, plot_bounds)
      split_interpenetrating_segments(data, proj, plot_bounds)
}

#' Order (already split) primitives back to front
#'
#' @param data Split sort-stage data. Optional columns `.radius_pt` and
#'   `.halfwidth_pt` give the drawn extents of point markers and lines, and
#'   `.layer` each row's layer.
#' @param panel_pt Panel width and height in points, if known; marker and
#'   line extents are used only when given.
#' @return `data` with rows in drawing order.
#' @keywords internal
#' @noRd
order_by_depth <- function(data, proj = NULL, plot_bounds = NULL, panel_pt = NULL) {
      if (nrow(data) == 0) return(data)

      vertex_order <- stats::ave(seq_len(nrow(data)), data$group, FUN = seq_along)
      render_order <- pw_order(data, "depth", proj, plot_bounds, panel_pt)
      data[order(render_order, vertex_order), , drop = FALSE]
}


# Segment-polygon interpenetration splitting -----------------------------------

# Distance (cube units) below which a segment end counts as touching a polygon's
# plane rather than passing through it, e.g. residual lines that start on the
# surface they belong to
.split_tol <- 0.01

#' Split segments where they pass through polygons
#'
#' For each segment, finds every polygon it passes through in 3D and splits
#' the segment at those crossing points, so that each resulting piece lies on
#' one side of each polygon's plane and can be ordered against it exactly.
#'
#' Crossings are computed in standardized cube space (`.x3d`, `.y3d`, `.z3d`):
#' the segment's endpoints must lie on opposite sides of the polygon's plane,
#' each by more than `.split_tol` plus the polygon's warp (for non-planar
#' polygons, the distance of its vertices from their best-fit plane). The
#' crossing point must also fall inside the polygon's screen footprint, which
#' is tested by projecting it; for a point on the polygon's plane this is
#' equivalent to lying inside the polygon in 3D. New endpoints are projected
#' with the same functions used by `coord_3d()`.
#'
#' @param data Data frame with projected coordinates, `.x3d`/`.y3d`/`.z3d`,
#'   `group`, and `.prim`.
#' @param proj Projection parameters, as in `panel_params$proj`.
#' @param plot_bounds Plot bounds used for NPC scaling.
#' @return Data frame with interpenetrating segments replaced by sub-segments
#'   (groups suffixed `__split1`, `__split2`, ...), flagged with `.seg_first`
#'   and `.seg_last` to mark the pieces holding the original segment's start
#'   and end. Other rows are unchanged.
#' @keywords internal
#' @noRd
split_interpenetrating_segments <- function(data, proj, plot_bounds) {

      if (is.null(proj) || is.null(plot_bounds)) return(data)
      if (!all(c(".x3d", ".y3d", ".z3d", ".prim") %in% names(data))) return(data)

      grp <- as.character(data$group)
      ugrp <- unique(grp)
      grp_idx <- split(seq_along(grp), factor(grp, levels = ugrp))
      prim <- vapply(grp_idx, function(idx) data$.prim[idx[1]], character(1))

      seg_ids <- which(prim == "segment" & lengths(grp_idx) == 2)
      poly_ids <- which(prim == "polygon" & lengths(grp_idx) >= 3)
      if (length(seg_ids) == 0 || length(poly_ids) == 0) return(data)

      xyz <- cbind(data$.x3d, data$.y3d, data$.z3d)
      poly_rows <- unlist(grp_idx[poly_ids], use.names = FALSE)
      poly_off <- c(0L, cumsum(lengths(grp_idx[poly_ids])))
      ends <- vapply(grp_idx[seg_ids], function(idx) seg_endpoint_rows(data, idx), integer(2))

      # Candidate crossings (segment, polygon, t, 3D crossing point)
      cr <- segment_crossings_cpp(xyz[ends[1, ], 1], xyz[ends[1, ], 2], xyz[ends[1, ], 3],
                                  xyz[ends[2, ], 1], xyz[ends[2, ], 2], xyz[ends[2, ], 3],
                                  xyz[poly_rows, 1], xyz[poly_rows, 2], xyz[poly_rows, 3],
                                  as.integer(poly_off), .split_tol)
      if (length(cr$t) == 0) return(data)

      # Keep crossings that fall inside the polygon's footprint
      scr <- scale_to_npc_coordinates(
            transform_3d_standard(data.frame(x = cr$x, y = cr$y, z = cr$z), proj),
            plot_bounds = plot_bounds)
      inside <- points_in_polygons_cpp(scr$x, scr$y, cr$poly, data$x[poly_rows], data$y[poly_rows],
                                       as.integer(poly_off))
      if (!any(inside)) return(data)

      # Crossing parameters per split segment, well inside its ends
      split_ts <- split(cr$t[inside], cr$seg[inside] + 1L)
      split_ts <- lapply(split_ts, function(ts) {
            ts <- sort(unique(ts))
            ts[ts > 1e-3 & ts < 1 - 1e-3]
      })
      split_ts <- split_ts[lengths(split_ts) > 0]
      if (length(split_ts) == 0) return(data)

      # Build sub-segments: piece k of a segment runs between consecutive
      # breaks, as a start row and an end row
      data$group <- as.character(data$group)
      which_seg <- as.integer(names(split_ts))
      n_piece <- lengths(split_ts) + 1L
      piece_seg <- rep(which_seg, n_piece)
      piece_k <- sequence(n_piece)
      piece_n <- rep(n_piece, n_piece)
      breaks <- lapply(split_ts, function(ts) c(0, ts, 1))
      t0 <- unlist(lapply(breaks, function(br) br[-length(br)]), use.names = FALSE)
      t1 <- unlist(lapply(breaks, function(br) br[-1]), use.names = FALSE)

      start_idx <- ends[1, piece_seg]
      end_idx <- ends[2, piece_seg]
      row_idx <- as.vector(rbind(start_idx, end_idx))
      row_t <- as.vector(rbind(t0, t1))
      row_piece <- rep(seq_along(piece_seg), each = 2)
      new_rows <- data[row_idx, , drop = FALSE]
      a <- xyz[rep(start_idx, each = 2), , drop = FALSE]
      b <- xyz[rep(end_idx, each = 2), , drop = FALSE]
      p3 <- a + row_t * (b - a)
      new_rows$.x3d <- p3[, 1]
      new_rows$.y3d <- p3[, 2]
      new_rows$.z3d <- p3[, 3]
      new_rows$.new_vertex <- as.vector(rbind(t0 > 0, t1 < 1))
      new_rows$group <- paste0(data$group[start_idx], "__split", piece_k)[row_piece]
      # Which pieces hold the original segment's ends, so that arrowheads are
      # drawn only there (see render_segments())
      new_rows$.seg_first <- (piece_k == 1)[row_piece]
      new_rows$.seg_last <- (piece_k == piece_n)[row_piece]
      if ("segment_id" %in% names(data)) {
            # Pieces of a numbered segment share its number, offset by piece
            sid <- data$segment_id[start_idx][row_piece]
            new_rows$segment_id <- ifelse(is.na(sid), new_rows$segment_id,
                                          sid + (piece_k[row_piece] - 1) * 0.01)
      }
      rows_to_drop <- as.vector(ends[, which_seg])

      new_rows <- project_new_vertices(new_rows, proj, plot_bounds)
      new_rows$.new_vertex <- NULL
      rownames(new_rows) <- NULL

      kept <- data[-rows_to_drop, , drop = FALSE]
      kept$.seg_first <- rep(NA, nrow(kept))
      kept$.seg_last <- rep(NA, nrow(kept))
      rbind(kept, new_rows)
}


#' Start and end row indices of a two-row segment group
#'
#' @param data Data frame, optionally with a `point_type` column.
#' @param idx Row indices of the segment group.
#' @return Integer vector `c(start, end)`.
#' @keywords internal
#' @noRd
seg_endpoint_rows <- function(data, idx) {
      if ("point_type" %in% names(data)) {
            pt <- data$point_type[idx]
            if (all(c("start", "end") %in% pt)) {
                  return(c(idx[pt == "start"][1], idx[pt == "end"][1]))
            }
      }
      idx[1:2]
}


# Polygon-polygon interpenetration splitting -----------------------------------

#' Split polygons that interpenetrate other polygons in 3D
#'
#' Painter's algorithm cannot order two polygons that pass through each other.
#' This pre-pass finds such pairs and cuts one polygon of each pair along the
#' other's plane, so that each resulting piece lies entirely on one side of
#' that plane and has a consistent occlusion order with it. Cutting one member
#' of a pair is sufficient. When both are eligible, the smaller polygon is
#' cut, since each cut runs along the cutter's full plane: cutting a large
#' polygon by many small ones would fragment it across its whole extent.
#'
#' All geometry is computed in standardized cube space (`.x3d`, `.y3d`, `.z3d`),
#' where planes are exactly planar under both orthographic and perspective
#' projection. New vertices are then projected with the same functions used by
#' `coord_3d()`.
#'
#' A pair is split only when each polygon has vertices strictly on both sides
#' of the other's plane and their crossing intervals along the planes' line of
#' intersection overlap. Polygons that merely touch, such as adjacent tiles of
#' a surface sharing an edge or vertex, are left intact.
#'
#' Only planar polygons can act as cutters. Non-planar polygons (e.g. warped
#' quads on a curved surface) can still be cut by a planar polygon, with their
#' best-fit plane and warp used in the straddle test. Polygons with holes
#' (multiple subgroups) are never cut. Pairs of two non-planar polygons are
#' left unsplit.
#'
#' Each vertex of a split piece carries a `.cut_edge` flag indicating whether
#' the edge from that vertex to the next lies along a cut, so that outlines can
#' be drawn along original edges only.
#'
#' @param data Data frame with projected coordinates, `.x3d`/`.y3d`/`.z3d`
#'   cube-space coordinates, `group`, and optionally `.prim`.
#' @param proj Projection parameters, as in `panel_params$proj`.
#' @param plot_bounds Plot bounds used for NPC scaling.
#' @param tol Absolute distance tolerance in standardized cube units.
#' @return Data frame with interpenetrating polygons replaced by pieces. When
#'   any split occurs, `group` becomes character and `.cut_edge` is added.
#' @keywords internal
#' @noRd
split_intersecting_polygons <- function(data, proj, plot_bounds, tol = 1e-6) {

      if (is.null(proj) || is.null(plot_bounds)) return(data)
      if (!all(c(".x3d", ".y3d", ".z3d") %in% names(data))) return(data)
      if (nrow(data) == 0) return(data)

      grp <- data$group
      ugrp <- unique(grp)
      grp_idx <- split(seq_along(grp), factor(grp, levels = ugrp))

      if (".prim" %in% names(data)) {
            is_poly <- vapply(grp_idx, function(idx) identical(data$.prim[idx[1]], "polygon"),
                              logical(1))
      } else {
            is_poly <- rep(TRUE, length(grp_idx))
      }
      is_poly <- is_poly & lengths(grp_idx) >= 3
      if (sum(is_poly) < 2) return(data)

      sg_col <- intersect(c(".subgroup", "subgroup"), names(data))
      poly_ids <- which(is_poly)
      poly_rows <- unlist(grp_idx[poly_ids], use.names = FALSE)
      poly_off <- c(0L, cumsum(lengths(grp_idx[poly_ids])))

      # Polygons with holes (several subgroups) are never cut
      cuttable <- if (length(sg_col)) {
            vapply(grp_idx[poly_ids], function(idx) length(unique(data[[sg_col[1]]][idx])) <= 1, logical(1))
      } else {
            rep(TRUE, length(poly_ids))
      }

      pc <- split_polygons_cpp(data$.x3d[poly_rows], data$.y3d[poly_rows], data$.z3d[poly_rows],
                               as.integer(poly_off), cuttable, tol)
      if (length(pc$poly) == 0) return(data)

      # Assemble the pieces, copying each new vertex's other columns from the
      # vertex it was cut from
      data$group <- as.character(data$group)
      data$.cut_edge <- FALSE
      new_rows <- data[poly_rows[pc$src + 1L], , drop = FALSE]
      new_rows$.x3d <- pc$x3
      new_rows$.y3d <- pc$y3
      new_rows$.z3d <- pc$z3
      orig_group <- data$group[poly_rows[poly_off[pc$poly + 1L] + 1L]]
      new_rows$group <- paste0(orig_group, "__cut", pc$piece)
      new_rows$.cut_edge <- pc$cut_edge
      new_rows$.new_vertex <- pc$is_new
      rows_to_drop <- unlist(grp_idx[poly_ids[unique(pc$poly) + 1L]], use.names = FALSE)

      new_rows <- project_new_vertices(new_rows, proj, plot_bounds)
      new_rows$.new_vertex <- NULL
      rownames(new_rows) <- NULL

      rbind(data[-rows_to_drop, , drop = FALSE], new_rows)
}


#' Fit a plane to a polygon's 3D vertices
#'
#' Uses Newell's method for the normal, which is robust for non-convex and
#' slightly non-planar polygons. Planarity is judged by the maximum vertex
#' distance from the plane through the vertex centroid.
#'
#' @param p Numeric matrix of vertices (n x 3).
#' @param tol Absolute planarity tolerance.
#' @return List with `normal` (unit vector), `offset` (plane is
#'   `normal . v = offset`), `warp` (max residual), `planar`, and `area`;
#'   or NULL for degenerate polygons.
#' @keywords internal
#' @noRd
polygon_plane <- function(p, tol = 1e-6) {
      nxt <- c(seq_len(nrow(p))[-1], 1L)
      x <- p[, 1]; y <- p[, 2]; z <- p[, 3]
      nrm <- c(sum((y - y[nxt]) * (z + z[nxt])),
               sum((z - z[nxt]) * (x + x[nxt])),
               sum((x - x[nxt]) * (y + y[nxt])))
      len <- sqrt(sum(nrm^2))
      if (len < 1e-12) return(NULL)
      nrm <- nrm / len
      offset <- sum(colMeans(p) * nrm)
      warp <- max(abs(p %*% nrm - offset))
      list(normal = nrm, offset = offset, warp = warp,
           planar = warp <= tol, area = len / 2)
}


#' Test whether two polygons pass through each other in 3D
#'
#' Requires each polygon to have vertices strictly on both sides of the
#' other's plane (beyond a tolerance widened by that plane's warp), and the
#' two polygons' crossing intervals along the planes' line of intersection to
#' overlap.
#'
#' @param pts_i,pts_j Vertex matrices (n x 3).
#' @param pl_i,pl_j Plane info lists from `polygon_plane()`.
#' @param tol Absolute distance tolerance.
#' @return Logical.
#' @keywords internal
#' @noRd
polygons_interpenetrate <- function(pts_i, pl_i, pts_j, pl_j, tol) {

      eps_i <- tol + pl_i$warp
      eps_j <- tol + pl_j$warp

      d_i <- as.vector(pts_i %*% pl_j$normal - pl_j$offset)
      if (!(max(d_i) > eps_j && min(d_i) < -eps_j)) return(FALSE)

      d_j <- as.vector(pts_j %*% pl_i$normal - pl_i$offset)
      if (!(max(d_j) > eps_i && min(d_j) < -eps_i)) return(FALSE)

      u <- cross3(pl_i$normal, pl_j$normal)
      u_len <- sqrt(sum(u^2))
      if (u_len < 1e-9) return(FALSE)
      u <- u / u_len

      int_i <- crossing_interval(pts_i, d_i, eps_j, u)
      int_j <- crossing_interval(pts_j, d_j, eps_i, u)

      min(int_i[2], int_j[2]) - max(int_i[1], int_j[1]) > tol
}


#' Range of a polygon's plane crossings, projected onto a direction
#'
#' @param pts Vertex matrix (n x 3).
#' @param d Signed distances of vertices to the crossing plane.
#' @param eps Tolerance below which a vertex is treated as on the plane.
#' @param u Unit direction of the planes' intersection line.
#' @return Length-2 numeric vector (min, max).
#' @keywords internal
#' @noRd
crossing_interval <- function(pts, d, eps, u) {
      d[abs(d) <= eps] <- 0
      nxt <- c(seq_along(d)[-1], 1L)
      cross <- (d > 0 & d[nxt] < 0) | (d < 0 & d[nxt] > 0)
      on <- d == 0
      proj <- c(
            if (any(cross)) {
                  t <- d[cross] / (d[cross] - d[nxt][cross])
                  pts_x <- pts[cross, , drop = FALSE] +
                        t * (pts[nxt[cross], , drop = FALSE] - pts[cross, , drop = FALSE])
                  as.vector(pts_x %*% u)
            },
            if (any(on)) as.vector(pts[on, , drop = FALSE] %*% u)
      )
      range(proj)
}


cross3 <- function(a, b) {
      c(a[2] * b[3] - a[3] * b[2],
        a[3] * b[1] - a[1] * b[3],
        a[1] * b[2] - a[2] * b[1])
}


#' Split a polygon ring by a plane
#'
#' Two-sided Sutherland-Hodgman clip of a single ring. Each vertex carries a
#' source row index (for copying aesthetics), a new-vertex flag, and a
#' cut-edge flag for the edge leaving that vertex. Edges running along the
#' cutting plane are flagged as cut edges; sub-edges of an original edge
#' inherit that edge's flag.
#'
#' @param ring List with `p` (n x 3), `src`, `is_new`, `flag`.
#' @param normal,offset Cutting plane (`normal . v = offset`).
#' @param tol Distance tolerance for snapping vertices onto the plane.
#' @return List of one ring (no split) or two rings.
#' @keywords internal
#' @noRd
split_ring_by_plane <- function(ring, normal, offset, tol) {
      d <- as.vector(ring$p %*% normal - offset)
      d[abs(d) <= tol] <- 0
      if (!(any(d > 0) && any(d < 0))) return(list(ring))

      sides <- lapply(c(1, -1), function(s) clip_ring_side(ring, s * d))
      sides <- sides[vapply(sides, function(r) !is.null(r) && nrow(r$p) >= 3,
                            logical(1))]
      if (length(sides) < 2) return(list(ring))
      sides
}


#' Keep the non-negative side of a ring
#'
#' @param ring Ring list (see `split_ring_by_plane()`).
#' @param dd Signed distances, oriented so the kept side is non-negative.
#' @return Clipped ring list, or NULL if empty.
#' @keywords internal
#' @noRd
clip_ring_side <- function(ring, dd) {
      n <- length(dd)
      p <- ring$p
      out_p <- list(); out_src <- integer(0)
      out_new <- logical(0); out_flag <- logical(0)

      for (i in seq_len(n)) {
            j <- if (i == n) 1L else i + 1L

            if (dd[i] >= 0) {
                  out_p <- c(out_p, list(p[i, ]))
                  out_src <- c(out_src, ring$src[i])
                  out_new <- c(out_new, ring$is_new[i])
                  out_flag <- c(out_flag, if (dd[i] == 0 && dd[j] < 0) TRUE else ring$flag[i])
            }

            if ((dd[i] > 0 && dd[j] < 0) || (dd[i] < 0 && dd[j] > 0)) {
                  t <- dd[i] / (dd[i] - dd[j])
                  out_p <- c(out_p, list(p[i, ] + t * (p[j, ] - p[i, ])))
                  out_src <- c(out_src, ring$src[i])
                  out_new <- c(out_new, TRUE)
                  out_flag <- c(out_flag, if (dd[i] > 0) TRUE else ring$flag[i])
            }
      }

      if (length(out_p) == 0) return(NULL)
      list(p = do.call(rbind, out_p), src = out_src,
           is_new = out_new, flag = out_flag)
}


#' Project newly created vertices into screen space
#'
#' Applies the same projection and NPC scaling as `coord_3d()` to rows flagged
#' by `.new_vertex`, using their `.x3d`/`.y3d`/`.z3d` coordinates.
#'
#' @param rows Data frame of piece rows.
#' @param proj Projection parameters.
#' @param plot_bounds Plot bounds for NPC scaling.
#' @return `rows` with projected columns updated for new vertices.
#' @keywords internal
#' @noRd
project_new_vertices <- function(rows, proj, plot_bounds) {
      nv <- which(rows$.new_vertex)
      if (length(nv) == 0) return(rows)

      pts <- data.frame(x = rows$.x3d[nv], y = rows$.y3d[nv], z = rows$.z3d[nv])
      tr <- transform_3d_standard(pts, proj)
      tr <- scale_to_npc_coordinates(tr, plot_bounds = plot_bounds)

      rows$x[nv] <- tr$x
      rows$y[nv] <- tr$y
      rows$z[nv] <- tr$z
      rows$depth[nv] <- tr$depth
      rows$depth_scale[nv] <- tr$depth_scale
      if ("depth_3d" %in% names(rows)) rows$depth_3d[nv] <- tr$depth
      rows
}

# Exact ordering ------------------------------------------------------------------

#' Viewer location in standardized cube space
#'
#' Inverts the z-flip and rotation of `transform_3d_standard()`. Under
#' perspective the viewer is a point at distance `dist` from the cube center;
#' under orthographic projection it is a direction toward the viewer.
#'
#' @param proj Projection parameters.
#' @return List with `persp` and `v` (eye point or unit direction), or NULL.
#' @keywords internal
#' @noRd
eye_in_cube_space <- function(proj) {
      if (is.null(proj)) return(NULL)
      rot <- rotate_3d(diag(3), proj$pitch, proj$roll, proj$yaw)
      r <- if (isTRUE(proj$persp)) c(0, 0, -proj$dist) else c(0, 0, -1)
      f <- as.vector(r %*% t(rot))
      list(persp = isTRUE(proj$persp), v = c(f[1], f[2], -f[3]))
}


# R entry points to the individual steps of the C++ pairwise comparison. The
# sorter itself doesn't use these; they expose each step for testing.

# Two-group scene from screen matrices (x, y, depth columns) and optional
# cube-space vertex matrices
pair_scene <- function(si, sj, ti, tj, pi = NULL, pj = NULL, eye = NULL, proj = numeric(0)) {
      if (is.null(si)) si <- matrix(0, nrow(pi), 3)
      if (is.null(sj)) sj <- matrix(0, nrow(pj), 3)
      has3d <- !is.null(pi) && !is.null(pj)
      list(x = c(si[, 1], sj[, 1]), y = c(si[, 2], sj[, 2]), d = c(si[, 3], sj[, 3]),
           x3 = if (has3d) c(pi[, 1], pj[, 1]) else numeric(0),
           y3 = if (has3d) c(pi[, 2], pj[, 2]) else numeric(0),
           z3 = if (has3d) c(pi[, 3], pj[, 3]) else numeric(0),
           offsets = as.integer(c(0, nrow(si), nrow(si) + nrow(sj))),
           type = unname(.prim_codes[c(ti, tj)]),
           eye = if (is.null(eye)) numeric(0) else c(as.numeric(eye$persp), eye$v),
           proj = proj)
}

geometry_type <- function(g) {
      if (!is.null(g$plane)) "polygon" else if (nrow(g$pts) == 2) "segment" else "point"
}

#' Screen-space depth delta between two primitives
#'
#' @param di,dj Screen matrices (x, y, depth in the first three columns).
#' @param type_i,type_j Primitive types.
#' @return Depth delta (positive: i behind j), or NA for no constraint.
#' @keywords internal
#' @noRd
pw_compare_pair <- function(di, dj, type_i, type_j) {
      res <- do.call(pair_order_cpp, pair_scene(di, dj, type_i, type_j))
      res$screen
}

#' Exact occlusion order of two primitives from plane-side tests
#'
#' @param gi,gj Lists with `pts` (cube-space vertex matrix) and, for
#'   polygons, `plane`.
#' @param eye Viewer from `eye_in_cube_space()`.
#' @return 1 if i is behind j, -1 if in front, or NA if undecided.
#' @keywords internal
#' @noRd
plane_side_order <- function(gi, gj, eye) {
      res <- do.call(pair_order_cpp, pair_scene(NULL, NULL, geometry_type(gi), geometry_type(gj),
                                                gi$pts, gj$pts, eye))
      res$plane_side
}

#' Order of a polygon and a smaller primitive lying in its plane
#'
#' Points, segments, and text lying exactly in a planar polygon's plane are
#' drawn on top of it.
#'
#' @param gi,gj Lists with `pts` and, for polygons, `plane`.
#' @param ti,tj Primitive types.
#' @return 1 if i is the polygon (drawn behind j), -1 if j is, or NA.
#' @keywords internal
#' @noRd
coplanar_order <- function(gi, gj, ti, tj) {
      res <- do.call(pair_order_cpp, pair_scene(NULL, NULL, ti, tj, gi$pts, gj$pts))
      res$coplanar
}

#' Exact occlusion order of two disjoint primitives from one line of sight
#'
#' @param gi,gj Lists with `pts` and, for polygons, `plane`.
#' @param di,dj Screen matrices.
#' @param ti,tj Primitive types.
#' @param proj,plot_bounds Projection parameters and plot bounds.
#' @return 1 if i is behind j, -1 if in front, or NA.
#' @keywords internal
#' @noRd
ray_sample_order <- function(gi, gj, di, dj, ti, tj, proj, plot_bounds) {
      res <- do.call(pair_order_cpp, pair_scene(di, dj, ti, tj, gi$pts, gj$pts,
                                                eye_in_cube_space(proj),
                                                proj_vector(proj, plot_bounds)))
      res$ray
}

#' Midpoint of the portion of a segment overlapping a polygon on screen
#'
#' @param poly_data,seg_data Screen matrices.
#' @return Numeric `c(x, y)`, or NULL if they do not overlap.
#' @keywords internal
#' @noRd
segment_overlap_midpoint <- function(poly_data, seg_data) {
      out <- segment_overlap_midpoint_cpp(poly_data[, 1], poly_data[, 2],
                                          seg_data[1:2, 1], seg_data[1:2, 2])
      if (length(out) == 0) NULL else out
}

#' Line of sight through a screen point, in standardized cube space
#'
#' Inverts the NPC scaling, perspective division, rotation, and z-flip of
#' `coord_3d()`'s projection.
#'
#' @param sx,sy Screen point in NPC units.
#' @param proj Projection parameters.
#' @param plot_bounds Plot bounds used for NPC scaling.
#' @return List with origin `o` and direction `d`; points along the ray are
#'   `o + lambda * d`, with larger `lambda` farther from the viewer.
#' @keywords internal
#' @noRd
screen_ray <- function(sx, sy, proj, plot_bounds) {
      screen_ray_cpp(sx, sy, proj_vector(proj, plot_bounds))
}
