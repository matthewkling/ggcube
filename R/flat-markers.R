# Flat markers
#
# geom_point_3d(flat = TRUE) draws markers that have been projected onto a
# face of the panel (by position_on_face()) as polygons lying in that face,
# rather than as upright symbols facing the viewer. Each marker is the
# outline of its plotting symbol, sized so that at the centre of the cube it
# matches the symbol's drawn size, and drawn in true perspective. Since the
# drawn size of a symbol depends on the panel's size, the outlines are built
# when the plot is drawn.


# Plotting symbols with a flat outline: open, solid, and filled circles,
# squares, diamonds, and triangles
.flat_shapes <- c(0, 1, 2, 5, 6, 15:25)


#' Outline of a plotting symbol
#'
#' Follows the geometry of R's plotting symbols (as drawn by grid), in units
#' of the symbol's font size.
#'
#' @param shape A plotting symbol in `.flat_shapes`.
#' @param n Number of vertices for circles.
#' @return Two-column matrix of outline vertices, centred on the origin.
#' @keywords internal
#' @noRd
flat_shape_outline <- function(shape, n = 32) {
      radius <- 0.375
      sqrc <- 0.88622692545275801364            # sqrt(pi / 4): square of equal area
      dmdc <- 1.25331413731550025119            # sqrt(pi / 4) * sqrt(2): diamond of equal area
      trc0 <- 1.55512030155621416073            # triangles of equal area
      trc1 <- 1.34677368708859836060
      trc2 <- 0.77756015077810708036

      circle <- function(r) {
            a <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
            cbind(r * cos(a), r * sin(a))
      }
      square <- function(h) cbind(c(-h, h, h, -h), c(-h, -h, h, h))
      diamond <- function(h) cbind(c(-h, 0, h, 0), c(0, -h, 0, h))
      triangle <- function(xc, up = TRUE) {
            s <- if (up) 1 else -1
            cbind(c(0, -trc1 * xc, trc1 * xc), s * c(trc0 * xc, -trc2 * xc, -trc2 * xc))
      }

      switch(as.character(shape),
             "0" = , "22" = square(radius * sqrc),
             "15" = square(radius),
             "1" = , "16" = , "19" = , "21" = circle(radius),
             "20" = circle(0.25),
             "2" = , "17" = , "24" = triangle(radius),
             "6" = , "25" = triangle(radius, up = FALSE),
             "5" = , "23" = diamond(radius * dmdc),
             "18" = diamond(radius),
             rlang::abort(paste0("Shape ", shape, " has no flat outline."), call = NULL))
}


#' Check that markers can be drawn flat
#'
#' @param shape Shapes of the markers to be drawn flat.
#' @return Numeric shapes.
#' @keywords internal
#' @noRd
check_flat_shapes <- function(shape) {
      num <- suppressWarnings(as.numeric(as.character(shape)))
      bad <- unique(shape[is.na(num) | !num %in% .flat_shapes])
      if (length(bad) > 0) {
            rlang::abort(c(
                  paste0("Flat markers can't be drawn with shape ",
                         paste0("`", bad, "`", collapse = ", "), "."),
                  i = "Use open, solid, or filled circles, squares, diamonds, or triangles: shapes 0, 1, 2, 5, 6, or 15 to 25."
            ), call = NULL)
      }
      num
}


#' Points per unit of cube space at the centre of the cube
#'
#' The drawn length of a short segment through the cube's centre,
#' perpendicular to the line of sight.
#'
#' @param proj,plot_bounds Projection parameters and plot bounds.
#' @param panel_pt Panel width and height in points.
#' @return Number of points per cube unit.
#' @keywords internal
#' @noRd
cube_unit_pt <- function(proj, plot_bounds, panel_pt) {
      v <- eye_in_cube_space(proj)$v
      v <- v / sqrt(sum(v^2))
      u1 <- cross3(v, diag(3)[which.min(abs(v)), ])
      u1 <- u1 / sqrt(sum(u1^2))
      u2 <- cross3(v, u1)
      e <- 0.01
      pts <- data.frame(x = c(0, e * u1[1], e * u2[1]),
                        y = c(0, e * u1[2], e * u2[2]),
                        z = c(0, e * u1[3], e * u2[3]))
      tr <- scale_to_npc_coordinates(transform_3d_standard(pts, proj), plot_bounds = plot_bounds)
      len <- function(k) sqrt(((tr$x[k] - tr$x[1]) * panel_pt[1])^2 +
                                    ((tr$y[k] - tr$y[1]) * panel_pt[2])^2)
      mean(c(len(2), len(3))) / e
}


#' Expand flat markers into polygons lying in their faces
#'
#' Rows flagged `.flat` are single points already projected onto a face
#' (`project_to_face`). Each is replaced by the outline of its symbol in the
#' plane of that face, in cube space, projected to the screen.
#'
#' @param rows Projected rows of a point layer.
#' @param panel_pt Panel width and height in points.
#' @param proj,plot_bounds Projection parameters and plot bounds.
#' @return `rows`, with each flat marker's row replaced by its outline's
#'   vertices, typed as a polygon.
#' @keywords internal
#' @noRd
expand_flat_markers <- function(rows, panel_pt, proj, plot_bounds) {
      if (!".flat" %in% names(rows)) return(rows)
      flat <- rows$.flat %in% TRUE
      if (!any(flat)) return(rows)
      if (is.null(panel_pt)) panel_pt <- c(360, 360)

      idx <- which(flat)
      shape <- as.numeric(as.character(rows$shape[idx]))
      size <- rows$size[idx]
      stroke <- if ("stroke" %in% names(rows)) rows$stroke[idx] else rep(0.5, length(idx))
      stroke[is.na(stroke)] <- 0.5
      fontsize <- ifelse(shape %in% 21:25, (size + 0.5 * stroke) * .pt, size * .pt)
      scale <- fontsize / cube_unit_pt(proj, plot_bounds, panel_pt)
      axis <- substr(rows$project_to_face[idx], 1, 1)

      pieces <- lapply(seq_along(idx), function(j) {
            outline <- flat_shape_outline(shape[j]) * scale[j]
            uv <- switch(axis[j], x = c(2, 3), y = c(1, 3), z = c(1, 2))
            i <- idx[j]
            v <- matrix(c(rows$.x3d[i], rows$.y3d[i], rows$.z3d[i]), nrow(outline), 3, byrow = TRUE)
            v[, uv[1]] <- v[, uv[1]] + outline[, 1]
            v[, uv[2]] <- v[, uv[2]] + outline[, 2]
            v
      })
      n <- vapply(pieces, nrow, integer(1))
      vertices <- do.call(rbind, pieces)

      new <- rows[rep(idx, n), , drop = FALSE]
      new$.x3d <- vertices[, 1]
      new$.y3d <- vertices[, 2]
      new$.z3d <- vertices[, 3]
      new$.new_vertex <- TRUE
      new <- project_new_vertices(new, proj, plot_bounds)
      new$.new_vertex <- NULL
      new$.prim <- "polygon"

      # Outlines are drawn in true perspective, like the marker itself: scaled
      # by the perspective at the marker's centre, whatever the layer's
      # `scale_depth`
      new$.flat_stroke <- rep(stroke * rows$depth_scale[idx], n)

      if (!".prim" %in% names(rows)) rows$.prim <- NA_character_
      out <- dplyr::bind_rows(rows[!flat, , drop = FALSE], new)
      out <- out[order(c(which(!flat), rep(idx, n))), , drop = FALSE]
      rownames(out) <- NULL
      out
}


#' Style flat markers as polygons
#'
#' Open symbols have an outline in `colour`, solid ones are filled with
#' `colour`, and filled ones (21 to 25) are filled with `fill` and outlined in
#' `colour`, with outlines `stroke` wide at the centre of the cube and drawn
#' in true perspective (`.flat_stroke`, from `expand_flat_markers()`).
#'
#' @param coords Styled rows of a point layer.
#' @return `coords`, with flat markers typed and styled as polygons.
#' @keywords internal
#' @noRd
style_flat_markers <- function(coords) {
      if (!".flat" %in% names(coords)) return(coords)
      f <- coords$.flat %in% TRUE
      if (!any(f)) return(coords)
      shape <- as.numeric(as.character(coords$shape[f]))
      solid <- shape %in% 15:20
      open <- shape %in% c(0, 1, 2, 5, 6)
      stroke <- coords$.flat_stroke[f]
      colour <- coords$colour[f]
      fill <- if ("fill" %in% names(coords)) coords$fill[f] else rep(NA_character_, sum(f))

      coords$fill[f] <- ifelse(open, NA_character_, ifelse(solid, colour, fill))
      coords$colour[f] <- ifelse(solid, NA_character_, colour)
      coords$linewidth[f] <- ifelse(solid, 0, stroke)
      if (!"linetype" %in% names(coords)) coords$linetype <- 1
      coords$linetype[f] <- 1
      coords$.prim[f] <- "polygon"
      coords
}
