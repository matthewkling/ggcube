# Geom -------------------------------------------------------------------------

GeomDropline3D <- ggproto("GeomDropline3D", Geom,

                          required_aes = c("x", "y", "z"),

                          default_aes = aes(
                                colour = "black", linewidth = 0.25, linetype = 1, alpha = 0.5
                          ),

                          draw_panel = function(data, panel_params, coord, faces = "zmin",
                                                scale_depth = TRUE, on_top = FALSE, na.rm = FALSE) {

                                validate_coord3d(coord)
                                if (nrow(data) == 0) return(grid::nullGrob())

                                rows <- dropline_rows(data, faces)
                                coords <- coord$transform(rows, panel_params, sort = FALSE)
                                finish <- function(coords) apply_depth_scaling(coords, scale_depth)

                                layer_scene_grob(coords, panel_params, finish, list(lineend = "butt"),
                                                 on_top = on_top)
                          },

                          draw_key = draw_key_path
)


#' Segment rows of drop lines
#'
#' Each drop line runs from a point to its projection on a face of the panel.
#' The end row is marked with the face in `project_to_face`, so the coord
#' places it on that face when it projects the data.
#'
#' @param data Layer data, one row per point.
#' @param faces Faces to drop lines to.
#' @return Data frame with two rows per point and face.
#' @keywords internal
#' @noRd
dropline_rows <- function(data, faces) {
      n <- nrow(data)
      data$project_to_face <- NULL
      rows <- lapply(seq_along(faces), function(f) {
            start <- data
            start$point_type <- "start"
            start$project_to_face <- NA_character_
            end <- data
            end$point_type <- "end"
            end$project_to_face <- faces[f]
            d <- rbind(start, end)
            d$group <- rep(sprintf("dropline__%s_%06d", faces[f], seq_len(n)), 2)
            d$segment_id <- rep(seq_len(n) + (f - 1) * n, 2)
            d
      })
      rows <- dplyr::bind_rows(rows)
      rows$.prim <- "segment"
      rownames(rows) <- NULL
      rows
}


check_faces <- function(faces, call = rlang::caller_env()) {
      valid <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
      if (!is.character(faces) || length(faces) == 0 || anyNA(faces) || !all(faces %in% valid)) {
            rlang::abort(paste0("`faces` must contain only ",
                                paste0("\"", valid, "\"", collapse = ", "), "."),
                         call = call)
      }
      unique(faces)
}


# Constructor ------------------------------------------------------------------

#' 3D drop lines
#'
#' `geom_dropline_3d()` draws a line from each point to its projection on one
#' or more faces of the panel, such as the floor, to help show where points
#' lie in 3D. Use it alongside [geom_point_3d()], and with
#' [position_on_face()] to also mark where the points fall on the faces.
#'
#' Faces are the faces of the panel's cube, so a drop line to `"zmin"` ends on
#' the floor of the panel wherever the axis limits put it. Drop lines are
#' depth-sorted with the rest of the scene, and split where they pass through
#' polygons.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to
#'   [stat_identity_3d()].
#' @param position Position adjustment, defaults to `"identity"`.
#' @param ... Other arguments passed on to [layer()], such as aesthetics like
#'   `colour`, `linewidth`, `linetype`, and `alpha`.
#' @param faces Faces to draw lines to: one or more of `"xmin"`, `"xmax"`,
#'   `"ymin"`, `"ymax"`, `"zmin"` (the default), and `"zmax"`.
#' @inheritParams depth_params
#' @inheritParams on_top_param
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical indicating whether this layer should be included
#'   in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#'
#' @section Aesthetics:
#' `geom_dropline_3d()` understands the following aesthetics (required in
#' bold): **x**, **y**, **z**, `colour`, `linewidth` (default `0.25`),
#' `linetype`, and `alpha` (default `0.5`).
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt, qsec)) + coord_3d()
#'
#' # Drop lines to the floor
#' p + geom_dropline_3d() + geom_point_3d()
#'
#' # Drop lines to two side faces, ending in flat markers on those faces
#' p + geom_dropline_3d(faces = c("xmax", "ymax")) +
#'   geom_point_3d(position = position_on_face(c("xmax", "ymax")),
#'                 flat = TRUE, alpha = .5) +
#'   geom_point_3d()
#'
#' # Styled, with colour mapped
#' p + geom_dropline_3d(aes(colour = factor(cyl)), linetype = "dotted", alpha = 1) +
#'   geom_point_3d(aes(colour = factor(cyl)))
#' @export
geom_dropline_3d <- function(mapping = NULL, data = NULL, stat = StatIdentity3D,
                             position = "identity", ...,
                             faces = "zmin", scale_depth = TRUE, on_top = FALSE,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
      faces <- check_faces(faces)
      check_on_top(on_top)

      layer(
            geom = GeomDropline3D, mapping = mapping, data = data, stat = get_proto(stat, "stat"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(faces = faces, scale_depth = scale_depth, on_top = on_top,
                          na.rm = na.rm, ...)
      )
}
