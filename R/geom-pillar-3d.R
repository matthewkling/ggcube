StatPillar3D <- ggproto("StatPillar3D", Stat,
                        required_aes = c("x", "y", "z"),
                        default_aes = aes(group = after_stat(group)),

                        compute_panel = function(data, scales, na.rm = FALSE,
                                                 width = 1.0, faces = "all",
                                                 light = NULL, zmin = NULL,
                                                 cull_backfaces = NULL) {

                              # Remove missing values if requested
                              if (na.rm) {
                                    data <- data[complete.cases(data[c("x", "y", "z")]), ]
                              }

                              # Check we have enough data
                              if (nrow(data) < 1) {
                                    stop("stat_pillar_3d requires at least 1 point")
                              }

                              # Handle zmin with proper precedence:
                              # 1. Parameter (if provided) - overrides aesthetic
                              # 2. Aesthetic (if mapped) - used if no parameter
                              # 3. Default (min of data) - if neither provided

                              if (!is.null(zmin)) {
                                    # Parameter provided - use for all pillars (overrides aesthetic)
                                    data$zmin <- zmin
                              } else if (!"zmin" %in% names(data)) {
                                    # No aesthetic mapped and no parameter - use default
                                    data$zmin <- min(data$z, na.rm = TRUE)
                              }
                              # If aesthetic exists and no parameter, keep the aesthetic values (no action needed)

                              # Ensure z is never less than zmin
                              zmin_vals <- pmin(data$zmin, data$z)
                              zmax_vals <- pmax(data$zmin, data$z)
                              data$zmin <- zmin_vals
                              data$z <- zmax_vals

                              # Generate numeric positions before calculating spacing
                              data <- convert_to_numeric(data)

                              # Calculate grid spacing using resolution (works for both regular and sparse grids)
                              x_spacing <- resolution(data$x, zero = FALSE)
                              y_spacing <- resolution(data$y, zero = FALSE)

                              # Validate and process faces parameter
                              selected_faces <- select_faces(faces)

                              if (length(selected_faces) == 0) {
                                    # Return empty data frame with required columns for consistency
                                    return(data.frame(
                                          x = numeric(0), y = numeric(0), z = numeric(0),
                                          group = character(0), pillar_id = integer(0), face_type = character(0),
                                          light = numeric(0),
                                          normal_x = numeric(0), normal_y = numeric(0), normal_z = numeric(0)
                                    ))
                              }

                              # Create pillars
                              pillar_faces <- create_pillars(data, x_spacing, y_spacing, width, selected_faces)

                              if (nrow(pillar_faces) == 0) {
                                    # Return empty data frame with required columns for consistency
                                    return(data.frame(
                                          x = numeric(0), y = numeric(0), z = numeric(0),
                                          group = character(0), pillar_id = integer(0), face_type = character(0),
                                          light = numeric(0),
                                          normal_x = numeric(0), normal_y = numeric(0), normal_z = numeric(0)
                                    ))
                              }

                              pillar_faces %>%
                                    average_aesthetics() %>%
                                    mutate(cull_backfaces = cull_backfaces) %>%
                                    attach_light(light) %>%
                                    return()
                        }
)

#' Create pillar faces from grid data
#'
#' @param data Data frame with x, y, z, zmin columns
#' @param x_spacing Grid spacing in x direction
#' @param y_spacing Grid spacing in y direction
#' @param width Width factor (1.0 = full grid spacing)
#' @param selected_faces Character vector of face names to render
#' @return Data frame with pillar face vertices
#' @keywords internal
create_pillars <- function(data, x_spacing, y_spacing, width, selected_faces) {

      # Calculate actual pillar dimensions
      pillar_width_x <- x_spacing * width
      pillar_width_y <- y_spacing * width
      half_x <- pillar_width_x / 2
      half_y <- pillar_width_y / 2

      all_faces <- list()

      # Debug: check inputs
      if (nrow(data) == 0) {
            warning("create_pillars: No data points provided")
            return(data.frame())
      }

      if (length(selected_faces) == 0) {
            warning("create_pillars: No faces selected")
            return(data.frame())
      }

      for (i in 1:nrow(data)) {
            point <- data[i, ]
            cx <- point$x  # Center x
            cy <- point$y  # Center y
            l <- cx - half_x
            r <- cx + half_x
            f <- cy - half_y
            k <- cy + half_y
            t <- point$z
            b <- point$zmin

            # Define the 8 corners of the pillar
            corners <- list(
                  c(l, k, b),  # 1: left-back-bottom
                  c(r, k, b),  # 2: right-back
                  c(r, f, b),  # 3: right-front
                  c(l, f, b),  # 4: left-front
                  c(l, k, t),  # 5: left-back-top
                  c(r, k, t),  # 6: right-back
                  c(r, f, t),  # 7: right-front
                  c(l, f, t)  # 8: left-front
            )

            # Define faces using corner indices (order: ccw from outside)
            face_definitions <- list(
                  zmin = c(1, 2, 3, 4),  # Bottom
                  zmax = c(5, 8, 7, 6),  # Top
                  xmin = c(1, 4, 8, 5),  # Left
                  xmax = c(3, 2, 6, 7),  # Right
                  ymin = c(1, 5, 6, 2),  # Back
                  ymax = c(4, 3, 7, 8)   # Front
            )

            # Create requested faces
            for (face_name in selected_faces) {
                  if (face_name %in% names(face_definitions)) {
                        corner_indices <- face_definitions[[face_name]]

                        # Create 2-level hierarchical group ID: pillar_id__face_type
                        hierarchical_group <- paste0("pillar", i, "__", face_name)

                        # Create face vertices with hierarchical grouping
                        face_vertices <- data.frame(
                              x = sapply(corner_indices, function(idx) corners[[idx]][1]),
                              y = sapply(corner_indices, function(idx) corners[[idx]][2]),
                              z = sapply(corner_indices, function(idx) corners[[idx]][3]),
                              z_raw = point$z_raw,
                              group = hierarchical_group,
                              pillar_id = i,
                              face_type = face_name
                        )

                        # Preserve all non-coordinate columns
                        non_coord_cols <- setdiff(names(point), names(face_vertices))
                        for (col_name in non_coord_cols) {
                              face_vertices[[col_name]] <- rep(point[[col_name]], 4)
                        }

                        all_faces[[length(all_faces) + 1]] <- face_vertices
                  }
            }
      }

      if (length(all_faces) == 0) {
            return(data.frame())
      }

      # Combine all faces
      result <- do.call(rbind, all_faces)
      rownames(result) <- NULL

      return(result)
}


#' 3D pillars from grid data
#'
#' Creates 3D pillars from regular grid data in which x and y fall on a regular grid.
#' Works with both complete and sparse grid data. Each data point becomes a rectangular
#' 3D column extending from a base level to the data value.
#'
#' Note that pillar geometries often require pairwise depth sorting for correct rendering.
#' This is the default for smaller data sets, but not for larger data sets due to compute speed;
#' in those cases you may wish to manually specify `sort_method = "pairwise"` for good results.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to `StatPillar3D`.
#' @param geom The geometric object used to display the data. Defaults to `GeomPolygon3D`.
#'
#' @param zmin Base level for all pillars. When provided as a parameter, overrides any
#'   \code{zmin} aesthetic mapping. If \code{NULL} (default), uses the \code{zmin} aesthetic
#'   if mapped, otherwise defaults to the minimum \code{z} value in the data.
#'
#' @inheritParams pillar_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#' @inheritParams position_param
#'
#' @section Aesthetics:
#' `stat_pillar_3d()` requires the following aesthetics:
#' - **x**: X coordinate (grid position)
#' - **y**: Y coordinate (grid position)
#' - **z**: Z coordinate (pillar top height)
#'
#' And optionally understands:
#' - **zmin**: Base level for each pillar (can be overridden by the \code{zmin} parameter)
#'
#' @section Computed variables:
#' - `normal_x`, `normal_y`, `normal_z`: Face normal components
#' - `pillar_id`: Sequential pillar number
#' - `face_type`: Face name ("zmax", "xmin", etc.)
#'
#' @examples
#' # Basic 3D bar chart from regular grid
#' d <- expand.grid(x = 1:5, y = 1:5)
#' d$z <- d$x + d$y + rnorm(25, 0, 0.5)
#' ggplot(d, aes(x, y, z)) +
#'   geom_pillar_3d() +
#'   coord_3d()
#'
#' # Set uniform base level using `zmin` parameter
#' ggplot(d, aes(x, y, z)) +
#'   geom_pillar_3d(aes(fill = z), color = "white",
#'                 zmin = 0) +
#'   coord_3d(roll = 90)
#'
#' # Set variable base levels using `zmin` aesthetic
#' d$base_level <- runif(nrow(d), -1, 1)
#' ggplot(d, aes(x, y, z = z, zmin = base_level)) +
#'   geom_pillar_3d(color = "black") +
#'   coord_3d(roll = 90)
#'
#' # Show only a subset of pillar faces
#' ggplot(d, aes(x, y, z)) +
#'   geom_pillar_3d(faces = c("zmax", "ymin"),
#'     cull_backfaces = FALSE,
#'     fill = "steelblue", color = "black") +
#'   coord_3d()
#'
#' # With gaps between pillars
#' ggplot(d, aes(x, y, z)) +
#'   geom_pillar_3d(color = "black", width = 0.6) +
#'   coord_3d()
#'
#' @seealso [stat_surface_3d()] for smooth surface rendering, [coord_3d()] for 3D coordinate systems,
#'   [light()] for lighting specifications, [GeomPolygon3D] for the default geometry.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_pillar_3d
#' @export
geom_pillar_3d <- function(mapping = NULL, data = NULL,
                           stat = StatPillar3D,
                           position = "identity",
                           ...,
                           width = 1.0, faces = "all", zmin = NULL,
                           light = NULL,
                           cull_backfaces = TRUE, sort_method = NULL,
                           scale_depth = TRUE, force_convex = FALSE,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = stat, geom = GeomPolygon3D,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, width = width, faces = faces, light = light, zmin = zmin,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          ...)
      )
}


#' @rdname geom_pillar_3d
#' @export
stat_pillar_3d <- function(mapping = NULL, data = NULL,
                           geom = GeomPolygon3D,
                           position = "identity",
                           ...,
                           width = 1.0, faces = "all", zmin = NULL,
                           light = NULL,
                           cull_backfaces = TRUE, sort_method = NULL,
                           scale_depth = TRUE, force_convex = FALSE,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = StatPillar3D, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, width = width, faces = faces, light = light, zmin = zmin,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          ...)
      )
}
