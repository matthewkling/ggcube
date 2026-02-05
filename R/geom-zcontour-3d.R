# Geom that converts point grid to contour band polygons for "layer cake" 3D rendering

# Geom ggproto object ---------------------------------------------------------

GeomContour3D <- ggproto("GeomContour3D", GeomPolygon3D,

                         required_aes = c("x", "y", "z"),

                         default_aes = aes(
                               colour = "white",
                               fill = "black",
                               linewidth = 0.1,
                               linetype = 1,
                               alpha = NA
                         ),

                         extra_params = c("na.rm", "bins", "binwidth", "breaks",
                                          "cull_backfaces", "sort_method", "scale_depth", "force_convex"),

                         setup_params = function(data, params) {
                               params$bins <- params$bins %||% 10
                               params
                         },

                         setup_data = function(data, params) {

                               # Preserve aesthetic columns for later merging
                               aes_cols <- intersect(names(data),
                                                     c("fill", "colour", "color", "alpha",
                                                       "linewidth", "linetype", "PANEL",
                                                       "cull_backfaces", "lighting_spec"))

                               all_data <- split(data, data$group)

                               all_contour_data <- lapply(all_data, function(data){

                                     # Validate grid structure
                                     n_unique_x <- length(unique(round(data$x, 10)))
                                     n_unique_y <- length(unique(round(data$y, 10)))

                                     if (n_unique_x < 2 || n_unique_y < 2) {
                                           stop("geom_contour_3d() requires at least a 2x2 grid of points")
                                     }

                                     # Convert point grid to contour band polygons
                                     contour_data <- points_to_contours(
                                           data,
                                           breaks = params$breaks,
                                           bins = params$bins,
                                           binwidth = params$binwidth
                                     )

                                     # Transfer point-level aesthetic columns to contour data
                                     breaks <- c(unique(contour_data$z), Inf)
                                     distill <- function(x){
                                           ux <- unique(x)
                                           if(length(ux) == 1) return(ux)
                                           if(is.numeric(x)) return(mean(x))
                                           return(x[1]) # fallback
                                     }
                                     ds <- data %>%
                                           mutate(z = cut(z, breaks, include.lowest = TRUE),
                                                  z = breaks[as.integer(z)]) %>%
                                           group_by(z) %>%
                                           summarize(across(all_of(aes_cols), distill))
                                     contour_data <- left_join(contour_data, ds, by = join_by(z))

                                     return(na.omit(contour_data))
                               }) %>%
                                     bind_rows() %>%
                                     mutate(group = gsub("__", "_", group),
                                            group = paste0("contour__layer", group))

                               return(all_contour_data)
                         }
)

#' Convert point grid to contour band polygons
#'
#' @param data Data frame with x, y, z columns forming a regular grid.
#' @param breaks Numeric vector of break points for contour levels. If NULL,
#'   breaks are computed automatically using `bins` or `binwidth`.
#' @param bins Number of contour bins. Ignored if `breaks` or `binwidth` is provided.
#' @param binwidth Width of contour bins. Ignored if `breaks` is provided.
#' @param group_prefix String prefix for polygon group IDs.
#'
#' @return Data frame with polygon vertices for contour band rendering, including
#'   `group` and `subgroup` columns for proper hole handling.
#'
#' @keywords internal
points_to_contours <- function(data,
                               breaks = NULL,
                               bins = NULL,
                               binwidth = NULL) {

      # Get unique x and y values to form the grid
      x_vals <- sort(unique(data$x))
      y_vals <- sort(unique(data$y))

      nx <- length(x_vals)
      ny <- length(y_vals)

      # Check if data forms a complete regular grid
      expected_n <- nx * ny
      if (nrow(data) != expected_n) {
            warning("Data does not form a complete regular grid. ",
                    "Expected ", expected_n, " points, got ", nrow(data), ". ",
                    "Missing values will be treated as NA.")
      }

      # Create z matrix in the format isoband expects
      # isoband expects z[i, j] where i indexes y (rows) and j indexes x (columns)
      z_matrix <- matrix(NA_real_, nrow = ny, ncol = nx)

      # Map data points to matrix positions
      x_idx <- match(data$x, x_vals)
      y_idx <- match(data$y, y_vals)

      for (i in seq_len(nrow(data))) {
            z_matrix[y_idx[i], x_idx[i]] <- data$z[i]
      }

      # Compute breaks if not provided
      z_range <- range(data$z, na.rm = TRUE)

      if (is.null(breaks)) {
            if (!is.null(binwidth)) {
                  # Extend range slightly to include endpoints
                  breaks <- seq(
                        floor(z_range[1] / binwidth) * binwidth,
                        ceiling(z_range[2] / binwidth) * binwidth,
                        by = binwidth
                  )
            } else {
                  # Use bins (default 10)
                  bins <- bins %||% 10
                  breaks <- pretty(z_range, n = bins)
            }
      }

      # Ensure breaks span the data range
      if (min(breaks) > z_range[1]) {
            breaks <- c(z_range[1], breaks)
      }
      if (max(breaks) < z_range[2]) {
            breaks <- c(breaks, z_range[2])
      }

      breaks <- sort(unique(breaks))

      if (length(breaks) < 2) {
            stop("Need at least 2 breaks to create contour bands")
      }

      # For solid discs (layer cake), each level goes from data minimum to that level
      # This means we compute isobands from z_min to each break point
      z_min <- z_range[1]

      if (length(breaks) == 0) {
            warning("No contour levels above data minimum")
            return(data.frame(
                  x = numeric(0), y = numeric(0), z = numeric(0),
                  order = integer(0), group = character(0), subgroup = character(0),
                  face_type = character(0)
            ))
      }

      bands <- isoband::isobands(
            x = x_vals,
            y = y_vals,
            z = z_matrix,
            levels_high = rep(Inf, length(breaks)),
            levels_low = breaks
      )

      bands <- lapply(1:length(breaks), function(i){
            mutate(as.data.frame(bands[[i]]),
                   z = breaks[i],
                   group = paste0(data$group[1], "::", breaks[i]))}) %>% bind_rows() %>%
            rename(.subgroup = id)

      return(bands)
}




# Layer function --------------------------------------------------------------

#' Contours of a 3D surface
#'
#' Renders a surface as stacked horizontal contour bands. Each contour band is a
#' polygon placed at its corresponding z-level.
#'
#' This geom takes point grid data (like that produced by [stat_surface_3d()],
#' [stat_function_3d()], [stat_smooth_3d()], or [stat_density_3d()]) and converts
#' it to filled contour polygons using the `isoband` package.
#'
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param data Point grid data with x, y, z coordinates.
#' @param stat Statistical transformation. Defaults to [stat_surface_3d()].
#' @param position Position adjustment, defaults to "identity".
#' @param ... Other arguments passed to the layer.
#' @param bins Number of contour levels. Default is 20. Ignored if `breaks` or
#'   `binwidth` is provided.
#' @param binwidth Width of each contour band. Overrides `bins` if provided.
#' @param breaks Numeric vector specifying exact contour break points. Overrides
#'   both `bins` and `binwidth` if provided.
#' @inheritParams polygon_params
#' @inheritParams light_param
#'
#' @section Aesthetics:
#' `geom_contour_3d()` requires:
#' \describe{
#'   \item{x, y, z}{Point coordinates forming a regular grid}
#' }
#'
#' And understands these additional aesthetics:
#' \describe{
#'   \item{fill}{Band fill color. For automatic coloring by elevation, use
#'     `aes(fill = after_stat(z))`. Default is "grey60".}
#'   \item{colour}{Band border color (default: "grey30")}
#'   \item{alpha}{Transparency}
#'   \item{linewidth}{Border width (default: 0.1)}
#'   \item{linetype}{Border line type}
#' }
#'
#' @section Computed variables:
#' Each contour band is placed at its corresponding z-level (the upper boundary
#' of the band). To color by elevation, use `aes(fill = after_stat(z))`.
#'
#' @examples
#' # Basic usage with volcano data
#' ggplot(mountain, aes(x, y, z)) +
#'       geom_contour_3d(color = "white", fill = "black") +
#'       coord_3d(light = "none", ratio = c(1.5, 2, 1))
#'
#' # Map fill to elevation and customize number of levels
#' ggplot(mountain, aes(x, y, z, fill = after_stat(z))) +
#'       geom_contour_3d(bins = 12, color = "white") +
#'       scale_fill_viridis_c() +
#'       coord_3d(light = "none", ratio = c(1.5, 2, 1))
#'
#' # Specify exact breaks
#' ggplot(mountain, aes(x, y, z, fill = after_stat(z))) +
#'   geom_contour_3d(breaks = seq(0, 200, by = 5)) +
#'   scale_fill_viridis_c() +
#'   coord_3d(light = "none")
#'
#' # With stat_density_3d
#' ggplot(faithful, aes(eruptions, waiting)) +
#'   stat_density_3d(geom = "contour_3d",
#'     sort_method = "pairwise") +
#'   coord_3d()
#'
#' # With stat_function_3d
#' ggplot() +
#'   stat_function_3d(
#'     fun = function(x, y) sin(x) * cos(y),
#'     xlim = c(-pi, pi), ylim = c(-pi, pi),
#'     geom = "contour_3d",
#'     bins = 50, color = "black"
#'   ) +
#'   scale_fill_viridis_c(option = "B") +
#'   coord_3d(light = "none")
#'
#' @seealso [geom_surface_3d()] for continuous surface rendering,
#'   [geom_ridgeline_3d()] for cross-sectional ridgeline rendering,
#'   [stat_function_3d()] for mathematical function surfaces,
#'   [stat_smooth_3d()] for fitted model surfaces,
#'   [coord_3d()] for 3D coordinate systems,
#'   [ggplot2::geom_contour_filled()] for the 2D equivalent.
#'
#' @return A `Layer` object that can be added to a ggplot.
#' @export
geom_contour_3d <- function(mapping = NULL, data = NULL,
                            stat = "surface_3d", position = "identity",
                            ...,
                            bins = 20,
                            binwidth = NULL,
                            breaks = NULL,
                            cull_backfaces = FALSE,
                            sort_method = "pairwise",
                            scale_depth = TRUE,
                            force_convex = FALSE,
                            light = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

      layer(
            data = data,
            mapping = mapping,
            stat = get_proto(stat, "stat"),
            geom = GeomContour3D,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  bins = bins,
                  binwidth = binwidth,
                  breaks = breaks,
                  cull_backfaces = cull_backfaces,
                  sort_method = sort_method,
                  scale_depth = scale_depth,
                  force_convex = force_convex,
                  light = light,
                  na.rm = na.rm,
                  ...
            )
      )
}


#' @rdname geom_contour_3d
#' @export
stat_contour_3d <- function(mapping = NULL, data = NULL,
                            geom = "contour_3d", position = "identity",
                            ...,
                            bins = 20,
                            binwidth = NULL,
                            breaks = NULL,
                            cull_backfaces = FALSE,
                            sort_method = "pairwise",
                            scale_depth = TRUE,
                            force_convex = FALSE,
                            light = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

      layer(
            data = data,
            mapping = mapping,
            stat = StatSurface3D,
            geom = get_proto(geom, "geom"),
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  bins = bins,
                  binwidth = binwidth,
                  breaks = breaks,
                  cull_backfaces = cull_backfaces,
                  sort_method = sort_method,
                  scale_depth = scale_depth,
                  force_convex = force_convex,
                  light = light,
                  na.rm = na.rm,
                  ...
            )
      )
}
