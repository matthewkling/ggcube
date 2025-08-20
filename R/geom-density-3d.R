StatDensity3D <- ggproto("StatDensity3D", Stat,
                         required_aes = c("x", "y"),
                         default_aes = aes(fill = after_stat(density), z = after_stat(z)),

                         compute_group = function(data, scales, n = NULL, grid = NULL, direction = NULL,
                                                  h = NULL, adjust = 1, pad = 0.1, min_ndensity = 0,
                                                  cull_backfaces = NULL,
                                                  light = NULL, na.rm = FALSE) {

                               # Remove missing values if requested
                               if (na.rm) {
                                     data <- data[complete.cases(data[c("x", "y")]), ]
                               }

                               # Check we have enough data
                               if (nrow(data) < 3) {
                                     stop("stat_density_3d requires at least 3 points per group")
                               }

                               # Load MASS for kde2d
                               if (!requireNamespace("MASS", quietly = TRUE)) {
                                     stop("stat_density_3d requires the 'MASS' package. Install with: install.packages('MASS')")
                               }

                               # Calculate bandwidth if not provided
                               if (is.null(h)) {
                                     # Use same approach as MASS::kde2d default
                                     h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
                               } else if (length(h) == 1) {
                                     h <- c(h, h)
                               } else if (length(h) != 2) {
                                     stop("h must be a single number or a vector of length 2")
                               }

                               # Apply bandwidth adjustment
                               h <- h * adjust

                               # Determine grid limits from data range
                               x_range <- range(data$x, na.rm = TRUE)
                               y_range <- range(data$y, na.rm = TRUE)

                               # Add some padding (similar to what kde2d does by default)
                               x_extend <- diff(x_range) * pad
                               y_extend <- diff(y_range) * pad
                               xlim <- c(x_range[1] - x_extend, x_range[2] + x_extend)
                               ylim <- c(y_range[1] - y_extend, y_range[2] + y_extend)

                               # Generate grid
                               d <- make_tile_grid(grid, n, direction, xlim, ylim)
                               d$group <- paste0("surface__tile", d$group, "::grp", data$group[1])

                               # Compute kernel density
                               d$z <- kde2d(data$x, data$y, d$x, d$y, h)

                               # Preserve group-level aesthetics (everything except coordinates)
                               non_coord_cols <- setdiff(names(data), names(d))
                               if (length(non_coord_cols) > 0) {
                                     for (col_name in non_coord_cols) {
                                           d[[col_name]] <- rep(data[[col_name]][1], nrow(d))
                                     }
                               }

                               # Compute additional variables to match stat_density_2d
                               d$density <- d$z
                               n_obs <- nrow(data)
                               max_density <- max(d$density, na.rm = TRUE)
                               d$ndensity <- if (max_density > 0) d$density / max_density else 0
                               d$count <- d$density * n_obs
                               d$n <- n_obs

                               # Remove data below ndensity threshold
                               d <- filter(d, ndensity >= min_ndensity)

                               # Add computed variables and light info
                               d <- d %>%
                                     compute_surface_vars() %>%
                                     mutate(cull_backfaces = cull_backfaces) %>%
                                     attach_light(light)

                               return(d)
                         }
)

# Adapted from MASS::kde2d in order to support non-rectangular grids
kde2d <- function(x, y, eval_x, eval_y, h) {
      # x, y: data points
      # eval_x, eval_y: points where we want to evaluate density
      # h: bandwidth (optional)

      nx <- length(x)
      if (length(y) != nx)
            stop("data vectors must be the same length")
      if (length(eval_x) != length(eval_y))
            stop("evaluation point vectors must be the same length")
      if (any(!is.finite(x)) || any(!is.finite(y)))
            stop("missing or infinite values in the data are not allowed")
      if (any(!is.finite(eval_x)) || any(!is.finite(eval_y)))
            stop("missing or infinite values in evaluation points are not allowed")

      # Estimate bandwidth if not provided
      h <- if (missing(h))
            c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y))
      else rep(h, length.out = 2L)
      if (any(h <= 0))
            stop("bandwidths must be strictly positive")
      h <- h/4  # Same scaling as original kde2d

      n_eval <- length(eval_x)

      # For each evaluation point, compute density
      # Vectorized approach: compute all distances at once

      # Create matrices for vectorized computation
      # eval_x[i] - x[j] for all i,j combinations
      dx_matrix <- outer(eval_x, x, "-") / h[1L]  # n_eval x nx
      dy_matrix <- outer(eval_y, y, "-") / h[2L]  # n_eval x nx

      # Evaluate normal densities
      kernel_x <- dnorm(dx_matrix)  # n_eval x nx
      kernel_y <- dnorm(dy_matrix)  # n_eval x nx

      # For each evaluation point, sum over all data points
      # kernel_x[i,j] * kernel_y[i,j] gives kernel contribution from data point j to eval point i
      density <- rowSums(kernel_x * kernel_y) / (nx * h[1L] * h[2L])

      return(density)
}


#' 3D surface from 2D density estimate
#'
#' A 3D version of `ggplot2::geom_density_2d()`.
#' Creates surfaces from 2D point data using kernel density estimation.
#' The density values become the z-coordinates of the surface, allowing
#' visualization of data concentration as peaks and valleys in 3D space.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. This stat
#'   requires `x` and `y` aesthetics. By default, `fill` is mapped to
#'   `after_stat(density)` and `z` is mapped to `after_stat(density)`.
#' @param data The data to be displayed in this layer. Must contain x and y columns
#'   with point coordinates.
#' @param stat The statistical transformation to use on the data. Defaults to `StatDensity3D`.
#' @param geom The geometric object used to display the data. Defaults to `GeomPolygon3D`.
#'
#' @param h Bandwidth vector. If `NULL` (default), uses automatic bandwidth selection
#'   via `MASS::bandwidth.nrd()`. Can be a single number (used for both dimensions)
#'   or a vector of length 2 for different bandwidths in x and y directions.
#' @param adjust Multiplicative bandwidth adjustment factor. Values greater than 1
#'   produce smoother surfaces; values less than 1 produce more detailed surfaces.
#'   Default is 1.
#' @param pad Proportional range expansion factor. The computed density grid extends
#'   this proportion of the raw data range beyond each data limit. Default is 0.1.
#' @param min_ndensity Lower cutoff for normalized density (computed variable `ndensity`
#'   described below), below which to filter out results. This is particularly useful for
#'   removing low-density corners of rectangular density grids when density surfaces are
#'   shown for multiple groups, as in the example below. Default is 0 (no filtering).
#'
#' @inheritParams grid_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#' @inheritParams position_param
#'
#' @section Aesthetics:
#' `stat_density_3d()` requires the following aesthetics from input data:
#' - **x**: X coordinate of data points
#' - **y**: Y coordinate of data points
#'
#' And optionally understands:
#' - **group**: Grouping variable for computing separate density surfaces
#' - Additional aesthetics are passed through for surface styling
#'
#' @inheritSection surface_computed_vars Computed variables
#'
#' @section Computed variables specific to StatDensity3D:
#' - `density`: The kernel density estimate at each grid point
#' - `ndensity`: Density estimate scaled to maximum of 1 within each group
#' - `count`: Density estimate × number of observations in group (expected count)
#' - `n`: Number of observations in each group
#'
#' @section Grouping:
#' When aesthetics like `colour` or `fill` are mapped to categorical variables,
#' `stat_density_3d()` computes separate density surfaces for each group, just
#' like `stat_density_2d()`. Each group gets its own density calculation with
#' proper `count` and `n` values.
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic density surface from scattered points
#' p <- ggplot(faithful, aes(eruptions, waiting)) +
#'   coord_3d() +
#'   scale_fill_viridis_c()
#'
#' p + geom_density_3d() + guides(fill = guide_colorbar_3d())
#'
#' # Color by alternative density values
#' p + geom_density_3d(aes(fill = after_stat(count)))
#'
#' # Adjust bandwidth for smoother or more detailed surfaces
#' p + geom_density_3d(adjust = 0.5, color = "white")  # More detail
#' p + geom_density_3d(adjust = 2, color = "white")   # Smoother
#'
#' # Multiple density surfaces by group,
#' # using normalized density to equalize peak heights
#' ggplot(iris, aes(Petal.Length, Sepal.Length, fill = Species)) +
#'   geom_density_3d(aes(z = after_stat(ndensity), group = Species),
#'                   color = "black", alpha = .7, light = NULL) +
#'   coord_3d()
#'
#' # Same, but with extra padding to remove edge effects and
#' # with density filtering to remove rectangular artifacts
#' ggplot(iris, aes(Petal.Length, Sepal.Length, fill = Species)) +
#'   geom_density_3d(aes(z = after_stat(ndensity)),
#'                   pad = .3, min_ndensity = .001,
#'                   color = "black", alpha = .7, light = NULL) +
#'   coord_3d(ratio = c(3, 3, 1))
#'
#' # Specify alternative grid geometry and light model
#' p + geom_density_3d(grid = "hex", n = 30, direction = "y",
#'                     light = light("direct"),
#'                     color = "white", linewidth = .1) +
#'   guides(fill = guide_colorbar_3d())
#'
#' @seealso [stat_density_2d()] for 2D density contours, [stat_surface_3d()] for
#'   surfaces from existing grid data, [light()] for lighting specifications,
#'   [make_tile_grid()] for details about grid geometry options,
#'   [coord_3d()] for 3D coordinate systems.
#' @rdname geom_density_3d
#' @export
geom_density_3d <- function(mapping = NULL, data = NULL,
                            stat = StatDensity3D,
                            position = "identity",
                            ...,
                            n = NULL, grid = NULL, direction = NULL,
                            h = NULL, adjust = 1,
                            pad = 0.1,
                            min_ndensity = 0,
                            light = ggcube::light(),
                            cull_backfaces = FALSE, sort_method = NULL,
                            force_convex = TRUE, scale_depth = TRUE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = stat, geom = GeomPolygon3D,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(n = n, grid = grid, direction = direction,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          h = h, adjust = adjust, pad = pad, min_ndensity = min_ndensity,
                          light = light, na.rm = na.rm, ...)
      )
}

#' @rdname geom_density_3d
#' @export
stat_density_3d <- function(mapping = NULL, data = NULL,
                            geom = GeomPolygon3D,
                            position = "identity",
                            ...,
                            n = NULL, grid = NULL, direction = NULL,
                            h = NULL, adjust = 1,
                            pad = 0.1,
                            min_ndensity = 0,
                            light = ggcube::light(),
                            cull_backfaces = FALSE, sort_method = NULL,
                            force_convex = TRUE, scale_depth = TRUE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

      layer(data = data, mapping = mapping, stat = StatDensity3D, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(n = n, grid = grid, direction = direction,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          h = h, adjust = adjust, pad = pad, min_ndensity = min_ndensity,
                          light = light, na.rm = na.rm, ...)
      )
}
