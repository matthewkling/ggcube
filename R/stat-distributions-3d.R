# Stat for visualizing multiple 1D density distributions as 3D ridgelines

StatDistributions3D <- ggproto("StatDistributions3D", Stat,
                               required_aes = c("x", "y"),
                               default_aes = aes(z = after_stat(density)),

                               setup_params = function(data, params) {
                                     # Auto-detect direction if not specified
                                     if (is.null(params$direction)) {
                                           x_discrete <- is.factor(data$x) || is.character(data$x)
                                           y_discrete <- is.factor(data$y) || is.character(data$y) || inherits(data$y, "mapped_discrete")

                                           if (x_discrete && !y_discrete) {
                                                 params$direction <- "x"
                                           } else if (y_discrete && !x_discrete) {
                                                 params$direction <- "y"
                                           } else {
                                                 # Both continuous, both discrete, or can't determine - default to "x"
                                                 params$direction <- "x"
                                           }
                                     }

                                     # Compute joint bandwidth across all data if requested
                                     if (isTRUE(params$joint_bandwidth) && is.character(params$bw)) {
                                           # Get the density variable based on direction
                                           if (params$direction == "x") {
                                                 density_var <- data$y
                                           } else {
                                                 density_var <- data$x
                                           }

                                           # Remove non-finite values
                                           density_var <- density_var[is.finite(density_var)]

                                           if (length(density_var) >= 2) {
                                                 # Compute bandwidth using the specified method
                                                 bw_fun <- switch(params$bw,
                                                                  nrd0 = stats::bw.nrd0,
                                                                  nrd = stats::bw.nrd,
                                                                  ucv = stats::bw.ucv,
                                                                  bcv = stats::bw.bcv,
                                                                  SJ = stats::bw.SJ,
                                                                  `SJ-ste` = function(x) stats::bw.SJ(x, method = "ste"),
                                                                  `SJ-dpi` = function(x) stats::bw.SJ(x, method = "dpi"),
                                                                  stats::bw.nrd0  # fallback
                                                 )
                                                 params$bw <- bw_fun(density_var) * (params$adjust %||% 1)
                                                 message("Picking joint bandwidth of ", signif(params$bw, 3))
                                           }
                                     }

                                     params
                               },

                               compute_group = function(data, scales,
                                                        direction = "auto",
                                                        n = 512,
                                                        bw = "nrd0",
                                                        adjust = 1,
                                                        kernel = "gaussian",
                                                        trim = FALSE,
                                                        bounds = c(-Inf, Inf),
                                                        rel_min_height = 0,
                                                        joint_bandwidth = FALSE,
                                                        light = NULL,
                                                        cull_backfaces = FALSE,
                                                        na.rm = FALSE) {

                                     # Determine which variable is the density variable vs position variable
                                     if (direction == "x") {
                                           # direction = "x" means ridges march along x, density computed along y
                                           # Each unique x value gets a density curve in the y-z plane
                                           density_vals <- data$y
                                           position_val <- data$x[1]  # Should be constant within group
                                     } else {
                                           # direction = "y" means ridges march along y, density computed along x
                                           # Each unique y value gets a density curve in the x-z plane
                                           density_vals <- data$x
                                           position_val <- data$y[1]  # Should be constant within group
                                     }

                                     # Remove non-finite values
                                     density_vals <- density_vals[is.finite(density_vals)]

                                     # Need at least 2 points for density estimation
                                     if (length(density_vals) < 2) {
                                           warning("Group has fewer than 2 points, skipping density estimation")
                                           return(data.frame())
                                     }

                                     # Handle bounds (following ggplot2 pattern)
                                     if (any(is.finite(bounds))) {
                                           # Remove data outside bounds
                                           in_bounds <- density_vals >= bounds[1] & density_vals <= bounds[2]
                                           if (!all(in_bounds)) {
                                                 warning("Removed ", sum(!in_bounds), " observations outside `bounds`")
                                                 density_vals <- density_vals[in_bounds]
                                           }
                                     }

                                     if (length(density_vals) < 2) {
                                           warning("Group has fewer than 2 points after bounds filtering, skipping")
                                           return(data.frame())
                                     }

                                     # Determine range for density evaluation
                                     data_range <- range(density_vals)

                                     # Compute density using stats::density
                                     # bw can be numeric or a character rule like "nrd0", "SJ", etc.
                                     # When trim = TRUE, restrict to data range; otherwise let density() choose
                                     if (trim) {
                                           dens <- stats::density(
                                                 density_vals,
                                                 bw = bw,
                                                 adjust = adjust,
                                                 kernel = kernel,
                                                 n = n,
                                                 from = data_range[1],
                                                 to = data_range[2]
                                           )
                                     } else {
                                           dens <- stats::density(
                                                 density_vals,
                                                 bw = bw,
                                                 adjust = adjust,
                                                 kernel = kernel,
                                                 n = n
                                           )
                                     }

                                     # Apply boundary correction if bounds are finite (following ggplot2)
                                     if (any(is.finite(bounds))) {
                                           # Zero out density outside bounds
                                           if (is.finite(bounds[1])) {
                                                 dens$y[dens$x < bounds[1]] <- 0
                                           }
                                           if (is.finite(bounds[2])) {
                                                 dens$y[dens$x > bounds[2]] <- 0
                                           }
                                     }

                                     # Build output data frame
                                     n_obs <- length(density_vals)
                                     max_density <- max(dens$y, na.rm = TRUE)

                                     result <- data.frame(
                                           density = dens$y,
                                           ndensity = if (max_density > 0) dens$y / max_density else 0,
                                           count = dens$y * n_obs,
                                           n = n_obs,
                                           bw = dens$bw
                                     )

                                     # Set coordinates based on direction
                                     if (direction == "x") {
                                           result$x <- position_val
                                           result$y <- dens$x
                                           result$z <- dens$y
                                     } else {
                                           result$x <- dens$x
                                           result$y <- position_val
                                           result$z <- dens$y
                                     }

                                     # Apply rel_min_height filter (like ggridges)
                                     if (rel_min_height > 0 && nrow(result) > 0) {
                                           overall_max <- max(result$density, na.rm = TRUE)
                                           result <- result[result$density >= rel_min_height * overall_max, ]
                                     }

                                     # Preserve non-coordinate columns from input (like fill, colour, etc.)
                                     non_coord_cols <- setdiff(names(data), c("x", "y", "z", "PANEL", "group"))
                                     if (length(non_coord_cols) > 0) {
                                           for (col_name in non_coord_cols) {
                                                 result[[col_name]] <- data[[col_name]][1]
                                           }
                                     }

                                     # Add ggcube-specific columns
                                     if (!is.null(cull_backfaces)) result$cull_backfaces <- cull_backfaces

                                     # Add direction param (since the geom inherits the original un-resolved version)
                                     result$.direction <- direction

                                     result <- attach_light(result, light)

                                     return(result)
                               }
)


#' 3D ridgeline distributions
#'
#' Computes 1D kernel density estimates for each group and arranges them as
#' ridgeline polygons in 3D space. Similar to [ggridges::geom_density_ridges()],
#' but rendered as 3D surfaces using [geom_ridgeline_3d()].
#'
#' This stat is modeled after [ggplot2::stat_density()], with similar
#' parametrization for bandwidth selection, kernel choice, and boundary handling.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. This stat
#'   requires `x` and `y` aesthetics. One of these serves as the grouping/position
#'   variable (determined by `direction`), and the other provides values for
#'   density estimation.
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use to display the data. Defaults to
#'   [geom_ridgeline_3d()].
#' @param ... Other arguments passed to the layer.
#' @param bw The smoothing bandwidth to be used. If numeric, the standard
#'   deviation of the smoothing kernel. If character, a rule to choose the
#'   bandwidth, as listed in [stats::bw.nrd()]. Options include `"nrd0"` (default),
#'   `"nrd"`, `"ucv"`, `"bcv"`, `"SJ"`, `"SJ-ste"`, and `"SJ-dpi"`. Note that
#'
#'   automatic calculation is performed per-group unless `joint_bandwidth = TRUE`.
#' @param adjust A multiplicative bandwidth adjustment. This makes it possible
#'   to adjust the bandwidth while still using a bandwidth estimator. For
#'   example, `adjust = 1/2` means use half of the default bandwidth. Default is 1.
#' @param kernel Kernel function to use. One of `"gaussian"` (default),
#'   `"rectangular"`, `"triangular"`, `"epanechnikov"`, `"biweight"`, `"cosine"`,
#'   or `"optcosine"`. See [stats::density()] for details.
#' @param n Number of equally spaced points at which the density is estimated.
#'   Should be a power of two for efficiency. Default is 512.
#' @param trim If `FALSE` (the default), each density is computed on the full
#'   range of the data (extended by a factor based on bandwidth). If `TRUE`,
#'   each density is computed over the range of that group only.
#' @param bounds A numeric vector of length 2 giving the lower and upper bounds
#'   for bounded density estimation. Density values outside bounds are set to
#'   zero. Data points outside bounds are removed with a warning.
#'   Default is `c(-Inf, Inf)` (unbounded).
#' @param rel_min_height Lines with heights below this cutoff will be removed.
#'   The cutoff is measured relative to the maximum height within each group.
#'   For example, `rel_min_height = 0.01` removes points with density less than
#'   1% of the peak. Default is 0 (no removal). This is similar to the
#'   parameter of the same name in [ggridges::geom_density_ridges()].
#' @param joint_bandwidth If `TRUE`, bandwidth is computed jointly across all
#'   groups using the specified `bw` method, ensuring consistent smoothing
#'   across all density curves. This matches the behavior of
#'   [ggridges::stat_density_ridges()]. If `FALSE` (the default), bandwidth is
#'   computed separately for each group, matching [ggplot2::stat_density()].
#'   Only applies when `bw` is a character string (bandwidth rule), not when
#'   `bw` is provided as a numeric value.
#' @inheritParams ridgeline_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#' @inheritParams position_param
#'
#' @section Aesthetics:
#' `stat_distributions_3d()` understands the following aesthetics (required
#' aesthetics are in bold):
#' \describe{
#'   \item{**x**}{X coordinate - either density variable or position variable
#'     depending on `direction`}
#'   \item{**y**}{Y coordinate - either position variable or density variable
#'     depending on `direction`}
#'   \item{group}{Grouping variable (typically derived from the position aesthetic)}
#'   \item{fill, colour, alpha, linewidth, linetype}{Passed to [geom_ridgeline_3d()]}
#' }
#'
#' @section Direction:
#' The `direction` parameter determines how the data is interpreted:
#' \describe{
#'   \item{`direction = NULL` (default)}{Automatically detects direction based
#'     on whether `x` or `y` is discrete (factor/character). If `x` is discrete
#'     and `y` is continuous, uses `"x"`; if `y` is discrete and `x` is
#'     continuous, uses `"y"`. Falls back to `"x"` if ambiguous.}
#'   \item{`direction = "x"`}{Ridges march along the x-axis. Each unique
#'     x value defines a group, and density is computed from the y values within
#'     that group. The resulting density curves lie in the y-z plane.}
#'   \item{`direction = "y"`}{Ridges march along the y-axis. Each unique y value
#'     defines a group, and density is computed from the x values. The resulting
#'     density curves lie in the x-z plane.}
#' }
#'
#' @section Computed variables:
#' The following variables are computed and available via `after_stat()`:
#' \describe{
#'   \item{density}{The kernel density estimate at each point}
#'   \item{ndensity}{Density normalized to a maximum of 1 within each group}
#'   \item{count}{Density multiplied by number of observations (expected count)}
#'   \item{n}{Number of observations in the group}
#'   \item{bw}{Bandwidth actually used for this group}
#' }
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage with iris data
#' p <- ggplot(iris, aes(y = Sepal.Length, x = Species, fill = Species)) +
#'   coord_3d() +
#'   scale_z_continuous(expand = expansion(mult = c(0, NA))) + # remove gap beneath ridges
#'   theme(legend.position = "none")
#' p + stat_distributions_3d()
#'
#' # Normalize max ridge heights
#' p + stat_distributions_3d(aes(z = after_stat(ndensity)))
#'
#' # Adjust smoothing bandwidth
#' p + stat_distributions_3d(adjust = 0.5)
#'
#' # Use joint bandwidth for consistent smoothing across groups
#' p + stat_distributions_3d(joint_bandwidth = TRUE)
#'
#' # Different bandwidth selection rules
#' p + stat_distributions_3d(bw = "SJ")
#'
#' # Remove tails with rel_min_height
#' p + stat_distributions_3d(rel_min_height = 0.05)
#'
#' # Trim to data range
#' p + stat_distributions_3d(trim = TRUE)
#'
#' # Rotated to reduce perspective distortion
#' p + stat_distributions_3d(alpha = .7) +
#'    coord_3d(pitch = 0, roll = -90, yaw = 90, dist = 5,
#'       panels = c("zmin", "xmin"))
#'
#' @seealso [geom_ridgeline_3d()] for rendering pre-computed ridgeline data,
#'   [stat_density_3d()] for 2D kernel density surfaces,
#'   [ggplot2::stat_density()] for the parametrization this stat follows,
#'   [ggridges::geom_density_ridges()] for the 2D ridgeline equivalent
#'
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname stat_distributions_3d
#' @export
stat_distributions_3d <- function(mapping = NULL, data = NULL,
                                  geom = "ridgeline_3d",
                                  position = "identity",
                                  ...,
                                  direction = NULL,
                                  bw = "nrd0",
                                  adjust = 1,
                                  kernel = "gaussian",
                                  n = 512,
                                  trim = FALSE,
                                  bounds = c(-Inf, Inf),
                                  rel_min_height = 0,
                                  joint_bandwidth = FALSE,
                                  base = 0,
                                  light = NULL,
                                  cull_backfaces = FALSE,
                                  sort_method = NULL,
                                  scale_depth = TRUE,
                                  force_convex = FALSE,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {

      layer(
            data = data,
            mapping = mapping,
            stat = StatDistributions3D,
            geom = ggproto_lookup(geom, "geom"),
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  direction = direction,
                  bw = bw,
                  adjust = adjust,
                  kernel = kernel,
                  n = n,
                  trim = trim,
                  bounds = bounds,
                  rel_min_height = rel_min_height,
                  joint_bandwidth = joint_bandwidth,
                  base = base,
                  light = light,
                  cull_backfaces = cull_backfaces,
                  sort_method = sort_method,
                  scale_depth = scale_depth,
                  force_convex = force_convex,
                  na.rm = na.rm,
                  ...
            )
      )
}


#' @rdname stat_distributions_3d
#' @export
geom_distributions_3d <- function(mapping = NULL, data = NULL,
                                  stat = "distributions_3d",
                                  position = "identity",
                                  ...,
                                  direction = NULL,
                                  bw = "nrd0",
                                  adjust = 1,
                                  kernel = "gaussian",
                                  n = 512,
                                  trim = FALSE,
                                  bounds = c(-Inf, Inf),
                                  rel_min_height = 0,
                                  joint_bandwidth = FALSE,
                                  base = 0,
                                  light = NULL,
                                  cull_backfaces = FALSE,
                                  sort_method = NULL,
                                  scale_depth = TRUE,
                                  force_convex = FALSE,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {

      layer(
            data = data,
            mapping = mapping,
            stat = ggproto_lookup(stat, "stat"),
            geom = GeomRidgeline3D,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                  direction = direction,
                  bw = bw,
                  adjust = adjust,
                  kernel = kernel,
                  n = n,
                  trim = trim,
                  bounds = bounds,
                  rel_min_height = rel_min_height,
                  joint_bandwidth = joint_bandwidth,
                  base = base,
                  light = light,
                  cull_backfaces = cull_backfaces,
                  sort_method = sort_method,
                  scale_depth = scale_depth,
                  force_convex = force_convex,
                  na.rm = na.rm,
                  ...
            )
      )
}
