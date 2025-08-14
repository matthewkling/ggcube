StatDensity3D <- ggproto("StatDensity3D", Stat,
                         required_aes = c("x", "y"),
                         default_aes = aes(fill = after_stat(density), z = after_stat(z)),

                         compute_group = function(data, scales, n = 30, h = NULL,
                                                  adjust = 1, pad = 0.1, min_ndensity = 0,
                                                  light = NULL, na.rm = FALSE) {

                               # Remove missing values if requested
                               if (na.rm) {
                                     data <- data[complete.cases(data[c("x", "y")]), ]
                               }

                               # Check we have enough data
                               if (nrow(data) < 3) {
                                     stop("stat_density_3d requires at least 3 points per group")
                               }

                               # Handle n parameter (grid resolution)
                               if (length(n) == 1) {
                                     nx <- ny <- n
                               } else if (length(n) == 2) {
                                     nx <- n[1]
                                     ny <- n[2]
                               } else {
                                     stop("n must be a single number or a vector of length 2")
                               }

                               if (!is.numeric(c(nx, ny)) || any(c(nx, ny) < 2)) {
                                     stop("Grid resolution (n) must be at least 2 in each dimension")
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
                               xlims <- c(x_range[1] - x_extend, x_range[2] + x_extend)
                               ylims <- c(y_range[1] - y_extend, y_range[2] + y_extend)

                               # Compute 2D kernel density estimate
                               tryCatch({
                                     kde_result <- MASS::kde2d(data$x, data$y, h = h, n = c(nx, ny), lims = c(xlims, ylims))
                               }, error = function(e) {
                                     stop("Error computing kernel density estimate: ", e$message)
                               })

                               # Convert kde2d result to data frame
                               grid_data <- expand.grid(x = kde_result$x, y = kde_result$y)
                               grid_data$z <- as.vector(t(kde_result$z))

                               # Preserve group-level aesthetics (everything except coordinates)
                               non_coord_cols <- setdiff(names(data), c("x", "y", "z"))
                               if (length(non_coord_cols) > 0) {
                                     for (col_name in non_coord_cols) {
                                           grid_data[[col_name]] <- rep(data[[col_name]][1], nrow(grid_data))
                                     }
                               }

                               # Process surface using common pipeline (same as stat_surface_3d)
                               surface <- create_grid_quads(grid_data, light)

                               # Compute additional variables to match stat_density_2d
                               surface$density <- surface$z
                               n_obs <- nrow(data)
                               max_density <- max(surface$density, na.rm = TRUE)
                               surface$ndensity <- if (max_density > 0) surface$density / max_density else 0
                               surface$count <- surface$density * n_obs
                               surface$n <- n_obs

                               # Remove data below ndensity threshold
                               surface <- filter(surface, ndensity >= min_ndensity)

                               return(surface)
                         }
)

#' 3D kernel density estimation surface
#'
#' Creates 3D surfaces from 2D point data using kernel density estimation.
#' The density values become the z-coordinates of the surface, allowing
#' visualization of data concentration as peaks and valleys in 3D space.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. This stat
#'   requires `x` and `y` aesthetics. By default, `fill` is mapped to
#'   `after_stat(density)` and `z` is mapped to `after_stat(density)`.
#' @param data The data to be displayed in this layer. Must contain x and y columns
#'   with point coordinates.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomPolygon3D] for proper 3D depth sorting.
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `TRUE`, removes missing values before computing density.
#'   If `FALSE`, missing values will cause an error. Default is `FALSE`.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param n Either a single integer specifying grid resolution in both dimensions,
#'   or a vector of length 2 specifying `c(nx, ny)` for different resolutions.
#'   Default is 30 (lower than ggplot2's default for better performance with 3D rendering).
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
#' @param light A lighting specification object created by [light()], or NULL to disable shading.
#' @param ... Other arguments passed on to [layer()], such as `colour`, `fill`,
#'   `alpha`, etc, or `sort_method` and `scale_depth` arguments to `geom_polygon_3d()`.
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
#' @section Computed variables:
#' - `x`, `y`: Grid coordinates for the density surface
#' - `z`: Same as `density` (for 3D surface height)
#' - `density`: The kernel density estimate at each grid point
#' - `ndensity`: Density estimate scaled to maximum of 1 within each group
#' - `count`: Density estimate × number of observations in group (expected count)
#' - `n`: Number of observations in each group
#' - `light`: Computed lighting value (numeric for most methods, hex color for `normal_rgb`)
#' - `normal_x`, `normal_y`, `normal_z`: Surface normal components
#' - `slope`: Gradient magnitude from surface calculations
#' - `aspect`: Direction of steepest slope from surface calculations
#' - `dzdx`, `dzdy`: Partial derivatives from surface calculations
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
#' p + stat_density_3d() + guides(fill = guide_colorbar_shaded())
#'
#' # Color by alternative density values
#' p + stat_density_3d(aes(fill = after_stat(count)))
#'
#' # Adjust bandwidth for smoother or more detailed surfaces
#' p + stat_density_3d(adjust = 0.5, color = "white")  # More detail
#' p + stat_density_3d(adjust = 2, color = "white")   # Smoother
#'
#' # Higher resolution grid for smoother surfaces
#' p + stat_density_3d(n = 50, color = "black", fill = "darkgreen", alpha = 0.85,
#'                   light = light(direction = c(1, 1, 0.5)))
#'
#' # Multiple density surfaces by group,
#' # using normalized density to equalize peak heights
#' ggplot(iris, aes(Petal.Length, Sepal.Length, fill = Species)) +
#'   stat_density_3d(aes(z = after_stat(ndensity)),
#'                   color = "black", alpha = .7, light = NULL) +
#'   coord_3d()
#'
#' # Same, but with extra padding and with
#' # density filtering to remove rectangular artifacts
#' ggplot(iris, aes(Petal.Length, Sepal.Length, fill = Species)) +
#'   stat_density_3d(aes(z = after_stat(ndensity)),
#'                   pad = .3, min_ndensity = .001,
#'                   color = "black", alpha = .7, light = NULL) +
#'   coord_3d(ratio = c(3, 3, 1))
#'
#' @seealso [stat_density_2d()] for 2D density contours, [stat_surface_3d()] for
#'   surfaces from existing grid data, [light()] for lighting specifications,
#'   [coord_3d()] for 3D coordinate systems.
#' @export
stat_density_3d <- function(mapping = NULL, data = NULL,
                            geom = GeomPolygon3D,
                            position = "identity",
                            n = 30,
                            h = NULL,
                            adjust = 1,
                            pad = 0.1,
                            min_ndensity = 0,
                            light = ggcube::light(),
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...) {

      layer(
            stat = StatDensity3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(n = n, h = h, adjust = adjust, pad = pad, min_ndensity = min_ndensity,
                          light = light, na.rm = na.rm, ...)
      )
}
