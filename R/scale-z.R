

#' Continuous z-axis scale for 3D plots
#'
#' \code{scale_z_continuous} creates a continuous scale for the z aesthetic in 3D plots.
#' It works similarly to \code{scale_x_continuous} and \code{scale_y_continuous}, providing
#' control over axis breaks, labels, limits, and transformations for the z dimension.
#'
#' @param name The name of the scale, used as the axis title. Use \code{waiver()} for
#'   the default, or \code{NULL} to omit the title.
#' @param breaks One of:
#'   \itemize{
#'     \item \code{NULL} for no breaks
#'     \item \code{waiver()} for the default breaks
#'     \item A numeric vector of positions
#'     \item A function that takes the limits as input and returns breaks as output
#'   }
#' @param n.breaks An integer guiding the number of major breaks. The algorithm
#'   may choose a slightly different number to ensure nice break labels.
#' @param labels One of:
#'   \itemize{
#'     \item \code{NULL} for no labels
#'     \item \code{waiver()} for the default labels
#'     \item A character vector giving labels (must be same length as breaks)
#'     \item A function that takes the breaks as input and returns labels as output
#'   }
#' @param limits A numeric vector of length two providing limits of the scale.
#'   Use \code{NA} to refer to the existing minimum or maximum.
#' @param expand For position scales, a vector of range expansion constants used to add some
#'   padding around the data to ensure that they are placed some distance away from the axes.
#' @param transform For continuous scales, the name of a transformation object or the object itself.
#' @param guide A function used to create a guide or its name. Since z-axis guides are not
#'   yet supported, this defaults to \code{"none"}.
#' @param ... Other arguments passed on to \code{continuous_scale()}.
#'
#' @return A ggplot2 scale object for the z aesthetic.
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic z-axis scaling
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   scale_z_continuous(name = "Quarter Mile Time") +
#'   coord_3d()
#'
#' # Custom breaks and labels
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   scale_z_continuous(
#'     breaks = c(15, 17, 19, 21),
#'     labels = c("Fast", "Medium", "Slow", "Very Slow")
#'   ) +
#'   coord_3d()
#'
#' # Set explicit limits
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   scale_z_continuous(limits = c(10, 25)) +
#'   coord_3d()
#'
#' @seealso \code{\link{zlim}} for a shorthand way to set z-axis limits,
#'   \code{\link{coord_3d}} for the 3D coordinate system
#' @family 3D scale functions
#' @export
scale_z_continuous <- function(name = waiver(), breaks = waiver(), n.breaks = NULL,
                               labels = waiver(), limits = NULL, expand = waiver(),
                               transform = "identity", guide = "none", ...) {

      # Store scale parameters for later access by coord_3d
      .z_scale_cache$limits <- limits
      .z_scale_cache$breaks <- breaks
      .z_scale_cache$n.breaks <- n.breaks
      .z_scale_cache$name <- name
      .z_scale_cache$labels <- labels
      .z_scale_cache$expand <- expand

      continuous_scale(
            aesthetics = "z",
            palette = identity,
            name = name,
            breaks = breaks,
            n.breaks = n.breaks,
            labels = labels,
            limits = limits,
            expand = expand,
            transform = transform,
            guide = guide,
            super = ScaleContinuous,
            ...
      )
}

#' Set z-axis limits
#'
#' This is a shorthand for \code{scale_z_continuous(limits = c(min, max))}.
#' It's a convenient way to set the z-axis limits without specifying other
#' scale parameters.
#'
#' @param min,max The minimum and maximum values for the z-axis.
#' @param ... Additional arguments passed to \code{scale_z_continuous()}.
#'
#' @return A ggplot2 scale object for the z aesthetic with specified limits.
#'
#' @examples
#' library(ggplot2)
#'
#' # Set z-axis limits
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   zlim(15, 20) +
#'   coord_3d()
#'
#' # Equivalent to:
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   scale_z_continuous(limits = c(15, 20)) +
#'   coord_3d()
#'
#' @seealso \code{\link{scale_z_continuous}} for more control over z-axis scaling,
#'   \code{\link{xlim}}, \code{\link{ylim}} for x and y axis limits
#' @family 3D scale functions
#' @export
zlim <- function(min, max, ...) {
      scale_z_continuous(limits = c(min, max), ...)
}
