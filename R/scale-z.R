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
#' @param minor_breaks One of:
#'   \itemize{
#'     \item \code{NULL} for no minor breaks
#'     \item \code{waiver()} for the default minor breaks
#'     \item A numeric vector of positions
#'     \item A function that takes the limits as input and returns minor breaks as output
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
#' @param oob One of:
#'   \itemize{
#'     \item Function that handles limits outside the scale limits (out of bounds).
#'     \item \code{scales::censor} for replacing out of bounds values with \code{NA}
#'     \item \code{scales::squish} for squishing out of bounds values into range
#'   }
#' @param na.value Missing values will be replaced with this value.
#' @param transform The name of a transformation object or the object itself. Default is "identity",
#'   but works with standard transform options such as "log10", "sqrt", and "reverse",
#'   detailed in the documentation for [`ggplot2::scale_x_continuous()`].
#' @param guide A function used to create a guide or its name. Since z-axis guides are not
#'   yet supported, this defaults to \code{"none"}.
#' @param ... Other arguments passed on to \code{continuous_scale()}.
#'
#' @return A ggplot2 scale object for the z aesthetic.
#'
#' @examples
#' # Custom breaks, labels, and limits
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   scale_z_continuous(
#'     breaks = c(15, 17, 19, 21),
#'     labels = c("Fast", "Medium", "Slow", "Very Slow"),
#'     limits = c(10, NA)) +
#'   coord_3d()
#'
#' # Works with standard scale transformations like "reverse", "log10", etc.
#' ggplot(mtcars, aes(mpg, wt, z = qsec)) +
#'   geom_point() +
#'   scale_z_continuous(transform = "reverse") +
#'   coord_3d()
#'
#' @seealso \code{\link{zlim}} for a shorthand way to set z-axis limits,
#'   \code{\link{coord_3d}} for the 3D coordinate system
#' @family 3D scale functions
#' @export
scale_z_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                               n.breaks = NULL, labels = waiver(), limits = NULL,
                               expand = waiver(), oob = scales::censor, na.value = NA_real_,
                               transform = "identity", guide = "none", ...) {

      scale_obj <- continuous_scale(
            aesthetics = "z",
            palette = identity,
            name = name,
            breaks = breaks,
            minor_breaks = minor_breaks,
            n.breaks = n.breaks,
            labels = labels,
            limits = limits,
            expand = expand,
            oob = oob,
            na.value = na.value,
            transform = transform,
            guide = guide,
            super = ScaleContinuousPosition,
            ...
      )
      .z_scale_cache$scale <- scale_obj
      return(scale_obj)
}

#' Discrete z-axis scale for 3D plots
#'
#' \code{scale_z_discrete} creates a discrete scale for the z aesthetic in 3D plots.
#' It works with categorical/factor data, positioning each level at integer coordinates
#' (1, 2, 3, ...) in 3D space.
#'
#' @param name The name of the scale, used as the axis title. Use \code{waiver()} for
#'   the default, or \code{NULL} to omit the title.
#' @param breaks One of:
#'   \itemize{
#'     \item \code{NULL} for no breaks
#'     \item \code{waiver()} for all breaks
#'     \item A character vector of breaks
#'     \item A function that takes the limits as input and returns breaks as output
#'   }
#' @param labels One of:
#'   \itemize{
#'     \item \code{NULL} for no labels
#'     \item \code{waiver()} for the default labels
#'     \item A character vector giving labels (must be same length as breaks)
#'     \item A function that takes the breaks as input and returns labels as output
#'   }
#' @param limits A character vector that defines possible values of the scale and their order.
#' @param expand A numeric vector of length two giving multiplicative and additive expansion constants.
#'   These constants ensure that data is placed some distance away from the axes. The defaults are
#'   to expand by 0.6 units on each side for discrete scales.
#' @param guide A function used to create a guide or its name. Since z-axis guides are not
#'   yet supported, this defaults to \code{"none"}.
#' @param na.translate Unlike continuous scales, discrete scales can easily show missing values,
#'   and do so by default. If you want to remove missing values from a discrete scale, specify
#'   \code{na.translate = FALSE}.
#' @param na.value If \code{na.translate = TRUE}, what aesthetic value should the missing values be
#'   displayed as? Does not apply to position scales where \code{NA} is always placed at the far right.
#' @param drop Should unused factor levels be omitted from the scale? The default, \code{TRUE}, uses
#'   the levels that appear in the data; \code{FALSE} uses all the levels in the factor.
#' @param ... Other arguments passed on to \code{discrete_scale()}.
#'
#' @return A ggplot2 scale object for the z aesthetic.
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage to control order, breaks, labels, expansion, etc.
#' ggplot(mpg, aes(displ, cty, drv, color = drv)) +
#'       geom_point() +
#'       scale_z_discrete(limits = c("f", "r", "4"), # change default order
#'                        breaks = c("f", "r", "4"),
#'                        labels = c("front", "rear", "4-wheel"),
#'                        expand = expansion(.5)) +
#'       coord_3d()
#'
#' @seealso \code{\link{scale_z_continuous}} for continuous z-axis scaling,
#'   \code{\link{coord_3d}} for the 3D coordinate system
#' @family 3D scale functions
#' @export
scale_z_discrete <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                             limits = NULL, expand = waiver(), guide = "none",
                             na.translate = TRUE, na.value = NA_real_, drop = TRUE,
                             continuous.limits = NULL, ...) {

      # Create the scale object like ggplot2's scale_x_discrete
      sc <- discrete_scale(
            aesthetics = "z",
            palette = seq_len,  # Use seq_len like ggplot2
            name = name,
            breaks = breaks,
            labels = labels,
            limits = limits,
            expand = expand,
            guide = guide,
            na.translate = na.translate,
            na.value = na.value,
            drop = drop,
            super = ScaleDiscretePosition,
            ...
      )

      # Add the missing range_c component, using a template scale to copy the range_c structure
      template_scale <- scale_x_discrete()
      sc$range_c <- template_scale$range_c$clone()
      sc$continuous_limits <- continuous.limits

      .z_scale_cache$scale <- sc
      return(sc)
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
