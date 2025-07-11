
#' Aesthetic mapping with positional z support
#'
#' This function extends \code{ggplot2::aes()} to support positional mapping
#' to the z aesthetic. It maintains full backward compatibility with the original
#' \code{aes()} function while enabling the convenient \code{aes(x, y, z)} syntax
#' for 3D plots.
#'
#' @param x Variable to map to x aesthetic (required)
#' @param y Variable to map to y aesthetic (required)
#' @param z Variable to map to z aesthetic (optional)
#' @param ... Other aesthetic mappings (color, size, etc.)
#'
#' @return An aesthetic mapping object, same as \code{ggplot2::aes()}
#'
#' @details
#' This function is a lightweight wrapper around \code{ggplot2::aes()} that:
#' \itemize{
#'   \item Maintains full backward compatibility with existing 2D plots
#'   \item Enables positional z mapping: \code{aes(x_var, y_var, z_var)}
#'   \item Works with any geom that uses the z aesthetic (contour, raster, 3D plots)
#'   \item Passes through all other aesthetics unchanged
#' }
#'
#' @examples
#' library(ggplot2)
#'
#' # 2D plots work like regular ggplot2
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'
#' # 3D plots can use positional syntax, or explicitly map to z
#' # same as: ggplot(mtcars, aes(mpg, wt, z = qsec)) + geom_point() + coord_3d()
#' ggplot(mtcars, aes(mpg, wt, qsec)) + geom_point() + coord_3d()
#'
#' # Also works with non-ggcube z-aesthetic geoms
#' ggplot(mountain, aes(x, y, z)) + geom_contour()
#'
#' @seealso \code{\link[ggplot2]{aes}} for the original aesthetic mapping function,
#'   \code{\link{coord_3d}} for 3D coordinate systems
#' @export
aes <- function(x, y, z, ...) {
      if (missing(z)) {
            # 2D case - pass through to ggplot2::aes with positional x, y
            ggplot2::aes({{x}}, {{y}}, ...)
      } else {
            # 3D case - map x, y, z positionally and pass other aesthetics
            ggplot2::aes(x = {{x}}, y = {{y}}, z = {{z}}, ...)
      }
}
