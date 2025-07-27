


#' Rectangle theme element with alpha support
#'
#' This function extends \code{ggplot2::element_rect()} to support transparency
#' via an alpha parameter. It maintains full backward compatibility with the original
#' \code{element_rect()} function while enabling transparent panel styling,
#' particularly useful for foreground panels in 3D plots.
#'
#' @param fill Fill color for the rectangle. Use \code{NA} for no fill.
#' @param colour,color Line color for the rectangle border. Use \code{NA} for no border.
#' @param linewidth Line width for the rectangle border.
#' @param linetype Line type for the rectangle border (e.g., "solid", "dashed").
#' @param inherit.blank Should this element inherit from \code{element_blank}?
#' @param size \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
#'   Use \code{linewidth} instead.
#' @param alpha Transparency level for the rectangle fill, ranging from 0 (completely transparent)
#'   to 1 (completely opaque). Particularly useful for styling foreground panels in 3D plots
#'   to create layered visual effects.
#'
#' @return A theme element object that can be used in \code{theme()} specifications.
#'
#' @examples
#' # Basic 3D plot with semi-transparent foreground panels
#' ggplot(mountain, aes(x, y, z)) +
#'   stat_surface_3d(fill = "darkblue", color = "lightblue", linewidth = .1) +
#'   coord_3d(faces = c("background", "ymax")) +
#'   theme(panel.foreground = element_rect(alpha = 0.6))
#'
#' # Completely transparent foreground panels
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point() +
#'   coord_3d() +
#'   theme(panel.foreground = element_rect(fill = "blue", alpha = 0))
#'
#' @seealso \code{\link[ggplot2]{element_rect}} for the original function,
#'   \code{\link{coord_3d}} for 3D coordinate systems that utilize foreground panels
#' @export
element_rect <- function(fill = NULL, colour = NULL, linewidth = NULL, linetype = NULL,
                         color = NULL, inherit.blank = FALSE, size = lifecycle::deprecated(),
                         alpha = NULL) {

      if (lifecycle::is_present(size)) {
            deprecate_soft0("3.4.0", "element_rect(size)", "element_rect(linewidth)")
            linewidth <- size
      }
      if (!is.null(color))
            colour <- color
      structure(list(fill = fill, alpha = alpha, colour = colour, linewidth = linewidth,
                     linetype = linetype, inherit.blank = inherit.blank),
                class = c("element_rect", "element"))
}


