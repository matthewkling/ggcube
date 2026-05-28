
#' Using themes to style 3D panels and axis labels
#'
#' In ggcube, standard ggplot2 themes generally influence 3D plots as expected, including adding
#' complete themes like `ggplot2::theme_dark()` and modifying theme elements like
#' `theme(panel.background = element_rect(fill = "darkblue")`. However, ggcube also
#' provides additional theme elements that control 3D-specific styling of panels
#' and labels.
#'
#' @section Text elements:
#' - `axis.text.z`: Styling for z-axis tick labels (inherits from `axis.text`)
#' - `axis.title.z`: Styling for z-axis title (inherits from `axis.title`)
#' - `axis.text`, `axis.text`: Standard styling with `element_text()`.
#'
#' Use `element_text(margin = margin(...))` to adjust text padding, with left/right
#' margins affecting axis text and top/bottom margins affecting axis titles;
#' since placement and justification of these elements varies dynamically,
#' no distinction is made between left and right margins, or between top and
#' bottom margins -- you can set either, and the maximum of the two will be used.
#'
#' @section Panel elements:
#' - `panel.foreground`: Styling for cube faces rendered in front of data (inherits from `panel.background`).
#'          Uses `element_rect(alpha = .2)` by default, to prevent foreground panels from obscuring the data.
#' - `panel.border.foreground`: Styling for cube faces rendered in front of data (inherits from `panel.border`)
#' - `panel.grid.foreground`: Styling for grid lines on foreground faces (inherits from `panel.grid`)
#' - `panel.grid.major.foreground`: Major grid lines on foreground faces (inherits from `panel.grid.foreground`)
#'
#' Background panels use standard `panel.background`, `panel.border`, `panel.grid`, etc., while foreground panels
#' use the `*.foreground` variants listed above. Since the foreground elements inherit from the standard background
#' and grid elements, you can use `panel.background`, etc. to style both background and foreground faces simultaneously.
#'
#' @section Enhanced elements:
#' - `element_rect()` extends `ggplot2::element_rect()` by adding an `alpha` parameter for transparency effects.
#' This is particularly useful for `panel.foreground` components that sit in front of the data.
#'
#' @examples
#' # example code
#' p <- ggplot(sphere_points, aes(x, y, z)) +
#'   geom_hull_3d() +
#'   coord_3d(panels = "all") +
#'   theme(panel.background = element_rect(color = "black"),
#'           panel.border = element_rect(color = "black"),
#'           panel.foreground = element_rect(alpha = .3),
#'           panel.grid.foreground = element_line(color = "gray", linewidth = .25),
#'           axis.text = element_text(color = "darkblue"),
#'           axis.text.z = element_text(color = "darkred"),
#'           axis.title = element_text(margin = margin(t = 30)), # add padding
#'           axis.title.x = element_text(color = "magenta"))
#'
#' @name cube_theming
NULL



#' Enhanced rectangle theme element with alpha support
#'
#' This function extends \code{ggplot2::element_rect()} to support transparency
#' via an \code{alpha} parameter. This is particularly useful for styling
#' foreground panels in 3D plots to create layered visual effects.
#'
#' This function delegates to \code{ggplot2::element_rect()} to construct a fully
#' valid theme element, then attaches the \code{alpha} field.
#'
#' @param fill Fill colour for the rectangle.
#' @param colour,color Border colour for the rectangle.
#' @param linewidth Border width.
#' @param linetype Border line type.
#' @param inherit.blank Should this element inherit the existence of an
#'   \code{element_blank} among its parents? See
#'   \code{\link[ggplot2]{element_rect}} for details.
#' @param alpha Transparency level for the rectangle fill, ranging from 0
#'   (completely transparent) to 1 (completely opaque). Particularly useful for
#'   styling foreground panels in 3D plots to create layered visual effects.
#'
#' @return A theme element object that can be used in \code{theme()}
#'   specifications.
#'
#' @examples
#' # Basic 3D plot with semi-transparent foreground panels
#' ggplot(mountain, aes(x, y, z)) +
#'   stat_surface_3d(fill = "darkblue", color = "lightblue", linewidth = .1) +
#'   coord_3d(panels = c("background", "ymin")) +
#'   theme(panel.foreground = element_rect(alpha = 0.6))
#'
#' # Completely transparent foreground panels
#' ggplot(mtcars, aes(mpg, wt, qsec)) +
#'   geom_point() +
#'   coord_3d(panels = "all") +
#'   theme(panel.border = element_rect(color = "black"),
#'         panel.foreground = element_rect(fill = "blue", alpha = 0))
#'
#' @seealso \code{\link[ggplot2]{element_rect}} for the original function,
#'   \code{\link{coord_3d}} for 3D coordinate systems that utilise foreground panels
#' @export
element_rect <- function(fill = NULL, colour = NULL, linewidth = NULL,
                         linetype = NULL, color = NULL,
                         inherit.blank = FALSE, alpha = NULL) {
      el <- ggplot2::element_rect(
            fill = fill, colour = colour, linewidth = linewidth,
            linetype = linetype, color = color, inherit.blank = inherit.blank
      )
      el$alpha <- alpha
      el
}
