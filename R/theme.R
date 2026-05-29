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
#' - `axis.text`, `axis.title`: Standard styling with `element_text()`.
#'
#' Use `element_text(margin = margin(...))` to adjust text padding, with left/right
#' margins affecting axis text and top/bottom margins affecting axis titles;
#' since placement and justification of these elements varies dynamically,
#' no distinction is made between left and right margins, or between top and
#' bottom margins -- you can set either, and the maximum of the two will be used.
#'
#' @section Panel elements:
#' - `panel.foreground`: Styling for cube faces rendered in front of data (inherits from `panel.background`).
#'          Foreground panels default to 20% opacity to keep them from obscuring the data; this can be
#'          overridden by setting `alpha` on either `panel.foreground` or `panel.background`.
#' - `panel.border.foreground`: Styling for cube faces rendered in front of data (inherits from `panel.border`)
#' - `panel.grid.foreground`: Styling for grid lines on foreground faces (inherits from `panel.grid`)
#' - `panel.grid.major.foreground`: Major grid lines on foreground faces (inherits from `panel.grid.foreground`)
#'
#' Background panels use standard `panel.background`, `panel.border`, `panel.grid`, etc., while foreground panels
#' use the `*.foreground` variants listed above. Since the foreground elements inherit from the standard background
#' and grid elements, you can use `panel.background`, etc. to style both background and foreground faces simultaneously.
#' This also extends to `alpha` set via the enhanced `element_rect()` below: setting `alpha` on `panel.background`
#' will tint both background and foreground panels (overriding the foreground's 20% default), unless an explicit
#' `alpha` is also set on `panel.foreground`.
#'
#' @section Enhanced elements:
#' - `element_rect()` extends `ggplot2::element_rect()` by adding an `alpha` parameter for transparency effects
#' on `coord_3d()`'s foreground and background panels.
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


#' Enhanced rectangle element with alpha support
#'
#' This function extends \code{ggplot2::element_rect()} to support transparency
#' via an \code{alpha} parameter, which is attached to the returned element as
#' an R attribute (\code{"ggcube_alpha"}). The attribute survives ggplot2's
#' theme inheritance machinery, so the alpha value set here is available to
#' the renderer at draw time. The \code{alpha} parameter is consumed only by
#' \code{coord_3d()}'s panel rendering and has no effect on other theme
#' elements.
#'
#' @param fill Fill colour for the rectangle.
#' @param alpha Transparency level applied to the fill of foreground or
#'   background panels in \code{coord_3d()}, ranging from 0 (completely
#'   transparent) to 1 (completely opaque). For \code{panel.foreground}, the
#'   alpha resolves in order: an explicit value here, then alpha inherited
#'   from \code{panel.background}, then a default of 0.2. For
#'   \code{panel.background}, an explicit value is used directly; otherwise
#'   the panel is fully opaque. Has no effect on other theme elements or in
#'   plots without a 3D coordinate system.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{element_rect}},
#'   such as \code{colour}, \code{linewidth}, \code{linetype}, and
#'   \code{inherit.blank}.
#'
#' @return A \code{ggplot2::element_rect} theme element with an attached
#'   \code{"ggcube_alpha"} attribute.
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
#'   \code{\link{coord_3d}} for 3D coordinate systems that utilize foreground panels
#' @export
element_rect <- function(fill = NULL, alpha = NA, ...) {
      el <- ggplot2::element_rect(fill = fill, ...)
      attr(el, "ggcube_alpha") <- alpha
      el
}
