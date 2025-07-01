


# Enhanced element_rect with alpha support
element_rect <- function(fill = NULL, colour = NULL, linewidth = NULL, linetype = NULL,
                         color = NULL, inherit.blank = FALSE, size = lifecycle::deprecated(), alpha = NULL) {

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


