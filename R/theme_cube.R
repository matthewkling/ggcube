
#' A ggplot theme for ggcube
#'
#' This is a variant on `ggplot::theme_gray()` with blank panel background,
#' grid, and axis elements.
#'
#' @param ... arguments passed to `ggplot2::theme()`
#' @return a gg theme object
#' @export
theme_cube <- function(...){
      theme_gray() +
            theme(panel.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank()) +
            theme(...)
}
