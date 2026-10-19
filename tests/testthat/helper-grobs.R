# ggcube layers are depth-sorted and built into grobs only when drawn (see
# R/scene.R), so tests that inspect grobs look at a plot or grob as drawn.

# Grob tree of a ggplot, gtable, or grob after drawing it off-screen
drawn_grob <- function(x, width = 6, height = 5) {
      if (inherits(x, "ggplot")) x <- ggplotGrob(x)
      grDevices::pdf(NULL, width = width, height = height)
      on.exit(grDevices::dev.off())
      grid::grid.newpage()
      grid::grid.draw(x)
      grid::grid.force()
      grid::grid.grab(wrap.grobs = FALSE)
}

# Children of a grob (forced gtables in a drawn tree keep their built
# children in `children`, and undrawn gtables in `grobs`)
grob_kids <- function(g) {
      if (inherits(g, "gtable") && length(g$children) == 0) return(g$grobs)
      if (inherits(g, "gTree")) g$children else list()
}

# All grobs of a given class in a drawn tree, in drawing order
find_grobs <- function(g, cl) {
      if (inherits(g, cl)) return(list(g))
      unlist(lapply(grob_kids(g), find_grobs, cl = cl), recursive = FALSE)
}
