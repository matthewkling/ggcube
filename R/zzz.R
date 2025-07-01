
.onLoad <- function(libname, pkgname) {

      # Define custom theme elements
      register_theme_elements(
            axis.text.z = element_text(),
            axis.title.z = element_text(),
            panel.foreground = element_rect(),
            panel.grid.foreground = element_line(),
            panel.grid.major.foreground = element_line(),
            element_tree = list(
                  axis.text.z = el_def("element_text", "axis.text"),
                  axis.title.z = el_def("element_text", "axis.title"),
                  panel.foreground = el_def("element_rect", "panel.background"),
                  panel.grid.foreground = el_def("element_line", "panel.grid"),
                  panel.grid.major.foreground = el_def("element_line", "panel.grid.foreground")
            )
      )

      # Initialize cache environment
      assign(".z_scale_cache", new.env(), envir = parent.env(environment()))
}
