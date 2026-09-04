.onLoad <- function(libname, pkgname) {
      # Define custom theme elements
      register_theme_elements(
            axis.text.z = element_text(),
            axis.title.z = element_text(),
            axis.ticks.z = element_line(),

            # No alpha set here intentionally: leaving the foreground default
            # without an explicit alpha lets it inherit from panel.background
            # at render time. The final 0.2 fallback (when neither layer has
            # an explicit alpha) lives in create_panel_polygons() in
            # R/panel-rendering.R.
            panel.foreground = element_rect(),

            panel.grid.foreground = element_line(),
            panel.grid.major.foreground = element_line(),
            panel.border.foreground = element_rect(),
            element_tree = list(
                  axis.text.z = el_def("element_text", "axis.text"),
                  axis.title.z = el_def("element_text", "axis.title"),
                  axis.ticks.z = el_def("element_line", "axis.ticks"),

                  # Registered in the element tree only. A unit-valued element
                  # has no sensible standalone default, so the value comes from
                  # axis.ticks.length by inheritance.
                  axis.ticks.length.z = el_def("unit", "axis.ticks.length"),
                  panel.foreground = el_def("element_rect", "panel.background"),
                  panel.grid.foreground = el_def("element_line", "panel.grid"),
                  panel.grid.major.foreground = el_def("element_line", "panel.grid.foreground"),
                  panel.border.foreground = el_def("element_rect", "panel.border")
            )
      )
}
