# Shiny bindings for orbit_3d

Output and render functions for using
[`orbit_3d()`](https://matthewkling.github.io/ggcube/reference/orbit_3d.md)
within Shiny applications and interactive R Markdown documents.

## Usage

``` r
orbit3dOutput(outputId, width = "100%", height = "400px")

renderOrbit3d(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  Output variable to read from.

- width, height:

  Must be a valid CSS unit (like `"100%"`, `"400px"`, `"auto"`) or a
  number, which will be coerced to a string and have `"px"` appended.

- expr:

  An expression that generates a orbit (a call to
  [`orbit_3d()`](https://matthewkling.github.io/ggcube/reference/orbit_3d.md)).

- env:

  The environment in which to evaluate `expr`.

- quoted:

  Is `expr` a quoted expression (with
  [`quote()`](https://rdrr.io/r/base/substitute.html))? This is useful
  if you want to save an expression in a variable.

## Examples

``` r
if (FALSE) { # interactive() && requireNamespace("shiny", quietly = TRUE) && requireNamespace("base64enc", quietly = TRUE)
library(shiny)

p <- ggplot(mountain, aes(x, y, z, fill = z, color = z)) +
  geom_surface_3d(linewidth = .25) +
  coord_3d(light = light(anchor = "camera", direction = c(1, 1, 0)),
           ratio = c(1.5, 2, 1)) +
  scale_fill_viridis_c() + scale_color_viridis_c() +
  theme_void()

ui <- fluidPage(
  titlePanel("Interactive ggcube"),
  orbit3dOutput("fb", width = "500px", height = "500px")
)

server <- function(input, output, session) {
  output$fb <- renderOrbit3d({
    orbit_3d(p, yaw = c(360, 0), n = 12)
  })
}

shinyApp(ui, server)
}
```
