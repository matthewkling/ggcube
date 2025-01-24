
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggcube

<!-- badges: start -->
<!-- badges: end -->

The ggcube library is an extension of ggplot2 for 3D plotting.

This package is under development, and a stable version has not yet been
released.

## Example

To use ggcube, you use standard ggplot2 code along with a `projection()`
and one or more `proj_*()` layer functions.

Let’s make a 3D scatterplot of the `mpg` dataset. We’ll use
`proj_data(geom = "point")` to add our main data, and `proj_margin()` to
add some marginal scatter plots as well. ggplot2 is flexible and
powerful, but a downside to using it for 3D plots is that in addition to
adding our data, we also have to explicitly build up the reference
layers like the panel background, gridlines, and labels.

``` r
library(ggcube)

# get data
d <- select(mpg, displ, hwy, cty, cyl)

# define projection
prj <- projection(pitch = -20, roll = 160, 
                  persp = TRUE, dist = 2, data = d)

# build plot
d %>%
      ggplot(aes(displ, hwy, z = cty, color = factor(cyl))) +
      proj_panel(prj = prj) +
      proj_gridlines(prj = prj) +
      proj_margin(prj = prj, geom = "point", 
                  color = "gray40", size = 1, alpha = .5) +
      proj_data(prj = prj, geom = "point", size = 3) +
      proj_label(prj = prj, axis = "x", title = "displacement",
                 edge = c("ymin", "zmin")) +
      proj_label(prj = prj, axis = "y", title = "highway MPG",
                 edge = c("zmax", "xmax")) +
      proj_label(prj = prj, axis = "z", title = "city MPG",
                 edge = c("xmin", "ymax")) +
      coord_fixed() +
      theme_void()
```

<img src="man/figures/README-example-1.png" width="100%" />

As another example, let’s plot the `mountain` data set that comes with
the package, using the `proj_ridgeline()` function to visualize a 3D
surface:

``` r
prj <- projection(pitch = 75, yaw = 55,
                  persp = T, dist = 1,
                  data = mountain)

mountain %>%
      ggplot(aes(x, y, z = z)) +
      proj_ridgeline(prj = prj, geom = "polygon", piece = "y",
                     fill = "black", color = "white", linewidth = .15) +
      theme_void()
```

<img src="man/figures/README-example2-1.png" width="100%" />

## Installation

You can install the development version of ggcube from
[GitHub](https://github.com/matthewkling/ggcube) with:

``` r
devtools::install_github("matthewkling/ggcube")
```
