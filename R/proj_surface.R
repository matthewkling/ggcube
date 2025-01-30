

StatProjSurface <- ggproto("StatProjSurface", Stat,
                        required_aes = c("x", "y", "z"),
                        compute_panel = function(data, scales, prj = projection()) {
                              data <- data %>%
                                    mutate(group = 1:nrow(.))
                              dy <- data %>%
                                    group_by(x) %>%
                                    mutate(y = lag(y),
                                           z = lag(z)) %>%
                                    ungroup()
                              dx <- data %>%
                                    group_by(y) %>%
                                    mutate(x = lag(x),
                                           z = lag(z)) %>%
                                    ungroup()
                              dxy <- data.frame(x = dx$x,
                                           y = dy$y) %>%
                                    left_join(data) %>%
                                    mutate(group = dx$group)
                              d <- bind_rows(data, dx, dxy, dy) %>%
                                    na.omit() %>%
                                    group_by(group) %>%
                                    filter(n() == 4) %>%
                                    mutate(order = 1:4) %>%
                                    arrange(x) %>%
                                    mutate(dzdx = (mean(z[3:4]) - mean(z[1:2])) / (mean(x[3:4]) - mean(x[1:2]))) %>%
                                    arrange(y) %>%
                                    mutate(dzdy = (mean(z[3:4]) - mean(z[1:2])) / (mean(y[3:4]) - mean(y[1:2])),
                                           slope = sqrt(dzdy^2 + dzdx^2),
                                           aspect = atan2(dzdy, dzdx)) %>%
                                    ungroup() %>%
                                    as.data.frame()

                              out <- project(d, prj)

                              ord <- out %>%
                                    group_by(group) %>%
                                    summarize(z = mean(z, na.rm = T)) %>%
                                    arrange(z)
                              out$group <- ordered(out$group, ord$group)
                              arrange(out, group, order)
                        }
)



#' Surface
#'
#' @param prj A `proj` object created by \code{projection()}.
#' @examples
#' library(tidyverse)
#'
#' # set up data and define projection
#' d <- expand_grid(x = seq(-10, 10, .5),
#'                  y = seq(-10, 10, .5)) %>%
#'       mutate(z = cos(sqrt(x^2 + y^2)))
#' prj <- projection(data = d, pitch = 35, yaw = 55,
#'       persp = T, dist = 1, zlim = c(-3, 3))
#'
#' # surface plot, colored by z
#' ggplot(d, aes(x, y, z = z, fill = z)) +
#'       proj_surface(prj = prj) +
#'       scale_fill_viridis_c(option = "B") +
#'       theme_cube() +
#'       coord_fixed()
#'
#' # highlight terrain attributes using after_stat
#' ggplot(d, aes(x, y, z = z, fill = after_stat(dzdy))) +
#'       proj_surface(prj = prj) +
#'       scale_fill_viridis_c(option = "B") +
#'       theme_cube() +
#'       coord_fixed()
#' @return a ggplot
#' @export
proj_surface <- function(mapping = NULL, data = NULL,
                      geom = "polygon", position = "identity",
                      na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE,
                      prj,
                      ...) {
      layer(
            stat = StatProjSurface,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(prj = prj, na.rm = na.rm, ...)
      )
}



