

StatProjRidgeline <- ggproto("StatProjRidgeline", Stat,
                             required_aes = c("x", "y", "z"),
                             compute_panel = function(data, scales, prj = projection(),
                                                      piece = "x") {
                                   # browser()

                                   data$group <- data[, piece]
                                   data$order <- data[, setdiff(names(data)[1:2], piece)]

                                   data <- data %>%
                                         group_by(group) %>%
                                         filter(order %in% range(order, na.rm = T)) %>%
                                         arrange(order) %>%
                                         slice(1:2) %>%
                                         mutate(order = max(order) + 2:1) %>%
                                         ungroup() %>%
                                         mutate(z = min(prj$zbreaks)) %>%
                                         bind_rows(data) %>%
                                         arrange(group, order) %>%
                                         as.data.frame()

                                   out <- project(data, prj)
                                   ord <- out %>%
                                         mutate(z0 = data$z) %>%
                                         group_by(group) %>%
                                         summarize(z = mean(z[z0 == min(z0, na.rm = T)], na.rm = T)) %>%
                                         arrange(z)
                                   out$group <- ordered(out$group, ord$group)
                                   arrange(out, group, order)
                             }
)



#' Ridgeline layer
#'
#' @export
proj_ridgeline <- function(mapping = NULL, data = NULL,
                           geom = "polygon", position = "identity",
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
      layer(
            stat = StatProjRidgeline,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
      )
}



