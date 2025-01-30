

StatProjMesh <- ggproto("StatProjMesh", Stat,
                        required_aes = c("x", "y", "z"),
                        compute_panel = function(data, scales, prj = projection()) {

                              data <- bind_rows(mutate(data, group = paste("x", x)),
                                                mutate(data, group = paste("y", y)))

                              out <- project(data, prj)

                              ord <- out %>%
                                    group_by(group) %>%
                                    summarize(z = mean(z, na.rm = T)) %>%
                                    arrange(z)
                              out$group <- ordered(out$group, ord$group)
                              arrange(out, group, z)
                        }
)



#' Ridgeline layer
#'
#' @export
proj_mesh <- function(mapping = NULL, data = NULL,
                      geom = "line", position = "identity",
                      na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE,
                      prj,
                      ...) {
      layer(
            stat = StatProjMesh,
            data = data,
            mapping = mapping,
            geom = geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(prj = prj, na.rm = na.rm, ...)
      )
}



