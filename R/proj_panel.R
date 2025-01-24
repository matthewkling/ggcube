
StatProjPanel <- ggproto("StatProjPanel", Stat,

                           required_aes = c("x", "y", "z"),
                           default_aes = aes(group = after_stat(group)),

                           compute_panel = function(data, scales, prj, faces = "open", expand = 0) {
                                   if(is.null(prj$xlim) | is.null(prj$ylim) | is.null(prj$zlim)){
                                           prj <- add_limits(prj, data[,c("x", "y", "z")])
                                   }
                                   g <- gridlines(prj, faces) %>%
                                           mutate(group = facet) %>%
                                           group_by(group) %>%
                                           filter(x %in% range(x), y %in% range(y), z %in% range(z)) %>%
                                           arrange(x, y, z) %>%
                                           ungroup() %>%
                                           select(x, y, z, group) %>%
                                           distinct() %>%
                                           group_by(group) %>%
                                           mutate(order = c(1, 2, 4, 3)) %>%
                                           ungroup() %>%
                                           mutate(group = factor(group)) %>%
                                           arrange(group, order) %>%
                                           as.data.frame()
                                   project(g, prj, expand)
                           },
)

#' Add 3D cube faces
#'
#' @export
proj_panel <- function(mapping = NULL, data = NULL,
                      position = "identity",
                      na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE,
                      faces = "open",
                      color = "black", fill = "gray80", linewidth = .25,
                      ...) {
        layer(
                stat = StatProjPanel,
                data = data,
                mapping = mapping,
                geom = "polygon",
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, faces = faces,
                              color = color, fill = fill, linewidth = linewidth,
                              ...)
        )
}

