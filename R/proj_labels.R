



StatProjLabel <-
        ggproto("StatProjLabel", Stat,
                compute_panel = function(data, scales, prj,
                                         axis = "x",
                                         edge = c("ymin", "zmax"),
                                         title = "x",
                                         rotate = TRUE,
                                         text_nudge = .01,
                                         title_nudge = .15
                ) {
                        if(is.null(prj$xlim) | is.null(prj$ylim) | is.null(prj$zlim)){
                                prj <- add_limits(prj, data[,c("x", "y", "z")])
                        }
                        g <- gridlines(prj, facets = "all")
                        gp <- project(g, prj)

                        # if(edge[1] == "auto") edge <- select_edge(prj, axis)

                        # axis text
                        ax <- axis
                        xx <- g %>% filter(axis != ax, facet == edge[2])
                        xx <- g %>% filter(axis != ax,
                                           facet == edge[1],
                                           paste(x, y, z) %in% paste(xx$x, xx$y, xx$z))
                        xx <- gp %>%
                                bind_cols(g %>% dplyr::select(xr = x, yr = y, zr = z)) %>%
                                filter(piece %in% xx$piece,
                                       facet %in% edge) %>%
                                mutate(lab = xr %in% unique(xx$x) &
                                               yr %in% unique(xx$y) &
                                               zr %in% unique(xx$z)) %>%
                                group_by(piece) %>%
                                arrange(lab) %>%
                                mutate(angle = atan2(diff(y), diff(x)) / pi * 180,
                                       angle = ifelse(between(angle, -90, 90), angle, angle + 180),
                                       angle = ifelse(rotate, angle, 0),
                                       xt = mean(x) + (x - mean(x)) * (1 + title_nudge * 2), # shift outward
                                       yt = mean(y) + (y - mean(y)) * (1 + title_nudge * 2),
                                       x = mean(x) + (x - mean(x)) * (1 + text_nudge * 2), # shift outward
                                       y = mean(y) + (y - mean(y)) * (1 + text_nudge * 2),
                                       label = case_when(ax == "x" ~ xr,
                                                         ax == "y" ~ yr,
                                                         ax == "z" ~ zr),
                                       label = as.character(label)) %>%
                                slice(2) %>%
                                ungroup() %>%
                                mutate(hjust = ifelse(mean(x) > 0, 0, 1),
                                       lab_axis = axis)
                        text <- xx
# browser()
                        # axis title
                        title <- text %>%
                                summarize(angle = atan2(diff(y[1:2]), diff(x[1:2])) / pi * 180,
                                          length = sqrt(diff(range(x))^2 + diff(range(y))^2),
                                          # y = median(y) + sin((90-angle) / 180 * pi) * title_nudge * length, # shift outward
                                          # x = median(x) - cos((90-angle) / 180 * pi) * title_nudge * length,
                                          x = median(xt),
                                          y = median(yt),
                                          angle = ifelse(between(angle, -90, 90), angle, angle + 180)
                                ) %>%
                                mutate(label = title)

                        bind_rows(title, text)
                },
                required_aes = c("x", "y", "z")
        )

#' Add axis labels
#'
#' @export
proj_label <- function(mapping = NULL, data = NULL,
                       position = "identity",
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE,
                       ...) {
        layer(
                stat = StatProjLabel,
                data = data,
                mapping = mapping,
                geom = "text",
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}

#############

StatProjLabels <-
        ggproto("StatProjLabels", Stat,
                compute_panel = function(data, scales, prj,
                                         edges = list(c("ymin", "zmax"),
                                                      c("xmin", "zmax"),
                                                      c("ymin", "xmax")),
                                         xtitle = "x", ytitle = "y", ztitle = "z"
                ) {
                        if(is.null(prj$xlim) | is.null(prj$ylim) | is.null(prj$zlim)){
                                prj <- add_limits(prj, data[,c("x", "y", "z")])
                        }
                        g <- gridlines(prj, facets = "all")
                        gp <- project(g, prj)

                        # axis text
                        make_labs <- function(edge = c("ymin", "zmax")){
                                ax <- case_when(!any(str_detect(edge, "xm")) ~ "x",
                                                !any(str_detect(edge, "ym")) ~ "y",
                                                !any(str_detect(edge, "zm")) ~ "z")
                                xx <- g %>% filter(axis != ax, facet == edge[2])
                                xx <- g %>% filter(axis != ax,
                                                   facet == edge[1],
                                                   paste(x, y, z) %in% paste(xx$x, xx$y, xx$z))
                                xx <- gp %>%
                                        bind_cols(g %>% dplyr::select(xr = x, yr = y, zr = z)) %>%
                                        filter(piece %in% xx$piece,
                                               facet %in% edge) %>%
                                        mutate(lab = xr %in% unique(xx$x) &
                                                       yr %in% unique(xx$y) &
                                                       zr %in% unique(xx$z)) %>%
                                        group_by(piece) %>%
                                        arrange(lab) %>%
                                        mutate(angle = atan2(diff(y), diff(x)) / pi * 180,
                                               angle = ifelse(between(angle, -90, 90), angle, angle + 180),
                                               label = case_when(ax == "x" ~ xr,
                                                                 ax == "y" ~ yr,
                                                                 ax == "z" ~ zr),
                                               label = paste0(" ", label, " ")) %>%
                                        slice(2) %>%
                                        ungroup() %>%
                                        mutate(hjust = ifelse(mean(x) > 0, 0, 1),
                                               lab_axis = ax)
                                xx
                        }
                        text <- map_dfr(edges, make_labs)

                        # axis titles
                        # browser()
                        text <-
                                text %>%
                                group_by(lab_axis) %>%
                                summarize(angle = atan2(diff(y[1:2]), diff(x[1:2])) / pi * 180,
                                          length = sqrt(diff(range(x))^2 + diff(range(y))^2),
                                          x = median(x),
                                          y = median(y),
                                          # y = y + sin((90-angle) / 180 * pi) * .15 * length, # shift outward
                                          # x = x - cos((90-angle) / 180 * pi) * .15 * length,
                                          angle = ifelse(between(angle, -90, 90), angle, angle + 180)
                                ) %>%
                                mutate(label = c(xtitle, ytitle, ztitle)) %>%
                                bind_rows(text)
                        text
                },
                required_aes = c("x", "y", "z")
        )

proj_labels <- function(mapping = NULL, data = NULL,
                        position = "identity",
                        na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE,
                        ...) {
        layer(
                stat = StatProjLabels,
                data = data,
                mapping = mapping,
                geom = "text",
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}
