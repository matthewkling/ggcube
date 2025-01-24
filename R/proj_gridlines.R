

gridlines <- function(prj, facets = "auto"){

        breaks <- expand_grid(x = prj$xbreaks,
                              y = prj$ybreaks,
                              z = prj$zbreaks)

        x <- bind_rows(filter(breaks, x == min(x), y %in% range(y) | z %in% range(z)),
                       filter(breaks, x == max(x), y %in% range(y) | z %in% range(z))) %>%
                mutate(piece = paste("x", y, z, sep = "_"))
        y <- bind_rows(filter(breaks, y == min(y), x %in% range(x) | z %in% range(z)),
                       filter(breaks, y == max(y), x %in% range(x) | z %in% range(z))) %>%
                mutate(piece = paste("y", x, z, sep = "_"))
        z <- bind_rows(filter(breaks, z == min(z), y %in% range(y) | x %in% range(x)),
                       filter(breaks, z == max(z), y %in% range(y) | x %in% range(x))) %>%
                mutate(piece = paste("z", x, y, sep = "_"))
        g <- as.data.frame(bind_rows(x, y, z)) %>%
                mutate(axis = str_sub(piece, 1, 1))

        # convert to facets
        g <- bind_rows(g %>% filter(x == min(x)) %>% mutate(facet = "xmin"),
                       g %>% filter(x == max(x)) %>% mutate(facet = "xmax"),
                       g %>% filter(y == min(y)) %>% mutate(facet = "ymin"),
                       g %>% filter(y == max(y)) %>% mutate(facet = "ymax"),
                       g %>% filter(z == min(z)) %>% mutate(facet = "zmin"),
                       g %>% filter(z == max(z)) %>% mutate(facet = "zmax"))

        if(facets[1] %in% c("near", "far", "open", "closed")){
                facets <- select_faces(prj, facets[1])
        }
        if(facets[1] != "all") g <- g %>% filter(facet %in% facets)

        g %>%
                group_by(piece, facet) %>%
                mutate(n = n()) %>%
                filter(n > 1) %>%
                dplyr::select(-n) %>%
                ungroup() %>%
                as.data.frame()

}

StatProjGridlines <- ggproto("StatProjGridlines", Stat,
                                compute_panel = function(data, scales, prj, faces = "open", expand = 0) {
                                        if(is.null(prj$xlim) | is.null(prj$ylim) | is.null(prj$zlim)){
                                                prj <- add_limits(prj, data[,c("x", "y", "z")])
                                        }
                                        g <- gridlines(prj, faces)
                                        g$group <- paste(g$facet, g$piece)
                                        project(g, prj, expand)
                                },
                                required_aes = c("x", "y", "z")
)

#' Add gridlines
#'
#' @export
proj_gridlines <- function(mapping = NULL, data = NULL,
                           position = "identity",
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE,
                           faces = "open",
                           color = "gray40", linewidth = .25,
                           ...) {
        layer(
                stat = StatProjGridlines,
                data = data,
                mapping = mapping,
                geom = "path",
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, faces = faces,
                              color = color, linewidth = linewidth,
                              ...)
        )
}

