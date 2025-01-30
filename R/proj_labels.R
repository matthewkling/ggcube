
select_edge <- function(prj, axis = "x", faces = "open"){


      expand_grid(x1 = range(prj$xbreaks),
                  x2 = range(prj$xbreaks),
                  y1 = range(prj$ybreaks),
                  y2 = range(prj$ybreaks),
                  z1 = range(prj$zbreaks),
                  z2 = range(prj$zbreaks)) %>%
            filter(x1 != x2 | y1 != y2 | z1 != z2,
                   x1 == x2 | y1 == y2 | z1 == z2) %>%
            mutate(id = 1:nrow(.)) %>%
            gather(key, value, -id) %>%
            separate(key, c("var", "i"), sep = 1) %>%
            spread(var, value)

            mutate(x0 = pmin(x1, x2),
                   y0 = pmin(y1, y2),
                   z0 = pmin(z1, z2),
                   x1 = pmax(x1, x2),
                   y1 = pmax(y1, y2),
                   z1 = pmax(z1, z2)) %>%
            select(-x2, -y2, -z2) %>%
            distinct()


      # ideally, edge should be on 1 and only 1 of selected_faces

      f <- face_info(prj)
      faces <- select_faces(prj, faces)
      faces <- faces[str_sub(faces, 1, 1) != axis]

      e <- setdiff(c("x", "y", "z"), axis)
      e <- expand_grid(facet1 = c("min", "max"),
                       facet2 = c("min", "max")) %>%
            mutate(facet1 = paste0(e[1], facet1),
                   facet2 = paste0(e[2], facet2),
                   n = facet1 %in% faces + facet2 %in% faces) %>%
            left_join(select(f, facet1 = ))




      #####

      v <- vertices(prj)

      f <- face_info(prj) %>%
            mutate(plane = str_sub(facet, 1, 1)) %>%
            filter(plane != axis)


      # ax <- axis
      # g <- gridlines(prj, faces = "all") %>%
      #       filter(axis == ax,
      #              x %in% range(x),
      #              y %in% range(y),
      #              z %in% range(z))
      # g[,ax] <- NA
      # g <- distinct(g)


      # gp <- project(g, prj)
      # ax <- axis
      # xx <- g %>% filter(axis != ax, facet == edge[2])
      # xx <- g %>% filter(axis != ax, facet == edge[1],
      #                    paste(x, y, z) %in% paste(xx$x, xx$y, xx$z))


      ####

      # f <- facets(prj) %>%
      #       filter(str_sub(facet, 1, 1) != axis)
      #
      # e <- expand_grid(facet1 = unique(f$facet),
      #             facet2 = unique(f$facet)) %>%
      #       mutate(f1 = str_sub(facet1, 1, 1),
      #              f2 = str_sub(facet2, 1, 1)) %>%
      #       filter(f1 != f2) %>%
      #       select(-f1, -f2)
      #
      # facet1 = "ymin"
      # facet2 = "zmax"
      # filter(f, facet %in% c(facet1, facet2)) %>%
      #       group_by(x, y, z) %>%
      #       filter(n() == 2) %>%
      #       select(-facet) %>%
      #       distinct()
      #
      #
      # e <- expand_grid(facet = c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
      #                  x = range(prj$xbreaks),
      #                  y = range(prj$ybreaks),
      #                  z = range(prj$zbreaks))



}


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
                    g <- gridlines(prj, faces = "all")
                    gp <- project(g, prj)

                    mean_angle <- function(deg){
                          r <- deg / 180
                          y <- mean(sinpi(r))
                          x <- mean(cospi(r))
                          atan2(y, x) / pi * 180
                    }

                    # axis text
                    ax <- axis
                    xx <- g %>% filter(axis != ax, facet == edge[2])
                    xx <- g %>% filter(axis != ax, facet == edge[1],
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
                                 # angle = ifelse(between(angle, -90, 90), angle, angle + 180),
                                 # angle = ifelse(rotate, angle, 0),
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
                                 lab_axis = axis,
                                 angle = ifelse(between(mean_angle(angle), -90, 90), angle, angle + 180),
                                 angle = ifelse(rotate, angle, 0)
                                 )
                    text <- xx

                    # axis title
                    title <- text %>%
                          summarize(angle = atan2(diff(y[1:2]), diff(x[1:2])) / pi * 180,
                                    length = sqrt(diff(range(x))^2 + diff(range(y))^2),
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
#' @inheritParams ggplot2::stat_identity
#' @param prj A `proj` object created by \code{projection()}.
#' @param axis A character indicating which axis to label; either "x", "y", or "z".
#' @param edge A character vector of length 2 specifying where to place the labels.
#'    For example, if \code{axis = "x"}, then \code{edge = c("ymin", "zmin")} will
#'    place the labels on the cube edge corresponding to low values of the y and z
#'    variables. The order of the vector determines the orientation of the labels,
#'    which will be inline with the gridlines on the face listed first.
#' @param title String giving the axis title.
#' @param rotate Logical indicating whether to rotate axis text to be inline with
#'    gridlines; default is TRUE.
#' @param text_nudge,title_nudge Scalars giving distance beyond the axis to place
#'    the axis text and axis title, as a proportion of axis length.
#'
#' @export
proj_label <- function(mapping = NULL, data = NULL,
                       position = "identity",
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE,
                       prj,
                       axis = "x",
                       edge = c("ymin", "zmax"),
                       title = "x",
                       rotate = TRUE,
                       text_nudge = .01,
                       title_nudge = .15,
                       ...) {
      layer(
            stat = StatProjLabel,
            data = data,
            mapping = mapping,
            geom = "text",
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(prj = prj, axis = axis, edge = edge,
                          title = title, rotate = rotate,
                          text_nudge = text_nudge, title_nudge = title_nudge,
                          na.rm = na.rm, ...)
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
                    g <- gridlines(prj, faces = "all")
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
