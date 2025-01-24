

facets <- function(prj){
        facets <- as.matrix(expand.grid(x = c("xmin", "xmax"),
                                        y = c("ymin", "ymax"),
                                        z = c("zmin", "zmax")))
        corners <- expand.grid(x = range(prj$xbreaks),
                               y = range(prj$xbreaks),
                               z = range(prj$xbreaks))
        corners_p <- project(corners, prj)

        f <- bind_cols(setNames(as.data.frame(facets),
                                c("fx", "fy", "fz")),  corners_p) %>%
                gather(facet, value, fx:fz) %>%
                mutate(facet = str_remove(facet, "f"),
                       value = str_remove(value, facet),
                       x = scales::rescale(x),
                       y = scales::rescale(y),
                       z = scales::rescale(z)) %>%
                unite("facet", facet, value, sep = "") %>%
                arrange(facet)
        f
}

select_faces <- function(prj, faces = "far"){

        faces <- match.arg(faces, c("far", "near", "open", "closed"))

        f <- facets(prj)

        if(faces == "near") return(f$facet[f$z == 1])
        if(faces == "far") return(f$facet[f$z == 0])

        # sides facing the user touch the nearest corner
        # and touch more of the convex hull than other sides
        h <- f %>%
                mutate(on_hull = 1:nrow(.) %in% chull(x, y)) %>%
                group_by(x,y,z) %>%
                mutate(on_hull = any(on_hull)) %>%
                group_by(facet) %>%
                mutate(near_corner = any(z == 1)) %>%
                ungroup()
        h <- h %>%
                select(-facet) %>%
                filter(on_hull) %>%
                distinct() %>%
                mutate(vert = 1:nrow(.)) %>%
                full_join(h, by = join_by(x, y, z, on_hull, near_corner))
        nvert <- max(h$vert, na.rm = T)
        h <- h %>%
                group_by(facet) %>%
                summarize(nvert = length(unique(na.omit(vert))),
                          near = any(near_corner),
                          .groups = "drop") %>%
                mutate(closed = near & nvert == max(nvert))

        if(faces == "open") return(h$facet[! h$closed])
        if(faces == "closed") return(h$facet[h$closed])
}

# select_edge <- function(prj, axis){
#         browser()
#
#
#         f <- facets(prj) %>%
#                 arrange(x, y, z)
#
#         e <- expand_grid(facet_1 = unique(f$facet),
#                          facet_2 = unique(f$facet)) %>%
#                 filter(str_sub(facet_1, 1, 1) != str_sub(facet_2, 1, 1))
#
#
#
#         # if possible, select an edge on chull
#
# }
