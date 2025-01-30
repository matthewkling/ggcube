

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

vertices <- function(prj){
      f <- facets(prj)

      v <- f %>%
            mutate(on_hull = 1:nrow(.) %in% chull(x, y)) %>%
            group_by(x,y,z) %>%
            mutate(on_hull = any(on_hull)) %>%
            group_by(facet) %>%
            mutate(near_corner = any(z == 1),
                   far_corner = any(z == 0)) %>%
            ungroup()
      v <- v %>%
            select(-facet) %>%
            filter(on_hull) %>%
            distinct() %>%
            mutate(vert = 1:nrow(.)) %>%
            full_join(v, by = join_by(x, y, z, on_hull,
                                      near_corner, far_corner))
      v
}


face_info <- function(prj){

      # sides facing the user touch the nearest corner
      # and touch more of the convex hull than other sides
      v <- vertices(prj)
      nvert <- max(v$vert, na.rm = T)
      h <- v %>%
            group_by(facet) %>%
            summarize(xmin = min(x), xmean = mean(x), xmax = max(x),
                      ymin = min(y), ymean = mean(y), ymax = max(y),
                      zmin = min(z), zmean = mean(z), zmax = max(z),
                      xfar = mean(x[z == min(z)]),
                      xnear = mean(x[z == max(z)]),
                      yfar = mean(y[z == min(z)]),
                      ynear = mean(y[z == max(z)]),
                      nvchull = length(unique(na.omit(vert))),
                      near = any(near_corner),
                      far = any(far_corner),
                      .groups = "drop")

      if((prj$shear_xz == 0 & prj$shear_yz == 0 & prj$hjust == 0.5 & prj$vjust == 0.5) | prj$pitch != 0 | prj$roll != 0){
            h <- mutate(h, closed = near & nvchull == max(nvchull))
      }else{
            # for a given face, if the distant edge is farther from middle than near edge, it's closed
            h <- mutate(h, closed = case_when(
                  near & !far ~ TRUE,
                  !near & far ~ FALSE,
                  facet == "xmax" & xfar > xnear ~ TRUE,
                  facet == "xmin" & xfar < xnear ~ TRUE,
                  facet == "ymax" & yfar > ynear ~ TRUE,
                  facet == "ymin" & yfar < ynear ~ TRUE,
                  TRUE ~ FALSE
            ))
      }

      h %>%
            arrange(zmean, closed) %>%
            mutate(render = 1:nrow(.))
}


#' Select cube faces to display
#'
#' @param prj A `proj` object created by \code{projection()}.
#' @param faces A character vector indicating which of the six faces of the projected cube to
#' select. Can be any combination of the following:
#' \itemize{
#'   \item \code{"all"} all six faces
#'   \item \code{"near"} faces touching the cube corner nearest the viewer
#'   \item \code{"far"} faces touching the cube corner farthest from the viewer
#'   \item \code{"open"} faces whose inside surface faces the viewer
#'   \item \code{"closed"} faces whose outside surface faces the viewer
#'   \item \code{"xmin"} face at the low end of the x-axis
#'   \item \code{"xmax"} face at the high end of the x-axis
#'   \item \code{"ymin"} face at the low end of the y-axis
#'   \item \code{"ymax"} face at the high end of the y-axis
#'   \item \code{"zmin"} face at the low end of the z-axis
#'   \item \code{"zmax"} face at the high end of the z-axis
#'   \item \code{"left"} face farthest to the left
#'   \item \code{"right"} face farthest to the right
#'   \item \code{"top"} face closest to the top
#'   \item \code{"bottom"} face closest to the bottom
#'   \item \code{"front"} face closest to the viewer
#'   \item \code{"back"} face farthest from the viewer
#' }
#' @export
select_faces <- function(prj, faces = "far"){

      rules <- c("all", "near", "far", "open", "closed",
                 "left", "right", "top", "bottom", "front", "back")
      facets <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
      faces <- match.arg(faces, c(rules, facets), several.ok = TRUE)

      rules <- rules[rules %in% faces]
      facets <- facets[facets %in% faces]

      f <- face_info(prj)
      rules <- sapply(rules, function(x){
            switch(x,
                   all = f$facet,
                   near = f$facet[f$near],
                   far = f$facet[f$far],
                   open = f$facet[!f$closed],
                   closed = f$facet[f$closed],
                   left = f$facet[f$xmean == min(f$xmean)][1],
                   right = f$facet[f$xmean == max(f$xmean)][1],
                   top = f$facet[f$ymean == min(f$ymean)][1],
                   bottom = f$facet[f$ymean == max(f$ymean)][1],
                   front = f$facet[f$zmean == max(f$zmean)][1],
                   back = f$facet[f$zmean == min(f$zmean)][1]
            )
      })


      unique(c(rules, facets))
}
