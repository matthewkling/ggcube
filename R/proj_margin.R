
StatProjMargin <- ggproto("StatProjMargin", Stat,
                             compute_panel = function(data, scales, prj = projection(),
                                                      faces = "far") {

                                     if(faces[1] %in% c("near", "far", "open", "closed")) faces <- select_faces(prj, faces)
                                     d <- lapply(faces, function(f){
                                             if(f == "xmin") return(mutate(data, x = min(prj$xbreaks)))
                                             if(f == "xmax") return(mutate(data, x = max(prj$xbreaks)))
                                             if(f == "ymin") return(mutate(data, y = min(prj$ybreaks)))
                                             if(f == "ymax") return(mutate(data, y = max(prj$ybreaks)))
                                             if(f == "zmin") return(mutate(data, z = min(prj$zbreaks)))
                                             if(f == "zmax") return(mutate(data, z = max(prj$zbreaks)))
                                     }) %>%
                                           bind_rows()
                                     project(d, prj)

                             },
                             required_aes = c("x", "y", "z")
)

#' Add marginal data
#'
#' @export
proj_margin <- function(mapping = NULL, data = NULL,
                        geom = "point", position = "identity",
                        na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE,
                        faces = "far",
                        ...) {
        layer(
                stat = StatProjMargin,
                data = data,
                mapping = mapping,
                geom = geom,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(facets = faces, na.rm = na.rm, ...)
        )
}

