

StatProj <- ggproto("StatProj", Stat,
                       required_aes = c("x", "y", "z"),
                       compute_panel = function(data, scales, prj = projection(),
                                                reorder = TRUE) {
                               out <- project(data, prj)
                               if(reorder){
                                       if("order" %in% names(out)){
                                               out <- arrange(out, group, order, z)
                                       }else{
                                               out <- arrange(out, group, z)
                                       }
                               }
                               out
                       }
)



#' Add projected data
#'
#' @export
proj_data <- function(mapping = NULL, data = NULL,
                      geom = "point", position = "identity",
                      na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
        layer(
                stat = StatProj,
                data = data,
                mapping = mapping,
                geom = geom,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}



