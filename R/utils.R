
# Aggregate aesthetic values per group.
# Intended for use within compute methods of stats producing polygons or lines, so that
# e.g. `aes(fill = x)` sets fill based on mean polygon x rather than only the first vertex's x.
average_aesthetics <- function(data, aesthetics = c("fill", "colour", "color", "alpha", "size", "stroke")){
      aesthetics <- intersect(names(data), aesthetics)

      average <- function(x){
            if(is.numeric(x)){
                  return(mean(x, na.rm = TRUE))
            }else{
                  return(x)
            }
      }

      # Compute group-level aesthetic means
      if (length(aesthetics) > 0) {
            data <- data %>%
                  group_by(group) %>%
                  mutate(across(all_of(aesthetics), average)) %>%
                  ungroup()
      }

      return(data)
}


ggproto_lookup <- function(x, type = "geom"){
      if(!inherits(x, "character")) return(x)
      if(!grepl("_3d", x)) return(x)
      cap <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
      get(paste0(cap(type), cap(gsub("_3d", "3D", x))))
}
