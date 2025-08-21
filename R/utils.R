
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
