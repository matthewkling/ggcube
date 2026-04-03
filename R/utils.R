# Aggregate numeric values per polygon group.
# Averages all numeric columns except positional/structural ones, so that
# computed variables (e.g. `fitted`, `density`, `se`) and visual aesthetics
# (e.g. `fill`, `colour`) get per-polygon values before ggplot2 resolves
# `after_stat()` mappings or picks a single vertex's value.
average_aesthetics <- function(data,
                               exclude = c("x", "y", "z", "order",
                                           ".vertex_order", "PANEL",
                                           "row", "column")){

      numeric_cols <- names(data)[sapply(data, is.numeric)]
      avg_cols <- setdiff(numeric_cols, exclude)

      if (length(avg_cols) > 0) {
            data <- data %>%
                  group_by(group) %>%
                  mutate(across(all_of(avg_cols),
                                ~ if(is.numeric(.x)) mean(.x, na.rm = TRUE) else .x)) %>%
                  ungroup()
      }

      return(data)
}


get_proto <- function(x, type = c("geom", "stat")){
      if(!inherits(x, "character")) return(x)
      if(!grepl("_3d", x)) return(x)
      cap <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
      get(paste0(cap(type), cap(gsub("_3d", "3D", x))))
}
