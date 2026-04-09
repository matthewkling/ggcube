# Aesthetic override resolution
#
# Shared infrastructure for applying aesthetic overrides to sub-elements
# within a layer (e.g. reference points/lines in geom_point_3d, data points
# in geom_smooth_3d). The pattern is: explicit override > mapped value in
# data > default.


#' Resolve aesthetic overrides for sub-elements
#'
#' For each aesthetic, applies the first available value from:
#' (1) explicit override parameter, (2) existing value in data, (3) default.
#'
#' @param data Data frame with current aesthetic columns.
#' @param overrides Named list of override values (NULL entries are skipped).
#' @param defaults Named list of default values for aesthetics not in data.
#' @return Data frame with resolved aesthetic values.
#' @keywords internal
resolve_aesthetic_overrides <- function(data, overrides = list(), defaults = list()) {
      aes_names <- union(names(overrides), names(defaults))

      for (aes_name in aes_names) {
            override <- overrides[[aes_name]]
            default <- defaults[[aes_name]]

            if (!is.null(override)) {
                  data[[aes_name]] <- override
            } else if (!aes_name %in% names(data) && !is.null(default)) {
                  data[[aes_name]] <- default
            }
            # else: keep existing mapped value in data
      }

      return(data)
}


#' Create point rows from raw data for mixed-geometry rendering
#'
#' Takes raw data points and prepares them for inclusion alongside polygon
#' surfaces in a mixed-geometry layer. Assigns unique group IDs and sets
#' `.prim = "point"`. Aesthetic columns are stripped so that annotation
#' points don't contaminate scale training; styling is applied at render
#' time in the geom's `draw_panel` via override parameters.
#'
#' @param data Data frame with x, y, z and aesthetic columns.
#' @param group_prefix Prefix for point group IDs.
#' @return Data frame with one row per point, tagged with `.prim = "point"`.
#' @keywords internal
make_point_rows <- function(data, group_prefix = "data_point") {

      n <- nrow(data)

      # Keep only positional and structural columns — strip mapped aesthetics
      # so annotation points don't contaminate scale training
      keep_cols <- intersect(names(data),
                             c("x", "y", "z", "fitted", "se", "level"))
      pt_data <- data[, keep_cols, drop = FALSE]

      pt_data$group <- paste0(group_prefix, "__pt", seq_len(n))
      pt_data$.prim <- "point"

      return(pt_data)
}


#' Create segment rows connecting data points to a surface
#'
#' For each data point at (x, y, z), creates a two-row segment group
#' connecting the point to the surface at (x, y, fitted). Used for
#' residual lines in geom_smooth_3d. Aesthetic columns are stripped so
#' annotations don't contaminate scale training; styling is applied at
#' render time via override parameters.
#'
#' @param data Data frame with x, y, z columns (raw data points).
#' @param fitted Numeric vector of fitted z values at each data point.
#' @param group_prefix Prefix for segment group IDs.
#' @return Data frame with two rows per segment, tagged with `.prim = "segment"`.
#' @keywords internal
make_residual_segment_rows <- function(data, fitted,
                                       group_prefix = "residual_line") {

      n <- nrow(data)
      seg_groups <- paste0(group_prefix, "__seg", seq_len(n))

      # Start points (raw data)
      start_rows <- data.frame(
            x = data$x, y = data$y, z = data$z,
            group = seg_groups,
            point_type = "start",
            segment_id = seq_len(n)
      )

      # End points (on the surface)
      end_rows <- data.frame(
            x = data$x, y = data$y, z = fitted,
            group = seg_groups,
            point_type = "end",
            segment_id = seq_len(n)
      )

      # Combine — no aesthetic columns carried forward
      seg_data <- rbind(start_rows, end_rows)
      seg_data$.prim <- "segment"

      return(seg_data)
}


#' Bind rows with automatic type coercion for aesthetic columns
#'
#' Coerces shared colour/fill columns to character before calling `bind_rows`,
#' preventing type conflicts when one data frame has logical `NA` (from
#' unmapped aesthetics) and the other has character values.
#'
#' @param ... Data frames to bind.
#' @return Combined data frame.
#' @keywords internal
safe_bind_rows <- function(...) {
      dfs <- list(...)
      colour_cols <- c("colour", "fill")
      for (col in colour_cols) {
            has_col <- vapply(dfs, function(df) col %in% names(df), logical(1))
            if (any(has_col)) {
                  for (i in which(has_col)) {
                        dfs[[i]][[col]] <- as.character(dfs[[i]][[col]])
                  }
            }
      }
      do.call(bind_rows, dfs)
}




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
