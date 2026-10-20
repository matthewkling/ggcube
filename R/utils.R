
#' Create segment rows connecting data points to a surface
#'
#' For each data point at (x, y, z), creates a two-row segment group
#' connecting the point to the surface at (x, y, fitted). Used for
#' residual lines in geom_smooth_3d. Aesthetic columns are stripped so
#' overlays don't contaminate scale training; styling is applied at
#' render time via override parameters.
#'
#' @param data Data frame with x, y, z columns (raw data points).
#' @param fitted Numeric vector of fitted z values at each data point.
#' @param group_prefix Prefix for segment group IDs.
#' @return Data frame with two rows per segment, tagged with `.prim = "segment"`.
#' @keywords internal
#' @noRd
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
#' @noRd
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


#' Find the plot object currently being built
#'
#' Several pieces of plot-level state -- the labels, a `+ light()` stash, the
#' z scale -- live on the plot, but `Coord3D` methods and their helpers are
#' handed only scales and params. This walks the calling frames to reach the
#' plot that `ggplot_build()` is working on.
#'
#' The walk stops at the global environment, so a user object that happens to
#' be named `plot` can't be picked up when the call chain runs out before
#' `max_frames` does.
#'
#' @param max_frames Maximum number of calling frames to inspect.
#' @return The `ggplot` object being built, or `NULL` if it can't be reached.
#' @keywords internal
#' @noRd
find_build_plot <- function(max_frames = 40) {
      found <- NULL
      tryCatch({
            for (i in seq_len(max_frames)) {
                  env <- parent.frame(i)
                  if (identical(env, globalenv())) break
                  if (exists("plot", envir = env, inherits = FALSE)) {
                        candidate <- get("plot", envir = env, inherits = FALSE)
                        if (inherits(candidate, "ggplot")) {
                              found <- candidate
                              break
                        }
                  }
            }
      }, error = function(e) {
            # If frame walking fails, callers fall back to their defaults
      })
      found
}


#' Warn that a layer's `sort_method` is deprecated
#'
#' All ggcube layers in a panel are depth-sorted together, pairwise, so a
#' layer's `sort_method` has no effect.
#'
#' @param sort_method The layer function's `sort_method` argument.
#' @param fn Name of the layer function.
#' @param user_env Environment of the user's call to the layer function.
#' @keywords internal
#' @noRd
check_sort_method <- function(sort_method, fn, user_env = rlang::caller_env(2)) {
      if (lifecycle::is_present(sort_method)) {
            lifecycle::deprecate_warn(
                  "0.4.0", paste0(fn, "(sort_method)"),
                  details = paste("All 3D layers in a panel are now depth-sorted together,",
                                  "pairwise, so `sort_method` has no effect."),
                  user_env = user_env
            )
      }
      invisible()
}
