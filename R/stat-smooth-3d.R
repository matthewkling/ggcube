StatSmooth3D <- ggproto("StatSmooth3D", Stat,
                        required_aes = c("x", "y", "z"),
                        default_aes = aes(x = after_stat(x),
                                          y = after_stat(y),
                                          z = after_stat(z),
                                          fill = after_stat(fitted)),

                        compute_panel = function(data, scales, method = "loess", formula = NULL,
                                                 method.args = list(), xlim = NULL, ylim = NULL,
                                                 n = 30, light = NULL, na.rm = FALSE, domain = "chull",
                                                 se = FALSE, level = 0.95, se.fill = NULL, se.colour = NULL,
                                                 se.alpha = 0.5, se.linewidth = NULL) {

                              # Remove missing values if requested
                              if (na.rm) {
                                    complete_cases <- complete.cases(data[c("x", "y", "z")])
                                    data <- data[complete_cases, ]
                              }

                              # Check we have enough data
                              if (nrow(data) < 4) {
                                    stop("stat_smooth_3d requires at least 4 complete cases")
                              }

                              # Validate method
                              valid_methods <- c("loess", "lm", "glm", "gam")
                              if (!method %in% valid_methods) {
                                    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
                              }



                              # Validate grid size
                              if (length(n) == 1) {
                                    n <- c(n, n)
                              }
                              if (length(n) != 2 || any(n < 3)) {
                                    stop("n must be a single integer >= 3 or a vector of length 2 with values >= 3")
                              }

                              # Get model specification
                              model_func <- switch(method,
                                                   lm = lm_model,
                                                   glm = glm_model,
                                                   loess = loess_model,
                                                   gam = gam_model,
                                                   stop("invalid model"))

                              # Handle formula defaults
                              if (is.null(formula)) {
                                    model_spec <- model_func()
                                    if("default_formula" %in% names(model_spec)) {
                                          formula <- model_spec$default_formula
                                    } else {
                                          formula <- z ~ x + y
                                    }
                              }

                              # Determine confidence levels to compute
                              z_score <- qnorm(1 - (1 - level)/2)
                              if(se) {
                                    surface_types <- c("upper", "fitted", "lower")
                              } else {
                                    surface_types <- "fitted"
                              }


                              # Create grid polygons, with hull clipping if applicable
                              polys <- create_grid_polys(data, domain, xlim, ylim, n)

                              # Fit model and predict
                              predictions <- fit_and_predict(data, polys, method, formula,
                                                             method.args, se = se, level = level)
                              polys$fitted <- predictions$fitted
                              polys$se <- predictions$se

                              # Create surfaces for each level
                              surfaces <- surface_types %>%
                                    lapply(function(surface_type, data = polys) {
                                          data$level <- factor(surface_type,
                                                               levels = c("upper", "fitted", "lower"),
                                                               labels = c(paste0("upper ", level*100, "% CI"),
                                                                          "fitted",
                                                                          paste0("lower ", level*100, "% CI")))
                                          data$group <- paste0(data$group, "-", surface_type)
                                          data$z <- switch(surface_type,
                                                           fitted = data$fitted,
                                                           lower = data$fitted - z_score * data$se,
                                                           upper = data$fitted + z_score * data$se)

                                          # Apply confidence band styling
                                          if(surface_type != "fitted"){
                                                if (!is.null(se.fill)) {
                                                      data$fill <- se.fill
                                                }
                                                if (!is.null(se.colour)) {
                                                      data$colour <- se.colour
                                                }
                                                if (!is.null(se.linewidth)) {
                                                      data$linewidth <- se.linewidth
                                                }
                                                data$alpha <- se.alpha
                                          }

                                          return(data)
                                    }) %>%
                                    bind_rows()

                              # Add computed variables and light info
                              surfaces <- surfaces %>%
                                    bind_rows(compute_surface_gradients_from_vertices(surfaces)) %>%
                                    mutate(slope = sqrt(dzdy^2 + dzdx^2),
                                           aspect = atan2(dzdy, dzdx)) %>%
                                    attach_light(light)

                              return(surfaces)

                        }
)

create_grid_polys <- function(point_data, domain, xlim, ylim, n) {

      ### create regular rectangular grid ###

      if (is.null(xlim)) xlim <- range(point_data$x, na.rm = TRUE)
      if (is.null(ylim)) ylim <- range(point_data$y, na.rm = TRUE)
      x_seq <- seq(xlim[1], xlim[2], length.out = n[1])
      y_seq <- seq(ylim[1], ylim[2], length.out = n[2])

      data <- expand_grid(x = x_seq, y = y_seq)
      data$group <- 1

      data <- data %>%
            ungroup() %>%
            mutate(quad_id = 1:nrow(.))

      dy <- data %>%
            group_by(x) %>%
            mutate(y = lag(y)) %>%
            ungroup()

      dx <- data %>%
            group_by(y) %>%
            mutate(x = lag(x)) %>%
            ungroup()

      dxy <- data.frame(x = dx$x,
                        y = dy$y) %>%
            left_join(data, by = join_by(x, y)) %>%
            mutate(quad_id = dx$quad_id)

      d <- bind_rows(data, dx, dxy, dy) %>%
            na.omit() %>%
            group_by(quad_id) %>%
            filter(n() == 4) %>%
            arrange(x, y) %>%
            mutate(vertex_order = c(1, 2, 4, 3)) %>%
            ungroup() %>%
            arrange(quad_id, vertex_order) %>%
            mutate(group = paste0("surface__quad", quad_id, "::", group)) %>%
            as.data.frame()


      if(domain == "bbox") return(d)


      ### clip to convex hull, if applicable ###

      hull_ind <- grDevices::chull(point_data[, c("x", "y")])
      hull <- as.matrix(point_data[hull_ind, c("x", "y")])
      d <- d %>%
            group_by(group) %>%
            reframe(poly = sutherland_hodgman_clip(cbind(x, y), hull)) %>%
            ungroup()

      xy <- d[[2]]
      colnames(xy) <- c("x", "y")
      d <- d %>% select(group) %>% bind_cols(xy, .) %>%
            group_by(group) %>%
            mutate(vertex_order = 1:n()) %>%
            ungroup()

      return(d)
}

#' Fit and predict with 3D smoothing models
#'
#' @param data Data frame with x, y, z columns
#' @param new_data Data frame with x, y columns for prediction
#' @param method Smoothing method ("loess", "lm", "glm", "gam")
#' @param formula Model formula (NULL for default)
#' @param method.args Additional arguments for fitting function
#' @param se Logical, whether to compute standard errors
#' @param level Confidence level (not used here, passed to create_confidence_surfaces)
#' @return List with $fitted and optionally $se vectors
#' @keywords internal
fit_and_predict <- function(data, new_data, method, formula, method.args,
                            se = FALSE, level = 0.95){

      model <- switch(method,
                      lm = lm_model(),
                      glm = glm_model(),
                      loess = loess_model(),
                      gam = gam_model(),
                      stop("invalid model"))

      # Formula precedence: user > method default > global default
      if (is.null(formula)) {
            if("default_formula" %in% names(model)) {
                  formula <- model$default_formula
            } else {
                  formula <- z ~ x + y
            }
      }

      # Default arguments for fitting
      default_args <- list(data = data, formula = formula)

      # Merge with user arguments (user args take precedence)
      args <- modifyList(default_args, method.args)

      # Fit model
      tryCatch({
            fit <- do.call(model$fit_function, args)
      }, error = function(e) {
            stop("Error fitting model: ", e$message)
      })

      # Predict with or without standard errors
      tryCatch({
            predictions <- model$predict_function(fit, new_data)
      }, error = function(e) {
            stop("Error predicting fitted values: ", e$message)
      })

      # Validate predictions
      if (!is.data.frame(predictions) || is.null(predictions$fitted)) {
            stop("Model prediction returned invalid structure")
      }

      if (length(predictions$fitted) != nrow(new_data)) {
            stop("Model prediction returned wrong number of values")
      }

      if (!is.numeric(predictions$fitted)) {
            stop("Model prediction must return numeric values")
      }

      predictions
}

lm_model <- function(){
      list(
            fit_function = lm,
            predict_function = function(fit, data){
                  pred <- predict(fit, data, se.fit = TRUE)
                  data.frame(fitted = pred$fit,
                             se = pred$se.fit)
            }
      )
}

glm_model <- function(){
      list(
            fit_function = glm,
            predict_function = function(fit, data){
                  pred <- predict(fit, data, type = "response", se.fit = TRUE)
                  data.frame(fitted = pred$fit,
                             se = pred$se.fit)
            }
      )
}

loess_model <- function(){
      list(
            fit_function = loess,
            predict_function = function(fit, data){
                  pred <- predict(fit, data, se = TRUE)
                  data.frame(fitted = as.vector(pred$fit),
                             se = as.vector(pred$se.fit))
            }
      )
}

gam_model <- function(){
      list(
            fit_function = function(...) {
                  if (!requireNamespace("mgcv", quietly = TRUE)) {
                        stop("GAM method requires the 'mgcv' package. Install with: install.packages('mgcv')")
                  }
                  mgcv::gam(...)
            },
            predict_function = function(fit, data){
                  pred <- predict(fit, data, se.fit = TRUE)
                  data.frame(fitted = pred$fit,
                             se = pred$se.fit)
            },
            default_formula = z ~ s(x) + s(y)
      )
}


#' 3D smooth surface from scattered data
#'
#' Creates 3D surfaces by fitting smooth models to scattered (x,y,z) data points.
#' The fitted model is evaluated on a regular grid and rendered as a 3D surface
#' with optional standard error surfaces and lighting effects.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. This stat
#'   requires `x`, `y`, and `z` aesthetics from the input data. By default, fill is
#'   mapped to `after_stat(fitted)`.
#' @param data The data to be displayed in this layer. Must contain x, y, z columns.
#' @param geom The geometric object to use display the data. Defaults to
#'   [GeomPolygon3D] for proper 3D depth sorting.
#' @param position Position adjustment, defaults to "identity".
#' @param na.rm If `TRUE`, removes missing values before fitting the model.
#'   If `FALSE`, missing values will cause an error. Default is `FALSE`.
#' @param show.legend Logical indicating whether this layer should be included in legends.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param method Smoothing method to use. Currently supported:
#'   \itemize{
#'     \item \code{"loess"} (default): Local polynomial regression
#'     \item \code{"lm"}: Linear model
#'     \item \code{"glm"}: Generalized linear model
#'     \item \code{"gam"}: Generalized additive model (requires \code{mgcv} package)
#'   }
#' @param formula Model formula. If `NULL` (default), uses method-appropriate defaults:
#'   `z ~ x + y` for lm and glm, `z ~ s(x) + s(y)` for gam, auto for loess.
#' @param method.args List of additional arguments passed to the fitting function.
#'   For loess, this might include `span` or `degree`. For lm, this might include `weights`.
#'   For glm, this might include `family` (defaults to `gaussian()`). For gam, this might
#'   include smoothing parameters or basis specifications.
#' @param domain Character indicating the x-y domain over which to visualize the surface.
#'   The default, `"bbox"`, shows predictions over the full rectangular bounding box of
#'   the predictors.The alternative, `"chull"`, shows predictions only within the convex
#'   hull of the input data, which prevents extrapolation into unoccupied corners of predictor space.
#' @param xlim,ylim Numeric vectors of length 2 giving the range for prediction grid.
#'   If `NULL` (default), uses the exact data range with no extrapolation, following
#'   [geom_smooth()] conventions.
#' @param n Either a single integer specifying grid resolution in both dimensions,
#'   or a vector of length 2 specifying `c(nx, ny)` for different resolutions.
#'   Default is 30. Higher values create smoother surfaces but slower rendering.
#' @param se Logical indicating whether to display confidence interval bands around
#'   the smooth; if `TRUE`, these are rendered as additional surfaces; they inherit
#'   aesthetics from the primary smooth layer unless otherwise specified.
#'   Defaults to `FALSE`.
#' @param level Level of confidence interval to use (0.95 by default).
#' @param se.fill Fill colour for confidence interval bands. If `NULL`, inherits from
#'   the main surface `fill` aesthetic.
#' @param se.colour,se.color Colour for confidence interval band borders. If `NULL`,
#'   inherits from the main surface `colour` aesthetic.
#' @param se.alpha Alpha transparency for confidence interval bands. Defaults to 0.5.
#' @param se.linewidth Line width for confidence interval band borders. If `NULL`,
#'   inherits from the main surface `linewidth` aesthetic.
#' @param light A lighting specification object created by [light()], or NULL to disable shading.
#' @param ... Other arguments passed on to the geom (typically `geom_smooth_3d()`), such as
#'   `sort_method` and `scale_depth` as well as aesthetics like `colour`, `fill`, `linewidth`, etc.
#'
#' @section Aesthetics:
#' `stat_smooth_3d()` requires the following aesthetics from input data:
#' - **x**: X coordinate
#' - **y**: Y coordinate
#' - **z**: Z coordinate (response variable to be smoothed)
#'
#' @section Computed variables:
#' - `x`, `y`, `z`: Grid coordinates and smoothed predictions
#' - `fitted`: Smoothed predictions (same as `z` when `level == "fitted"`)
#' - `se`: Standard errors of the fitted values (available when `se = TRUE`)
#' - `level`: Type of surface ("fitted", "upper CI", or "lower CI" for confidence bands)
#' - `light`: Computed lighting value (numeric for most methods, hex color for `normal_rgb`)
#' - `normal_x`, `normal_y`, `normal_z`: Surface normal components
#' - `slope`: Gradient magnitude from surface calculations
#' - `aspect`: Direction of steepest slope from surface calculations
#' - `dzdx`, `dzdy`: Partial derivatives from surface calculations
#'
#' @examples
#' library(ggplot2)
#'
#' # Generate scattered 3D data
#' set.seed(123)
#' d <- data.frame(
#'   x = runif(100, -1, 3),
#'   y = runif(100, -3, 3)
#' )
#' d$z <- abs(1 + d$x^2 - d$y^2 + rnorm(100, 0, 1))
#'
#' # Base plot
#' p <- ggplot(d, aes(x, y, z)) + coord_3d()
#'
#' # Basic smooth surface with default loess model
#' p + stat_smooth_3d()
#'
#' # Linear model surface with 90% confidence intervals
#' p + stat_smooth_3d(method = "lm", color = "black", se = TRUE,
#'                    level = 0.9, se.alpha = .8)
#'
#' # Linear model surface with custom model formula
#' p + stat_smooth_3d(method = "lm",
#'        formula = z ~ poly(x, 2) + poly(y, 2) + x:y)
#'
#' # Loess with custom span parameter, and lighting aesthetics
#' p + stat_smooth_3d(
#'       method = "loess", method.args = list(span = 0.3),
#'       fill = "steelblue", color = "white",
#'       light = light(direction = c(-1, 0, 0)))
#'
#' # GLM with gamma family and log link
#' p + stat_smooth_3d(
#'       method = "glm",
#'       method.args = list(family = Gamma(link = "log")),
#'       formula = z ~ poly(x, 2) + poly(y, 2)) +
#'   scale_fill_viridis_c()
#'
#' # GAM with default smoothers, with fill colored by confidence interval
#' p + stat_smooth_3d(aes(fill = after_stat(level)),
#'                    method = "gam", se = TRUE, color = "black") +
#'   scale_fill_manual(values = c("red", "darkorchid4", "steelblue"))
#'
#' # Visualize uncertainty with computed "standard error" variable
#' p + stat_smooth_3d(aes(fill = after_stat(se * 2))) +
#'   scale_fill_viridis_c()
#'
#' # Extend surface beyond training data range (explicit extrapolation)
#' p + stat_smooth_3d(method = "lm", xlim = c(-5, 5), ylim = c(-5, 5))

#' # Clip surface to predictor convex hull
#' # to prevent extrapolation into corner areas
#' p + stat_smooth_3d(method = "lm", domain = "chull")
#'
#' @seealso [stat_surface_3d()] for surfaces from existing grid data,
#'   [stat_function_3d()] for mathematical function surfaces,
#'   [light()] for lighting specifications, [coord_3d()] for 3D coordinate systems.
#' @export
stat_smooth_3d <- function(mapping = NULL, data = NULL,
                           geom = GeomSmooth3D,
                           position = "identity",
                           method = "loess",
                           formula = NULL,
                           method.args = list(),
                           xlim = NULL,
                           ylim = NULL,
                           domain = c("bbox", "chull"),
                           n = 30,
                           se = FALSE,
                           level = 0.95,
                           se.fill = NULL,
                           se.colour = NULL,
                           se.color = NULL,
                           se.alpha = 0.5,
                           se.linewidth = NULL,
                           light = NULL,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {

      # Handle both American and British spellings
      if(is.null(se.colour)) se.colour <- se.color

      domain <- match.arg(domain)

      layer(
            stat = StatSmooth3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(method = method, formula = formula, method.args = method.args,
                          xlim = xlim, ylim = ylim, domain = domain, n = n, se = se, level = level,
                          se.fill = se.fill, se.colour = se.colour, se.alpha = se.alpha,
                          se.linewidth = se.linewidth, light = light, na.rm = na.rm, ...)
      )
}



