StatSmooth3D <- ggproto("StatSmooth3D", Stat,
                        required_aes = c("x", "y", "z"),
                        default_aes = aes(x = after_stat(x),
                                          y = after_stat(y),
                                          z = after_stat(z),
                                          fill = after_stat(fitted)),

                        compute_panel = function(data, scales, method = "loess", formula = NULL,
                                                 method.args = list(), xlim = NULL, ylim = NULL,
                                                 n = 30, light = lighting(), na.rm = FALSE,
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

                              # Get x and y ranges from data if not specified
                              if (is.null(xlim)) {
                                    xlim <- range(data$x, na.rm = TRUE)
                              }

                              if (is.null(ylim)) {
                                    ylim <- range(data$y, na.rm = TRUE)
                              }

                              # Validate ranges
                              if (length(xlim) != 2 || !is.numeric(xlim) || xlim[1] >= xlim[2]) {
                                    stop("xlim must be a numeric vector of length 2 with xlim[1] < xlim[2]")
                              }

                              if (length(ylim) != 2 || !is.numeric(ylim) || ylim[1] >= ylim[2]) {
                                    stop("ylim must be a numeric vector of length 2 with ylim[1] < ylim[2]")
                              }

                              # Warn about loess extrapolation limitations
                              if (method == "loess") {
                                    data_x_range <- range(data$x, na.rm = TRUE)
                                    data_y_range <- range(data$y, na.rm = TRUE)

                                    if (xlim[1] < data_x_range[1] || xlim[2] > data_x_range[2] ||
                                        ylim[1] < data_y_range[1] || ylim[2] > data_y_range[2]) {
                                          warning("loess can't extrapolate beyond the data range. Consider method = 'lm', 'glm', or 'gam' for extrapolation.")
                                    }
                              }

                              # Handle n parameter (grid resolution)
                              if (length(n) == 1) {
                                    nx <- ny <- n
                              } else if (length(n) == 2) {
                                    nx <- n[1]
                                    ny <- n[2]
                              } else {
                                    stop("n must be a single number or a vector of length 2")
                              }

                              if (!is.numeric(c(nx, ny)) || any(c(nx, ny) < 2)) {
                                    stop("Grid resolution (n) must be at least 2 in each dimension")
                              }

                              # Generate regular grid for prediction
                              x_seq <- seq(xlim[1], xlim[2], length.out = nx)
                              y_seq <- seq(ylim[1], ylim[2], length.out = ny)
                              grid_data <- expand.grid(x = x_seq, y = y_seq)

                              # Fit and predict model
                              grid_data <- fit_and_predict(data, grid_data, method, formula, method.args,
                                                           se = se, level = level) %>%
                                    bind_cols(grid_data, .)


                              z_score <- qnorm(1 - (1 - level)/2)
                              if(se) {surfaces <- c("upper", "fitted", "lower")} else {surfaces <- "fitted"}
                              surfaces <- surfaces %>%
                                    lapply(function(surface, data = grid_data){
                                          data$level <- factor(surface,
                                                               levels = c("upper", "fitted", "lower"),
                                                               labels = c(paste0("upper ", level*100, "% CI"),
                                                                          "fitted", paste0("lower ", level*100, "% CI")))
                                          data$group <- surface
                                          data$z <- switch(surface,
                                                           fitted = data$fitted,
                                                           lower = data$fitted - z_score * data$se,
                                                           upper = data$fitted + z_score * data$se)
                                          surf <- process_surface_grid(data, light)

                                          # Apply confidence band styling
                                          if(surface != "fitted"){
                                                if (!is.null(se.fill)) {
                                                      surf$fill <- se.fill
                                                }
                                                if (!is.null(se.colour)) {
                                                      surf$colour <- se.colour
                                                }
                                                if (!is.null(se.linewidth)) {
                                                      surf$linewidth <- se.linewidth
                                                }
                                                surf$alpha <- se.alpha
                                          }

                                          return(surf) }) %>%
                                    bind_rows()

                              return(surfaces)
                        }
)

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
#' with proper depth sorting and optional lighting effects.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. This stat
#'   requires `x`, `y`, and `z` aesthetics from the input data.
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
#' @param se.alpha Alpha transparency for confidence interval bands. Defaults to 0.3.
#' @param se.linewidth Line width for confidence interval band borders. If `NULL`,
#'   inherits from the main surface `linewidth` aesthetic.
#' @param light A lighting specification object created by [lighting()]
#' @param ... Other arguments passed on to [layer()], such as `colour`, `fill`,
#'   `linewidth`, etc..
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
#'   x = runif(100, -2, 2),
#'   y = runif(100, -2, 2)
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
#' p + stat_smooth_3d(method = "lm", se = TRUE, level = 0.9)
#'
#' # Linear model surface with custom model formula
#' p + stat_smooth_3d(method = "lm",
#'        formula = z ~ poly(x, 2) + poly(y, 2) + x:y)
#'
#' # Loess with custom span parameter, and lighting aesthetics
#' p + stat_smooth_3d(method = "loess", method.args = list(span = 0.3),
#'                  fill = "steelblue", color = "white",
#'                  light = lighting(blend = "both"))
#'
#' # GLM with Gamma family for positive data
#' p + stat_smooth_3d(
#'       method = "glm",
#'       method.args = list(family = Gamma(link = "log")),
#'       formula = z ~ poly(x, 2) + poly(y, 2),
#'       light = lighting(blend = "both", blend_mode = "hsl", blend_strength = .8))
#'
#' # GAM with default smoothers, with fill colored by uncertainty layer
#' p + stat_smooth_3d(aes(fill = after_stat(level)),
#'                    method = "gam", se = TRUE, color = "black") +
#'   scale_fill_manual(values = c("red", "darkorchid4", "steelblue"))
#'
#' # Color surface bands by uncertainty
#' p + stat_smooth_3d(aes(fill = after_stat(se * 2)), se = T) +
#'   scale_fill_viridis_c()
#'
#' # Extend surface beyond data range (explicit extrapolation)
#' p + stat_smooth_3d(method = "lm",
#'                  xlim = c(-3, 3), ylim = c(-3, 3))
#'
#' # Project 2D views of the surface onto face panels
#' ggplot(mtcars, aes(mpg, disp, qsec)) +
#'   stat_smooth_3d(position = position_on_face(faces = c("xmax", "ymin", "zmin"))) +
#'   scale_fill_viridis_c() +
#'   coord_3d()
#'
#' @seealso [stat_surface()] for surfaces from existing grid data,
#'   [stat_function_3d()] for mathematical function surfaces,
#'   [lighting()] for lighting specifications, [coord_3d()] for 3D coordinate systems.
#' @export
stat_smooth_3d <- function(mapping = NULL, data = NULL,
                           geom = GeomPolygon3D,
                           position = "identity",
                           method = "loess",
                           formula = NULL,
                           method.args = list(),
                           xlim = NULL,
                           ylim = NULL,
                           n = 30,
                           se = FALSE,
                           level = 0.95,
                           colour = NULL,
                           color = "white",
                           se.fill = NULL,
                           se.colour = NULL,
                           se.color = NULL,
                           se.alpha = 0.5,
                           se.linewidth = NULL,
                           light = lighting(),
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {

      # Handle both American and British spellings
      if(is.null(colour)) colour <- color
      if(is.null(se.colour)) se.colour <- se.color

      layer(
            stat = StatSmooth3D, data = data, mapping = mapping, geom = geom,
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(method = method, formula = formula, method.args = method.args,
                          xlim = xlim, ylim = ylim, n = n, se = se, level = level,
                          color = colour,
                          se.fill = se.fill, se.colour = se.colour, se.alpha = se.alpha,
                          se.linewidth = se.linewidth, light = light, na.rm = na.rm, ...)
      )
}
