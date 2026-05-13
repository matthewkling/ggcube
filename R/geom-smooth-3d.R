# ggprotos ---------------

StatSmooth3D <- ggproto("StatSmooth3D", Stat,
                        required_aes = c("x", "y", "z"),
                        default_aes = aes(x = after_stat(x),
                                          y = after_stat(y),
                                          z = after_stat(z),
                                          fill = after_stat(fitted)),

                        extra_params = c("na.rm", "points", "residuals"),

                        compute_group = function(data, scales, method = "loess", formula = NULL,
                                                 method.args = list(), cull_backfaces = FALSE,
                                                 xlim = NULL, ylim = NULL,
                                                 n = NULL, grid = NULL, direction = NULL, trim = NULL,
                                                 light = NULL, na.rm = FALSE, domain = "chull",
                                                 se = FALSE, level = 0.95, se.fill = NULL, se.colour = NULL,
                                                 se.alpha = 0.5, se.linewidth = NULL,
                                                 points = FALSE, residuals = FALSE) {

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

                              # Generate point grid and expand to tile polygons
                              if(is.null(xlim)) xlim <- range(data$x, na.rm = TRUE)
                              if(is.null(ylim)) ylim <- range(data$y, na.rm = TRUE)
                              if(is.null(trim)) trim <- TRUE
                              points_grid <- make_point_grid(grid, n, direction, xlim, ylim, trim)
                              points_grid$z <- 0 # placeholder for points_to_tiles
                              points_grid$group <- data$group[1]
                              polys <- points_to_tiles(points_grid,
                                                       method = "grid",
                                                       grid_type = grid %||% "rectangle",
                                                       group_prefix = "surface__tile")
                              polys$group <- paste0(polys$group, "::grp", data$group[1])

                              # Clip to hull if applicable (before prediction, since
                              # clipping changes vertex positions and creates new ones)
                              if(domain == "chull") polys <- clip_polys_to_chull(polys, data)

                              # Fit model and predict at polygon vertex positions
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
                                    compute_surface_vars() %>%
                                    average_aesthetics() %>%
                                    mutate(cull_backfaces = cull_backfaces) %>%
                                    attach_light(light)

                              # Optionally append data points
                              if (points) {
                                    pt_rows <- make_point_rows(
                                          data,
                                          group_prefix = paste0("smooth_data__grp", data$group[1])
                                    )

                                    # Ensure computed variables exist so after_stat() mappings
                                    # don't produce NA (e.g. fill = after_stat(fitted))
                                    if (!"fitted" %in% names(pt_rows)) pt_rows$fitted <- pt_rows$z
                                    if (!"se" %in% names(pt_rows)) pt_rows$se <- 0
                                    if (!"level" %in% names(pt_rows)) pt_rows$level <- "fitted"

                                    # Drop PANEL to prevent NA conflicts in bind_rows
                                    # (ggplot2 re-adds it after compute_group returns)
                                    pt_rows$PANEL <- NULL

                                    surfaces <- safe_bind_rows(surfaces, pt_rows)
                              }

                              # Optionally append residual lines connecting points to surface
                              if (residuals) {
                                    data_fitted <- fit_and_predict(data, data, method, formula,
                                                                   method.args, se = FALSE)

                                    seg_rows <- make_residual_segment_rows(
                                          data,
                                          fitted = data_fitted$fitted,
                                          group_prefix = paste0("smooth_resid__grp", data$group[1])
                                    )

                                    if (!"fitted" %in% names(seg_rows)) seg_rows$fitted <- seg_rows$z
                                    if (!"se" %in% names(seg_rows)) seg_rows$se <- 0
                                    if (!"level" %in% names(seg_rows)) seg_rows$level <- "fitted"

                                    surfaces <- safe_bind_rows(surfaces, seg_rows)
                              }

                              return(surfaces)

                        }
)


# helpers ---------------

clip_polys_to_chull <- function(d, point_data){

      hull_ind <- grDevices::chull(point_data[, c("x", "y")])
      hull <- as.matrix(point_data[hull_ind, c("x", "y")])

      d <- d %>%
            group_by(group) %>%
            reframe(poly = bind_cols(polyclip::polyclip(A = list(x = x, y = y),
                                                        B = list(x = hull[,1], y = hull[,2])))) %>%
            mutate(.vertex_order = 1:n()) %>%
            ungroup()

      xy <- d[[2]]
      colnames(xy) <- c("x", "y")
      d <- d %>% select(group) %>% bind_cols(xy, .)
      d <- d %>%
            group_by(group) %>%
            mutate(.vertex_order = 1:n()) %>%
            ungroup()
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
#' @noRd
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

# exported layer functions ------------------------------

#' 3D surface from smoothed conditional means
#'
#' A 3D version of `ggplot2::geom_smooth()`.
#' Creates surfaces by fitting smoothing models to scattered (x,y,z) data points.
#' The fitted statistical model is evaluated on a regular grid and rendered as a
#' 3D surface with optional standard error surfaces.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. This stat
#'   requires `x`, `y`, and `z` aesthetics from the input data. By default, fill is
#'   mapped to `after_stat(fitted)`.
#' @param data The data to be displayed in this layer. Must contain x, y, z columns.
#' @param stat The statistical transformation to use on the data. Defaults to `StatSmooth3D`.
#' @param geom The geometric object used to display the data. Defaults to `GeomPolygon3D`.
#'
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
#'   If `NULL` (default), uses the exact data range with no extrapolation.
#' @param se Logical indicating whether to display confidence interval bands around
#'   the smooth; if `TRUE`, these are rendered as additional surfaces; they inherit
#'   aesthetics from the primary smooth layer unless otherwise specified.
#'   Defaults to `FALSE`.
#' @param level Level of confidence interval to use (0.95 by default).
#' @param se_fill Fill colour for confidence interval bands. If `NULL`, inherits from
#'   the main surface `fill` aesthetic.
#' @param se_colour,se_color Color for confidence interval band borders. If `NULL`,
#'   inherits from the main surface `color` aesthetic.
#' @param se_alpha Alpha transparency for confidence interval bands. Defaults to 0.5.
#' @param se_linewidth Line width for confidence interval band borders. If `NULL`,
#'   inherits from the main surface `linewidth` aesthetic.
#' @param points Logical indicating whether to overlay the original data points
#'   on the fitted surface. Points are depth-sorted together with the surface
#'   polygons for proper 3D rendering. Point styling is controlled via the
#'   `point_*` parameters; mapped aesthetics from the layer are not inherited
#'   by annotation points. Default is `FALSE`.
#' @param point_colour,point_color Color for data points. Defaults to `"black"`.
#' @param point_fill Fill color for data points (only relevant for shapes 21-25).
#'   Defaults to `NA` (transparent).
#' @param point_size Size of data points.
#' @param point_shape Shape of data points.
#' @param point_alpha Alpha transparency for data points.
#' @param point_stroke Stroke width for data points.
#' @param residuals Logical indicating whether to draw residual lines connecting
#'   data points to the fitted surface. Default is `FALSE`. Residual styling is
#'   controlled via the `residual_*` parameters. Note: residual lines may render
#'   incorrectly when combined with `se = TRUE`, as the lines intersect the
#'   confidence interval surfaces and cannot be split at intersection points.
#' @param residual_colour,residual_color Color for residual lines.
#' @param residual_linewidth Line width for residual lines.
#' @param residual_linetype Line type for residual lines.
#' @param residual_alpha Alpha transparency for residual lines.
#'
#' @inheritParams grid_params
#' @inheritParams polygon_params
#' @inheritParams light_param
#' @inheritParams position_param
#'
#' @section Aesthetics:
#' `stat_smooth_3d()` requires the following aesthetics from input data:
#' - **x**: X coordinate
#' - **y**: Y coordinate
#' - **z**: Z coordinate (response variable to be smoothed)
#'
#' @inheritSection surface_computed_vars Computed variables
#'
#' @section Computed variables specific to StatSmooth3D:
#' - `level`: Type of surface ("fitted", "upper CI", or "lower CI" for confidence bands)
#' - `fitted`: Smoothed predictions (same as `z` when `level == "fitted"`)
#' - `se`: Standard errors of the fitted values (available when `se = TRUE`)
#'
#' @examples
#' # Generate scattered 3D data
#' set.seed(123)
#' d <- data.frame(
#'   x = runif(100, -1, 3),
#'   y = runif(100, -3, 3)
#' )
#' d$z <- abs(1 + d$x^2 - d$y^2 + rnorm(100, 0, 1))
#'
#' # Base plot
#' p <- ggplot(d, aes(x, y, z)) +
#'   coord_3d(light = NULL) +
#'   scale_fill_viridis_c()
#'
#' # Basic smooth surface with default loess model
#' p + geom_smooth_3d()
#'
#' # Show data points
#' p + geom_smooth_3d(points = TRUE)
#'
#' # Show data points with residual lines
#' p + geom_smooth_3d(points = TRUE, residuals = TRUE,
#'       point_color = "red", alpha = .8)
#'
#' # Linear model surface with 90% confidence intervals
#' p + geom_smooth_3d(aes(fill = after_stat(level)),
#'       method = "lm", color = "black", se = TRUE,
#'       level = 0.99, se_alpha = .7, n = 10) +
#'       scale_fill_manual(values = c("red", "darkorchid4", "steelblue"))
#'
#' # Linear model surface with custom model formula
#' p + geom_smooth_3d(method = "lm", n = 10,
#'       formula = z ~ poly(x, 2) + poly(y, 2) + x:y)
#'
#' # Loess with custom span parameter, and lighting effects
#' p + geom_smooth_3d(
#'       method = "loess", method.args = list(span = 0.3),
#'       fill = "steelblue", color = "white", n = 20,
#'       light = light(direction = c(0, -1, 0), color = FALSE))
#'
#' # GLM with gamma family and log link
#' p + geom_smooth_3d(
#'       method = "glm", n = 10,
#'       method.args = list(family = Gamma(link = "log")),
#'       formula = z ~ poly(x, 2) + poly(y, 2))
#'
#' # Visualize uncertainty with computed "standard error" variable
#' p + geom_smooth_3d(aes(fill = after_stat(se * 2))) +
#'   scale_fill_viridis_c()
#'
#' # Extend surface beyond training data range (explicit extrapolation)
#' p + geom_smooth_3d(method = "lm", xlim = c(-5, 5), ylim = c(-5, 5))
#'
#' # Clip surface to predictor convex hull
#' # to prevent extrapolation into corner areas
#' p + geom_smooth_3d(method = "lm", domain = "chull")
#'
#' # Specify alternative grid geometry
#' p + geom_smooth_3d(grid = "right1", n = 30, direction = "y")
#'
#' # Separate fits for data subgroups
#' ggplot(mtcars, aes(wt, mpg, qsec, fill = factor(cyl))) +
#'   geom_smooth_3d(method = "lm", alpha = .7,
#'     xlim = c(0, 5), ylim = c(0, 40)) + # specify shared domain
#'   coord_3d() + theme_light()
#'
#' @seealso [stat_surface_3d()] for surfaces from existing grid data,
#'   [stat_function_3d()] for mathematical function surfaces.
#' @return A `Layer` object that can be added to a ggplot.
#' @rdname geom_smooth_3d
#' @export
geom_smooth_3d <- function(mapping = NULL, data = NULL, stat = StatSmooth3D,
                           position = "identity",
                           ...,
                           method = "loess",
                           formula = NULL,
                           method.args = list(),
                           xlim = NULL,
                           ylim = NULL,
                           n = NULL, grid = NULL, direction = NULL, trim = NULL,
                           domain = c("bbox", "chull"),
                           se = FALSE,
                           level = 0.95,
                           se_fill = NULL,
                           se_colour = NULL,
                           se_color = NULL,
                           se_alpha = 0.5,
                           se_linewidth = NULL,
                           points = FALSE,
                           point_colour = "black", point_color = NULL,
                           point_fill = NA,
                           point_size = 1.5,
                           point_shape = 19,
                           point_alpha = 1,
                           point_stroke = 0.5,
                           residuals = FALSE,
                           residual_colour = "black", residual_color = NULL,
                           residual_linewidth = 0.5,
                           residual_linetype = 1,
                           residual_alpha = 1,
                           light = NULL,
                           cull_backfaces = FALSE, sort_method = NULL,
                           force_convex = TRUE, scale_depth = TRUE,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

      # Resolve colour spellings (American -> British)
      se_colour <- se_colour %||% se_color
      if (!is.null(point_color)) point_colour <- point_color
      if (!is.null(residual_color)) residual_colour <- residual_color

      domain <- match.arg(domain)

      # Convert underscore param names to periods for layer internals
      # (avoids ggplot2 aesthetic name standardization on stat params)
      layer(
            geom = GeomPolygon3D, mapping = mapping, data = data, stat = get_proto(stat, "stat"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(method = method, formula = formula, method.args = method.args,
                          xlim = xlim, ylim = ylim, domain = domain, n = n, se = se, level = level,
                          grid = grid, direction = direction, trim = trim,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          se.fill = se_fill, se.colour = se_colour, se.alpha = se_alpha,
                          se.linewidth = se_linewidth,
                          points = points,
                          point.colour = point_colour,
                          point.fill = point_fill, point.size = point_size,
                          point.shape = point_shape, point.alpha = point_alpha,
                          point.stroke = point_stroke,
                          residuals = residuals,
                          residual.colour = residual_colour,
                          residual.linewidth = residual_linewidth,
                          residual.linetype = residual_linetype,
                          residual.alpha = residual_alpha,
                          light = light, na.rm = na.rm, ...)
      )
}

#' @rdname geom_smooth_3d
#' @export
stat_smooth_3d <- function(mapping = NULL, data = NULL,
                           geom = GeomPolygon3D,
                           position = "identity",
                           ...,
                           method = "loess",
                           formula = NULL,
                           method.args = list(),
                           xlim = NULL,
                           ylim = NULL,
                           n = NULL, grid = NULL, direction = NULL, trim = NULL,
                           domain = c("bbox", "chull"),
                           se = FALSE,
                           level = 0.95,
                           se_fill = NULL,
                           se_colour = NULL,
                           se_color = NULL,
                           se_alpha = 0.5,
                           se_linewidth = NULL,
                           points = FALSE,
                           point_colour = "black", point_color = NULL,
                           point_fill = NA,
                           point_size = 1.5,
                           point_shape = 19,
                           point_alpha = 1,
                           point_stroke = 0.5,
                           residuals = FALSE,
                           residual_colour = "black", residual_color = NULL,
                           residual_linewidth = 0.5,
                           residual_linetype = 1,
                           residual_alpha = 1,
                           light = NULL,
                           cull_backfaces = FALSE, sort_method = NULL,
                           force_convex = TRUE, scale_depth = TRUE,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

      # Resolve colour spellings (American -> British)
      se_colour <- se_colour %||% se_color
      if (!is.null(point_color)) point_colour <- point_color
      if (!is.null(residual_color)) residual_colour <- residual_color

      domain <- match.arg(domain)

      # Convert underscore param names to periods for layer internals
      layer(
            stat = StatSmooth3D, data = data, mapping = mapping, geom = get_proto(geom, "geom"),
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(method = method, formula = formula, method.args = method.args,
                          xlim = xlim, ylim = ylim, domain = domain, n = n, se = se, level = level,
                          grid = grid, direction = direction, trim = trim,
                          force_convex = force_convex, cull_backfaces = cull_backfaces,
                          sort_method = sort_method, scale_depth = scale_depth,
                          se.fill = se_fill, se.colour = se_colour, se.alpha = se_alpha,
                          se.linewidth = se_linewidth,
                          points = points,
                          point.colour = point_colour,
                          point.fill = point_fill, point.size = point_size,
                          point.shape = point_shape, point.alpha = point_alpha,
                          point.stroke = point_stroke,
                          residuals = residuals,
                          residual.colour = residual_colour,
                          residual.linewidth = residual_linewidth,
                          residual.linetype = residual_linetype,
                          residual.alpha = residual_alpha,
                          light = light, na.rm = na.rm, ...)
      )
}
