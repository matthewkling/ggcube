# Fit and predict with 3D smoothing models

Fit and predict with 3D smoothing models

## Usage

``` r
fit_and_predict(
  data,
  new_data,
  method,
  formula,
  method.args,
  se = FALSE,
  level = 0.95
)
```

## Arguments

- data:

  Data frame with x, y, z columns

- new_data:

  Data frame with x, y columns for prediction

- method:

  Smoothing method ("loess", "lm", "glm", "gam")

- formula:

  Model formula (NULL for default)

- method.args:

  Additional arguments for fitting function

- se:

  Logical, whether to compute standard errors

- level:

  Confidence level (not used here, passed to create_confidence_surfaces)

## Value

List with \$fitted and optionally \$se vectors
