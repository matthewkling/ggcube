# Compute least-squares plane betas for all polygon groups

Returns a list parallel to grp_data where polygon entries contain a
length-3 numeric vector (intercept, x_coef, y_coef) and non-polygon
entries are NULL.

## Usage

``` r
compute_poly_betas(grp_data, prim_lookup)
```

## Arguments

- grp_data:

  List of per-group matrices (x, y, z, bbox...).

- prim_lookup:

  Named character vector of primitive types per group.

## Value

List of beta vectors (or NULL for non-polygons / degenerate cases).
