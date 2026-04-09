# Resolve aesthetic overrides for sub-elements

For each aesthetic, applies the first available value from: (1) explicit
override parameter, (2) existing value in data, (3) default.

## Usage

``` r
resolve_aesthetic_overrides(data, overrides = list(), defaults = list())
```

## Arguments

- data:

  Data frame with current aesthetic columns.

- overrides:

  Named list of override values (NULL entries are skipped).

- defaults:

  Named list of default values for aesthetics not in data.

## Value

Data frame with resolved aesthetic values.
