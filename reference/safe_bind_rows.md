# Bind rows with automatic type coercion for aesthetic columns

Coerces shared colour/fill columns to character before calling
`bind_rows`, preventing type conflicts when one data frame has logical
`NA` (from unmapped aesthetics) and the other has character values.

## Usage

``` r
safe_bind_rows(...)
```

## Arguments

- ...:

  Data frames to bind.

## Value

Combined data frame.
