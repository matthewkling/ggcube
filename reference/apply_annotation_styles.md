# Apply annotation styles from dot-prefixed columns

After coord transformation, copies `.ann_*` style columns to the
standard aesthetic columns for annotation rows only (identified by
`.ann` marker). This allows annotation styling to survive scale training
without contaminating colour/fill/etc. scales.

## Usage

``` r
apply_annotation_styles(data)
```

## Arguments

- data:

  Transformed data frame potentially containing annotation rows.

## Value

Data frame with annotation styles applied.
