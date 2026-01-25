# Validate stat compatibility with position_on_face

This function walks parent frames to find the layer's stat and checks if
it's compatible with position_on_face. Some stats have scale training
conflicts that cause incorrect rendering when used with
position_on_face.

## Usage

``` r
validate_position_stat()
```

## Value

NULL if compatible, otherwise issues a warning
