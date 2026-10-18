# Specify a direction using angles

Converts a spherical direction given as azimuth and elevation into the
length-3 direction vector used by
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
and
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md).
Both of those arguments take a vector of the form `c(x, y, z)` whose
magnitude is ignored; `direction()` is an alternative way to express
such a vector for people who find angles easier to reason about than
components.

## Usage

``` r
direction(azimuth, elevation)
```

## Arguments

- azimuth:

  Numeric bearing in degrees, measured clockwise from the positive y
  axis. Values outside `[0, 360)` are wrapped.

- elevation:

  Numeric angle in degrees above the xy plane, between -90 and 90.

## Value

A unit-length numeric vector of length 3.

## Details

Azimuth is a compass-style bearing measured clockwise from the positive
y axis, matching the convention used by terrain shading functions such
as `terra::shade()`. Elevation is measured upward from the xy plane, so
`elevation = 90` points along the positive z axis and `elevation = -90`
points along the negative z axis.

The returned vector is scaled to unit length, but since both
[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
and
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md)
normalize their inputs, the magnitude carries no meaning.

## See also

[`coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.md)
and
[`light()`](https://matthewkling.github.io/ggcube/reference/light.md),
which accept the result.

## Examples

``` r
# The default viewing angle, expressed as a direction
direction(210, 30)
#> [1] -0.4330127 -0.7500000  0.5000000

# The default lighting angle
direction(270, 63.43)
#> [1] -4.472908e-01 -8.216600e-17  8.943886e-01

# Light from directly overhead
light(direction = direction(0, 90))
#> Lighting specification:
#>   Method: diffuse 
#>   Direction: [ 0, 6.12323399573677e-17, 1 ] (directional lighting)
#>     azimuth: 180 deg, elevation: 90 deg
#>   anchor: scene 
#>   Shading: fill + color (contrast = 1 , mode = hsv )
```
