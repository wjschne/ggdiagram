# Convert hjust and vjust parameters from polar coordinates

This function is how
[`ob_label`](https://wjschne.github.io/ggdiagram/reference/ob_label.md)'s
`vjust` and `hjust` values are recalculated automatically when the
`polar_just` parameter is specified.

## Usage

``` r
polar2just(x, multiplier = NULL, axis = c("h", "v"))
```

## Arguments

- x:

  angle. Can be a named direction (e.g., "north"), number (in degrees),
  [`degree`](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [`radian`](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  or [`turn`](https://wjschne.github.io/ggdiagram/reference/ob_angle.md)

- multiplier:

  distance

- axis:

  vertical (v) or horizontal (h)

## Value

ob_angle object

## Examples

``` r
a <- "northwest"
polar2just(a, axis = "h")
#> [1] 0.9242641
polar2just(a, axis = "v")
#> [1] 0.07573593
```
