# equation

Get equation for object

## Usage

``` r
equation(
  x,
  type = c("y", "general", "parametric"),
  output = c("markdown", "latex"),
  digits = 2
)
```

## Arguments

- x:

  object

- type:

  equation type. Can be `y` (default), `general`, or `parametric`

- output:

  Can be `markdown` (default) or `latex`

- digits:

  rounding digits

## Value

string

## Examples

``` r
l1 <- ob_line(slope = 2, intercept = 4)
c1 <- ob_circle(radius = 3)
equation(c1)
#> [1] "*x*<sup>2</sup> + *y*<sup>2</sup> = 3<sup>2</sup>"
equation(l1)
#> [1] "*y* = 2*x* + 4"
```
