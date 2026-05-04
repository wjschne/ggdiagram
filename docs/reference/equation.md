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
ggdiagram() +
  l1 +
  c1 +
  ob_label(label = equation(c1),
           center = c1@center,
           size = 16) +
  ob_label(label = equation(l1),
           center = ob_segment(intersection(l1, c1))@midpoint(),
           angle = l1@angle,
           size = 16) +
 ggplot2::theme_minimal(base_size = 20)
```
