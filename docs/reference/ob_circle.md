# ob_circle class

ob_circle class

## Usage

``` r
ob_circle(
  center = ob_point(0, 0),
  radius = 1,
  label = character(0),
  alpha = numeric(0),
  color = character(0),
  fill = character(0),
  linewidth = numeric(0),
  linetype = numeric(0),
  n = numeric(0),
  style = S7::class_missing,
  x = numeric(0),
  y = numeric(0),
  id = character(0),
  ...
)
```

## Arguments

- center:

  Point at center of the circle

- radius:

  Distance between center and edge circle

- label:

  A character, angle, or label object

- alpha:

  numeric value for alpha transparency

- color:

  character string for color

- fill:

  character string for fill color

- linewidth:

  Width of lines

- linetype:

  type of lines

- n:

  Number of points in circle (default = 360)

- style:

  An ob_style object

- x:

  x-coordinate of center point. If specified, overrides x-coordinate of
  `@center`.

- y:

  x-coordinate of center point. If specified, overrides y-coordinate of
  `@center`.

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style object

## Value

ob_circle object

## Additional properties

- `@aesthetics`:

  A list of information about the circle's aesthetic properties

- `@angle_at`:

  A function that finds the angle of the specified point in relation to
  the circle's center

- `@arc`:

  A function that creates an arc object with the same center as the
  circle object. Must specify the `start` angle and the `end` angle. In
  addition, any of the parameters of `ob_arc` can be applied.

- `@area`:

  area of the circle

- `@bounding_box`:

  A rectangle that contains all the circles

- `@circumference`:

  Circumference of the circle

- `@diameter`:

  The diameter of the circle

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggforce::geom_circle`](https://ggforce.data-imaginist.com/reference/geom_circle.html).

- `@length`:

  The number of circles in the circle object

- `@normal_at`:

  A function that finds a point that is perpendicular from the circle
  and at a specified distance

- `@point_at`:

  A function that finds a point on the circle at the specified angle

- `@polar_line_at`:

  A function that creates an `ob_line` that passes through the circle's
  center and the point specified in `x`

- `@polygon`:

  A tibble containing information to create all the polygon points in a
  circle

- `@tangent_at`:

  A function that finds the tangent line at the specified angle

- `@tibble`:

  Gets a tibble (data.frame) containing parameters and styles used by
  [`ggforce::geom_circle`](https://ggforce.data-imaginist.com/reference/geom_circle.html)

- `@east`:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at rightmost point of circle

- `@north`:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at highest point of circle

- `@west`:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at leftmost point of circle

- `@south`:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at lowest point of circle

- `@northeast`:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at top-right point of circle

- `@northwest`:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at top-left point of circle

- `@southwest`:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at bottom-left point of circle

- `@southeast`:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at bottom-right point of circle

## Examples

``` r
# specify center point and radius
ob_circle(center = ob_point(0,0), radius = 6)
#> 
#> ── <ob_circle> 
#> # A tibble: 1 × 3
#>       x     y     r
#>   <dbl> <dbl> <dbl>
#> 1     0     0     6
```
