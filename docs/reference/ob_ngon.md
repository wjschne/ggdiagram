# The ob_ngon (regular polygon) class

An ngon is a regular polygon, meaning that each side is of equal length.
The `ob_ngon` object can be specified with a center, n (number of
sides), radius, and angle. Instead of specifying a radius, one can
specify either the `side_length` or the length of the `apothem` (i.e.,
the distance from the center to a side's midpoint.

## Usage

``` r
ob_ngon(
  center = ob_point(0, 0),
  n = 3L,
  radius = numeric(0),
  angle = 0,
  label = character(0),
  side_length = numeric(0),
  apothem = numeric(0),
  vertex_radius = numeric(0),
  alpha = numeric(0),
  color = character(0),
  fill = character(0),
  linewidth = numeric(0),
  linetype = numeric(0),
  style = S7::class_missing,
  x = numeric(0),
  y = numeric(0),
  id = character(0),
  ...
)
```

## Arguments

- center:

  point at center of the ngon

- n:

  Number of sides

- radius:

  Distance from center to a vertex

- angle:

  Angle of rotation for ngon

- label:

  A character, angle, or label object

- side_length:

  Distance of each side (can be used instead of radius to set size of
  ngon)

- apothem:

  Distance from center to a side's midpoint (can be used instead of the
  radius to set size of ngon)

- vertex_radius:

  A numeric or unit vector of length one, specifying the corner radius

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

- style:

  Gets and sets the styles associated with `ob_ngon`

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
  properties passed to style

## Value

`ob_ngon` object

## Additional properties

- `@aesthetics`:

  A list of information about the object's aesthetic properties

- `@area`:

  The area of the ngons in the `ob_ngon` object

- `@bounding_box`:

  a rectangle that contains all the ngons

- `@circumscribed`:

  Returns the circle that circumscribes the ngon.

- `@inscribed`:

  Returns the circle that inscribes the ngon.

- `@length`:

  The number of ngons in the `ob_ngon` object

- `@normal_at`:

  A function that finds a point that is perpendicular from the ngon and
  at a specified distance

- `@perimeter`:

  The length of the sum of all the side segments

- `@point_at`:

  A function that finds a point on the ngon at the specified angle.

- `@segments`:

  side segments of the regular polygon

- `@tangent_at`:

  A function that finds the tangent line at the specified angle.

- `@tibble`:

  Gets a tibble (data.frame) containing parameters and styles used by
  [`ggforce::geom_shape`](https://ggforce.data-imaginist.com/reference/geom_shape.html).

- `@vertices`:

  points on the regular polygon

- `@east`:

  right point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- `@north`:

  top point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- `@west`:

  left point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- `@south`:

  top point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- `@northeast`:

  upper right point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- `@northwest`:

  upper left point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- `@southwest`:

  lower left point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- `@southeast`:

  lower right point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

## Examples

``` r
ob_ngon(center = ob_point(x = 3:8, y = 0),
        n = 3:8,
        radius = .4)
#> 
#> ── <ob_ngon> 
#> # A tibble: 6 × 5
#>       x     y radius     n angle
#>   <int> <dbl>  <dbl> <int> <dbl>
#> 1     3     0    0.4     3     0
#> 2     4     0    0.4     4     0
#> 3     5     0    0.4     5     0
#> 4     6     0    0.4     6     0
#> 5     7     0    0.4     7     0
#> 6     8     0    0.4     8     0
```
