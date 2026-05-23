# ob_rectangle class

ob_rectangle class

## Usage

``` r
ob_rectangle(
  center = S7::class_missing,
  width = numeric(0),
  height = numeric(0),
  east = S7::class_missing,
  north = S7::class_missing,
  west = S7::class_missing,
  south = S7::class_missing,
  northeast = S7::class_missing,
  northwest = S7::class_missing,
  southwest = S7::class_missing,
  southeast = S7::class_missing,
  angle = numeric(0),
  vertex_radius = numeric(0),
  label = character(0),
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

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at center of the rectangle

- width:

  width

- height:

  height

- east:

  right middle point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- north:

  top middle point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- west:

  left middle point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- south:

  top middle point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- northeast:

  upper right point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- northwest:

  upper left point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- southwest:

  lower left point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- southeast:

  lower right point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- angle:

  Rectangle rotation. *Settable.*

- vertex_radius:

  A numeric or unit vector of length one, specifying the corner radius
  for rounded corners

- label:

  A character, angle, or
  [`ob_label`](https://wjschne.github.io/ggdiagram/reference/ob_label.md)
  object

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

  a style object

- x:

  overrides x-coordinate in `center@x`

- y:

  overrides y-coordinate in `center@x`

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to `style`

## Value

`ob_rectangle` object

## Additional properties

- `@xy`:

  returns a matrix of xy coordinates of `center` points

- `@aesthetics`:

  A list of information about the object's aesthetic properties

- `@area`:

  returns rectangle area

- `@bounding_box`:

  returns the `ob_rectangle` that contains all the rectangles in the
  object

- `@perimeter`:

  returns the rectangle perimeter

- `@side`:

  returns the east, north, west, and south `ob_segment` of the
  rectangles

- `@length`:

  returns the number of rectangles in the object

- `@geom`:

  a function that returns a
  [`ggforce::geom_shape`](https://ggforce.data-imaginist.com/reference/geom_shape.html)
  object

- `@normal_at`:

  A function that finds a point perpendicular to the rectangle at angle
  `theta` at the specified distance.

- `@point_at`:

  A function that finds a point on the rectangle at an angle `theta`

- `@tangent_at`:

  A function that finds a tangent line on the rectangle. Uses point_at
  to find the tangent point at angle theta and then returns the tangent
  line at that point. If a point is supplied instead of an angle, the
  point is projected onto the ellipse and then the tangent line is found
  from there.

- `@tibble`:

  Gets a tibble (data.frame) containing parameters and styles used by
  [`ggforce::geom_shape`](https://ggforce.data-imaginist.com/reference/geom_shape.html)

## Examples

``` r
ob_rectangle(center = ob_point(0,0), width = 3, height = 2)
#> 
#> ── <ob_rectangle> 
#> # A tibble: 1 × 7
#>       x     y width height angle color fill 
#>   <dbl> <dbl> <dbl>  <dbl> <dbl> <chr> <chr>
#> 1     0     0     3      2     0 black NA   
```
