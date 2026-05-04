# ob_ellipse class

Makes ellipses and superellipses

## Usage

``` r
ob_ellipse(
  center = ob_point(0, 0),
  a = 1,
  b = a,
  angle = 0,
  m1 = numeric(0),
  m2 = numeric(0),
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

  point at center of ellipse. *Settable.*

- a:

  distance of semi-major axis. *Settable.*

- b:

  distance of semi-minor axis. *Settable.*

- angle:

  ellipse rotation. *Settable.*

- m1:

  exponent of semi-major axis. *Settable.* Controls roundedness of
  superellipse

- m2:

  exponent of semi-minor axis. *Settable.* By default equal to `m1`. If
  different, some functions may not work as expected (e.g., `point_at`).

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

  number of points in ellipse (default = 360). *Settable.*

- style:

  gets and sets style parameters

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

ob_ellipse object

## Additional properties

- `@aesthetics`:

  A list of information about the object's aesthetic properties

- `@angle_at`:

  A function that finds the angle of the specified point in relation to
  the ellipse's center

- `@area`:

  area of the ellipse

- `@bounding_box`:

  a rectangle that contains all the ellipses

- `@focus_1`:

  left focus point of the ellipse

- `@focus_2`:

  right focus point of the ellipse

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggforce::geom_ellipse`](https://ggforce.data-imaginist.com/reference/geom_ellipse.html).

- `@length`:

  Gets the number of ellipses

- `@normal_at`:

  A function that finds a point perpendicular to the ellipse at angle
  `theta` at the specified `distance`. The `definitional` parameter is
  passed to the `point_at` function. If a point is supplied instead of
  an angle, the point is projected onto the ellipse and then the normal
  is calculated found from the projected point.

- `@perimeter`:

  returns the ellipse's perimeter

- `@point_at`:

  A function that finds a point on the ellipse at an angle `theta`. If
  `definitional` is `FALSE` (default), then `theta` is interpreted as an
  angle. If `TRUE`, then `theta` is the parameter in the definition of
  the ellipse in polar coordinates.

- `@polar_line_at`:

  A function that creates an `ob_line` that passes through the ellipse's
  center and the point specified in `x`.

- `@polygon`:

  a tibble containing information to create all the polygon points in
  ellipse.

- `@tangent_at`:

  A function that finds a tangent line on the ellipse. Uses `point_at`
  to find the tangent point at angle `theta` and then returns the
  tangent line at that point. If a point is supplied instead of an
  angle, the point is projected onto the ellipse and then the tangent
  line is found from there.

- `@tibble`:

  Gets a tibble (data.frame) containing parameters and styles used by
  [`ggforce::geom_ellipse`](https://ggforce.data-imaginist.com/reference/geom_ellipse.html).

## Examples

``` r
# specify center point and semi-major axes
e <- ob_ellipse(center = ob_point(0,0), a = 2, b = 3)
ggdiagram() +
  e
```
