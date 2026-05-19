# The ob_bezier (i.e., bezier curve) class

The ob_bezier is specified with an ob_point object that contains at
least 2 points, the start and the end. Such a "curve" would actually be
a straight line segment. If three points are specified, the middle point
is a control point, and a quadratic bezier curve will result.
Higher-order bezier curves can be created by having more control points
in the middle.

## Usage

``` r
ob_bezier(
  p = S7::class_missing,
  label = character(0),
  label_sloped = TRUE,
  n = 101,
  alpha = numeric(0),
  arrow_head = S7::class_missing,
  arrow_fins = S7::class_missing,
  arrowhead_length = numeric(0),
  length_head = numeric(0),
  length_fins = numeric(0),
  color = character(0),
  fill = character(0),
  lineend = numeric(0),
  linejoin = numeric(0),
  linewidth = numeric(0),
  linewidth_fins = numeric(0),
  linewidth_head = numeric(0),
  linetype = numeric(0),
  resect = numeric(0),
  resect_fins = numeric(0),
  resect_head = numeric(0),
  stroke_color = character(0),
  stroke_width = numeric(0),
  style = S7::class_missing,
  id = character(0),
  ...
)
```

## Arguments

- p:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  or list of
  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  objects

- label:

  A character, angle, or label object

- label_sloped:

  A logical value indicating whether the label should be sloped with the
  curve

- n:

  Number of points in a polygon, circle, arc, or ellipse

- alpha:

  numeric value for alpha transparency

- arrow_head:

  A 2-column matrix of polygon points

- arrow_fins:

  A 2-column matrix of polygon points

- arrowhead_length:

  Determines the size of the arrow ornaments. This parameter becomes the
  `length` parameter in ggarrow functions. Numeric values set the
  ornament size relative to the linewidth. A
  [grid::unit](https://rdrr.io/r/grid/unit.html) value sets the ornament
  size in an absolute manner.

- length_head:

  Determines the size of the arrow head. Numeric values set the ornament
  size relative to the linewidth. A
  [grid::unit](https://rdrr.io/r/grid/unit.html) value sets the ornament
  size in an absolute manner. From ggarrow.

- length_fins:

  Determines the size of the arrow fins. Numeric values set the ornament
  size relative to the linewidth. A
  [grid::unit](https://rdrr.io/r/grid/unit.html) value sets the ornament
  size in an absolute manner. From ggarrow.

- color:

  character string for color

- fill:

  character string for fill color

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- linewidth:

  Width of lines

- linewidth_fins:

  Line width for arrow fins

- linewidth_head:

  Line width for arrow fins

- linetype:

  type of lines

- resect:

  A numeric(1) denoting millimeters or
  [grid::unit](https://rdrr.io/r/grid/unit.html) to shorten the arrow
  head and fins.

- resect_fins:

  A numeric(1) denoting millimeters or
  [grid::unit](https://rdrr.io/r/grid/unit.html) to shorten the arrow
  fins

- resect_head:

  A numeric(1) denoting millimeters or
  [grid::unit](https://rdrr.io/r/grid/unit.html) to shorten the arrow
  head.

- stroke_color:

  Color of point border line

- stroke_width:

  Stroke width in arrows

- style:

  Gets and sets the styles associated with ob_beziers

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

ob_bezier object

## Details

If you wish to specify multiple bezier curves, you must supply a list of
ob_point objects. When plotted, the ob_bezier function uses the
bezier::bezier function to create the point coordinates of the curve and
the ggarrow::geom_arrow function to create the geom.

## Additional properties

- `@aesthetics`:

  A list of information about the object's aesthetic properties

- `@bounding_box`:

  A rectangle that contains all the bezier curves

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggarrow::geom_arrow`](https://teunbrand.github.io/ggarrow/reference/geom_arrow.html).

- `@length`:

  The number of curves in the ob_bezier object

- `@midpoint`:

  A function that selects 1 or more midpoints of the ob_bezier. The
  `position` argument can be between 0 and 1. Additional arguments are
  passed to `ob_point`.

- `@path`:

  A path object consisting of the control points

- `@point_at_x`:

  A function that finds the point on each curve where x is equal to the
  `x` argument.

- `@point_at_y`:

  A function that finds the point on each curve where y is equal to the
  `y` argument.

- `@set_label_x`:

  A function that sets labels to have the same x coordinate. The
  `position` argument can be between 0 and 1, indicating how far along
  on the first curve the x coordinate is selected. If the `x` argument
  is set, the `position` argument is overridden, and the x-coordinate is
  set directly.

- `@set_label_y`:

  A function that sets labels to have the same y coordinate. The
  `position` argument can be between 0 and 1, indicating how far along
  on the first curve the y coordinate is selected. If the `y` argument
  is set, the `position` argument is overridden, and the y-coordinate is
  set directly.

- `@tibble`:

  Gets a tibble (data.frame) containing parameters and styles used by
  [`ggarrow::geom_arrow`](https://teunbrand.github.io/ggarrow/reference/geom_arrow.html).

## Examples

``` r
control_points <- ob_point(c(0,1,2,4), c(0,4,0,1))
ggdiagram() +
  ob_bezier(control_points, color = "blue")
```
