# The ob_path class

An `ob_path` is specified with an
[`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
object that contains at least 2 points, the start and the end. Any
number of intermediate points are possible.

## Usage

``` r
ob_path(
  p = S7::class_missing,
  label = character(0),
  label_sloped = TRUE,
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
  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)s

- label:

  A character, angle, or
  [`ob_label`](https://wjschne.github.io/ggdiagram/reference/ob_label.md)
  object

- label_sloped:

  A logical value indicating whether the label should be sloped with the
  curve

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

  Gets and sets the styles associated with paths

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

ob_path object

## Details

If you wish to specify multiple paths, you must supply a list of
[`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
objects. When plotted, the `ob_path` function uses the
ggarrow::geom_arrow function to create the geom.

## Additional properties

- `@aesthetics`:

  A list of information about the path's aesthetic properties

- `@bounding_box`:

  A rectangle that contains all the paths

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggarrow::geom_arrow`](https://teunbrand.github.io/ggarrow/reference/geom_arrow.html).

- `@length`:

  The number of paths in the `ob_path` object

- `@midpoint`:

  A function that selects 1 or more midpoints of the ob_segment. The
  `position` argument can be between 0 and 1. Additional arguments are
  passed to `ob_point`.

- `@tibble`:

  Gets a
  [`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
  containing parameters and styles used by
  [`ggarrow::geom_arrow`](https://teunbrand.github.io/ggarrow/reference/geom_arrow.html).

- `@segments`:

  Gets the segments from the path

- `@vertex_angle`:

  Gets angles at each vertex

## Examples

``` r
ggdiagram() +
 ob_path(list(ob_point(c(0, 0, 4), c(0, 1, 4)),
              ob_point(c(1, 2, 5, 6), c(0, 1, 2, 0))), color = c("red", "blue"))
```
