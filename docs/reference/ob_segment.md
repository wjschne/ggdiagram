# ob_segment class

ob_segment class

## Usage

``` r
ob_segment(
  p1 = S7::class_missing,
  p2 = S7::class_missing,
  label = character(0),
  label_sloped = TRUE,
  alpha = numeric(0),
  arrow_head = class_arrowhead(ggarrow::arrow_head_minimal(90)),
  arrow_fins = list(),
  arrowhead_length = 7,
  length_head = numeric(0),
  length_fins = numeric(0),
  color = character(0),
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
  x = S7::class_missing,
  xend = S7::class_missing,
  y = S7::class_missing,
  yend = S7::class_missing,
  id = character(0),
  ...
)
```

## Arguments

- p1:

  starting point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- p2:

  end point
  ([`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md))

- label:

  A character, angle, or
  [`ob_label`](https://wjschne.github.io/ggdiagram/reference/ob_label.md)
  object

- label_sloped:

  A logical value indicating whether the label should be sloped with the
  segment

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

  a style list

- x:

  overrides the x-coordinate of p1

- xend:

  overrides the x-coordinate of p2

- y:

  overrides the y-coordinate of p1

- yend:

  overrides the y-coordinate of p2

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

ob_segment object

## Additional properties

- `@aesthetics`:

  A list of information about the segment's aesthetic properties

- `@bounding_box`:

  a rectangle that contains all the segments

- `@distance`:

  Distance between segment endpoints

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggarrow::geom_arrow_segment`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_segment.html).

- `@hatch`:

  A function that puts hatch (tally) marks on segments. Often used to
  indicate which segments have the same length. The `k` parameter
  controls how many hatch marks to display. The `height` parameter
  controls how long the hatch mark segment is. The `sep` parameter
  controls the separation between hatch marks when `k > 2`. Additional
  parameters sent to `ob_segment`.

- `@length`:

  The number of segments in the segment object

- `@line`:

  The line object associated with the segment

- `@midpoint`:

  A function that selects 1 or more midpoints of the ob_segment. The
  `position` argument can be between 0 and 1. Additional arguments are
  passed to `ob_point`.

- `@nudge`:

  A function to move the segment by x and y units.

- `@set_label_x`:

  A function that sets labels to have the same x coordinate. The
  `position` argument can be between 0 and 1, indicating how far along
  on the first segment the x coordinate is selected. If the `x` argument
  is set, the `position` argument is overridden, and the x-coordinate is
  set directly.

- `@set_label_y`:

  A function that sets labels to have the same y coordinate. The
  `position` argument can be between 0 and 1, indicating how far along
  on the first segment the y coordinate is selected. If the `y` argument
  is set, the `position` argument is overridden, and the y-coordinate is
  set directly.

- `@tibble`:

  Gets a tibble (data.frame) containing parameters and styles used by
  [`ggarrow::geom_arrow_segment`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_segment.html)
