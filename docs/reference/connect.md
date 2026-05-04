# Arrow connect one shape to another

By default, will create an
[`ob_segment`](https://wjschne.github.io/ggdiagram/reference/ob_segment.md)
with an arrowhead on the end. If `arc_bend` is specified, an
[`ob_arc`](https://wjschne.github.io/ggdiagram/reference/ob_arc.md) with
an arrowhead will be created instead. If `from_offset` or `to_offset`
are specified, an
[`ob_bezier`](https://wjschne.github.io/ggdiagram/reference/ob_bezier.md)
with an arrowhead will be created.

## Usage

``` r
connect(
  from,
  to,
  ...,
  label = character(0),
  arc_bend = NULL,
  from_offset = NULL,
  to_offset = NULL,
  alpha = numeric(0),
  arrow_head = the$arrow_head,
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
  label_sloped = TRUE,
  id = character(0)
)
```

## Arguments

- from:

  first shape object

- to:

  second shape object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Arguments passed to
  [ob_style](https://wjschne.github.io/ggdiagram/reference/ob_style.md)

- label:

  A character, angle, or label object

- arc_bend:

  If specified, the arrow will be an arc with a sagitta sized in
  proportion to the distance between points. The sagitta is is the
  largest distance from the arc's chord to the arc itself. Negative
  values bend left. Positive values bend right. 1 and -1 create
  semi-circles. 0 is a straight segment. If specified, will override
  `from_offset` and `to_offset`.

- from_offset:

  If specified, arrow will be a bezier curve. The `from_offset` is a
  point (ob_point or ob_polar) that is added to `from` to act as a
  control point in the bezier curve.

- to_offset:

  If specified, arrow will be a bezier curve. The `to_offset` is a point
  (ob_point or ob_polar) that is added to `to` to act as a control point
  in the bezier curve.

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

  Gets and sets the styles associated with ob_beziers

- label_sloped:

  A logical value indicating whether the label should be sloped with the
  curve

- id:

  character string to identify object

## Value

ob_segment
