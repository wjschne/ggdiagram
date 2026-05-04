# ob_style class

ob_style class

## Usage

``` r
ob_style(
  id = character(0),
  alpha = numeric(0),
  angle = numeric(0),
  arrow_head = list(),
  arrow_fins = list(),
  arrow_mid = list(),
  color = character(0),
  family = character(0),
  fill = character(0),
  fontface = character(0),
  hjust = numeric(0),
  justify = numeric(0),
  label.color = character(0),
  label.margin = list(),
  label.padding = list(),
  label.r = numeric(0),
  label.size = numeric(0),
  arrowhead_length = numeric(0),
  length_head = numeric(0),
  length_fins = numeric(0),
  length_mid = numeric(0),
  lineend = numeric(0),
  lineheight = numeric(0),
  linejoin = numeric(0),
  linewidth_fins = numeric(0),
  linewidth_head = numeric(0),
  linewidth = numeric(0),
  linetype = numeric(0),
  n = numeric(0),
  nudge_x = numeric(0),
  nudge_y = numeric(0),
  polar_just = numeric(0),
  resect = numeric(0),
  resect_fins = numeric(0),
  resect_head = numeric(0),
  shape = numeric(0),
  size = numeric(0),
  size.unit = numeric(0),
  straight = logical(0),
  stroke = numeric(0),
  stroke_color = character(0),
  stroke_width = numeric(0),
  text.color = character(0),
  vjust = numeric(0),
  ...
)
```

## Arguments

- id:

  character string to identify object

- alpha:

  numeric value for alpha transparency

- angle:

  angle of text

- arrow_head:

  A 2-column matrix of polygon points

- arrow_fins:

  A 2-column matrix of polygon points

- arrow_mid:

  A 2-column matrix of polygon points

- color:

  character string for color

- family:

  font family

- fill:

  character string for fill color

- fontface:

  Can be plain, bold, italic, or bold.italic

- hjust:

  horizontal justification. 0 means left justified, 1 means right
  justified, 0.5 means horizontally centered

- justify:

  A numeric(1) between 0 and 1 to control where the arrows should be
  drawn relative to the path's endpoints. A value of 0 sets the arrow's
  tips at the path's end, whereas a value of 1 sets the arrow's base at
  the path's end. From ggarrow.

- label.color:

  Color of label outline.

- label.margin:

  Amount of distance around label. A
  [grid::unit](https://rdrr.io/r/grid/unit.html) vector of length four.
  Usually created with
  [`ggplot2::margin`](https://ggplot2.tidyverse.org/reference/element.html).

- label.padding:

  Amount of padding around label. A
  [grid::unit](https://rdrr.io/r/grid/unit.html) vector of length four.
  Usually created with
  [`ggplot2::margin`](https://ggplot2.tidyverse.org/reference/element.html).

- label.r:

  Radius of rounded corners. Defaults to 0.15 lines.

- label.size:

  Width of label outline.

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

- length_mid:

  Determines the size of the middle arrows. Numeric values set the
  ornament size relative to the linewidth. A
  [grid::unit](https://rdrr.io/r/grid/unit.html) value sets the ornament
  size in an absolute manner. From ggarrow.

- lineend:

  Line end style (round, butt, square).

- lineheight:

  Height of line of text

- linejoin:

  Line join style (round, mitre, bevel).

- linewidth_fins:

  Line width for arrow fins

- linewidth_head:

  Line width for arrow fins

- linewidth:

  Width of lines

- linetype:

  type of lines

- n:

  Number of points in a polygon, circle, arc, or ellipse

- nudge_x:

  Horizontal adjustment to nudge labels by.

- nudge_y:

  Vertical adjustment to nudge labels by.

- polar_just:

  an angle, polar point, or point that alters hjust and vjust (polar
  polar_just not stored in style)

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

- shape:

  Point shape type. Can be specified with an integer (between 0 and 25),
  a single character (which uses that character as the plotting symbol),
  a . to draw the smallest rectangle that is visible (i.e., about one
  pixel), an NA to draw nothing, or a mapping to a discrete variable.

- size:

  numeric size

- size.unit:

  How the size aesthetic is interpreted: as points (`"pt"`), millimeters
  (`"mm"`), centimeters (`"cm"`), inches (`"in"`), or picas (`"pc"`).

- straight:

  logical. If TRUE, make bzpath label text straight instead of curved.

- stroke:

  Width of point border line

- stroke_color:

  Color of point border line

- stroke_width:

  Stroke width in arrows

- text.color:

  Color of label text.

- vjust:

  vertical justification. 0 means bottom aligned, 1 means top aligned,
  0.5 means vertically centered

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  unused

## Value

ob_style object
