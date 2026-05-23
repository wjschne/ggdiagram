# ob_arc class

Create arcs and wedges

## Usage

``` r
ob_arc(
  center = ob_point(0, 0),
  radius = 1,
  start = 0,
  end = 0,
  label = character(0),
  label_sloped = FALSE,
  start_point = S7::class_missing,
  end_point = S7::class_missing,
  n = 360L,
  type = "arc",
  alpha = numeric(0),
  arrow_head = list(),
  arrow_fins = list(),
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
  x = numeric(0),
  y = numeric(0),
  id = character(0),
  ...
)

ob_wedge(
  center = ob_point(0, 0),
  radius = 1,
  start = 0,
  end = 0,
  label = character(0),
  label_sloped = FALSE,
  start_point = S7::class_missing,
  end_point = S7::class_missing,
  n = 360L,
  type = "wedge",
  alpha = numeric(0),
  arrow_head = list(),
  arrow_fins = list(),
  arrowhead_length = numeric(0),
  length_head = numeric(0),
  length_fins = numeric(0),
  color = NA,
  fill = "black",
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
  x = numeric(0),
  y = numeric(0),
  id = character(0),
  ...
)

ob_circular_segment(
  center = ob_point(0, 0),
  radius = 1,
  start = 0,
  end = 0,
  label = character(0),
  label_sloped = FALSE,
  start_point = S7::class_missing,
  end_point = S7::class_missing,
  n = 360L,
  type = "segment",
  alpha = numeric(0),
  arrow_head = list(),
  arrow_fins = list(),
  arrowhead_length = numeric(0),
  length_head = numeric(0),
  length_fins = numeric(0),
  color = NA,
  fill = "black",
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
  x = numeric(0),
  y = numeric(0),
  id = character(0),
  ...
)
```

## Arguments

- center:

  point at center of the arc (default = `ob_point(0,0)`)

- radius:

  distance between center and edge arc (default = 1)

- start:

  start angle. Can be numeric (degrees),
  [degree](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [radian](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [turn](https://wjschne.github.io/ggdiagram/reference/ob_angle.md), or
  named direction (e.g., "northwest", "east", "below", "left"). Defaults
  to 0.

- end:

  end angle Can be numeric (degrees),
  [degree](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [radian](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [turn](https://wjschne.github.io/ggdiagram/reference/ob_angle.md), or
  named direction (e.g., "northwest", "east", "below", "left"). Defaults
  to 0.

- label:

  A character, angle, or label object

- label_sloped:

  If TRUE, label runs along arc.

- start_point:

  Specify where arc starts. Overrides `@center`

- end_point:

  Specify where arc ends Overrides `@center`

- n:

  number of points in arc (default = 360)

- type:

  Type of object to drawn. Can be "arc", "wedge", or "segment"

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

  an
  [`ob_style`](https://wjschne.github.io/ggdiagram/reference/ob_style.md)
  object

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

ob_arc object

## Additional properties

- `@aesthetics`:

  A list of information about the arc's aesthetic properties

- `@angle_at`:

  A function that finds the angle of the specified point in relation to
  the arc's center

- `@apothem`:

  Distance from center to the chord's midpoint

- `@arc_length`:

  Distance along arc from `start_point` to `end_point`

- `@auto_label`:

  Places an label at the arc's midpoint

- `@bounding_box`:

  A rectangle that contains all the arcs

- `@chord`:

  [`ob_segment`](https://wjschne.github.io/ggdiagram/reference/ob_segment.md)
  from `start_point` to `end_point`

- `@circle`:

  Circle object associated with the arc object

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggarrow::geom_arrow`](https://teunbrand.github.io/ggarrow/reference/geom_arrow.html).

- `@hatch`:

  A function that puts hatch (tally) marks on arcs. Often used to
  indicate which arcs have the same angle. The `k` parameter controls
  how many hatch marks to display. The `height` parameter controls how
  long the hatch mark segment is. The `sep` parameter controls the
  separation between hatch marks when `k > 2`. Additional parameters
  sent to
  [`ob_segment`](https://wjschne.github.io/ggdiagram/reference/ob_segment.md).

- `@length`:

  The number of arcs in the arc object

- `@midpoint`:

  A function that selects 1 or more midpoints of the ob_arc. The
  `position` argument can be between 0 and 1. Additional arguments are
  passed to
  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md).

- `@point_at`:

  A function that finds a point on the arc at the specified angle.

- `@polygon`:

  A tibble containing information to create all the polygon points in an
  arc

- `@sagitta`:

  [`ob_segment`](https://wjschne.github.io/ggdiagram/reference/ob_segment.md)
  from `chord` midpoint to `ob_arc` midpoint

- `@set_label_x`:

  A function that sets labels to have the same x coordinate. The
  `position` argument can be between 0 and 1, indicating how far along
  on the first arc the x coordinate is selected. If the `x` argument is
  set, the `position` argument is overridden, and the x-coordinate is
  set directly.

- `@set_label_y`:

  A function that sets labels to have the same y coordinate. The
  `position` argument can be between 0 and 1, indicating how far along
  on the first arc the y coordinate is selected. If the `y` argument is
  set, the `position` argument is overridden, and the y-coordinate is
  set directly.

- `@tangent_at`:

  A function that finds the tangent line at the specified angle.

- `@theta`:

  Interior angle (end - start)

- `@tibble`:

  Gets a
  [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
  or data.frame containing parameters and styles used by
  [`ggarrow::geom_arrow`](https://teunbrand.github.io/ggarrow/reference/geom_arrow.html).

## Examples

``` r
ob_arc(start = degree(0), end = degree(60))
#> 
#> ── <ob_arc> 
#> # A tibble: 1 × 7
#>       x     y radius start   end     n type 
#>   <dbl> <dbl>  <dbl> <dbl> <dbl> <int> <chr>
#> 1     0     0      1     0    60   360 arc  
ob_circular_segment(start = degree(120), end = degree(180))
#> 
#> ── <ob_arc> 
#> # A tibble: 1 × 9
#>       x     y radius start   end color fill      n type   
#>   <dbl> <dbl>  <dbl> <dbl> <dbl> <chr> <chr> <int> <chr>  
#> 1     0     0      1   120   180 NA    black   360 segment
ob_wedge(start = degree(240), end = degree(300))
#> 
#> ── <ob_arc> 
#> # A tibble: 1 × 9
#>       x     y radius start   end color fill      n type 
#>   <dbl> <dbl>  <dbl> <dbl> <dbl> <chr> <chr> <int> <chr>
#> 1     0     0      1   240   300 NA    black   360 wedge
```
