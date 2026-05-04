# ob_label class

ob_label class

## Usage

``` r
ob_label(
  label = character(0),
  center = S7::class_missing,
  angle = numeric(0),
  alpha = numeric(0),
  color = character(0),
  family = character(0),
  fill = character(0),
  fontface = character(0),
  hjust = numeric(0),
  label.color = character(0),
  label.margin = class_margin(ggplot2::margin(1, 1, 1, 1, "pt")),
  label.padding = class_margin(ggplot2::margin(2, 2, 2, 2, "pt")),
  label.r = numeric(0),
  label.size = numeric(0),
  lineheight = numeric(0),
  polar_just = numeric(0),
  nudge_x = numeric(0),
  nudge_y = numeric(0),
  size = numeric(0),
  straight = logical(0),
  text.color = character(0),
  vjust = numeric(0),
  style = S7::class_missing,
  plot_point = FALSE,
  position = 0.5,
  spacing = numeric(0),
  x = S7::class_missing,
  y = S7::class_missing,
  id = character(0),
  ...
)
```

## Arguments

- label:

  text label

- center:

  [ob_point](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  indicating the center of the label

- angle:

  angle of text

- alpha:

  numeric value for alpha transparency

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

- lineheight:

  Height of line of text

- polar_just:

  an angle, polar point, or point that alters hjust and vjust (polar
  polar_just not stored in style)

- nudge_x:

  Horizontal adjustment to nudge labels by.

- nudge_y:

  Vertical adjustment to nudge labels by.

- size:

  numeric size

- straight:

  logical. If TRUE, make bzpath label text straight instead of curved.

- text.color:

  Color of label text.

- vjust:

  vertical justification. 0 means bottom aligned, 1 means top aligned,
  0.5 means vertically centered

- style:

  a style list

- plot_point:

  plot center
  [ob_point](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  (default = FALSE)

- position:

  position (0 to 1). Used to position a label on an
  [ob_segment](https://wjschne.github.io/ggdiagram/reference/ob_segment.md),
  [ob_arc](https://wjschne.github.io/ggdiagram/reference/ob_arc.md),
  [ob_path](https://wjschne.github.io/ggdiagram/reference/ob_path.md),
  or
  [ob_bezier](https://wjschne.github.io/ggdiagram/reference/ob_bezier.md)

- spacing:

  letter spacing for labels used with ob_path and ob_bezier

- x:

  x-coordinate of center point. If specified, overrides x-coordinate of
  `@center`

- y:

  x-coordinate of center point. If specified, overrides y-coordinate of
  `@center`

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

ob_label object

## Additional properties

- `@aesthetics`:

  A list of information about the label's aesthetic properties

- `@auto_label`:

  Places a label of the xy coordinates (e.g., `(0, 1)`)

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggtext::geom_richtext`](https://wilkelab.org/ggtext/reference/geom_richtext.html).

- `@tibble`:

  Gets a tibble (data.frame) containing parameters and styles used by
  [`ggtext::geom_richtext`](https://wilkelab.org/ggtext/reference/geom_richtext.html)

- `@xy`:

  Gets a 2-column matrix of the x and y coordinates of the label's
  `center`
