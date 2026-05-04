# ob_line class

Creates a line

## Usage

``` r
ob_line(
  slope = numeric(0),
  intercept = numeric(0),
  xintercept = numeric(0),
  a = numeric(0),
  b = numeric(0),
  c = numeric(0),
  alpha = numeric(0),
  color = character(0),
  lineend = numeric(0),
  linejoin = numeric(0),
  linewidth = numeric(0),
  linetype = numeric(0),
  style = S7::class_missing,
  id = character(0),
  ...
)
```

## Arguments

- slope:

  coefficient in y = slope \* x + intercept

- intercept:

  value of y when x is 0

- xintercept:

  value of x when y is 0

- a:

  coefficient in general form: a \* x + b \* y + c = 0

- b:

  coefficient in general form: a \* x + b \* y + c = 0

- c:

  constant in general form: a \* x + b \* y + c = 0

- alpha:

  numeric value for alpha transparency

- color:

  character string for color

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- linewidth:

  Width of lines

- linetype:

  type of lines

- style:

  an
  [ob_style](https://wjschne.github.io/ggdiagram/reference/ob_style.md)
  object

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

ob_line object

## Additional properties

- `@aesthetics`:

  A list of information about the circle's aesthetic properties

- `@angle`:

  The angle of the line

- `@equation`:

  Returns a character string with the equation for the line

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggplot2::geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html).

- `@length`:

  The number of lines in the line object

- `@point_at_x`:

  A function that finds the point on each line where x is equal to the
  `x` argument

- `@point_at_y`:

  A function that finds the point on each line where y is equal to the
  `y` argument

- `@projection`:

  A function that returns the projected point from the point object
  specified in `p`

- `@tibble`:

  Returns a tibble (data frame) with object parameters, one row for each
  line in the line object
