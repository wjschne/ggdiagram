# ob_point

Points are specified with x and y coordinates.

Polar points are ordinary points but are specified with an angle (theta)
and a radial distance (r)

## Usage

``` r
ob_point(
  x = 0,
  y = 0,
  alpha = numeric(0),
  color = character(0),
  fill = character(0),
  shape = numeric(0),
  size = numeric(0),
  stroke = numeric(0),
  style = S7::class_missing,
  id = character(0),
  ...
)

ob_polar(
  theta = S7::class_missing,
  r = numeric(0),
  alpha = numeric(0),
  color = character(0),
  fill = character(0),
  shape = numeric(0),
  size = numeric(0),
  stroke = numeric(0),
  style = S7::class_missing,
  id = character(0)
)
```

## Arguments

- x:

  Vector of coordinates on the x-axis (also can take a tibble/data.frame
  or 2-column matrix as input.)

- y:

  Vector of coordinates on the y-axis

- alpha:

  numeric value for alpha transparency

- color:

  character string for color

- fill:

  character string for fill color

- shape:

  Point shape type. Can be specified with an integer (between 0 and 25),
  a single character (which uses that character as the plotting symbol),
  a . to draw the smallest rectangle that is visible (i.e., about one
  pixel), an NA to draw nothing, or a mapping to a discrete variable.

- size:

  numeric size

- stroke:

  Width of point border line

- style:

  Gets and sets the styles associated with points

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to `style`

- theta:

  Angle of the vector from the origin to the `ob_point`

- r:

  Radius = Distance from the origin to the ob_point

## Value

ob_point object

## Additional properties

- `@auto_label`:

  Gets x and y coordinates and makes a label `"(x,y)"`

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- `@length`:

  The number of points in the ob_point object

- `@tibble`:

  Gets a
  [`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
  containing parameters and styles used by
  [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- `@xy`:

  Gets a 2-column matrix of the x and y coordinates of the ob_point
  object.

- `@centroid`:

  ob_point at the average of the x and y values

- `@bounding_box`:

  ob_rectangle that contains all the points in the object

- `@place`:

  function to place point in relation to other objects

- `@label`:

  function to create ob_label for points in the object

- `@aesthetics`:

  returns class_aesthetics for ob_point

## Examples

``` r
ggdiagram(theme = ggplot2::theme_minimal) +
  ob_point(x = -1:1, y = -1:1, color = "red") +
  ob_polar(theta = degree(seq(0, 330, 30)), r = 2, color = "blue")
```
