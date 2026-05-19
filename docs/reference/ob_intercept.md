# ob_intercept

Triangle polygons used in path diagrams.

## Usage

``` r
ob_intercept(
  center = ob_point(0, 0),
  width = 1,
  label = character(0),
  vertex_radius = numeric(0),
  alpha = numeric(0),
  color = character(0),
  fill = character(0),
  linewidth = numeric(0),
  linetype = numeric(0),
  style = S7::class_missing,
  x = numeric(0),
  y = numeric(0),
  id = character(0),
  ...
)
```

## Arguments

- center:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at center

- width:

  length of side

- label:

  A character, angle, or
  [`ob_label`](https://wjschne.github.io/ggdiagram/reference/ob_label.md)
  object

- vertex_radius:

  A numeric or unit vector of length one, specifying the vertex radius

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

- style:

  Gets and sets the styles associated with polygons

- x:

  overrides x-coordinate in `center@x`

- y:

  overrides x-coordinate in `center@y`

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

ob_polygon object

## Additional properties

- `@top`:

  Top vertex of triangle

- `@left`:

  Left vertex of triangle

- `@right`:

  Right vertex of triangle

- `@length`:

  The number of polygons in the ob_polygon object

- `@aesthetics`:

  A list of information about the object's aesthetic properties

- `@tibble`:

  Gets a tibble (data.frame) containing parameters and styles used by
  [`ggplot2::geom_polygon`](https://ggplot2.tidyverse.org/reference/geom_polygon.html).
