# The ob_polygon class

A polygon is specified with an
[`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
that contains at least 3 points, the start and the end. Any number of
intermediate points are possible.

## Usage

``` r
ob_polygon(
  p = S7::class_missing,
  label = character(0),
  vertex_radius = numeric(0),
  alpha = numeric(0),
  color = character(0),
  fill = character(0),
  linewidth = numeric(0),
  linetype = numeric(0),
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

  A character, angle, or
  [`ob_label`](https://wjschne.github.io/ggdiagram/reference/ob_label.md)
  object

- vertex_radius:

  A numeric or unit vector of length one, specifying the corner radius

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

- id:

  character string to identify object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

ob_polygon object

## Details

If you wish to specify multiple polygons, you must supply a list of
[`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
objects. When plotted, the ob_polygon function uses the
[`ggforce::geom_shape`](https://ggforce.data-imaginist.com/reference/geom_shape.html)
function to create the geom.

## Additional properties

- `@aesthetics`:

  A list of information about the object's aesthetic properties

- `@bounding_box`:

  a rectangle that contains all the polygons

- `@geom`:

  A function that returns the output of
  [`ggforce::geom_shape`](https://ggforce.data-imaginist.com/reference/geom_shape.html)

- `@length`:

  The number of polygons in the ob_polygon object

- `@point_at`:

  A function that finds a point on the polygon at the specified angle.

- `@segment`:

  The segments of each polygon

- `@tibble`:

  Gets a tibble (data.frame) containing parameters and styles used by
  [`ggforce::geom_shape`](https://ggforce.data-imaginist.com/reference/geom_shape.html).

- `@center`:

  Points at the centroids of each polygon
