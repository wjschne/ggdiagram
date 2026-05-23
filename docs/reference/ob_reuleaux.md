# Reuleaux polygon

Reuleaux polygon

## Usage

``` r
ob_reuleaux(
  center = ob_point(0, 0),
  n = 5,
  radius = 1,
  angle = 90,
  label = character(0),
  vertex_radius = numeric(0),
  alpha = numeric(0),
  color = "black",
  fill = character(0),
  linewidth = numeric(0),
  linetype = numeric(0),
  style = S7::class_missing,
  id = character(0),
  ...
)
```

## Arguments

- center:

  [`ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  at center of the rectangle

- n:

  Number of sides. True Reuleaux polygons have an odd number of sides,
  but Reauleaux-like shapes with an even number of sides are possible.

- radius:

  Distance from center to a vertex

- angle:

  Angle of the object's rotation

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
  unused

## Value

ob_reuleaux object

## Additional properties

- `@aesthetics`:

  A list of information about the object's aesthetic properties

- `@angle_at`:

  A function that finds the angle of the specified point in relation to
  the circle's center

- `@arc_at`:

  A function that finds the arc at a specified angle

- `@arc_radius`:

  The radius of the arcs used to construct the reuleaux object

- `@arcs`:

  Returns the arcs of each reuleaux object

- `@bounding_box`:

  a rectangle that contains all the reuleaux objects

- `@central_angle`:

  The angle from the center to adjacent vertices of the reuleaux object

- `@chord_length`:

  The length of each chord of the arcs used to construct the reuleaux
  object

- `@circumference`:

  circumference of the reuleaux object

- `@circumscribed`:

  Returns the circle that circumscribes the object

- `@geom`:

  A function that converts the object to a geom. Any additional
  parameters are passed to
  [`ggforce::geom_shape`](https://ggforce.data-imaginist.com/reference/geom_shape.html).

- `@inscribed`:

  Returns the circle that inscribes the object

- `@inscribed_angle`:

  The angle of the arcs used to construct the reuleaux object

- `@length`:

  The number of circles in the circle object

- `@normal_at`:

  A function that finds a point that is perpendicular from the circle
  and at a specified distance

- `@point_at`:

  A function that finds a point on the circle at the specified angle.

- `@polygon`:

  a tibble containing information to create all the polygon points in a
  reuleaux object

- `@tangent_at`:

  A function that finds the tangent line at the specified angle.

- `@vertices`:

  Returns the vertices of the reuleaux object

## Examples

``` r
ob_reuleaux(n = 3, fill = "royalblue", color = NA)
#> 
#> ── <ob_reuleaux> 
#> # A tibble: 1 × 7
#>       x     y radius     n angle color fill     
#>   <dbl> <dbl>  <dbl> <int> <dbl> <chr> <chr>    
#> 1     0     0      1     3    90 NA    royalblue
```
