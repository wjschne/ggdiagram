# Covariance object

Creates double-headed arrow paths indicating variance

## Usage

``` r
ob_covariance(
  x,
  y,
  where = NULL,
  bend = 0,
  looseness = 1,
  arrow_head = the$arrow_head,
  length_head = 7,
  length_fins = 7,
  resect = 2,
  ...
)

## S7 method for classes <ggdiagram::centerpoint>, <ggdiagram::centerpoint>
ob_covariance(
  x,
  y,
  where = NULL,
  bend = 0,
  looseness = 1,
  arrow_head = the$arrow_head,
  length_head = 7,
  length_fins = 7,
  resect = 2,
  ...
)
```

## Arguments

- x:

  object

- y:

  object

- where:

  exit angle. Can be numeric (degrees),
  [degree](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [radian](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [turn](https://wjschne.github.io/ggdiagram/reference/ob_angle.md), or
  named direction (e.g., "northwest", "east", "below", "left")

- bend:

  Angle by which the control points are rotated. Can be numeric
  (degrees),
  [degree](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [radian](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [turn](https://wjschne.github.io/ggdiagram/reference/ob_angle.md), or
  named direction (e.g., "northwest", "east", "below", "left"). Defaults
  to 0

- looseness:

  distance of control points as a ratio of the distance to the object's
  center (e.g., in a circle of radius 1, looseness = 1.5 means that that
  the control points will be 1.5 units from the start and end points.)

- arrow_head:

  A 2-column matrix of polygon points

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

- resect:

  A numeric(1) denoting millimeters or
  [grid::unit](https://rdrr.io/r/grid/unit.html) to shorten the arrow
  head and fins.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

An
[`ob_bezier`](https://wjschne.github.io/ggdiagram/reference/ob_bezier.md)
object

## Methods

`ob_covariance` is an S7 generic with methods available for the
following classes:

- `ggdiagram::centerpoint,ggdiagram::centerpoint`

- `ggdiagram::ob_point,ggdiagram::ob_point`

## Examples

``` r
ggdiagram() +
  {x <- ob_circle(ob_point(c(-2, 2), 0))} +
  ob_covariance(x = x[1],
                y = x[2],
                label = ob_label("A"))


ggdiagram() +
  x +
  ob_covariance(x = x[1],
                y = x[2],
                label = ob_label("A"),
                where = -45,
                looseness = .75)
```
