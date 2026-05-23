# Variance object

Creates double-headed arrow paths indicating variance

## Usage

``` r
ob_variance(
  x,
  where = "north",
  theta = 50,
  bend = 0,
  looseness = 1,
  arrow_head = the$arrow_head,
  arrow_fins = the$arrow_head,
  resect = 2,
  ...
)

## S7 method for class <ggdiagram::centerpoint>
ob_variance(
  x,
  where = "north",
  theta = 50,
  bend = 0,
  looseness = 1,
  arrow_head = the$arrow_head,
  arrow_fins = the$arrow_head,
  resect = 2,
  ...
)
```

## Arguments

- x:

  object of type `ggdiagram::centerpoint`

- where:

  Location on object. Can be numeric (degrees),
  [degree](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [radian](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [turn](https://wjschne.github.io/ggdiagram/reference/ob_angle.md), or
  named direction (e.g., "northwest", "east", "below", "left")

- theta:

  angle width

- bend:

  Angle by which the control points are rotated. Can be numeric
  (degrees),
  [degree](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [radian](https://wjschne.github.io/ggdiagram/reference/ob_angle.md),
  [turn](https://wjschne.github.io/ggdiagram/reference/ob_angle.md), or
  named direction (e.g., "northwest", "east", "below", "left"). Defaults
  to 0.

- looseness:

  distance of control points as a ratio of the distance to the object's
  center (e.g., in a circle of radius 1, looseness = 1.5 means that that
  the control points will be 1.5 units from the start and end points.)

- arrow_head:

  A 2-column matrix of polygon points

- arrow_fins:

  A 2-column matrix of polygon points

- resect:

  A numeric(1) denoting millimeters or
  [grid::unit](https://rdrr.io/r/grid/unit.html) to shorten the arrow
  head and fins.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

Returns an object of type
[`ob_bezier`](https://wjschne.github.io/ggdiagram/reference/ob_bezier.md)

## Methods

`ob_variance` is an S7 generic with methods available for the following
classes:

- `ggdiagram::centerpoint`

## Examples

``` r
theta <- degree(seq(0, 360 - 45, 45))
ggdiagram() +
{x <- ob_circle(ob_polar(theta, r = 3))} +
connect(x, lag_cycle(x, 3), resect = 2) +
ob_variance(x,
            label = ob_label(LETTERS[seq_along(c(theta))]),
            where = theta,
            looseness = 1.25)
```
