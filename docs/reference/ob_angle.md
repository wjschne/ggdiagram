# ob_angle

Creates an angle in the metric of radians, degrees, and turns.

## Usage

``` r
ob_angle(
  .data = numeric(0),
  degree = numeric(0),
  radian = numeric(0),
  turn = numeric(0)
)

degree(degree = numeric(0))

radian(radian = numeric(0))

turn(turn = numeric(0))
```

## Arguments

- .data:

  a real number indicating the number of turns.

- degree:

  degrees

- radian:

  radians

- turn:

  proportion of full turns of a circle (1 turn = 2 \* pi radians)

## Value

ob_angle

## Details

Angles turns can be any real number, but degrees are displayed as values
between -360 and +360, and radians are between -2pi and +2pi.

## Additional properties

- `@positive`:

  if angle is negative, adds a full turn to ensure the angle is positive

- `@negative`:

  if angle is positive, subtracts a full turn to ensure the angle is
  negative

- `@upright`:

  Converts angle to an "upright" position so that text is never upside
  down (i.e., 91–270 degrees is flipped to )

## Examples

``` r
# Three Different ways to make a right angle
## 90 degrees
degree(90)
#> [1] "90°"

## half pi radians
radian(.5 * pi)
#> [1] "0.5π"

## A quarter turn
turn(.25)
#> [1] ".25"

# Operations
degree(30) + degree(20)
#> [1] "50°"
degree(350) + degree(20)
#> [1] "10°"
degree(30) - degree(30)
#> [1] "0°"
degree(30) - degree(50)
#> [1] "−20°"

degree(30) * 2
#> [1] "60°"
degree(30) / 3
#> [1] "10°"

radian(1) + 1 # added or subtracted numbers are radians
#> [1] "0.64π"
degree(10) + 10 # added or subtracted numbers are degrees
#> [1] "20°"
turn(.25) + .25 # added or subtracted numbers are turns
#> [1] ".50"

# Trigonometric functions work as normal
sin(degree(30))
#> [1] 0.5
cos(degree(30))
#> [1] 0.8660254
tan(degree(30))
#> [1] 0.5773503
```
