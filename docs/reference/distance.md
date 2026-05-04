# Calculate distance between 2 points

Calculate distance between 2 points

## Usage

``` r
distance(x, y, ...)
```

## Arguments

- x:

  an
  [ob_point](https://wjschne.github.io/ggdiagram/reference/ob_point.md),
  [ob_line](https://wjschne.github.io/ggdiagram/reference/ob_line.md),
  [ob_segment](https://wjschne.github.io/ggdiagram/reference/ob_segment.md),
  or object with a center point (e.g.,
  [ob_circle](https://wjschne.github.io/ggdiagram/reference/ob_circle.md),
  [ob_rectangle](https://wjschne.github.io/ggdiagram/reference/ob_rectangle.md),
  [ob_ellipse](https://wjschne.github.io/ggdiagram/reference/ob_ellipse.md))

- y:

  an
  [ob_point](https://wjschne.github.io/ggdiagram/reference/ob_point.md),
  [ob_line](https://wjschne.github.io/ggdiagram/reference/ob_line.md),
  [ob_segment](https://wjschne.github.io/ggdiagram/reference/ob_segment.md),
  or object with a center point (e.g.,
  [ob_circle](https://wjschne.github.io/ggdiagram/reference/ob_circle.md),
  [ob_rectangle](https://wjschne.github.io/ggdiagram/reference/ob_rectangle.md),
  [ob_ellipse](https://wjschne.github.io/ggdiagram/reference/ob_ellipse.md))

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Not used

## Value

numeric

## Examples

``` r
# Distance between two objects
p1 <- ob_point(0, 0)
p2 <- ob_point(3, 4)
distance(p1, p2)
#> [1] 5

# Distance between the endpoints of a segment
s1 <- ob_segment(p1, p2)
distance(s1)
#> [1] 5

# Distance between a point and a line
l1 <- ob_line(slope = 0, intercept = 1)
distance(p1, l1)
#> [1] 1

# Shortest distance between the edges of 2 circles
c1 <- ob_circle(p1, radius = 1)
c2 <- ob_circle(p2, radius = 2)
distance(c1, c2)
#> [1] 2
```
