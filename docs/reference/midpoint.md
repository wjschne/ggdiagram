# Get one or more points at positions from 0 to 1

It is possible to get more than one midpoint by specifying a position
vector with a length greater than 1. Position values outside 0 and 1
will usually work, but will be outside the object.

## Usage

``` r
midpoint(x, y, position = 0.5, ...)
```

## Arguments

- x:

  object

- y:

  object (can be omitted for segments and arcs)

- position:

  numeric vector. 0 is start, 1 is end. Defaults to .5

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

ob_point
