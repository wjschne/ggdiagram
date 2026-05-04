# Object Arrays

Make an array of shapes along a line

## Usage

``` r
ob_array(x, k = 2, sep = 1, where = "east", anchor = "center", ...)
```

## Arguments

- x:

  shape

- k:

  number of duplicate shapes to make

- sep:

  separation distance between shapes

- where:

  angle or named direction (e.g.,northwest, east, below, left)

- anchor:

  bounding box anchor

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to shape

## Value

An array of shapes of the same class as object passed to x

## Methods

`ob_array` is an S7 generic with methods available for the following
classes:

- [`ggdiagram::ob_circle`](https://wjschne.github.io/ggdiagram/reference/ob_circle.md)

- [`ggdiagram::ob_ellipse`](https://wjschne.github.io/ggdiagram/reference/ob_ellipse.md)

- [`ggdiagram::ob_point`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)

- [`ggdiagram::ob_rectangle`](https://wjschne.github.io/ggdiagram/reference/ob_rectangle.md)
