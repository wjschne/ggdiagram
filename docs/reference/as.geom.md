# as.geom function

Converts a ggdiagram shape to a ggplot2 geom

## Usage

``` r
as.geom(x, ...)
```

## Arguments

- x:

  a shape

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Pass arguments to ggplot2::geom_point

## Value

geom

## Details

Usually the `as.geom` function is not necessary to call explicitly
because it is called whenever a ggdiagram shape is added to a ggplot.
However, in complex situations (e.g., making a function that assembles
many objects), it is sometimes necessary to make the call explicitly.

## Examples

``` r
library(ggplot2)
c1 <- ob_circle(radius = 3)
ggplot() +
  as.geom(c1, fill = "black") +
  coord_equal()

```
