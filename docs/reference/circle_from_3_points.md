# Get a circle from 3 points

Get a circle from 3 points

## Usage

``` r
circle_from_3_points(p1, p2 = NULL, p3 = NULL, ...)
```

## Arguments

- p1:

  an ob_point of length 1 or length 3

- p2:

  an ob_point of length 1 or NULL

- p3:

  an ob_point of length 1 or NULL

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Pass arguments to ob_circle

## Value

ob_point object

## Examples

``` r
circle_from_3_points(ob_point(1,1),
                     ob_point(2,4),
                     ob_point(5,3))
#> 
#> ── <ob_circle> 
#> # A tibble: 1 × 3
#>       x     y     r
#>   <dbl> <dbl> <dbl>
#> 1     3     2  2.24
```
