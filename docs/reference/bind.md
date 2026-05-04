# bind method

bind method

## Usage

``` r
bind(x, ...)
```

## Arguments

- x:

  list of objects to bind

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

a bound object of same class as x (or list of objects if x contains
objects of different types)

## Examples

``` r
bind(c(ob_point(1,2), ob_point(3,4)))
#> 
#> ── <ob_point> 
#> # A tibble: 2 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     2
#> 2     3     4
bind(c(ob_circle(ob_point(0,0), radius = 1),
       ob_circle(ob_point(1,1), radius = 2)))
#> 
#> ── <ob_circle> 
#> # A tibble: 2 × 3
#>       x     y     r
#>   <dbl> <dbl> <dbl>
#> 1     0     0     1
#> 2     1     1     2
```
