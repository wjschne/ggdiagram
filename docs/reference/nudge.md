# Move an object

Move an object

## Usage

``` r
nudge(object, x, y, ...)
```

## Arguments

- object:

  object

- x:

  nudge right and left

- y:

  nudge up and down

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  properties passed to style

## Value

object of same class as `object`

## Examples

``` r
ob_circle() |> nudge(x = 2)
#> 
#> ── <ob_circle> 
#> # A tibble: 1 × 3
#>       x     y     r
#>   <dbl> <dbl> <dbl>
#> 1     2     0     1
# Alternative to nudge:
ob_circle() + ob_point(2, 0)
#> 
#> ── <ob_circle> 
#> # A tibble: 1 × 3
#>       x     y     r
#>   <dbl> <dbl> <dbl>
#> 1     2     0     1
```
