# Make a variant of a function with alternate defaults

Makes a copy of a function with new defaults. Similar to
[`purrr::partial`](https://purrr.tidyverse.org/reference/partial.html)
except that arguments with new defaults still accept input.

## Usage

``` r
redefault(.f, ...)
```

## Arguments

- .f:

  function

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  new defaults

## Value

function

## Examples

``` r
squircle <- redefault(ob_ellipse, m1 = 4)
squircle(a = 3)
#> 
#> ── <ob_ellipse> 
#> # A tibble: 1 × 7
#>       x     y     a     b angle    m1    m2
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     0     0     3     3     0     4     4
```
