# Make shapes from data

Allows a data.frame or tibble to be converted to shape objects.

## Usage

``` r
data2shape(data, shape)
```

## Arguments

- data:

  data.frame or tibble

- shape:

  shape function

## Value

shape object

## Examples

``` r
d <- data.frame(
  x = 1:2,
  y = 1:2,
  fill = c("blue", "forestgreen"),
  color = NA,
  radius = c(.25,0.5))

data2shape(d, ob_circle)
#> 
#> ── <ob_circle> 
#> # A tibble: 2 × 5
#>       x     y     r color fill       
#>   <int> <int> <dbl> <chr> <chr>      
#> 1     1     1  0.25 NA    blue       
#> 2     2     2  0.5  NA    forestgreen
```
