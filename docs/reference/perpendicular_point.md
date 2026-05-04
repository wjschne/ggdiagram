# Find point perpendicular to 2 points

Find point perpendicular to 2 points

## Usage

``` r
e1 %|-% e2

e1 %-|% e2
```

## Arguments

- e1:

  first ob_point

- e2:

  second ob_point

## Value

ob_point object

ob_point object

## Examples

``` r
x <- ob_point(0,0)
y <- ob_point(1,1)
# Find point perpendicular to x and y going vertically first
x %|-% y
#> 
#> ── <ob_point> 
#> # A tibble: 1 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1     0     1
# Find point perpendicular to x and y going horizontally first
x %-|% y
#> 
#> ── <ob_point> 
#> # A tibble: 1 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     0
```
