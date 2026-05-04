# Return default arrowhead

The arrowhead function returns the default arrowhead. The
set_default_arrowhead function will change the default arrowhead in the
current R session. For details about making arrowheads, see the
[ggarrow](https://teunbrand.github.io/ggarrow/articles/customisation.html)
and [arrowheadr](https://wjschne.github.io/arrowheadr/) packages.

## Usage

``` r
arrowhead()

set_default_arrowhead(m = NULL)
```

## Arguments

- m:

  A matrix used to make a ggarrow arrowhead

## Value

2-column matrix

previous default arrowhead

## Examples

``` r
arrowhead()
#> 
#> ── <class_arrowhead> 
#> A matrix with 2 columns and 200 rows. First 6 rows:
#>          x    y
#> [1,] -0.39 0.00
#> [2,] -0.39 0.02
#> [3,] -0.39 0.04
#> [4,] -0.40 0.06
#> [5,] -0.40 0.08
#> [6,] -0.40 0.10
# Set new default
set_default_arrowhead(ggarrow::arrow_head_wings(offset = 25))
arrowhead()
#> 
#> ── <class_arrowhead> 
#> A matrix with 2 columns and 4 rows.
#>          x     y
#> [1,]  1.00  0.00
#> [2,] -0.48  0.69
#> [3,]  0.00  0.00
#> [4,] -0.48 -0.69
# restore default
set_default_arrowhead()
arrowhead()
#> 
#> ── <class_arrowhead> 
#> A matrix with 2 columns and 200 rows. First 6 rows:
#>          x    y
#> [1,] -0.39 0.00
#> [2,] -0.39 0.02
#> [3,] -0.39 0.04
#> [4,] -0.40 0.06
#> [5,] -0.40 0.08
#> [6,] -0.40 0.10
```
