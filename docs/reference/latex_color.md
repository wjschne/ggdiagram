# Surround TeX expression with a color command

Surround TeX expression with a color command

## Usage

``` r
latex_color(x, color)
```

## Arguments

- x:

  TeX expression

- color:

  color

## Value

string

## Examples

``` r
latex_color("X^2", "red")
#> [1] "{\\color[HTML]{FF0000} X^2}"
```
