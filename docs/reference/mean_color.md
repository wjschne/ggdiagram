# Average across colors

Average across colors

## Usage

``` r
mean_color(x)
```

## Arguments

- x:

  color

## Value

string

## Examples

``` r
color_A <- "dodgerblue"
color_B <- "violet"
color_AB <- mean_color(c(color_A, color_B))
fills <- c(color_A,
           color_AB,
           color_B)
ggdiagram() +
  ob_circle(x = c(0, 3, 6),
            color = NA,
            fill = fills)
```
