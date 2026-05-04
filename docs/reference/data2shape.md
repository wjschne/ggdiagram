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

ggdiagram() +
  data2shape(d, ob_circle)
```
