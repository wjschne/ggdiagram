# ggdiagram function

This is a convenient way to specify geom defaults

## Usage

``` r
ggdiagram(
  font_family = "sans",
  font_size = 11,
  linewidth = 0.5,
  point_size = 1.5,
  rect_linewidth = linewidth,
  theme_function = ggplot2::theme_void,
  ...
)
```

## Arguments

- font_family:

  font family

- font_size:

  font size in points

- linewidth:

  line width

- point_size:

  point size

- rect_linewidth:

  line width of rectangles

- theme_function:

  A complete [ggplot2
  theme](https://ggplot2.tidyverse.org/reference/ggtheme.html) function
  (e.g.,
  [ggplot2::theme_minimal](https://ggplot2.tidyverse.org/reference/ggtheme.html)).
  Defaults to
  [ggplot2::theme_void](https://ggplot2.tidyverse.org/reference/ggtheme.html)

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Arguments sent to
  [ggplot2::theme](https://ggplot2.tidyverse.org/reference/theme.html)

## Value

ggplot function

## Examples

``` r
ggdiagram(font_size = 20, font_family = "serif", linewidth = 3) +
   ob_circle(label = "Circle") +
   ob_rectangle(label = "Rectangle", x = 3, width = 3)
```
