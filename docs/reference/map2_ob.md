# Map over two ggdiagram objects

A wrapper for
[purrr::map2](https://purrr.tidyverse.org/reference/map2.html). It takes
two ggdiagram objects with multiple elements, applies a function to each
element within the objects, and returns a ggdiagram object

## Usage

``` r
map2_ob(.x, .y, .f, ..., .progress = FALSE)
```

## Arguments

- .x:

  a ggdiagram object

- .y:

  a ggdiagram object

- .f:

  a function that returns a ggdiagram object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  arguments passed to .f

- .progress:

  display progress if TRUE

## Value

a ggdiagram object
