# map_ob

A wrapper for
[purrr::map](https://purrr.tidyverse.org/reference/map.html). It takes a
ggdiagram object with multiple elements, applies a function to each
element within the object, and returns a ggdiagram object

## Usage

``` r
map_ob(.x, .f, ..., .progress = FALSE)
```

## Arguments

- .x:

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
