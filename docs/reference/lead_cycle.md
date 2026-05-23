# Finds the "previous" (lag) or "next" (lead) values in a vector or object with values at the end of the vector recycled to the beginning.

Finds the "previous" (lag) or "next" (lead) values in a vector or object
with values at the end of the vector recycled to the beginning.

## Usage

``` r
lead_cycle(x, n = 1L)

lag_cycle(x, n = 1L)
```

## Arguments

- x:

  A vector or ggdiagram object

- n:

  Positive integer of length 1, giving the number of positions to lag or
  lead by

## Value

A vector with the same type and size as x but with elements shifted and
cycled by n.

## Examples

``` r
lead_cycle(1:5)
#> [1] 2 3 4 5 1
lead_cycle(1:5, 2)
#> [1] 3 4 5 1 2
lag_cycle(1:5)
#> [1] 5 1 2 3 4
lag_cycle(1:5, 2)
#> [1] 4 5 1 2 3
```
