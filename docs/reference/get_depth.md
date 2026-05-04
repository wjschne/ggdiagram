# Function to calculate hierarchy depth in lavaan models

Function to calculate hierarchy depth in lavaan models

## Usage

``` r
get_depth(x, model, depth = 0L, max_depth = 20)
```

## Arguments

- x:

  character vector of variables in a lavaan model

- model:

  character, lavaan fit object, or lavaan parameter table

- depth:

  initial depth

- max_depth:

  max depth at which to stop (prevents infinite loops for non-recursive
  models)

## Value

integer

## Examples

``` r
model <- "X =~ X1 + X2"
get_depth("X", model = model)
#> [1] 2
get_depth("X1", model = model)
#> [1] 1
```
