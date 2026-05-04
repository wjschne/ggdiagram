# Centering signed numbers

A wrapper function for the signs::signs function. It adds a space to the
right side of negative numbers so that it appear as if the minus sign
does not affect the number's centering.

## Usage

``` r
signs_centered(x, space = NULL, encoding = "UTF-8", ...)
```

## Arguments

- x:

  a numeric vector

- space:

  a character to be added to negative numbers (defaults to a UTF-8
  figure space)

- encoding:

  type of encoding (defaults to UTF-8)

- ...:

  parameters passed to signs:signs

## Value

a vector of numbers converted to characters
