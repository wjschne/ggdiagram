# Probability rounding

Rounds to significant digits, removing leading zeros.

## Usage

``` r
round_probability(
  p,
  accuracy = 0.01,
  digits = NULL,
  max_digits = NULL,
  remove_leading_zero = TRUE,
  round_zero_one = TRUE,
  phantom_text = NULL,
  phantom_color = NULL
)
```

## Arguments

- p:

  probability

- accuracy:

  smallest increment

- digits:

  significant digits

- max_digits:

  maximum rounding digits

- remove_leading_zero:

  remove leading zero

- round_zero_one:

  round 0 and 1

- phantom_text:

  invisible text inserted on the right

- phantom_color:

  color of phantom text

## Value

a character vector

## Examples

``` r
round_probability(c(0, .0012, .012, .12, .99, .992, .9997, 1), digits = 2)
#> [1] "0"     ".0012" ".012"  ".12"   ".99"   ".992"  ".9997" "1"    
```
