# Create subscripts

Create subscripts

Create superscript

## Usage

``` r
subscript(x, subscript = seq(length(x)), output = c("markdown", "latex"))

superscript(x, superscript = seq(length(x)), output = c("markdown", "latex"))
```

## Arguments

- x:

  string

- subscript:

  subscript

- output:

  Can be `markdown` (default) or `latex`

- superscript:

  superscript

## Value

text

string

## Examples

``` r
subscript("X", 1)
#> [1] "X<sub>1</sub>"
superscript("A", 2)
#> [1] "A<sup>2</sup>"
```
