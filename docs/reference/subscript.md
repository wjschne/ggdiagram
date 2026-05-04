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
ggdiagram() +
  ob_circle(label = ob_label(subscript("X", 1), size = 16)) +
  ob_circle(x = 3, label = ob_label(superscript("A", 2), size = 16))
```
