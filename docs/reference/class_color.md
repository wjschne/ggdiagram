# color class

Useful for manipulating colors in R.

## Usage

``` r
class_color(
  color = character(0),
  hue = NULL,
  saturation = NULL,
  brightness = NULL,
  alpha = NULL,
  id = character(0)
)
```

## Arguments

- color:

  character (R color or hex code)

- hue:

  get or set the hue of a color (i.e., the h in the hsv model)

- saturation:

  get or set the saturation of a color (i.e., the s in the hsv model)

- brightness:

  get or set the brightness of a color (i.e., the v in the hsv model)

- alpha:

  get or set the transparency of a color

- id:

  character identifier

## Value

class_color object

## Additional properties

- `@transparentize`:

  function to return the color with a new transparency (i.e., alpha)

- `@lighten`:

  function to return a lighter color

- `@darken`:

  function to return a darker color

- `@red`:

  Gets or sets red component of rgb color

- `@green`:

  Gets or sets green component of rgb color

- `@blue`:

  Gets or sets blue component of rgb color

- `@mean`:

  Averages the rbga components of the colors

- `@tex`:

  Gets the TeX code for the color

## Examples

``` r
mycolor <- class_color("blue")
mycolor
#> 
#> ── <ggdiagram::class_color> ────────────────────────────────────────────────────
#> @ color: chr "#0000FFFF"
#> Other props: transparentize, lighten, darken, saturation, hue,
#>              brightness, alpha, red, green, blue, mean, tex, id
# Display html hexcode
c(mycolor)
#> [1] "#0000FFFF"
# Set transparency
mycolor@transparentize(.5)
#> 
#> ── <ggdiagram::class_color> ────────────────────────────────────────────────────
#> @ color: chr "#0000FF80"
#> Other props: transparentize, lighten, darken, saturation, hue,
#>              brightness, alpha, red, green, blue, mean, tex, id
# Lighten color
mycolor@lighten(.5)
#> 
#> ── <ggdiagram::class_color> ────────────────────────────────────────────────────
#> @ color: chr "#8282FFFF"
#> Other props: transparentize, lighten, darken, saturation, hue,
#>              brightness, alpha, red, green, blue, mean, tex, id
# Darken color
mycolor@darken(.5)
#> 
#> ── <ggdiagram::class_color> ────────────────────────────────────────────────────
#> @ color: chr "#000082FF"
#> Other props: transparentize, lighten, darken, saturation, hue,
#>              brightness, alpha, red, green, blue, mean, tex, id
```
