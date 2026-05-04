# class_aesthetics_list

list of aesthetics

## Usage

``` r
class_aesthetics_list(
  geom = function() NULL,
  style = character(0),
  mappable_bare = character(0),
  mappable_identity = character(0),
  not_mappable = character(0),
  required_aes = character(0),
  omit_names = character(0),
  inherit.aes = logical(0)
)
```

## Arguments

- geom:

  Which geom function converts the shape

- style:

  vector of style names

- mappable_bare:

  aesthetics used without identity function

- mappable_identity:

  aesthetics used with identity function

- not_mappable:

  properties that cannot be mapped and thus are created with separate
  geom objects for each unique combination of values

- required_aes:

  required aesthetics

- omit_names:

  properties that are ignored

- inherit.aes:

  Defaults to `FALSE` so that ggdiagram objects do not interfere with
  other layers in the ggplot

## Value

a class_aesthetics_list object
