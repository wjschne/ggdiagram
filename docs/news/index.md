# Changelog

## ggdiagram 0.3.0 *2026-10-12*

### New features

- Color gradients for arrows. `ob_segment`, `ob_path`, `ob_bezier`, and
  `ob_arc` gain a `gradient` property: a vector of 2 or more colors (or
  a list of such vectors, one per object) that makes a smooth gradient
  from the arrow’s fins to its head. Gradients are drawn as grid
  gradient fills, so they require a graphics device that supports them
  (e.g., ragg, svg, or pdf). For paths, beziers, and arcs, the gradient
  follows a straight line from start to end, not the curve.
- New functions
  [`geom_arrow_segment_gradient()`](https://wjschne.github.io/ggdiagram/reference/geom_arrow_segment_gradient.md)
  and
  [`geom_arrow_gradient()`](https://wjschne.github.io/ggdiagram/reference/geom_arrow_gradient.md)
  apply the gradients in ordinary ggplot2 code (ggarrow does not accept
  gradient colors). The approach was inspired by
  [`ggforce::geom_link()`](https://ggforce.data-imaginist.com/reference/geom_link.html)
  and its color gradients along segments, by Thomas Lin Pedersen, the
  author of ggforce.
- Gradients are interpolated in the OKLab color space by default, which
  avoids the dull midpoints of RGB interpolation (e.g., blue to yellow
  no longer passes through gray). The gradient is sampled at many
  closely spaced stops, following the approach in Matt DesLauriers’ post
  [Perceptually Smooth Multi-Color Linear
  Gradients](https://observablehq.com/@mattdesl/perceptually-smooth-multi-color-linear-gradients).
  Use the `space` argument of the gradient geoms, or
  `options(ggdiagram.gradient_space = ...)`, to choose `"oklab"`,
  `"oklch"`, `"lab"`, `"lch"`, or `"rgb"`.
- `connect` now uses a color gradient by default when the `from` and
  `to` objects have different colors. An object’s color is its `color`,
  or its `fill` if `color` is `NA`, or black if both are `NA`. If the
  two colors are identical, no gradient is used and the connector has
  that color. Specifying `color`, `gradient`, or a style with a color
  overrides this behavior.
- `ob_style` and `ob_segment` gain `distort` (a ggarrow distortion,
  e.g., `"sinewave"`) and `force_arrow` (draw the arrowhead even if the
  segment is too short). `ob_path` gains `force_arrow`.

### Changes in behavior

- `ob_segment` now defaults to black when no color is specified (or
  inherited) unless `color = NA` is set explicitly. Objects derived from
  segments, such as `@midpoint()`, therefore carry `color = "black"`.
- The `connect` color now comes from the `from` object. Previously the
  `to` object’s color overrode it.
- `ob_path@segment` no longer carries arrowheads or fins, and each
  segment keeps the style of its own path.

### Documentation and internal changes

- New “Color gradients” sections in the segments, paths, Bézier, and
  arc/wedge articles.
- Reorganized source files: generics moved to `R/generics.R` and helper
  functions to `R/helpers.R`.
- Minimum R version is now 4.4.0.
- Added copyright holder (`cph`) role to the maintainer in
  `DESCRIPTION`.

## ggdiagram 0.2.1 *2026-06-12*

- Fixed bug in `round_probability` so that `NA` does not throw an error
  when digits are specified.

## ggdiagram 0.2.0 *2026-05-04*

CRAN release: 2026-05-23

- Better documentation of objects (used Posit Assistant to identify
  undocumented properties).
- More complete set of tests (used Posit Assistant to generate many of
  them)
- `ob_angle` objects (i.e., `degree`, `radian`, and `turn`) gain an
  `@upright` slot useful for preventing angled text from being upside
  down.
- `ob_reuleaux` and `ob_ngon` gain additional function slots similar to
  those of `ob_circle`.
- `ob_segment`, `ob_bezier`, and `connect` can set label positions with
  equal x and y coordinates via the `@set_label_x` and `@set_label_y`
  property functions.
- Added `map2_ob` function to map over 2 ggdiagram objects
- Added `rescale` function to resize ggdiagram objects
- Made `ob_intercept` a special case of `ob_ngon`.
- Breaking change: Renamed all instances of `@segments` to `@segment`
  for the sake of consistency.

## ggdiagram 0.1.3 *2025-10-15*

- [6](https://github.com/wjschne/ggdiagram/issues/6) ggforce 0.5.0 or
  higher is now required

## ggdiagram 0.1.2 *2025-09-15*

- Fix print method for `class_color`
- Fix bug in polygon slot for `ob_ellipse`

## ggdiagram 0.1.1 *2025-09-14*

CRAN release: 2025-09-14

- ggplot2 4.0.0 broke ggdiagram 0.1.0. New version requires ggplot2
  4.0.0 or higher.
- Fix print method for `class_color`
- Fix `@tangent_at` for ellipses when given a point

## ggdiagram 0.1.0 *2025-08-18*

CRAN release: 2025-08-19

- Initial CRAN submission.
