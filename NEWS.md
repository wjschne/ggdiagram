# ggdiagram 0.2.1 _2026-06-12_

* Fixed bug in `round_probability` so that `NA` does not throw an error when digits are specified.

# ggdiagram 0.2.0 _2026-05-04_

* Better documentation of objects (used Posit Assistant to identify undocumented properties).
* More complete set of tests (used Posit Assistant to generate many of them)
* `ob_angle` objects (i.e., `degree`, `radian`, and `turn`) gain an `@upright` slot useful for preventing angled text from being upside down.
* `ob_reuleaux` and `ob_ngon` gain additional function slots similar to those of `ob_circle`.
* `ob_segment`, `ob_bezier`, and `connect` can set label positions with equal x and y coordinates via the `@set_label_x` and `@set_label_y` property functions.
* Added `map2_ob` function to map over 2 ggdiagram objects
* Added `rescale` function to resize ggdiagram objects
* Made `ob_intercept` a special case of `ob_ngon`.
* Breaking change: Renamed all instances of `@segments` to `@segment` for the sake of consistency.

# ggdiagram 0.1.3 _2025-10-15_

* [6](https://github.com/wjschne/ggdiagram/issues/6) ggforce 0.5.0 or higher is now required

# ggdiagram 0.1.2 _2025-09-15_

* Fix print method for `class_color`
* Fix bug in polygon slot for `ob_ellipse`

# ggdiagram 0.1.1 _2025-09-14_

* ggplot2 4.0.0 broke ggdiagram 0.1.0. New version requires ggplot2 4.0.0 or higher. 
* Fix print method for `class_color`
* Fix `@tangent_at` for ellipses when given a point



# ggdiagram 0.1.0 _2025-08-18_

* Initial CRAN submission.
