# Package index

## Shapes

Functions for creating shape objects

- [`ob_arc()`](https://wjschne.github.io/ggdiagram/reference/ob_arc.md)
  [`ob_wedge()`](https://wjschne.github.io/ggdiagram/reference/ob_arc.md)
  [`ob_circular_segment()`](https://wjschne.github.io/ggdiagram/reference/ob_arc.md)
  : ob_arc class
- [`ob_bezier()`](https://wjschne.github.io/ggdiagram/reference/ob_bezier.md)
  : The ob_bezier (i.e., bezier curve) class
- [`ob_circle()`](https://wjschne.github.io/ggdiagram/reference/ob_circle.md)
  : ob_circle class
- [`ob_covariance()`](https://wjschne.github.io/ggdiagram/reference/ob_covariance.md)
  : Covariance object
- [`ob_ellipse()`](https://wjschne.github.io/ggdiagram/reference/ob_ellipse.md)
  : ob_ellipse class
- [`ob_intercept()`](https://wjschne.github.io/ggdiagram/reference/ob_intercept.md)
  : ob_intercept
- [`ob_label()`](https://wjschne.github.io/ggdiagram/reference/ob_label.md)
  : ob_label class
- [`ob_latex()`](https://wjschne.github.io/ggdiagram/reference/ob_latex.md)
  : ob_latex class
- [`ob_line()`](https://wjschne.github.io/ggdiagram/reference/ob_line.md)
  : ob_line class
- [`ob_ngon()`](https://wjschne.github.io/ggdiagram/reference/ob_ngon.md)
  : The ob_ngon (regular polygon) class
- [`ob_path()`](https://wjschne.github.io/ggdiagram/reference/ob_path.md)
  : The ob_path class
- [`ob_point()`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  [`ob_polar()`](https://wjschne.github.io/ggdiagram/reference/ob_point.md)
  : ob_point
- [`ob_polygon()`](https://wjschne.github.io/ggdiagram/reference/ob_polygon.md)
  : The ob_polygon class
- [`ob_rectangle()`](https://wjschne.github.io/ggdiagram/reference/ob_rectangle.md)
  : ob_rectangle class
- [`ob_reuleaux()`](https://wjschne.github.io/ggdiagram/reference/ob_reuleaux.md)
  : Reuleaux polygon
- [`ob_segment()`](https://wjschne.github.io/ggdiagram/reference/ob_segment.md)
  : ob_segment class
- [`ob_variance()`](https://wjschne.github.io/ggdiagram/reference/ob_variance.md)
  : Variance object

## Alter shapes

Functions that alter shapes

- [`nudge()`](https://wjschne.github.io/ggdiagram/reference/nudge.md) :
  Move an object
- [`place()`](https://wjschne.github.io/ggdiagram/reference/place.md) :
  Place an object a specified distance from another object
- [`rescale()`](https://wjschne.github.io/ggdiagram/reference/rescale.md)
  : Rescale an object in 2 dimensions
- [`resect()`](https://wjschne.github.io/ggdiagram/reference/resect.md)
  : resect
- [`rotate()`](https://wjschne.github.io/ggdiagram/reference/rotate.md)
  : Rotate an object in 2 dimensions

## Get information about shapes

Functions that compute information about shapes

- [`circle_from_3_points()`](https://wjschne.github.io/ggdiagram/reference/circle_from_3_points.md)
  : Get a circle from 3 points
- [`connect()`](https://wjschne.github.io/ggdiagram/reference/connect.md)
  : Arrow connect one shape to another
- [`distance()`](https://wjschne.github.io/ggdiagram/reference/distance.md)
  : Calculate distance between 2 points
- [`get_tibble()`](https://wjschne.github.io/ggdiagram/reference/get_tibble.md)
  [`get_tibble_defaults()`](https://wjschne.github.io/ggdiagram/reference/get_tibble.md)
  : Get object data with styles in a tibble
- [`intersection()`](https://wjschne.github.io/ggdiagram/reference/intersection.md)
  : intersection of 2 objects (e.g., lines)
- [`intersection_angle()`](https://wjschne.github.io/ggdiagram/reference/intersection_angle.md)
  : Compute the angle of the intersection of two objects
- [`inside()`](https://wjschne.github.io/ggdiagram/reference/inside.md)
  : is an ob_point inside a shape ?
- [`midpoint()`](https://wjschne.github.io/ggdiagram/reference/midpoint.md)
  : Get one or more points at positions from 0 to 1
- [`` `%|-%` ``](https://wjschne.github.io/ggdiagram/reference/perpendicular_point.md)
  [`` `%-|%` ``](https://wjschne.github.io/ggdiagram/reference/perpendicular_point.md)
  : Find point perpendicular to 2 points
- [`projection()`](https://wjschne.github.io/ggdiagram/reference/projection.md)
  : Find projection of a point on an object (e.g., line or segment)

## Automation

Functions for working with many shapes

- [`bind()`](https://wjschne.github.io/ggdiagram/reference/bind.md) :
  bind method
- [`data2shape()`](https://wjschne.github.io/ggdiagram/reference/data2shape.md)
  : Make shapes from data
- [`map_ob()`](https://wjschne.github.io/ggdiagram/reference/map_ob.md)
  : Map over a ggdiagram object
- [`map2_ob()`](https://wjschne.github.io/ggdiagram/reference/map2_ob.md)
  : Map over two ggdiagram objects
- [`ob_array()`](https://wjschne.github.io/ggdiagram/reference/ob_array.md)
  : Object Arrays
- [`ob_shape_list()`](https://wjschne.github.io/ggdiagram/reference/ob_shape_list.md)
  : ob_shape_list
- [`ob_style()`](https://wjschne.github.io/ggdiagram/reference/ob_style.md)
  : ob_style class
- [`redefault()`](https://wjschne.github.io/ggdiagram/reference/redefault.md)
  : Make a variant of a function with alternate defaults
- [`unbind()`](https://wjschne.github.io/ggdiagram/reference/unbind.md)
  : unbind

## Angles

Functions about angles

- [`ob_angle()`](https://wjschne.github.io/ggdiagram/reference/ob_angle.md)
  [`degree()`](https://wjschne.github.io/ggdiagram/reference/ob_angle.md)
  [`radian()`](https://wjschne.github.io/ggdiagram/reference/ob_angle.md)
  [`turn()`](https://wjschne.github.io/ggdiagram/reference/ob_angle.md)
  : ob_angle
- [`intersection_angle()`](https://wjschne.github.io/ggdiagram/reference/intersection_angle.md)
  : Compute the angle of the intersection of two objects

## Color

Functions about colors

- [`class_color()`](https://wjschne.github.io/ggdiagram/reference/class_color.md)
  : color class
- [`latex_color()`](https://wjschne.github.io/ggdiagram/reference/latex_color.md)
  : Surround TeX expression with a color command
- [`mean_color()`](https://wjschne.github.io/ggdiagram/reference/mean_color.md)
  : Average across colors

## Labels

Functions making and altering labels

- [`equation()`](https://wjschne.github.io/ggdiagram/reference/equation.md)
  : equation
- [`label_object()`](https://wjschne.github.io/ggdiagram/reference/label_object.md)
  : Automatic label for objects
- [`ob_label()`](https://wjschne.github.io/ggdiagram/reference/ob_label.md)
  : ob_label class
- [`ob_latex()`](https://wjschne.github.io/ggdiagram/reference/ob_latex.md)
  : ob_latex class
- [`polar2just()`](https://wjschne.github.io/ggdiagram/reference/polar2just.md)
  : Convert hjust and vjust parameters from polar coordinates
- [`round_probability()`](https://wjschne.github.io/ggdiagram/reference/round_probability.md)
  : Probability rounding
- [`signs_centered()`](https://wjschne.github.io/ggdiagram/reference/signs_centered.md)
  : Centering signed numbers
- [`subscript()`](https://wjschne.github.io/ggdiagram/reference/subscript.md)
  [`superscript()`](https://wjschne.github.io/ggdiagram/reference/subscript.md)
  : Create subscripts

## Arrowheads

Functions for getting and setting the default arrowhead

- [`arrowhead()`](https://wjschne.github.io/ggdiagram/reference/arrowhead.md)
  [`set_default_arrowhead()`](https://wjschne.github.io/ggdiagram/reference/arrowhead.md)
  : Return default arrowhead

## Plotting functions

Functions for plotting

- [`ggdiagram()`](https://wjschne.github.io/ggdiagram/reference/ggdiagram.md)
  : ggdiagram function
- [`as.geom()`](https://wjschne.github.io/ggdiagram/reference/as.geom.md)
  : as.geom function

## Other functions

Helper functions

- [`get_depth()`](https://wjschne.github.io/ggdiagram/reference/get_depth.md)
  : Function to calculate hierarchy depth in lavaan models
- [`lead_cycle()`](https://wjschne.github.io/ggdiagram/reference/lead_cycle.md)
  [`lag_cycle()`](https://wjschne.github.io/ggdiagram/reference/lead_cycle.md)
  : Finds the "previous" (lag) or "next" (lead) values in a vector or
  object with values at the end of the vector recycled to the beginning.
