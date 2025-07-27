# Rotate ----

#' Rotate an object in 2 dimensions
#'
#' @param x object
#' @param theta angle
#' @param origin length 2 vector  or point about which rotation occurs
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
#' @returns shape object
rotate <- S7::new_generic(
  name = "rotate",
  dispatch_args = c("x", "theta"),
  fun = function(x, theta, ..., origin = ob_point(0, 0)) {
    S7::S7_dispatch()})

# Rotate Line
S7::method(rotate, list(ob_line, ob_angle_or_numeric)) <- function(
  x,
  theta,
  origin = ob_point(0, 0), ...) {
# https://math.stackexchange.com/a/2278909
  if (!S7::S7_inherits(theta, ob_angle)) {
    theta <- degree(theta)
  }

A <- x@a * cos(theta) + x@b * sin(theta)
B <- x@b * cos(theta) - x@a * sin(theta)
C <- (x@a - A) * origin@x + (x@b - B) * origin@y + x@c
ob_line(a = A, b = B, c = C, ...)
}

# Rotate ----
## Rotate point ----
S7::method(rotate, list(ob_point, ob_angle_or_numeric)) <- function(
  x,
  theta,
  origin = ob_point(0, 0), ...) {

  if (!S7::S7_inherits(theta, ob_angle)) {
    theta <- degree(theta)
  }

  d <- tibble::tibble(
    x0 = x@x - origin@x,
    y0 = x@y - origin@y,
    th = theta@turn,
    xr = x0 * cospi(th * 2) - y0 * sinpi(th * 2) + origin@x,
    yr = x0 * sinpi(th * 2) + y0 * cospi(th * 2) + origin@y)


ob_point(x = d$xr, y = d$yr, style = x@style, ...)
}



## Rotate segment----
S7::method(rotate, list(ob_segment, ob_angle_or_numeric)) <- function(x, theta, origin = ob_point(0, 0), ...) {
  if (!S7::S7_inherits(theta, ob_angle)) {
    theta <- degree(theta)
  }
  p1r <- rotate(x@p1, theta, origin = origin)
  p2r <- rotate(x@p2, theta, origin = origin)
  style <- ob_style(...)
  x <- set_props(x, p1 = p1r, p2 = p2r)
  x@style <- style
  x


}





## Rotate centerpoint ----
S7::method(rotate, list(centerpoint, ob_angle_or_numeric)) <- function(
      x,
    theta,
    origin = ob_point(0, 0),
    ...) {
  if (!S7::S7_inherits(theta, ob_angle)) {
    theta <- degree(theta)
  }
  x@center <- rotate(x@center, theta, origin = origin, ...)

  if (S7::prop_exists(x, "angle")) {
    x@angle <- x@angle + theta
  }

  s <- rlang::list2(...)
  rlang::inject(set_props(x,!!!s))
  }

# # Rotate ob_ellipse
# S7::method(rotate, list(ob_ellipse, ob_angle_or_numeric)) <- function(x, theta, origin = ob_point(0, 0), ...) {
#   if (!S7::S7_inherits(theta, ob_angle)) {
#     theta <- degree(theta)
#   }
#
#   x@center <- rotate(x@center, theta, origin = origin, ...)
#   x@angle <- x@angle + theta
#   s <- rlang::list2(...)
#   rlang::inject(set_props(x, !!!s))
# }
# Rotate rectangle
# S7::method(rotate, list(ob_rectangle, ob_angle_or_numeric)) <- function(x, theta, origin = ob_point(0, 0), ...) {
#
#   x@center <- rotate(x@center, theta, origin = origin, ...)
#
#   ob_point(c(
#     rotate(ob_point(x@northeast), theta),
#     rotate(ob_point(x@northwest), theta),
#     rotate(ob_point(x@southwest), theta),
#     rotate(ob_point(x@southeast), theta)
#     ), ...)
# }
