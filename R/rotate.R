# Rotate ----

#' Rotate an object in 2 dimensions
#'
#' @param x object
#' @param theta angle
#' @param origin length 2 vector  or point about which rotation occurs
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
rotate <- new_generic(
  name = "rotate",
  dispatch_args = c("x", "theta"),
  fun = function(x, theta, ..., origin = point(0, 0)) {
    S7_dispatch()})

# Rotate Line
method(rotate, list(line, class_angle_or_numeric)) <- function(
  x,
  theta,
  origin = point(0, 0), ...) {
# https://math.stackexchange.com/a/2278909
  if (!S7_inherits(theta, class_angle)) theta = degree(theta)
A <- x@a * cos(theta) + x@b * sin(theta)
B <- x@b * cos(theta) + x@a * sin(theta)
C <- (x@a - A) * origin@x + (x@b - B) * origin@y + x@c
line(a = A, b = B, c = C, ...)
}



# Rotate ----
method(rotate, list(point, class_angle_or_numeric)) <- function(
  x,
  theta,
  origin = point(0, 0), ...) {

  if (!S7_inherits(theta, class_angle)) theta <- degree(theta)

  d <- tibble::tibble(x0 = x@x - origin@x,
              y0 = x@y - origin@y,
              theta = theta@radian)




xr <- purrr::map(unique(d$theta), \(th) {
  cbind(x = d$x0, y = d$y0) |>
  rotate2columnmatrix(th) |>
  `colnames<-`(c("x", "y")) |>
  tibble::as_tibble()}) |>
  dplyr::bind_rows() |>
  as.matrix()




dimnames(xr) <- list(NULL, NULL)


origin + point(x = xr[,1], y = xr[, 2], style = x@style, ...)
}



# Rotate segment
method(rotate, list(segment, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0), ...) {
  p1r <- rotate(x@p1, theta, origin = origin)
  p2r <- rotate(x@p2, theta, origin = origin)
  style <- style(...)
  x <- set_props(x, p1 = p1r, p2 = p2r)
  x@style <- style
  x


}


# # Rotate point


# Rotate centerpoint
method(rotate, list(centerpoint, class_angle_or_numeric)) <- function(
      x,
    theta,
    origin = point(0, 0),
    ...) {
  x_center_r <- rotate(x@center, theta, origin = origin, ...)
  x@center <- x_center_r
  s <- rlang::list2(...)
  rlang::inject(set_props(x,!!!s))
  }

# Rotate ellipse
method(rotate,
       list(ellipse, class_angle_or_numeric)) <- function(
    x,
    theta,
    origin = point(0, 0),
    ...) {
  x@center <- rotate(x@center, theta, origin = origin, ...)
  x@angle <- x@angle + theta
  s <- rlang::list2(...)
  rlang::inject(set_props(x,!!!s))
       }
# Rotate rectangle
method(rotate, list(rectangle, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0), ...) {

  point(c(
    rotate(point(x.northeast), theta),
    rotate(point(x.northwest), theta),
    rotate(point(x.southwest), theta),
    rotate(point(x.southeast), theta)
    ), ...)
}
