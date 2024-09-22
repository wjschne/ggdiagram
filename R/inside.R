# inside----
#' is an ob_point inside a shape ?
#'
#' @param x object
#' @param y object
#' @export
inside <- new_generic("inside", c("x", "y"), fun = function(x,y) S7_dispatch())
method(inside, list(ob_point, ob_rectangle)) <- function(x,y) {
 insideTF <-  (x@x <= y@northeast@x &&
    x@x >= y@northwest@x &&
    x@y <= y@northeast@y &&
    x@y >= y@southeast@y) * 2 - 1

 if (length(intersection(x,y)) > 0) {
   insideTF <- 0
 }

 insideTF
}

method(inside, list(ob_point, ob_circle)) <- function(x,y) {
  xc <- x - y@center
  -1 * sign(xc@r - y@radius)
}

method(inside, list(ob_point, ob_ellipse)) <- function(x,y) {
  rx <- rotate(x, theta = -1*y@angle, origin = y@center)
  xc <- rx - y@center
  -1 * sign((xc@x / y@a) ^ y@m1 + (xc@y / y@b) ^ y@m1 - 1)
}

