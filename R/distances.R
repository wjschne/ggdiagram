# distance----
#' Calculate distance between 2 points
#'
#' @param x an [ob_point], [ob_line], [ob_segment], or object with a center point (e.g., [ob_circle], [ob_rectangle], [ob_ellipse])
#' @param y an [ob_point], [ob_line], [ob_segment], or object with a center point (e.g., [ob_circle], [ob_rectangle], [ob_ellipse])
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Not used
#' @rdname distance
#' @returns numeric
#' @examples
#' # Distance between two objects
#' p1 <- ob_point(0, 0)
#' p2 <- ob_point(3, 4)
#' distance(p1, p2)
#'
#' # Distance between the endpoints of a segment
#' s1 <- ob_segment(p1, p2)
#' distance(s1)
#'
#' # Distance between a point and a line
#' l1 <- ob_line(slope = 0, intercept = 1)
#' distance(p1, l1)
#'
#' # Shortest distance between the edges of 2 circles
#' c1 <- ob_circle(p1, radius = 1)
#' c2 <- ob_circle(p2, radius = 2)
#' distance(c1, c2)
#' @export
distance <- S7::new_generic(name = "distance", c("x", "y"))
S7::method(distance, list(ob_point, ob_point)) <- function(x,y) {
  d <- (y - x)
  d@r
}
S7::method(distance, list(ob_point, S7::class_missing)) <- function(x,y) {
  x@r
}
S7::method(distance, list(ob_point, ob_line)) <- function(x,y) {
  abs(y@a * x@x + y@b * x@y + y@c) / sqrt(y@a * y@a + y@b * y@b)
}
S7::method(distance, list(ob_line, ob_point)) <- function(x,y) {
  distance(y, x)
}
S7::method(distance, list(ob_segment, S7::class_missing)) <- function(x,y) {
  distance(x@p1, x@p2)
}
S7::method(distance, list(ob_circle, ob_circle)) <- function(x,y) {
  d <- distance(x@center, y@center) - x@radius - y@radius
  d[d < 0] <- 0
  d
  # d <- (y@center - x@center)
  #
  #   if (x@radius + y@radius > distance(d)) {
  #     d <- ob_point(0,0)
  #   } else {
  #     px <- x@point_at(d@theta)
  #     py <- x@point_at(d@theta + pi)
  #     d <- py - px
  #   }
  #
  # abs(d@r)
}
S7::method(distance, list(ob_point, ob_circle)) <- function(x,y) {
  d <- y@center - x

    py <- y@point_at(radian(pi) + d@theta)
    d <- py - x

  abs(d@r)
}
S7::method(distance, list(ob_circle, ob_point)) <- function(x,y) {
  distance(y, x)
}

S7::method(distance, list(centerpoint, centerpoint)) <- function(x,y) {
  s <- ob_segment(x@center, y@center)
  p1 <- intersection(x, s)
  p2 <- intersection(y, s)
  distance(p1,p2)

}
