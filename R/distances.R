# distance----
#' Calculate distance between 2 points
#'
#' @param x a point, line, segment, or circle
#' @param y a point, line, or circle
#' @param center logical. if the distance between 2 circles should be calculated from their centers or their edges
#' @rdname distance
#' @return numeric
#' @examples
#' # Distance between two objects
#' p1 <- point(0, 0)
#' p2 <- point(3, 4)
#' distance(p1, p2)
#'
#' # Distance between the endpoints of a segment
#' s1 <- segment(p1, p2)
#' distance(s1)
#'
#' # Distance between a point and a line
#' l1 <- line(slope = 0, intercept = 1)
#' distance(p1, l1)
#'
#' # Shortest distance between the edges of 2 circles
#' c1 <- circle(p1, radius = 1)
#' c2 <- circle(p2, radius = 2)
#' distance(c1, c2)
#'
#' # Distance between the centers of 2 circles
#' distance(c1, c2, center = TRUE)
#' @export
distance <- new_generic("distance", c("x", "y"))
method(distance, list(point, point)) <- function(x,y) {
  d <- (y - x)
  d@r
}
method(distance, list(point, class_missing)) <- function(x,y) {
  x@r
}
method(distance, list(point, line)) <- function(x,y) {
  abs(y@a * x@x + y@b * x@y + y@c) / sqrt(y@a * y@a + y@b * y@b)
}
method(distance, list(line, point)) <- function(x,y) {
  distance(y, x)
}



method(distance, list(segment, class_missing)) <- function(x,y) {
  distance(x@p1, x@p2)
}

# add when circles are ready

method(distance, list(circle, circle)) <- function(x,y, center = FALSE) {
  d <- (y@center - x@center)
  if (!center) {
    if (x@radius + y@radius > distance(d)) {
      d <- point(0,0)
    } else {
      px <- x@point_at_theta(d@theta)
      py <- x@point_at_theta(d@theta + pi)
      d <- py - px
    }

  }
  d@r
}
method(distance, list(point, circle)) <- function(x,y, center = FALSE) {
  d <- y@center - x
  if (!center) {    
    py <- y@point_at_theta(radian(pi) + d@theta)
    d <- py - x
  }
  d@r
}
method(distance, list(circle, point)) <- function(x,y, center = FALSE) {
  distance(y, x)
}