# Intersection angle----
#' Compute the angle of the intersection of two objects
#'
#' @param x an object (point, segment, line)
#' @param y an object (point, segment, line)
#' @param degrees return angle in degrees if TRUE
#' @return an angle in radians
#' @export
intersection_angle <- new_generic("intersection_angle", c("x", 'y'))
method(intersection_angle, list(line, line)) <- function(x, y) {
  y@angle - x@angle

}
method(intersection_angle, list(segment, segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@line@angle - x@line@angle
  } else
    NA_real_
}
method(intersection_angle, list(line, segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@angle - x@line@angle
  } else
    NA_real_
}
method(intersection_angle, list(segment, line)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@line@angle - x@angle
  } else
    NA_real_
}

# intersection----
#' intersection of 2 objects (e.g., lines)
#'
#' @param x object
#' @param y object
#' @export
intersection <- new_generic("intersection", c("x", "y"))
method(intersection, list(line, line)) <- function(x,y) {
  c_p <- x@a * y@b - y@a * x@b
  if (identical(TRUE, all.equal(c_p, 0))) {
    p <- list()
  } else {
    a_p <- (x@b * y@c - y@b * x@c)
    b_p <- (x@c * y@a - y@c * x@a)

    p <- point(a_p / c_p, b_p / c_p)
  }
  p
}
method(intersection, list(segment, segment)) <- function(x,y) {

  i_line <- intersection(x@line, y@line)
  p <- list()
  if (length(i_line) > 0) {
    A <- t(rbind((x@p2 - x@p1)@xy,
                 (y@p1 - y@p2)@xy))
    B <- t((y@p1 - x@p1)@xy)
    C <- solve(A, B)
    if (all(C >= 0 & C <= 1)) {
      p <- i_line
    }
  }
  p
}



method(intersection, list(line, segment)) <- function(x,y) {
  intersection(intersection(x, y@line), y)
}

method(intersection, list(segment, line)) <- function(x,y) {
  intersection(y, x)
}

method(intersection, list(line, circle)) <- function(x,y) {
  # https://cp-algorithms.com/geometry/circle-line-intersection.html
  c0 <- circle(center = point(0,0), radius = y@radius)
  A <- x@a
  B <- x@b
  C <- x@c + A * y@center@x + B * y@center@y
  d0 <- abs(C) / sqrt(A ^ 2 + B ^ 2)
  A2B2 <- (A * A + B * B)
  x0 <- -A * C / A2B2
  y0 <- -B * C / A2B2
  if (C * C > y@radius * y@radius * A2B2 + .Machine$double.eps) {
    p <- list()
  } else if (
    abs(C * C - y@radius * y@radius * A2B2) < 3 * .Machine$double.eps) {
    p <- point(x0 , y0) + y@center
  } else {
    d <- y@radius * y@radius - C * C / A2B2
    m <- sqrt(d / A2B2)
    ax <- x0 + B * m
    bx <- x0 - B * m
    ay <- y0 - A * m
    by <- y0 + A * m
    pa <- point(ax, ay) + y@center
    pb <- point(bx, by) + y@center
    p <- c(pa, pb)
  }
  p

}

method(intersection, list(circle, line)) <- function(x,y) {
  intersection(y,x)
}

method(intersection, list(point, line)) <- function(x, y) {
  if (identical(TRUE, all.equal(0, y@a * x@x + y@b * x@y + y@c))) {
    x
  } else {
    list()
  }
}
method(intersection, list(line, point)) <- function(x, y) {
  intersection(y, x)
}


method(intersection, list(point, segment)) <- function(x, y) {
  if (identical(x, intersection(x, y@line))) {
    xp1 <- distance(x, y@p1)
    xp2 <- distance(x, y@p2)
    p1p2 <- distance(y)
    if (identical(TRUE, all.equal(p1p2, xp1 + xp2))) {
      x
    } else {
      list()
    }
  } else {
    list()
  }
}

method(intersection, list(segment, point)) <- function(x, y) {
  intersection(y, x)
}

method(intersection, list(line, rectangle)) <- function(x, y) {
  unique(c(intersection(x, segment(p1 = y@northeast,
                            p2 = y@northwest)),
    intersection(x, segment(p1 = y@northwest,
                            p2 = y@southwest)),
    intersection(x, segment(p1 = y@southwest,
                            p2 = y@southeast)),
    intersection(x, segment(p1 = y@southeast,
                            p2 = y@northeast))
  ))
}
method(intersection, list(rectangle, line)) <- function(x, y) {
  intersection(y, x)
}

method(intersection, list(segment, rectangle)) <- function(x, y) {
  unique(c(intersection(x, segment(p1 = y@northeast,
                                   p2 = y@northwest)),
           intersection(x, segment(p1 = y@northwest,
                                   p2 = y@southwest)),
           intersection(x, segment(p1 = y@southwest,
                                   p2 = y@southeast)),
           intersection(x, segment(p1 = y@southeast,
                                   p2 = y@northeast))
  ))
}
method(intersection, list(rectangle, segment)) <- function(x, y) {
  intersection(y, x)
}

method(intersection, list(point, rectangle)) <- function(x, y) {
  unique(c(intersection(x, segment(p1 = y@northeast,
                                   p2 = y@northwest)),
           intersection(x, segment(p1 = y@northwest,
                                   p2 = y@southwest)),
           intersection(x, segment(p1 = y@southwest,
                                   p2 = y@southeast)),
           intersection(x, segment(p1 = y@southeast,
                                   p2 = y@northeast))
  ))
}

method(intersection, list(rectangle, point)) <- function(x, y) {
  intersection(y, x)
}


method(intersection,
       list(segment, ellipse)) <- function(x, y, sep = .01) {
  p <- seq(0,1,sep)
  sp <-midpoint(x,position = p)
  i <- inside(sp, y)
  d <- data.frame(p = p, i = i, lag_i = c(i[-1], 2))
  d_change <- d[d$i * d$lag_i == -1,]
  d_on <- d[d$i == 0, ]
  if (nrow(d_change) == 0 && nrow(d_on) == 0) {
    return(NULL)
  } else {
    pp <- list()
    for (ii in d_on$p) {
      pp <- c(pp, midpoint(x, position = ii))
    }

    for (ii in d_change$p) {
      s <- segment(
        midpoint(x, position = ii),
        midpoint(x, position = ii + sep))

      if (distance(s) < (.1 ^ 15)) {
        pp <- c(pp, s@p1)
      } else {
        pp <- c(pp, intersection(s, y, sep = .5))
      }
    }
  }
 pp

}


# points(x = intersect_line_segment@x, y = intersect_line_segment@y)
method(intersection, list(line, ellipse)) <- function(x, y) {
  # theta <- angle(degree = seq(0, 360))
  x <- line(slope = 1, intercept = 0)
  y <- ellipse(center = point(0,0),a = 1, b = 1)
  
  eps <- distance(y@point_at_theta(radian(0)), y@point_at_theta(radian(.Machine$double.eps)))
  # eps <- .00001
  # par(pty = "s")
  xmax <- max(y@xy[,1])
  xmin <- min(y@xy[,1])
  ymax <- max(y@xy[,2])
  ymin <- min(y@xy[,2])
  if (is.infinite(x@slope)) {
    s <- segment(point(x@xintercept,ymin), point(x@xintercept,ymax))
  } else {
    s1 <- segment(x@point_at_x(xmin),
                  x@point_at_x(xmax))
                  
    s2 <- segment(point((ymin - x@intercept) / x@slope, ymin),
                  point((ymax - x@intercept) / x@slope,ymax))
    if (distance(s1) < distance(s2)) {
      s <- s1
    } else {
      s <- s2
    }
  }



  d <- s1@p2 - s1@p1
  s_points <- s1@p1 + seq(0,1,.5) * d
  c_points <- s_points - y@center


  test_x <- seq(xmax, ymax, length.out = 1000)
  text_y <- x@slope * test_x + x@intercept

  delta_angle <- 2
  current_angle <- 0

  m1 <- point(1,0)
  first <- FALSE
  second <- FALSE

  line_ellipse_intersection <- list()
  i <- 0

  while (current_angle < 362 && !(m1@r < eps * 10) && delta_angle > .Machine$double.eps * 10) {
    s <- segment(anchor(y, angle(degree = current_angle)),
                 anchor(y, angle(degree = current_angle + delta_angle)))
    intersect_line_segment <- intersection(x, s)
    # cat(paste0(distance(s), "\n"))



    if (length(intersect_line_segment) > 0) {
      first <- TRUE
      s_low <- segment(
        anchor(y, angle(degree = current_angle)),
        anchor(y, angle(degree = current_angle + delta_angle / 2)))

      s_high <- segment(
        anchor(y, angle(degree = current_angle + delta_angle / 2)),
        anchor(y, angle(degree = current_angle + delta_angle )))

      s_low@p1 == s@p1
      s_low@p1 == s_high@p1
      s_low@p2 == s_high@p1
      s_low@p1 == s_low@p2

      i_test_low <- intersection(x, s_low@line)
      i_test_high <- intersection(x, s_high@line)

      if (length(i_test_low) + length(i_test_high) == 0) {
        stop("wtf")
        s_low <- segment(
          anchor(
            y,
            angle(
              degree = current_angle - delta_angle * 0.0000001)),
          anchor(
            y,
            angle(degree = current_angle + .5 * delta_angle)))

        s_high <- segment(
          anchor(
            y,
            angle(degree = current_angle + .5 * delta_angle)),
          anchor(
            y,
            angle(
              degree = current_angle + delta_angle * 1.0000001 )))
        i_test_low <- intersection(x, s_low@line)
        i_test_high <- intersection(x, s_high@line)

        }

      delta_angle <- delta_angle / 2

      if (length(i_test_low) == 0) {
        current_angle <- current_angle + delta_angle
      }





      d1 <- s@p1 - intersect_line_segment
      m1 <- midpoint(s) - intersect_line_segment
      if (d1@r < eps * 10) {
        m1 <- point(0,0)
        line_ellipse_intersection <- s@p1
      }
      d2 <- s@p2 - intersect_line_segment
      if (d2@r < eps * 10) {
        m1 <- point(0,0)
        line_ellipse_intersection <- s@p2
      }
    } else {
      # delta_angle <- delta_angle * 1.01
      i <- i + 1
      if (delta_angle < 2) {
        if (first && !second) {
          line_ellipse_intersection$first <- y@point_theta(degree(current_angle))
          second <- TRUE
        }
        if (second) {
          line_ellipse_intersection$second <- y@point_theta(degree(current_angle))
        }

        delta_angle <- 2
        current_angle <- current_angle + delta_angle

      } else {
        current_angle <- current_angle + delta_angle
      }

    }
  }
  line_ellipse_intersection
}
