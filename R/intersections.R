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
    p <- point(c(ax, bx), c(ay, by)) + y@center
  }
  p

}

method(intersection, list(circle, line)) <- function(x,y) {
  intersection(y,x)
}


method(intersection, list(segment, circle)) <- function(x, y) {
  # https://raw.org/book/computer-graphics/line-segment-ellipse-intersection/

  # A <- x@p1 - y@center
  # B <- x@p2 - y@center
  #
  # V <- B - A
  #
  # r2 <- y@radius ^ 2
  #
  # aa <- r2 * (V@x ^ 2 + V@y ^ 2)
  # bb <- 2 * r2 * (A@y * V@y + A@x + V@x)
  # cc <- r2 * (A@y ^ 2 + A@x ^ 2 - r2)
  #
  # D <- bb ^ 2 - 4 * aa * cc
  #
  #
  # # Ax <- A@x
  # # Ay <- A@y
  # # Bx <- B@x
  # # By <- B@y
  # # r2 <- y@radius ^ 2
  # #
  # # aa <- r2 * ((By - Ay) ^ 2) + r2 * ((Bx - Ax) ^ 2)
  # # bb <- 2 * r2 * Ay * (By - Ay) + 2 * r2 * Ax * (Bx - Ax)
  # # cc <- r2 * (Ay ^ 2) + r2 * (Ax ^ 2) - r2 * r2
  # # D <- (bb ^ 2) - 4 * aa * cc
  #
  # D[D < 0] <- NA
  # print(D)
  # postt <- (-bb + sqrt(D)) / (2 * aa)
  # print(postt)
  # negtt <- (-bb - sqrt(D)) / (2 * aa)
  # print(negtt)
  #
  # same <- abs(postt - negtt) < .Machine$double.eps
  # print(same)
  # negtt[same] <- NULL
  # tt <- c(postt, negtt)
  # tt <- tt[tt <= 1 & tt >= 0]
  #
  # if (length(tt) == 0) {
  #   message("There are no points of intersections with this ellipse.")
  #   return(NULL)
  # }
  #
  #
  #
  #
  # P <- A + ((B - A) * tt)
  # y@center + P

  p <- intersection(x@line, y)
  betweenx <- .between(p@x, x@p1@x, x@p2@x)

  p[betweenx]



}

method(intersection, list(circle, segment)) <- function(x,y) {
  intersection(y, x)
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


method(intersection, list(segment, ellipse)) <- function(x, y) {
         # https://raw.org/book/computer-graphics/line-segment-ellipse-intersection/

         A <- rotate(x@p1 - y@center, y@angle * -1)
         B <- rotate(x@p2 - y@center, y@angle * -1)
         Ax <- A@x
         Ay <- A@y
         Bx <- B@x
         By <- B@y
         rx2 <- y@a ^ 2
         ry2 <- y@b ^ 2

         aa <- rx2 * ((By - Ay) ^ 2) + ry2 * ((Bx - Ax) ^ 2)
         bb <- 2 * rx2 * Ay * (By - Ay) + 2 * ry2 * Ax * (Bx - Ax)
         cc <- rx2 * (Ay ^ 2) + ry2 * (Ax ^ 2) - rx2 * ry2
         D <- (bb ^ 2) - 4 * aa * cc
         D[D < 0] <- NA

         postt <- (-bb + sqrt(D)) / (2 * aa)
         negtt <- (-bb - sqrt(D)) / (2 * aa)

         same <- abs(postt - negtt) < .Machine$double.eps
         negtt[same] <- NULL
         tt <- c(postt, negtt)
         tt <- tt[tt <= 1 & tt >= 0]

         if (length(tt) == 0) {
           message("There are no points of intersections with this ellipse.")
           return(NULL)
         }




         P <- A + ((B - A) * tt)
         y@center + rotate(P, y@angle)



}

method(intersection, list(ellipse, segment)) <- function(x,y) {
  interaction(y, x)
}

method(intersection, list(line, ellipse)) <- function(x,y) {
  x <- ellipse()
  y <- line(slope = 4, intercept = 1)
  p1 <- y@point_at_x(x@center - x@a - x@b)
  p2 <- y@point_at_x(x@center + x@a + x@b)
  intersection(segment(p1,p2), y)
}




# points(x = intersect_line_segment@x, y = intersect_line_segment@y)
method(intersection, list(line, ellipse)) <- function(x, y) {
  # theta <- angle(degree = seq(0, 360))
  x <- line(slope = 1, intercept = 0)
  y <- ellipse(center = point(0,0),a = 1, b = 1)

  eps <- distance(y@point_at(radian(0)), y@point_at(radian(.Machine$double.eps)))
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


method(intersection, list(circle, circle)) <- function(x,y) {
  # https://paulbourke.net/geometry/circlesphere/
  if (any(distance(x@center, y@center) > x@radius + y@radius)) {
    stop("At least one pair of circles is too far apart to intersect.")
  }

  if (any(distance(x@center, y@center) < abs(x@radius - y@radius))) {
    stop("At least one pair of circles does not intersect because one circle contains the other.")
  }

  if (any(x == y)) {
    stop("At least one pair of cirlces has the same center and radius, and thus they intersect at an infinite number of points.")

  }

  d <- tibble::tibble(
    P0 = as.list(x@center),
    P1 = as.list(y@center),
    xr = x@radius,
    yr = y@radius,
    d = distance(x@center,
                 y@center),
    a = (xr ^ 2 - yr ^ 2 + d ^ 2) / (2 * d),
    h = sqrt(xr ^ 2 - a ^ 2)
  ) |>
    dplyr::mutate(P2 = purrr::pmap(list(P0, P1, a, d), function(P0, P1, a, d) {
      P0 + (a / d) * (P1 - P0)
    }),
    P3 = purrr::pmap(list(P0, P1, P2, h, d),
                     function(P0, P1, P2, h, d) {
                       P10 <- P1 - P0
                       P2 + point(c(-1,1), c(1,-1)) * point(P10@y, P10@x) *  (h / d)
    }))
  d$P3[[1]]
}


method(intersection, list(pgon, segment)) <- function(x,y) {
  if (S7_inherits(x@p, point)) {
    x@p <- list(x@p)
  }
    purrr::map(x@p, \(p) {
      l <- as.list(segment(point(tibble::as_tibble(rbind(p@xy, p@xy[1,]))))) |>
        purrr::map(\(s) intersection(s, y))

      l <- Filter(\(x) S7_inherits(x, point), l)
      if (length(l) > 0) {
        l[[1]]
      }
    })[[1]]
  }


method(intersection, list(segment, pgon)) <- function(x,y) {
  intersection(y,x)
}
