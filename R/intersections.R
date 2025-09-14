# Intersection angle----
#' Compute the angle of the intersection of two objects
#'
#' @param x an object (e.g., [ob_point], [ob_segment], [ob_line])
#' @param y an object (e.g., [ob_point], [ob_segment], [ob_line])
#' @export
#' @returns [ob_angle] object
intersection_angle <- S7::new_generic("intersection_angle", c("x", 'y'), function(x,y) S7::S7_dispatch())
S7::method(intersection_angle, list(ob_line, ob_line)) <- function(x, y) {
  y@angle - x@angle

}
S7::method(intersection_angle, list(ob_segment, ob_segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@line@angle - x@line@angle
  } else
    NA_real_
}
S7::method(intersection_angle, list(ob_line, ob_segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@angle - x@line@angle
  } else
    NA_real_
}
S7::method(intersection_angle, list(ob_segment, ob_line)) <- function(x, y) {
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
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
#' @returns shape object
intersection <- S7::new_generic("intersection", c("x", "y"))
S7::method(intersection, list(ob_line, ob_line)) <- function(x,y, ...) {
  c_p <- x@a * y@b - y@a * x@b
  if (identical(TRUE, all.equal(c_p, 0))) {
    p <- list()
  } else {
    a_p <- (x@b * y@c - y@b * x@c)
    b_p <- (x@c * y@a - y@c * x@a)

    p <- ob_point(a_p / c_p, b_p / c_p, ...)
  }
  p
}
S7::method(intersection, list(ob_segment, ob_segment)) <- function(x,y, ...) {

  # i_line <- intersection(x@line, y@line, ...)

  # https://stackoverflow.com/a/1968345/4513316

  d <- tibble::tibble(
    p0_x = x@p1@x,
    p0_y = x@p1@y,
    p1_x = x@p2@x,
    p1_y = x@p2@y,
    p2_x = y@p1@x,
    p2_y = y@p1@y,
    p3_x = y@p2@x,
    p3_y = y@p2@y,
    s1_x = p1_x - p0_x,
    s1_y = p1_y - p0_y,
    s2_x = p3_x - p2_x,
    s2_y = p3_y - p2_y,
    s12_x = p0_x - p2_x,
    s12_y = p0_y - p2_y,
    denom = -s2_x * s1_y + s1_x * s2_y
  ) |>
    dplyr::filter(denom != 0) |>
    dplyr::mutate(
      s = (-s1_y * s12_x + s1_x * s12_y) / denom,
      u = (s2_x * s12_y - s2_y * s12_x) / denom
    ) |>
    dplyr::filter(
      s >= 0 & s <= 1 & u >= 0 & u <= 1
    ) |>
    unique()

  if (nrow(d) > 0) {
    p <- d |>
      dplyr::mutate(i_x = p0_x + (u * s1_x),
             i_y = p0_y + (u * s1_y)) |>
      dplyr::select(x = i_x,
                    y = i_y) |>
      ob_point(style = x@style + y@style + ob_style(...))

  } else {
    p <- list()
  }
  p
}

S7::method(intersection, list(ob_line, ob_segment)) <- function(x,y, ...) {
  ll <- intersection(x, y@line, ...)
  if (length(ll) > 0) {
    intersection(ll, y, ...)
  } else {
    ll
    }

}

S7::method(intersection, list(ob_segment, ob_line)) <- function(x,y, ...) {
  intersection(y,x, ...)
}

S7::method(intersection, list(ob_line, ob_circle)) <- function(x,y, ...) {
  # https://cp-algorithms.com/geometry/circle-line-intersection.html
  c0 <- ob_circle(center = ob_point(0,0), radius = y@radius)
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
    p <- y@center + ob_point(x0 , y0, ...)
  } else {
    d <- y@radius * y@radius - C * C / A2B2
    m <- sqrt(d / A2B2)
    ax <- x0 + B * m
    bx <- x0 - B * m
    ay <- y0 - A * m
    by <- y0 + A * m
    p <- y@center + ob_point(c(ax, bx), c(ay, by), ...)
  }
  p

}

S7::method(intersection, list(ob_circle, ob_line)) <- function(x,y, ...) {
  intersection(y,x, ...)
}

S7::method(intersection, list(ob_segment, ob_circle)) <- function(x, y, ...) {
  p <- intersection(x@line, y, ...)
  betweenx <- .between(p@x, x@p1@x, x@p2@x)
  p[betweenx]
}

S7::method(intersection, list(ob_circle, ob_segment)) <- function(x,y, ...) {
  intersection(y, x, ...)
}


S7::method(intersection, list(ob_point, ob_line)) <- function(x, y, ...) {

  is_on_line <- tibble::tibble(
    ya = y@a,
    xx = x@x,
    yb = y@b,
    xy = x@y,
    yc = y@c,
    is_on = abs(ya * xx + yb * xy + yc) < .Machine$double.eps * 2) |>
    dplyr::pull(is_on)

  if (all(!is_on_line)) {
    list()
  } else {
    s <- rlang::list2(...)
    if (x@length == 1) {
      rlang::inject(set_props(x, !!!s))
    } else {
      rlang::inject(set_props(x[is_on_line], !!!s))
    }
  }
}

S7::method(intersection, list(ob_line, ob_point)) <- function(x, y, ...) {
  intersection(y, x, ...)
}


S7::method(intersection, list(ob_point, ob_segment)) <- function(x, y, ...) {
  if (identical(x, intersection(x, y@line))) {
    is_same <- tibble::tibble(
      xp1 = distance(x, y@p1),
      xp2 = distance(x, y@p2),
      p1p2 = distance(y),
      equalish = abs(p1p2 - xp1 - xp2) < .Machine$double.eps * 5) |>
      dplyr::pull(equalish)

    s <- rlang::list2(...)

    if (all(!is_same)) {
      list()
    } else if (x@length == 1) {
      rlang::inject(set_props(x, !!!s))
    } else {
      rlang::inject(set_props(x[is_same], !!!s))
    }
  } else {
    list()
  }
}

S7::method(intersection, list(ob_segment, ob_point)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_line, ob_rectangle)) <- function(x, y, ...) {
  unique(c(intersection(x, ob_segment(p1 = y@northeast,
                            p2 = y@northwest), ...),
    intersection(x, ob_segment(p1 = y@northwest,
                            p2 = y@southwest), ...),
    intersection(x, ob_segment(p1 = y@southwest,
                            p2 = y@southeast), ...),
    intersection(x, ob_segment(p1 = y@southeast,
                            p2 = y@northeast), ...)
  ))
}
S7::method(intersection, list(ob_rectangle, ob_line)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_segment, ob_rectangle)) <- function(x, y, ...) {
  y@width <- ifelse(y@width == 0, .0001, y@width)
  y@height <- ifelse(y@height == 0, .0001, y@height)

  unique(c(intersection(x, ob_segment(p1 = y@northeast,
                                   p2 = y@northwest), ...),
           intersection(x, ob_segment(p1 = y@northwest,
                                   p2 = y@southwest), ...),
           intersection(x, ob_segment(p1 = y@southwest,
                                   p2 = y@southeast), ...),
           intersection(x, ob_segment(p1 = y@southeast,
                                   p2 = y@northeast), ...)
  )) |>
    bind()
}
S7::method(intersection, list(ob_rectangle, ob_segment)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_point, ob_rectangle)) <- function(x, y, ...) {
  unique(c(intersection(x, y@side@east, ...),
           intersection(x, y@side@west, ...),
           intersection(x, y@side@north, ...),
           intersection(x, y@side@south, ...)
  ))
}

S7::method(intersection, list(ob_rectangle, ob_point)) <- function(x, y, ...) {
  intersection(y, x, ...)
}


S7::method(intersection, list(ob_segment, ob_ellipse)) <- function(x, y, ...) {
  intersection(intersection(x@line, y), x, ...)

         # # https://raw.org/book/computer-graphics/line-segment-ellipse-intersection/
         #
         # A <- rotate(x@p1 - y@center, y@angle * -1)
         # B <- rotate(x@p2 - y@center, y@angle * -1)
         # Ax <- A@x
         # Ay <- A@y
         # Bx <- B@x
         # By <- B@y
         # rx2 <- y@a ^ 2
         # ry2 <- y@b ^ 2
         #
         # aa <- rx2 * ((By - Ay) ^ 2) + ry2 * ((Bx - Ax) ^ 2)
         # bb <- 2 * rx2 * Ay * (By - Ay) + 2 * ry2 * Ax * (Bx - Ax)
         # cc <- rx2 * (Ay ^ 2) + ry2 * (Ax ^ 2) - rx2 * ry2
         # D <- (bb ^ 2) - 4 * aa * cc
         # D[D < 0] <- NA
         #
         # postt <- (-bb + sqrt(D)) / (2 * aa)
         # negtt <- (-bb - sqrt(D)) / (2 * aa)
         #
         # same <- abs(postt - negtt) < .Machine$double.eps
         # negtt[same] <- NULL
         # tt <- c(postt, negtt)
         # tt <- tt[tt <= 1 & tt >= 0]
         #
         # if (length(tt) == 0) {
         #   message("There are no points of intersections with this ellipse.")
         #   return(list())
         # }
         #
         #
         #
         #
         # P <- A + ((B - A) * tt)
         # i <- y@center + rotate(P, y@angle)
         # s <- rlang::list2(...)
         # rlang::inject(set_props(i, !!!s))



}

S7::method(intersection, list(ob_ellipse, ob_segment)) <- function(x,y, ...) {
  intersection(y, x, ...)
}

#' @keywords internal
intersect1line1ellipse <- function(x,y, ...) {
  rx <- rotate(x, theta = y@angle, origin = y@center)

  A <- rx@a
  B <- rx@b
  C <- rx@c + A * y@center@x + B * y@center@y
  a <- y@a
  b <- y@b
  m1 <- y@m1
  m2 <- y@m2


  # Normalize direction vector perpendicular to line
  norm_factor <- sqrt(A^2 + B^2)
  dx <- -B / norm_factor
  dy <- A / norm_factor

  # Find a particular point on the line
  x0 <- double(x@length)
  y0 <- double(x@length)
  x0[B == 0 & A != 0] <- -C[B == 0 & A != 0] / A[B == 0 & A != 0]
  y0[B != 0] <- -C[B != 0] / B[B != 0]

  # Parametric form of line: (x(t), y(t)) = (x0 + dx * t, y0 + dy * t)
  superellipse_eq <- function(t) {
    xt <- x0 + dx * t
    yt <- y0 + dy * t
    (abs(xt / a) ^ m1 + abs(yt / b) ^ m2) - 1
  }

  # Root finding over a large enough range
  t_vals <- seq(-10 * max(a, b), 10 * max(a, b), length.out = 2000)
  sign_change <- which(diff(sign(superellipse_eq(t_vals))) != 0)

  roots <- vapply(sign_change, function(i) {
    tryCatch({
      stats::uniroot(superellipse_eq, lower = t_vals[i], upper = t_vals[i + 1])$root
    }, error = function(e) NA_real_)
  }, numeric(1))

  roots <- roots[!is.na(roots)]

  if (length(roots) > 0) {
    p <- ob_point(x0 + dx * roots,
                  y0 + dy * roots, ...)
    theta <- y@angle
    rotate(p, theta = y@angle) + y@center
  } else {
    ob_point(numeric(0), numeric(0))
  }

}


S7::method(intersection, list(ob_line, ob_ellipse)) <- function(x,y, ...) {
  purrr::map2(unbind(x), unbind(y), \(xx, yy) intersect1line1ellipse(xx, yy, ...)) |>
    bind()
}

S7::method(intersection, list(ob_ellipse, ob_line)) <- function(x,y, ...) {
  intersection(y, x, ...)
}


# S7::method(intersection, list(ob_line, ob_ellipse)) <- function(x, y, ...) {
#   # theta <- angle(degree = seq(0, 360))
#   x <- ob_line(slope = 1, intercept = 0)
#   y <- ob_ellipse(center = ob_point(0,0),a = 1, b = 1)
#
#   eps <- distance(y@point_at(radian(0)), y@point_at(radian(.Machine$double.eps)))
#   # eps <- .00001
#   # par(pty = "s")
#   xmax <- max(y@xy[,1])
#   xmin <- min(y@xy[,1])
#   ymax <- max(y@xy[,2])
#   ymin <- min(y@xy[,2])
#   if (is.infinite(x@slope)) {
#     s <- ob_segment(ob_point(x@xintercept,ymin),
#                  ob_point(x@xintercept,ymax))
#   } else {
#     s1 <- ob_segment(x@point_at_x(xmin),
#                   x@point_at_x(xmax))
#
#     s2 <- ob_segment(ob_point((ymin - x@intercept) / x@slope, ymin),
#                   ob_point((ymax - x@intercept) / x@slope,ymax))
#     if (distance(s1) < distance(s2)) {
#       s <- s1
#     } else {
#       s <- s2
#     }
#   }
#
#
#
#   d <- s1@p2 - s1@p1
#   s_points <- s1@p1 + seq(0,1,.5) * d
#   c_points <- s_points - y@center
#
#
#   test_x <- seq(xmax, ymax, length.out = 1000)
#   text_y <- x@slope * test_x + x@intercept
#
#   delta_angle <- 2
#   current_angle <- 0
#
#   m1 <- ob_point(1,0)
#   first <- FALSE
#   second <- FALSE
#
#   line_ellipse_intersection <- list()
#   i <- 0
#
#   while (current_angle < 362 && !(m1@r < eps * 10) && delta_angle > .Machine$double.eps * 10) {
#     s <- ob_segment(anchor(y, angle(degree = current_angle)),
#                  anchor(y, angle(degree = current_angle + delta_angle)))
#     intersect_line_segment <- intersection(x, s)
#
#     if (length(intersect_line_segment) > 0) {
#       first <- TRUE
#       s_low <- ob_segment(
#         anchor(y, angle(degree = current_angle)),
#         anchor(y, angle(degree = current_angle + delta_angle / 2)))
#
#       s_high <- ob_segment(
#         anchor(y, angle(degree = current_angle + delta_angle / 2)),
#         anchor(y, angle(degree = current_angle + delta_angle )))
#
#       s_low@p1 == s@p1
#       s_low@p1 == s_high@p1
#       s_low@p2 == s_high@p1
#       s_low@p1 == s_low@p2
#
#       i_test_low <- intersection(x, s_low@line)
#       i_test_high <- intersection(x, s_high@line)
#
#       if (length(i_test_low) + length(i_test_high) == 0) {
#         stop("wtf")
#         s_low <- ob_segment(
#           anchor(
#             y,
#             angle(
#               degree = current_angle - delta_angle * 0.0000001)),
#           anchor(
#             y,
#             angle(degree = current_angle + .5 * delta_angle)))
#
#         s_high <- ob_segment(
#           anchor(
#             y,
#             angle(degree = current_angle + .5 * delta_angle)),
#           anchor(
#             y,
#             angle(
#               degree = current_angle + delta_angle * 1.0000001 )))
#         i_test_low <- intersection(x, s_low@line)
#         i_test_high <- intersection(x, s_high@line)
#
#         }
#
#       delta_angle <- delta_angle / 2
#
#       if (length(i_test_low) == 0) {
#         current_angle <- current_angle + delta_angle
#       }
#
#
#
#
#
#       d1 <- s@p1 - intersect_line_segment
#       m1 <- midpoint(s) - intersect_line_segment
#       if (d1@r < eps * 10) {
#         m1 <- ob_point(0,0)
#         line_ellipse_intersection <- s@p1
#       }
#       d2 <- s@p2 - intersect_line_segment
#       if (d2@r < eps * 10) {
#         m1 <- ob_point(0,0)
#         line_ellipse_intersection <- s@p2
#       }
#     } else {
#       # delta_angle <- delta_angle * 1.01
#       i <- i + 1
#       if (delta_angle < 2) {
#         if (first && !second) {
#           line_ellipse_intersection$first <- y@point_theta(degree(current_angle))
#           second <- TRUE
#         }
#         if (second) {
#           line_ellipse_intersection$second <- y@point_theta(degree(current_angle))
#         }
#
#         delta_angle <- 2
#         current_angle <- current_angle + delta_angle
#
#       } else {
#         current_angle <- current_angle + delta_angle
#       }
#
#     }
#   }
#   s <- rlang::list2(...)
#   rlang::inject(set_props(line_ellipse_intersection, !!!s))
#
# }


S7::method(intersection, list(ob_circle, ob_circle)) <- function(x,y, ...) {
  dd <- distance(x@center, y@center)
  # https://paulbourke.net/geometry/circlesphere/
  if (any(dd > x@radius + y@radius)) {
    message("At least one pair of circles is too far apart to intersect.")
  }

  if (any(dd < abs(x@radius - y@radius))) {
    message("At least one pair of circles does not intersect because one circle contains the other.")
  }

  if (any(dd == 0) && x@radius == y@radius) {
    message("At least one pair of cirlces has the same center and radius, and thus they intersect at an infinite number of points.")

  }

  d <- tibble::tibble(
    P0 = unbind(x@center),
    P1 = unbind(y@center),
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
                       P2 + ob_point(c(-1,1), c(1,-1)) * ob_point(P10@y, P10@x) *  (h / d)
    }))
  i <- d$P3[[1]]
  s <- rlang::list2(...)
  rlang::inject(set_props(i, !!!s))
}


S7::method(intersection, list(ob_polygon, ob_segment)) <- function(x,y, ...) {
  if (S7::S7_inherits(x@p, ob_point)) {
    x@p <- list(x@p)
  }
    i <- purrr::map(x@p, \(p) {
      l <- unbind(ob_segment(ob_point(tibble::as_tibble(rbind(p@xy, p@xy[1,]))))) |>
        purrr::map(\(s) intersection(s, y))

      l <- Filter(\(x) S7::S7_inherits(x, ob_point), l)
      if (length(l) > 0) {
        l[[1]]
      }
    })[[1]]
    s <- rlang::list2(...)
    rlang::inject(set_props(i, !!!s))
  }


S7::method(intersection, list(ob_segment, ob_polygon)) <- function(x,y, ...) {
  intersection(y,x, ...)
}


S7::method(intersection, list(ob_ngon, ob_segment)) <- function(x,y, ...) {
  intersection(x@segments, y, ...)
}


S7::method(intersection, list(ob_segment, ob_ngon)) <- function(x,y, ...) {
  intersection(y,x@segments, ...)
}


S7::method(intersection, list(ob_circle, ob_point)) <- function(x,y, ...) {
  is_on <- abs(distance(x@center, y) - x@radius) < 10 * .Machine$double.eps
  if (any(is_on)) {
    y[is_on]
  } else {
    list()
  }
}

S7::method(intersection, list(ob_point, ob_circle)) <- function(x,y, ...) {
  intersection(y,x)
}

S7::method(intersection, list(ob_arc, ob_point)) <- function(x, y, ...) {

  xy <- intersection(x@circle, y)

  if (length(xy) > 0) {
    th <- c(x@angle_at(xy)) %% 1
    st <- c(x@start)
    en <- c(x@end)
    is_between <- ((st <= th & th <= en) | (en <= th & th <= st)) | ((st <= th + 1 & th + 1 <= en) | (en <= th + 1 & th + 1 <= st)) | ((st <= th - 1 & th - 1 <= en) | (en <= th - 1 & th - 1 <= st))

    if (any(is_between)) {
      xy[is_between]
    } else {
      list()
    }
  } else {
    list()
  }

}

S7::method(intersection, list(ob_point, ob_arc)) <- function(x, y, ...) {
  intersection(y, x)
}


S7::method(intersection, list(ob_arc, ob_circle)) <- function(x, y, ...) {
  intersection(x, intersection(x@circle, y))
}

S7::method(intersection, list(ob_circle, ob_arc)) <- function(x, y, ...) {
  intersection(y, x)
}

ob_arc_or_bezier <- S7::new_union(ob_arc, ob_bezier)

S7::method(intersection, list(ob_arc_or_bezier, centerpoint)) <- function(x, y, ...) {
  if (x@n == 360)
    x@n <- 3600
  d_x <- x@polygon |>
    tidyr::nest(.by = group)


  d_y <- tibble::tibble(group = factor(seq(y@length)), e = unbind(y))



  if (x@length == 1 & y@length > 1) {
    d_x <- d_x |>
      dplyr::mutate(k = y@length) |>
      dplyr::uncount(k)
  }

  if (y@length == 1 & x@length > 1) {
    d_y <- d_y |>
      dplyr::mutate(k = x@length) |>
      dplyr::uncount(k)
  }

  if (nrow(d_x) != nrow(d_y))
    stop(
      paste0(
        "Objects have incompatible lengths. x has ",
        x@length,
        " arcs, and y has ",
        y@length,
        " ellipses."
      )
    )

  d_x$e <- d_y$e


  d_x |>
    dplyr::mutate(p = purrr::map2(data, e, \(d, ee) {
      inside(ob_point(d), ee)
    })) |>
    dplyr::select(-e) |>
    tidyr::unnest(c(data, p)) |>
    dplyr::filter(abs(p - dplyr::lag(p)) == 2 | p == 0) |>
    dplyr::select(-p, -group) |>
    data2shape(ob_point)

}

S7::method(intersection, list(centerpoint, ob_arc)) <- function(x,y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_path, ob_line)) <- function(
    x,
    y,
    ...) {
  purrr::map(unbind(x), \(p) {
    intersection(p@segments(), y)
  }) |>
    bind()
}

S7::method(intersection, list(ob_line, ob_path)) <- function(
    x,
    y,
    ...) {
  intersection(y, x, ...)
}


