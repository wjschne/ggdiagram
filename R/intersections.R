# Intersection angle----
#' Compute the angle of the intersection of two objects
#'
#' @param x an object (e.g., [ob_point], [ob_segment], [ob_line])
#' @param y an object (e.g., [ob_point], [ob_segment], [ob_line])
#' @export
#' @returns [ob_angle] object
intersection_angle <- S7::new_generic(
  "intersection_angle",
  c("x", 'y'),
  function(x, y) S7::S7_dispatch()
)
S7::method(intersection_angle, list(ob_line, ob_line)) <- function(x, y) {
  y@angle - x@angle
}
S7::method(intersection_angle, list(ob_segment, ob_segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@line@angle - x@line@angle
  } else {
    NA_real_
  }
}
S7::method(intersection_angle, list(ob_line, ob_segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@line@angle - x@angle
  } else {
    NA_real_
  }
}
S7::method(intersection_angle, list(ob_segment, ob_line)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@angle - x@line@angle
  } else {
    NA_real_
  }
}

# intersection helpers ----

ob_arc_or_bezier <- S7::new_union(ob_arc, ob_bezier)

#' @keywords internal
arc_overlap <- function(x, y) {
  tibble::tibble(
    x1 = x@center@x,
    x2 = y@center@x,
    y1 = x@center@y,
    y2 = y@center@y,
    start_1 = x@start@degree,
    end_1 = x@end@degree,
    start_2 = y@start@degree,
    end_2 = y@end@degree
  ) |>
    dplyr::mutate(min_1 = min(start_1, end_1),
                  max_1 = max(start_1, end_1),
                  min_2 = min(start_2, end_2),
                  max_2 = max(start_2, end_2),
                  max_start = max(min_1, min_2),
                  min_end = min(max_1, max_2)) |>
    dplyr::filter(max_start <= min_end,
                  x1 == x2,
                  y1 == y2) |>
    dplyr::select(x = x1,
                  y = y2,
                  start = max_start,
                  end = min_end) |>
    data2shape(ob_arc)

}

#' @keywords internal
intersection1arc1point <- function(x, y, ...) {
  xy <- intersection(x@circle, y)

  if (length(xy) > 0) {
    th <- c(x@angle_at(xy)) %% 1
    st <- c(x@start)
    en <- c(x@end)
    is_between <- ((st <= th & th <= en) | (en <= th & th <= st)) |
      ((st <= th + 1 & th + 1 <= en) | (en <= th + 1 & th + 1 <= st)) |
      ((st <= th - 1 & th - 1 <= en) | (en <= th - 1 & th - 1 <= st))
    xy[is_between]
  } else {
    ob_point(numeric(0), numeric(0))
  }
}

#' @keywords internal
intersect1line1circle <- function(x, y, ...) {
  # https://cp-algorithms.com/geometry/circle-line-intersection.html
  c0 <- ob_circle(center = ob_point(0, 0), radius = y@radius)
  A <- x@a
  B <- x@b
  C <- x@c + A * y@center@x + B * y@center@y
  d0 <- abs(C) / sqrt(A^2 + B^2)
  A2B2 <- (A * A + B * B)
  x0 <- -A * C / A2B2
  y0 <- -B * C / A2B2
  if (C * C > y@radius * y@radius * A2B2 + .Machine$double.eps) {
    p <- ob_point(double(0), double(0))
  } else if (
    abs(C * C - y@radius * y@radius * A2B2) < 3 * .Machine$double.eps
  ) {
    p <- y@center + ob_point(x0, y0, ...)
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

#' @keywords internal
intersect1line1ellipse <- function(x, y, ...) {
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
    (abs(xt / a)^m1 + abs(yt / b)^m2) - 1
  }

  # Root finding over a large enough range
  t_vals <- seq(-10 * max(a, b), 10 * max(a, b), length.out = 2000)
  sign_change <- which(diff(sign(superellipse_eq(t_vals))) != 0)

  roots <- vapply(
    sign_change,
    function(i) {
      tryCatch(
        {
          stats::uniroot(
            superellipse_eq,
            lower = t_vals[i],
            upper = t_vals[i + 1]
          )$root
        },
        error = function(e) NA_real_
      )
    },
    numeric(1)
  )

  roots <- roots[!is.na(roots)]

  if (length(roots) > 0) {
    p <- ob_point(x0 + dx * roots, y0 + dy * roots, ...)
    theta <- y@angle
    rotate(p, theta = y@angle) + y@center
  } else {
    ob_point(numeric(0), numeric(0))
  }
}

#' @keywords internal
intersection1point1bezier <- function(
    x,
    b,
    samples = 1001
) {
  p <- as.numeric(x@xy)
  cp <- b@p[[1]]@xy

  bb <- b@p[[1]]@bounding_box

  tolerance = max(bb@width, bb@height) / 10000

  evaluate_bezier <- function(t) {
    cps <- cp
    degree <- nrow(cps) - 1

    for (r in seq_len(degree)) {
      cps <- (1 - t) * cps[-nrow(cps), , drop = FALSE] + t * cps[-1, , drop = FALSE]
    }

    as.numeric(cps)
  }

  distances <- vapply(
    seq(0, 1, length.out = samples),
    function(t) sqrt(sum((evaluate_bezier(t) - p)^2)),
    numeric(1)
  )

  min_distance <- min(distances)

  if (min_distance <= tolerance) {
    x
    } else {
    ob_point(double(0), double(0))
  }
}

#' @keywords internal
find_theta_on_object <- function(x, y, theta1, theta2, i1, i2, iteration = 1) {
  if (i1 == 0) {
    return(theta1)
  }
  if (iteration == 6) {
    return(theta1)
  }



  purrr::map2_df(unbind(x), unbind(y), \(xx, yy) {
    tibble::tibble(
      x = unbind(xx),
      y = unbind(yy),
      th1 = seq(theta1, theta2, (theta2 - theta1) / 100)
    ) |>
      dplyr::mutate(
        th2 = dplyr::lead(.data$th1),
        i1 = inside(xx@point_at(.data$th1), yy),
        i2 = dplyr::lead(i1),
        iteration = iteration + 1
      ) |>
      dplyr::filter(!is.na(.data$th2)) |>
      dplyr::filter(i1 == 0 | i1 == -i2) |>
      dplyr::rename(theta1 = th1, theta2 = th2) |>
      dplyr::slice(1)
  }) %>%
    purrr::pmap_dbl(find_theta_on_object)
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

## point by x ----

S7::method(intersection, list(ob_point, ob_point)) <- function(x, y, ...) {
  if (length(x) == 0 | length(y) == 0) {
    return(ob_point(double(0), double(0)))
  }
  map2_ob(x,y, \(xx, yy) {
    if (xx == yy) {
      xx
    } else {
      ob_point(double(0), double(0))
    }
  }) |>
    unique()
}

S7::method(intersection, list(ob_point, ob_line)) <- function(x, y, ...) {
  is_on_line <- tibble::tibble(
    ya = y@a,
    xx = x@x,
    yb = y@b,
    xy = x@y,
    yc = y@c,
    is_on = abs(ya * xx + yb * xy + yc) < .Machine$double.eps * 2
  ) |>
    dplyr::pull(is_on)

  if (all(!is_on_line)) {
    ob_point(double(0), double(0))
  } else {
    s <- rlang::list2(...)
    if (x@length == 1) {
      rlang::inject(set_props(x, !!!s))
    } else {
      rlang::inject(set_props(x[is_on_line], !!!s))
    }
  }
}

S7::method(intersection, list(ob_point, ob_segment)) <- function(x, y, ...) {
  tol <- 1e-9
  p <- map2_ob(x,y, \(xx, yy) {
    cross_product <- (xx@x - yy@p1@x) * (yy@p2@y - yy@p1@y) - (xx@y - yy@p1@y) * (yy@p2@x - yy@p1@x)
    if (abs(cross_product) > tol) {
      pp <- ob_point(double(0), double(0))
    } else {
      within_x <- xx@x >= min(yy@p1@x, yy@p2@x) - tol && xx@x <= max(yy@p1@x, yy@p2@x) + tol
      within_y <- xx@y >= min(yy@p1@y, yy@p2@y) - tol && xx@y <= max(yy@p1@y, yy@p2@y) + tol

      if (within_x && within_y) {
        pp <- xx
      } else {
        pp <- ob_point(double(0), double(0))
      }

    }
    pp
  })
  s <- rlang::list2(...)
  rlang::inject(set_props(p, !!!s))

}

S7::method(intersection, list(ob_point, ob_circle)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_point, ob_arc)) <- function(x, y, ...) {
  intersection(y, x, ...)
}


S7::method(intersection, list(ob_point, ob_ellipse)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_point, ob_rectangle)) <- function(x, y, ...) {
  unique(c(
    intersection(x, y@side@east, ...),
    intersection(x, y@side@west, ...),
    intersection(x, y@side@north, ...),
    intersection(x, y@side@south, ...)
  )) |>
    bind()
}

S7::method(intersection, list(ob_point, ob_bezier)) <- function(x, y, ...) {
  p <- map2_ob(x,y, \(xx, yy) {
    intersection1point1bezier(xx, yy)
  })

  s <- rlang::list2(...)
  rlang::inject(set_props(p, !!!s))

}

S7::method(intersection, list(ob_bezier, ob_point)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

## line by x ----

S7::method(intersection, list(ob_line, ob_point)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_line, ob_line)) <- function(x, y, ...) {
  map2_ob(x,y, \(xx, yy) {
    c_p <- xx@a * yy@b - yy@a * xx@b
    if (c_p == 0) {
      p <- ob_point(double(0), double(0), ...)
      } else {
        a_p <- (xx@b * yy@c - yy@b * xx@c)
        b_p <- (xx@c * yy@a - yy@c * xx@a)

        p <- ob_point(a_p / c_p, b_p / c_p, ...)
      }
    p
  })
}

S7::method(intersection, list(ob_line, ob_segment)) <- function(x, y, ...) {
  ll <- intersection(x, y@line, ...)
  if (length(ll) > 0) {
    intersection(ll, y, ...)
  } else {
    ll
  }
}

S7::method(intersection, list(ob_line, ob_circle)) <- function(x, y, ...) {
  purrr::map2(unbind(x), unbind(y), \(xx, yy) {
    intersect1line1circle(xx, yy, ...)
  }) |>
    bind()
}

S7::method(intersection, list(ob_line, ob_arc)) <- function(x, y, ...) {
  intersection(intersection(x, y@circle), y, ...)
}

S7::method(intersection, list(ob_line, ob_ellipse)) <- function(x, y, ...) {
  purrr::map2(unbind(x), unbind(y), \(xx, yy) {
    intersect1line1ellipse(xx, yy, ...)
  }) |>
    bind()
}

S7::method(intersection, list(ob_line, ob_rectangle)) <- function(x, y, ...) {
  unique(c(
    intersection(x, ob_segment(p1 = y@northeast, p2 = y@northwest), ...),
    intersection(x, ob_segment(p1 = y@northwest, p2 = y@southwest), ...),
    intersection(x, ob_segment(p1 = y@southwest, p2 = y@southeast), ...),
    intersection(x, ob_segment(p1 = y@southeast, p2 = y@northeast), ...)
  )) %>%
    bind()
}

S7::method(intersection, list(ob_line, ob_path)) <- function(
    x,
    y,
    ...
) {
  intersection(y, x, ...)
}

## segment by x ----

S7::method(intersection, list(ob_segment, ob_point)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_segment, ob_line)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_segment, ob_segment)) <- function(x, y, ...) {
  # i_line <- intersection(x@line, y@line, ...)
  tol <- 1e-09
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
    xa = y@line@a,
    xb = y@line@b,
    xc = y@line@c,
    ya = y@line@a,
    yb = y@line@b,
    yc = y@line@c,
    denom = -s2_x * s1_y + s1_x * s2_y
  )

  d1 <- d %>%
    dplyr::filter(abs(denom) > tol) |>
    dplyr::mutate(
      s = (-s1_y * s12_x + s1_x * s12_y) / denom,
      u = (s2_x * s12_y - s2_y * s12_x) / denom
    ) |>
    dplyr::filter(
      s >= -tol & s <= 1 + tol & u >= -tol & u <= 1 + tol
    ) |>
    unique()

  if (nrow(d1) > 0) {
    p <- d1 |>
      dplyr::mutate(i_x = p0_x + (u * s1_x),
                    i_y = p0_y + (u * s1_y)) |>
      dplyr::select(x = i_x, y = i_y) |>
      ob_point(style = x@style + y@style + ob_style(...))
  } else {
    p <- ob_point(double(0), double(0))
  }

  d_collinear <- d |>
    dplyr::filter(abs(denom) < tol) |>
    dplyr::mutate(collinear = s12_x * s1_y - s12_y * s1_x) |>
    dplyr::filter(abs(collinear) < tol) |>
    dplyr::mutate(r2 = s1_x ^ 2 + s1_y ^ 2)

  # What if x is a degenerate segment where p1 == p2?

  d_degenerate <- dplyr::filter(d_collinear, r2 < tol)

  if (nrow(d_degenerate)) {
    p_degenerate <- d_degenerate |>
    dplyr::mutate(
      within_x = p0_x >= min(p2_x, p3_x) - tol && p0_x <= max(p2_x, p3_x) + tol,
      within_y = p0_y >= min(p2_y, p3_y) - tol && p0_y <= max(p2_y, p3_y) + tol) |>
      dplyr::filter(within_x & within_y) |>
      dplyr::select(x = p0_x, y = p0_y) |>
      ob_point()
    p <- bind(c(p, p_degenerate))
  }

  # What if on same line and overlap?

  d_overlap <- dplyr::filter(d_collinear, r2 >= tol)

  if (nrow(d_overlap) > 0) {
    d_overlap <- d_overlap |>
      dplyr::mutate(s20_x = p2_x - p0_x,
                    s20_y = p2_y - p0_y,
                    s30_x = p3_x - p0_x,
                    s30_y = p3_y - p0_y,
                    t0 = (s20_x * s1_x + s20_y * s1_y) / r2,
                    t1 = (s30_x * s1_x + s30_y * s1_y) / r2,
                    t_start = max(0, min(t0, t1)),
                    t_end = min(1, max(t0, t1)),
                    p_start_x = p0_x + t_start * s1_x,
                    p_start_y = p0_y + t_start * s1_y,
                    p_end_x = p0_x + t_end * s1_x,
                    p_end_y = p0_y + t_end * s1_y,

      ) |>
      dplyr::filter(t_start <= t_end + tol)

    # What if overlap is only a single point?
    d_ends <- dplyr::filter(d_overlap, t_start == t_end)

    if (nrow(d_ends) > 0) {
      p_ends <- d_ends |>
        dplyr::select(x = p_end_x, y = p_end_y) |>
        ob_point()
      p <- bind(c(p, p_ends))
    }

    d_overlap <- dplyr::filter(d_overlap, t_start < t_end)
    if (nrow(d_overlap)  > 0) {
      s <- d_overlap |>
        dplyr::select(x = p_start_x,
                      y = p_start_y,
                      xend = p_end_x,
                      yend = p_end_y) |>
        data2shape(ob_segment)
      if (p@length == 0) {
        p <- s
      } else {
        p <- bind(c(p,s))
        }
    }
  }
  p
}

S7::method(intersection, list(ob_segment, ob_circle)) <- function(x, y, ...) {
  p <- intersection(x@line, y, ...)
  betweenx <- .between(p@x, x@p1@x, x@p2@x)
  p[betweenx]
}

S7::method(intersection, list(ob_segment, ob_arc)) <- function(x, y, ...) {
  p <- intersection(x@line, y, ...)
  betweenx <- .between(p@x, x@p1@x, x@p2@x)
  p[betweenx]
}

S7::method(intersection, list(ob_segment, ob_ellipse)) <- function(x, y, ...) {
  intersection(intersection(x@line, y), x, ...)
}

S7::method(intersection, list(ob_segment, ob_rectangle)) <- function(
    x,
    y,
    ...
) {
  y@width <- ifelse(y@width == 0, .0001, y@width)
  y@height <- ifelse(y@height == 0, .0001, y@height)

  unique(c(
    intersection(x, ob_segment(p1 = y@northeast, p2 = y@northwest), ...),
    intersection(x, ob_segment(p1 = y@northwest, p2 = y@southwest), ...),
    intersection(x, ob_segment(p1 = y@southwest, p2 = y@southeast), ...),
    intersection(x, ob_segment(p1 = y@southeast, p2 = y@northeast), ...)
  )) |>
    bind()
}


S7::method(intersection, list(ob_segment, ob_polygon)) <- function(x, y, ...) {
  intersection(y, x, ...)@tibble %>%
    unique() %>%
    data2shape(ob_point)
}

S7::method(intersection, list(ob_segment, ob_ngon)) <- function(x, y, ...) {
  intersection(x, y@segment, ...)@tibble %>%
    unique() %>%
    data2shape(ob_point)
}

## circle by x ----

S7::method(intersection, list(ob_circle, ob_point)) <- function(x, y, ...) {
  is_on <- abs(distance(x@center, y) - x@radius) < 10 * .Machine$double.eps
  y[is_on]
}

S7::method(intersection, list(ob_circle, ob_line)) <- function(x, y, ...) {
  intersection(y, x, ...)
}


S7::method(intersection, list(ob_circle, ob_segment)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_circle, ob_circle)) <- function(x, y, ...) {
  dd <- distance(x@center, y@center)
  # https://paulbourke.net/geometry/circlesphere/
  i <- map2_ob(x, y, \(xx, yy) {
    P0 <- xx@center
    P1 <- yy@center
    xr <- xx@radius
    yr <- yy@radius
    dd <- distance(P0, P1)
    if (dd == 0) {
      if (xr == yr) {
        ii <- xx
      } else {
        ii <- ob_point(double(0), double(0))
      }
    } else {
      a <- (xr ^ 2 - yr ^ 2 + dd ^ 2) / (2 * dd)
      if ((xr ^ 2)  -  (a ^ 2) < 0) {
        ii <- ob_point(double(0), double(0))
      } else {
        h <- sqrt(xr^2 - a^2)
        P10 <- P1 - P0
        P2 <- P0 + (a / dd) * P10
        P3 <- P2 + ob_point(c(-1, 1), c(1, -1)) * ob_point(P10@y, P10@x) * (h / dd)
        ii <- P3
      }

    }
    ii

  }) |>
    unique()
  s <- rlang::list2(...)

  if (S7::S7_inherits(i, ob_shape_list)) {
    purrr::map(i, \(ii) {
      rlang::inject(set_props(ii, !!!s))
    }) |>
      ob_shape_list()

  } else {
    rlang::inject(set_props(i, !!!s))
  }

}

S7::method(intersection, list(ob_circle, ob_arc)) <- function(x, y, ...) {
  intersection(y, x)
}

## arc by x ----

S7::method(intersection, list(ob_arc, ob_point)) <- function(x, y, ...) {
    map2_ob(x, y, \(xx, yy) {
      intersection1arc1point(xx, yy, ...)
    }) |>
    unique()
}

S7::method(intersection, list(ob_arc, ob_line)) <- function(x, y, ...) {
  intersection(x, intersection(x@circle, y), ...)
}

S7::method(intersection, list(ob_arc, ob_segment)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_arc, ob_circle)) <- function(x, y, ...) {
  map2_ob(x,y, \(xx, yy) {
    if (xx@circle == yy) {
      xx
    } else {
      intersection(xx, intersection(xx@circle, yy))
    }
  })
}

S7::method(intersection, list(ob_arc, ob_arc)) <- function(x, y, ...) {
  map2_ob(x,y, \(xx, yy) {
    if (xx@circle == yy@circle) {
      a <- arc_overlap(xx,yy)
      # What if the overlap is a single point?
      if (a@length == 0) {
        point(double(0), double(0))
      } else {
        if (all(a@start == a@end)) {
          a@point_at(a@start)
        } else {
          a
        }
      }

    } else {
      intersection(xx, intersection(xx@circle, yy))
    }
  })
}

## ellipse by x ----

S7::method(intersection, list(ob_ellipse, ob_point)) <- function(x, y, ...) {
  is_on <- y == x@point_at(x@angle_at(y))
  y[is_on]
}

S7::method(intersection, list(ob_ellipse, ob_line)) <- function(x, y, ...) {
  intersection(y, x, ...)
}


S7::method(intersection, list(ob_ellipse, ob_segment)) <- function(x, y, ...) {
  intersection(y, x, ...)
}


## rectangle by x ----

S7::method(intersection, list(ob_rectangle, ob_point)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(ob_rectangle, ob_line)) <- function(x, y, ...) {
  intersection(y, x, ...)
}


S7::method(intersection, list(ob_rectangle, ob_segment)) <- function(
  x,
  y,
  ...
) {
  intersection(y, x, ...)
}















## polygon by x ----


S7::method(intersection, list(ob_polygon, ob_segment)) <- function(x, y, ...) {
  p <- map2_ob(x,y, \(xx, yy) {
    purrr::map(xx@segment, \(s) {
      intersection(s, yy)}) %>%
      bind() %>%
        unique()
  })

  s <- rlang::list2(...)
  rlang::inject(set_props(p, !!!s))

  }



## ngon by x ----


S7::method(intersection, list(ob_ngon, ob_segment)) <- function(x, y, ...) {
  intersection(x@segment, y, ...)@tibble %>%
    unique() %>%
    data2shape(ob_point)
}

## path by x ----

S7::method(intersection, list(ob_path, ob_line)) <- function(
    x,
    y,
    ...
) {
  purrr::map(unbind(x), \(p) {
    intersection(p@segment, y)
  }) |>
    bind()
}


## centerpoint by x ----

S7::method(intersection, list(centerpoint, ob_arc)) <- function(x, y, ...) {
  intersection(y, x, ...)
}

S7::method(intersection, list(centerpoint, centerpoint)) <- function(
    x,
    y,
    ...
) {
  purrr::map2(unbind(x), unbind(y), \(xx, yy) {
    th <- tibble::tibble(
      x = unbind(xx),
      y = unbind(yy),
      theta1 = seq(0, 360, .1),
      theta2 = lead_cycle(theta1),
      i1 = inside(xx@point_at(theta1), yy),
      i2 = lead_cycle(i1),
      iteration = 0
    ) |>
      dplyr::filter(i1 == 0 | i1 == -i2) %>%
      purrr::pmap_dbl(find_theta_on_object)

    if (length(th) == 0) {
      ob_point(double(0), double(0))
    } else {
      xx@point_at(th)
    }
  }) %>%
    bind()
}


## arc or bezier by x ----

S7::method(intersection, list(ob_arc_or_bezier, centerpoint)) <- function(
  x,
  y,
  ...
) {
  if (x@n == 360) {
    x@n <- 3600
  }
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

  if (nrow(d_x) != nrow(d_y)) {
    stop(
      paste0(
        "Objects have incompatible lengths. x has ",
        x@length,
        " arcs, and y has ",
        y@length,
        " ellipses."
      )
    )
  }

  d_x$e <- d_y$e

  d_x |>
    dplyr::mutate(
      p = purrr::map2(data, e, \(d, ee) {
        inside(ob_point(d), ee)
      })
    ) |>
    dplyr::select(-e) |>
    tidyr::unnest(c(data, p)) |>
    dplyr::filter(abs(p - dplyr::lag(p)) == 2 | p == 0) |>
    dplyr::select(-p, -group) |>
    data2shape(ob_point)
}












