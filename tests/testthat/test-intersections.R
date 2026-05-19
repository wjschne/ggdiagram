library(testthat)
library(ggdiagram)

ep <- ob_point(double(0), double(0))

# point-point ----

test_that("intersection: point × line — point on line returns the point", {
  p1 <- ob_point(3, 3)
  p2 <- ob_point(3, 3)

  expect_identical(intersection(p1, p2), p1) # same point
  p3 <- ob_point(3, c(3,4))
  expect_identical(intersection(p1, p3), p1) # 1 point
  expect_identical(intersection(p3, p1), p1) # 1 point
  p4 <- ob_point(0,0)
  expect_length(intersection(p1, p4), 0) # empty point

  expect_length(intersection(p1, ep), 0) # empty point


})

# point-line ----

test_that("intersection: point × line — point on line returns the point", {
  l <- ob_line(slope = 1, intercept = 0)  # y = x
  p <- ob_point(3, 3)
  r <- intersection(p, l)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@x, 3, tolerance = 1e-10)
  expect_equal(r@y, 3, tolerance = 1e-10)
  p <- ob_point(3:4, c(3, 2))
  r <- intersection(p, l)
  expect_length(r, 1L)

})

test_that("intersection: point × line — point off line returns empty point", {
  l <- ob_line(slope = 1, intercept = 0)
  p <- ob_point(2, 5)
  r <- intersection(p, l)
  expect_length(r, 0)
})

test_that("intersection: point × line — symmetric (line × point)", {
  l <- ob_line(slope = 1, intercept = 0)
  p <- ob_point(2, 2)
  r1 <- intersection(p, l)
  r2 <- intersection(l, p)
  expect_equal(r1@x, r2@x, tolerance = 1e-10)
})

# point-segment ----

test_that("intersection: point × segment — midpoint on segment", {
  s <- ob_segment(ob_point(0, 0), ob_point(2, 2))
  p <- ob_point(1, 1)
  r <- intersection(p, s)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@x, 1, tolerance = 1e-10)
  expect_equal(r@y, 1, tolerance = 1e-10)
})

test_that("intersection: point × segment — endpoint is on segment", {
  s <- ob_segment(ob_point(0, 0), ob_point(2, 0))
  r <- intersection(ob_point(2, 0), s)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@x, 2, tolerance = 1e-10)
})

test_that("intersection: point × segment — point on line but past endpoint", {
  s <- ob_segment(ob_point(0, 0), ob_point(2, 2))
  p <- intersection(ob_point(3, 3), s)  # on y=x but past endpoint
  expect_identical(p, ep)
})

test_that("intersection: point × segment — point off the line", {
  s <- ob_segment(ob_point(0, 0), ob_point(2, 0))
  p <- intersection(ob_point(1, 1), s)
  expect_identical(p, ep)
})

test_that("intersection: point × segment — symmetric (segment × point)", {
  s <- ob_segment(ob_point(0, 0), ob_point(2, 2))
  p <- ob_point(1, 1)
  r1 <- intersection(p, s)
  r2 <- intersection(s, p)
  expect_equal(r1@x, r2@x, tolerance = 1e-10)
})

# point-circle ----

test_that("intersection: circle × point — point on circle returns point", {
  circ <- ob_circle(radius = 1)
  p_on <- ob_point(1, 0)
  r <- intersection(circ, p_on)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@length, 1)
  expect_equal(r@x, 1, tolerance = 1e-6)
})

test_that("intersection: circle × point — interior point returns empty point", {
  circ <- ob_circle(radius = 1)
  r <- intersection(circ, ob_point(0.5, 0))
  expect_length(r, 0)
})

test_that("intersection: point × circle — symmetric dispatch", {
  circ <- ob_circle(radius = 1)
  p    <- ob_point(1, 0)
  r1 <- intersection(circ, p)
  r2 <- intersection(p, circ)
  expect_equal(r1@x, r2@x, tolerance = 1e-6)
})

# point-arc ----

test_that("intersection: arc × point — point on arc", {
  arc <- ob_arc(center = ob_point(0, 0), radius = 1, start = 0, end = 90)
  p   <- ob_point(cos(pi / 4), sin(pi / 4))  # 45°, on arc
  r <- intersection(arc, p)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@x, cos(pi / 4), tolerance = 1e-6)
  expect_equal(r@y, sin(pi / 4), tolerance = 1e-6)
})

test_that("intersection: arc × point — start endpoint is on arc", {
  arc <- ob_arc(center = ob_point(0, 0), radius = 1, start = 0, end = 90)
  p   <- ob_point(1, 0)  # 0° = start
  r <- intersection(arc, p)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@x, 1, tolerance = 1e-6)
})

test_that("intersection: point × arc — symmetric dispatch", {
  arc <- ob_arc(center = ob_point(0, 0), radius = 1, start = 0, end = 90)
  p   <- ob_point(cos(pi / 4), sin(pi / 4))
  r1 <- intersection(arc, p)
  r2 <- intersection(p, arc)
  expect_equal(r1@x, r2@x, tolerance = 1e-6)
})


test_that("arc × point — point on circle but not on arc", {
  arc   <- ob_arc(center = ob_point(0, 0), radius = 1, start = 0, end = 90)
  p_off <- ob_point(-1, 0)  # on circle at 180°, outside [0°, 90°]
  expect_identical(intersection(arc, p_off)@length, 0L)
})

# point-ellipse ----

test_that("intersection: point × ellipse ", {
  e1 <- ob_ellipse(center = ob_point(0, 0), a = 2, b = 1)
  p_on <- ob_point(2, 0)
  p_off <- ob_point(0, 0)
  i <- intersection(p_on, e1) # on point
  expect_equal(i@x, 2, tolerance = 1e-10)

  i <- intersection(p_off, e1) # off point
  expect_length(i, 0)

  i <- intersection(bind(c(p_on, p_off)), e1) # 1 on, 1 off
  expect_length(i, 1)
  expect_equal(i@x, 2, tolerance = 1e-10)
})

# point-rectangle ----

test_that("intersection: point × rectangle — point on east side", {
  rect <- ob_rectangle(center = ob_point(0, 0), width = 2, height = 2)
  p_on <- ob_point(1, 0)
  r <- intersection(p_on, rect)
  expect_gt(length(r), 0)
  expect_equal(r@x, 1, tolerance = 1e-10)
})

test_that("intersection: point × rectangle — interior point misses perimeter", {
  rect <- ob_rectangle(center = ob_point(0, 0), width = 2, height = 2)
  p <- intersection(ob_point(0, 0), rect)
  expect_identical(p, ep)
})

test_that("intersection: point × rectangle — exterior point misses perimeter", {
  rect <- ob_rectangle(center = ob_point(0, 0), width = 2, height = 2)
  p <- intersection(ob_point(5, 0), rect)
  expect_identical(p@length, 0L)
})

test_that("intersection: rectangle × point — symmetric dispatch", {
  rect <- ob_rectangle(center = ob_point(0, 0), width = 2, height = 2)
  p    <- ob_point(1, 0)
  r1 <- intersection(p, rect)
  r2 <- intersection(rect, p)
  expect_identical(r1, r2)
})

# point-bezier ----

test_that("intersection: line × line — crossing lines", {
  p <- ob_point(0,0)
  b <- ob_bezier(p = ob_point(c(0,0,1,1), c(0,1,1,0)))
  pb <- intersection(p, b)
  expect_identical(p, pb)
  intersection(b@midpoint(), b)
  ggdiagram() + b + p + b@midpoint()

  intersection1point1bezier(b@midpoint(), b, samples = 1000)

})

# line-line ----

test_that("intersection: line × line — crossing lines", {
  l1 <- ob_line(slope = 1,  intercept = 0)  # y = x
  l2 <- ob_line(slope = -1, intercept = 2)  # y = -x + 2  →  meets at (1, 1)
  p <- intersection(l1, l2)
  expect_s3_class(p, "ggdiagram::ob_point")
  expect_equal(p@x, 1, tolerance = 1e-10)
  expect_equal(p@y, 1, tolerance = 1e-10)
})

test_that("intersection: line × line — vertical × horizontal", {
  lv <- ob_line(slope = Inf, xintercept = 3)
  lh <- ob_line(slope = 0,   intercept  = 2)
  p <- intersection(lv, lh)
  expect_equal(p@x, 3, tolerance = 1e-10)
  expect_equal(p@y, 2, tolerance = 1e-10)
})

test_that("intersection: line × line — parallel lines return empty point", {
  l1 <- ob_line(slope = 1, intercept = 0)
  l2 <- ob_line(slope = 1, intercept = 5)
  p <- intersection(l1, l2)
  expect_identical(p, ep)
})

test_that("intersection: line × line — coincident lines return empty point", {
  l <- ob_line(slope = 1, intercept = 0)
  p <- intersection(l, l)
  expect_identical(p, ep)
})


# line-segment ----

test_that("intersection: line × segment — line crosses segment", {
  l <- ob_line(slope = 0, intercept = 1)              # y = 1
  s <- ob_segment(ob_point(0, 0), ob_point(2, 2))    # y = x, from 0 to 2
  p <- intersection(l, s)
  expect_s3_class(p, "ggdiagram::ob_point")
  expect_equal(p@x, 1, tolerance = 1e-10)
  expect_equal(p@y, 1, tolerance = 1e-10)
})

test_that("intersection: line × segment — symmetric (segment × line)", {
  l <- ob_line(slope = 0, intercept = 1)
  s <- ob_segment(ob_point(0, 0), ob_point(2, 2))
  expect_equal(intersection(l, s)@x, intersection(s, l)@x, tolerance = 1e-10)
  expect_equal(intersection(l, s)@y, intersection(s, l)@y, tolerance = 1e-10)
})

test_that("intersection: line × segment — line misses segment returns empty list", {
  l <- ob_line(slope = 0, intercept = 5)  # y = 5, way above segment
  s <- ob_segment(ob_point(0, 0), ob_point(2, 2))
  p <- intersection(l, s)
  expect_identical(p, ep)
})

# line-circle ----

test_that("intersection: line by circle", {
  c1 <- ob_circle(center = ob_point(1, 1), radius = 1)
  l1 <- ob_line(intercept = 1)
  l2 <- ob_line(intercept = 2)
  p1 <- ob_point(0, 1)
  p2 <- ob_point(2, 1)

  expect_equal(intersection(l1, c1), bind(c(p2, p1))) # two points of intersection
  expect_equal(intersection(c1, l1), bind(c(p2, p1))) # same points
  expect_equal(intersection(l2, c1), ob_point(1, 2)) # 1 point at tangent
  l1 <- ob_line(xintercept = 1)
  l2 <- ob_line(xintercept = 2)
  p1 <- ob_point(1, 0)
  p2 <- ob_point(1, 2)

  expect_equal(intersection(l1, c1), bind(c(p1, p2))) # vertical line 2 points
  expect_equal(intersection(c1, l1), bind(c(p1, p2))) # same points
  expect_equal(intersection(l2, c1), ob_point(2, 1)) # 1 point at tangent

  l1 <- ob_line(slope = .5, intercept = 0)
  c1 <- ob_circle(ob_point(0, 0), radius = 1)
  ggdiagram() + c1 + l1
    expect_equal(intersection(l1, c1), bind(c(
    ob_point(x = cos(atan(.5)), y = sin(atan(.5))), ob_point(x = -cos(atan(.5)), y = -sin(atan(.5))) # 2 points at sloped line intersection
  )))
})

test_that("intersection: line by circle at tangent", {
  l1 <- ob_line(slope = 1, intercept = 2 * sin(degree(45)))
  c1 <- ob_circle(ob_point(0, 0), radius = 1)
  # intersect at tangent
  expect_equal(intersection(l1, c1), S7::convert(ob_polar(
    theta = radian(radian = pi * 3 / 4), r = 1
  ), ob_point))
})

test_that("intersection: line × circle — two intersection points", {
  l    <- ob_line(slope = 0, intercept = 0)  # y = 0 through center
  circ <- ob_circle(radius = 1)
  p <- intersection(l, circ)
  expect_s3_class(p, "ggdiagram::ob_point")
  expect_equal(p@length, 2)
  expect_equal(sort(p@x), c(-1, 1), tolerance = 1e-10)
  expect_equal(p@y, c(0, 0),        tolerance = 1e-10)
})

test_that("intersection: line × circle — tangent returns one point", {
  l    <- ob_line(slope = 0, intercept = 1)  # y = 1, tangent at top
  circ <- ob_circle(radius = 1)
  p <- intersection(l, circ)
  expect_s3_class(p, "ggdiagram::ob_point")
  expect_equal(p@length, 1)
  expect_equal(p@x, 0, tolerance = 1e-10)
  expect_equal(p@y, 1, tolerance = 1e-10)
})

test_that("intersection: circle × line — symmetric dispatch", {
  l    <- ob_line(slope = 0, intercept = 0)
  circ <- ob_circle(radius = 1)
  p1 <- intersection(l, circ)
  p2 <- intersection(circ, l)
  expect_equal(sort(p1@x), sort(p2@x), tolerance = 1e-10)
})

test_that("line × circle — no intersection", {
  l    <- ob_line(slope = 0, intercept = 2)  # y = 2, above unit circle
  circ <- ob_circle(radius = 1)
  expect_identical(intersection(l, circ)@length, 0L)
})

# line-ellipse ----

test_that("intersection: line × ellipse — through center gives 2 points", {
  ell <- ob_ellipse(center = ob_point(0, 0), a = 2, b = 1)
  l   <- ob_line(slope = 0, intercept = 0)  # y = 0 through center
  p <- intersection(l, ell)
  expect_s3_class(p, "ggdiagram::ob_point")
  expect_equal(p@length, 2)
  expect_equal(sort(p@x), c(-2, 2), tolerance = 1e-3)
  l2 <- ob_line(intercept = 10)
  intersection(ell, l2)
})

test_that("intersection: ellipse × line — symmetric dispatch", {
  ell <- ob_ellipse(center = ob_point(0, 0), a = 2, b = 1)
  l   <- ob_line(slope = 0, intercept = 0)
  p1 <- intersection(l, ell)
  p2 <- intersection(ell, l)
  expect_equal(sort(p1@x), sort(p2@x), tolerance = 1e-3)
})

# line-rectangle ----

test_that("intersection: line × rectangle — horizontal through center", {
  rect <- ob_rectangle(center = ob_point(0, 0), width = 2, height = 2)
  l    <- ob_line(slope = 0, intercept = 0)
  p    <- intersection(l, rect)
  # Returns a list with the two border intersection points
  expect_length(p, 2)
  # Points on west and east sides (x = ±1, y = 0)
  xs <- sort(c(p[1]@x, p[2]@x))
  expect_equal(xs, c(-1, 1), tolerance = 1e-10)
})

test_that("intersection: line × rectangle — line misses rectangle", {
  rect <- ob_rectangle(center = ob_point(0, 0), width = 2, height = 2)
  l    <- ob_line(slope = 0, intercept = 5)   # y = 5, above rect
  p    <- intersection(l, rect)
  expect_length(p, 0)
})

test_that("intersection: rectangle × line — symmetric dispatch", {
  rect <- ob_rectangle(center = ob_point(0, 0), width = 2, height = 2)
  l    <- ob_line(slope = 0, intercept = 0)
  p1 <- intersection(l, rect)
  p2 <- intersection(rect, l)
  expect_identical(p1, p2)
})

# line-path ----

test_that("intersection: path × line — zigzag path crosses horizontal line twice", {
  pts  <- ob_point(c(0, 1, 2), c(0, 1, 0))
  path <- ob_path(pts)
  l    <- ob_line(slope = 0, intercept = 0.5)  # y = 0.5
  r <- intersection(path, l)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@length, 2)
  expect_equal(r@y, c(0.5, 0.5), tolerance = 1e-6)
  expect_equal(sort(r@x), c(0.5, 1.5), tolerance = 1e-6)
})

test_that("intersection: line × path — symmetric dispatch", {
  pts  <- ob_point(c(0, 1, 2), c(0, 1, 0))
  path <- ob_path(pts)
  l    <- ob_line(slope = 0, intercept = 0.5)
  r1 <- intersection(path, l)
  r2 <- intersection(l, path)
  expect_equal(sort(r1@x), sort(r2@x), tolerance = 1e-6)
})

# segment-segment ----

test_that("intersection: segment × segment — crossing segments", {
  s1 <- ob_segment(ob_point(0, 1), ob_point(1, 0))
  s2 <- ob_segment(ob_point(0, 0), ob_point(1, 1))
  s3 <- ob_segment(ob_point(0.1, .1), ob_point(.9, .9))
  s4 <- ob_segment(ob_point(1, 1), ob_point(2, 2))
  p1 <- ob_point(.5, .5)
  ggdiagram() + s1 + s2 + s3 + s4 + p1
  ggdiagram() + s1  + s4
  expect_equal(intersection(s1, s2), p1) # normal intersection
  expect_equal(intersection(s1@line, s2), p1) # intersects at line
  expect_equal(intersection(s1, s2@line), p1) # intersects at line
  expect_equal(intersection(s1, s3), p1) # intersects with non-zero segment
  expect_equal(intersection(s1, s4), ep) # empty point
  expect_equal(intersection(s1@line, s4@line), p1) # lines intersect

  s1 <- ob_segment(ob_point(0, 0), ob_point(2, 2))   # diagonal up-right
  s2 <- ob_segment(ob_point(0, 2), ob_point(2, 0))   # diagonal down-right
  p <- intersection(s1, s2)
  expect_s3_class(p, "ggdiagram::ob_point")
  expect_equal(p@x, 1, tolerance = 1e-10)
  expect_equal(p@y, 1, tolerance = 1e-10)
})

test_that("intersection: segment × segment — T-intersection at midpoint", {
  s_h <- ob_segment(ob_point(0, 0), ob_point(2, 0))  # horizontal
  s_v <- ob_segment(ob_point(1, -1), ob_point(1, 1)) # vertical through midpoint
  p <- intersection(s_h, s_v)
  expect_equal(p@x, 1, tolerance = 1e-10)
  expect_equal(p@y, 0, tolerance = 1e-10)
})

test_that("intersection: segment × segment — endpoint on segment", {
  s1 <- ob_segment(ob_point(0, 0), ob_point(2, 0))
  s2 <- ob_segment(ob_point(2, 0), ob_point(2, 2))  # shares endpoint (2, 0)
  p <- intersection(s1, s2)
  expect_identical(p@xy[1,], c(x = 2, y = 0))
  s1 <- ob_segment(ob_point(0, 0), ob_point(2, 0))
  s2 <- ob_segment(ob_point(0, 0), ob_point(2, -2))  # shares endpoint (0, 0)
  p <- intersection(s1, s2)
  expect_identical(p@xy[1,], c(x = 0, y = 0))

  # collinear and share endpoint
  x <- ob_segment(ob_point(0, 0), ob_point(2, 2))
  y <- ob_segment(ob_point(2, 2), ob_point(4, 4))  # shares endpoint (2, 2)
  p <- intersection(x, y)
  expect_identical(p@xy[1,], c(x = 2, y = 2))
  x <- ob_segment(ob_point(0, 0), ob_point(2, 2))
  y <- ob_segment(ob_point(-2, -2), ob_point(0, 0))  # shares endpoint (0, 0)
  p <- intersection(x, y)
  expect_identical(p@xy[1,], c(x = 0, y = 0))

  # degenerate segment 1 on segment 2
  x <- ob_segment(ob_point(0, 0), ob_point(0, 0))
  y <- ob_segment(ob_point(0, 0), ob_point(1, 0))
  p <- intersection(x, y)
  expect_identical(p@xy[1,], c(x = 0, y = 0))
  # degenerate segment 1 on segment 2
  x <- ob_segment(ob_point(0.5, 0), ob_point(0.5, 0))
  y <- ob_segment(ob_point(0, 0), ob_point(1, 0))
  p <- intersection(x, y)
  expect_identical(p@xy[1,], c(x = 0.5, y = 0))


  # degenerate segment 2 on segment 1
  x <- ob_segment(ob_point(-1, 0), ob_point(1, 0))
  y <- ob_segment(ob_point(0, 0), ob_point(0, 0))
  p <- intersection(x, y)
  expect_identical(p@xy[1,], c(x = 0, y = 0))

  # overlapping segments return segment
  x <- ob_segment(ob_point(-1, 0), ob_point(1, 0))
  y <- ob_segment(ob_point(0, 0), ob_point(2, 0))
  s <- intersection(x, y)
  expect_identical(s, ob_segment(ob_point(0, 0), ob_point(1, 0)))

  # overlapping segments plus a point
  x <- ob_segment(ob_point(-1, 0), ob_point(1:2, c(0,5)))
  y <- ob_segment(ob_point(-1, 0), ob_point(2, 0))
  ggdiagram() + x + y
  s <- intersection(x, y)
  expect_identical(
    s,
    bind(c(ob_point(-1, 0),
           ob_segment(ob_point(-1, 0), ob_point(1, 0)))))

})

test_that("intersection: segment × segment — non-intersecting returns empty point", {
  s1 <- ob_segment(ob_point(0, 0), ob_point(1, 0))
  s2 <- ob_segment(ob_point(0, 1), ob_point(1, 1))  # parallel, above s1
  p <- intersection(s1, s2)
  expect_identical(ep@tibble, p@tibble)

})

test_that("intersection: segment × segment — collinear disjoint returns empty point", {
  s1 <- ob_segment(ob_point(0, 0), ob_point(1, 0))
  s2 <- ob_segment(ob_point(2, 0), ob_point(3, 0))
  p <- intersection(s1, s2)
  expect_identical(ep@tibble, p@tibble)
})

test_that("intersection: segment × segment — collinear overlap", {
  s1 <- ob_segment(ob_point(0, 0), ob_point(2, 0))
  s2 <- ob_segment(ob_point(1, 0), ob_point(3, 0))
  p <- intersection(s1, s2)
  expect_identical(p, ob_segment(ob_point(1, 0), ob_point(2, 0)))
})







# segment-circle ----

test_that("intersection: segment × circle — through center gives 2 points", {
  s    <- ob_segment(ob_point(-2, 0), ob_point(2, 0))
  circ <- ob_circle(radius = 1)
  p <- intersection(s, circ)
  expect_s3_class(p, "ggdiagram::ob_point")
  expect_equal(p@length, 2)
  expect_equal(sort(p@x), c(-1, 1), tolerance = 1e-10)
})

test_that("intersection: segment × circle — starts inside, exits once", {
  s    <- ob_segment(ob_point(0, 0), ob_point(2, 0))  # starts inside unit circle
  circ <- ob_circle(radius = 1)
  p <- intersection(s, circ)
  expect_equal(p@length, 1)
  expect_equal(p@x, 1, tolerance = 1e-10)
  expect_equal(p@y, 0, tolerance = 1e-10)
})

test_that("intersection: circle × segment — symmetric dispatch", {
  s    <- ob_segment(ob_point(-2, 0), ob_point(2, 0))
  circ <- ob_circle(radius = 1)
  p1 <- intersection(s, circ)
  p2 <- intersection(circ, s)
  expect_equal(sort(p1@x), sort(p2@x), tolerance = 1e-10)
})


# segment-ellipse ----

test_that("intersection: segment × ellipse — chord through center", {
  ell <- ob_ellipse(center = ob_point(0, 0), a = 2, b = 1)
  s   <- ob_segment(ob_point(-3, 0), ob_point(3, 0))
  p <- intersection(s, ell)
  expect_length(p, 2)
  expect_equal(sort(p@x), c(-2, 2), tolerance = 1e-3) # 2 points
  ggdiagram() + ell + s + p

  e1 <- ob_ellipse(a = 1, b = 2)
  s5 <- ob_segment(ob_point(-2, -2), ob_point(2, 2))
  ggdiagram() + e1 + s5
  expect_length(intersection(s5, e1), 2)

})

test_that("intersection: ellipse × segment — symmetric dispatch", {
  ell <- ob_ellipse(center = ob_point(0, 0), a = 2, b = 1)
  s   <- ob_segment(ob_point(-3, 0), ob_point(3, 0))
  p1 <- intersection(s, ell)
  p2 <- intersection(ell, s)
  expect_equal(sort(p1@x), sort(p2@x), tolerance = 1e-3)
})


# segment-rectangle ----

test_that("intersection: segment × rectangle — crosses two sides", {
  rect <- ob_rectangle(center = ob_point(0, 0), width = 2, height = 2)
  s    <- ob_segment(ob_point(-2, 0), ob_point(2, 0))
  p <- intersection(s, rect)
  expect_s3_class(p, "ggdiagram::ob_point")
  expect_equal(p@length, 2)
  expect_equal(sort(p@x), c(-1, 1), tolerance = 1e-10)
})

test_that("intersection: rectangle × segment — symmetric dispatch", {
  rect <- ob_rectangle(center = ob_point(0, 0), width = 2, height = 2)
  s    <- ob_segment(ob_point(-2, 0), ob_point(2, 0))
  p1 <- intersection(s, rect)
  p2 <- intersection(rect, s)
  expect_equal(sort(p1@x), sort(p2@x), tolerance = 1e-10)
})



# circle-circle ----

test_that("intersection: circle × circle — two unit circles at distance 1", {
  c1 <- ob_circle(center = ob_point(0, 0), radius = 1)
  c2 <- ob_circle(center = ob_point(1, 0), radius = 1)
  p <- intersection(c1, c2)
  expect_s3_class(p, "ggdiagram::ob_point")
  expect_equal(p@length, 2)
  # Both intersections at x = 0.5
  expect_equal(p@x, c(0.5, 0.5), tolerance = 1e-10)
  # y = ±sqrt(3)/2
  expect_equal(sort(p@y), c(-sqrt(3) / 2, sqrt(3) / 2), tolerance = 1e-10)

  # same center, different radius returns empty
  c3 <- ob_circle(center = ob_point(0, 0), radius = 2)
  expect_identical(intersection(c1, c3)@length, 0L)

  # same center and radius returns 1 circle
  expect_identical(intersection(c1, c1), c1)


  # mix of points and circles
  c4 <- ob_circle(x = 0:2, y = 0)
  osl <- intersection(c1, c4)

  expect_true(S7::S7_inherits(osl, ob_shape_list))

  # two circels too far apart
  c5 <- ob_circle(ob_point(5,0))
  expect_identical(intersection(c1, c5), ep)

})





# arc-line ----

test_that("intersection: arc × line — line crosses arc once", {
  arc <- ob_arc(center = ob_point(0, 0), radius = 1, start = 0, end = 180)
  l   <- ob_line(slope = Inf, xintercept = 0.5)  # x = 0.5 (vertical)
  r <- intersection(arc, l)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@length, 1)
  expect_equal(r@x, 0.5, tolerance = 1e-6)
  expect_gt(r@y, 0)  # on the upper arc
})

test_that("intersection: line × arc — symmetric dispatch", {
  arc <- ob_arc(center = ob_point(0, 0), radius = 1, start = 0, end = 180)
  l   <- ob_line(slope = Inf, xintercept = 0.5)
  r1 <- intersection(arc, l)
  r2 <- intersection(l, arc)
  expect_equal(r1@x, r2@x, tolerance = 1e-6)
})


# arc-circle ----

test_that("intersection: arc × circle — upper half-arc crosses shifted circle", {
  arc  <- ob_arc(center = ob_point(0, 0), radius = 1, start = 0, end = 90)
  circ <- ob_circle(center = ob_point(1, 0), radius = 1)
  # Circles intersect at (0.5, ±sqrt(3)/2); only (0.5, sqrt(3)/2) is in [0,90°]
  r <- intersection(arc, circ)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@x, 0.5,            tolerance = 1e-4)
  expect_equal(r@y, sqrt(3) / 2,    tolerance = 1e-4)
})

test_that("intersection: circle × arc — symmetric dispatch", {
  arc  <- ob_arc(center = ob_point(0, 0), radius = 1, start = 0, end = 90)
  circ <- ob_circle(center = ob_point(1, 0), radius = 1)
  r1 <- intersection(arc, circ)
  r2 <- intersection(circ, arc)
  expect_equal(r1@x, r2@x, tolerance = 1e-4)
})

# arc-ellipse ----

test_that("intersection: line × arc — symmetric dispatch", {
  a1 <- ob_arc(center = ob_point(0, 0), radius = 1, start = 0, end = 180)
  e1 <- ob_ellipse(ob_point(.5, 0))
  ggdiagram() + a1 + e1
  r1 <- intersection(a1, e1)
  r2 <- intersection(e1, a1)
  expect_equal(r1@x, r2@x, tolerance = 1e-6)
})





# polygon-segment ----

test_that("intersection: polygon × segment — segment crosses one side", {
  poly <- ob_polygon(ob_point(c(0, 2, 1), c(0, 0, 2)))
  s    <- ob_segment(ob_point(-1, 1), ob_point(3, 1))  # y = 1, horizontal
  r <- intersection(poly, s)
  expect_identical(r@y, c(1,1))
})

test_that("intersection: segment × polygon — symmetric dispatch", {
  poly <- ob_polygon(ob_point(c(0, 2, 1), c(0, 0, 2)))
  s    <- ob_segment(ob_point(-1, 1), ob_point(3, 1))
  r1 <- intersection(poly, s)
  r2 <- intersection(s, poly)
  expect_equal(r1@x, r2@x, tolerance = 1e-6)
})

# ngon-segment ----

test_that("intersection: ngon × segment — horizontal crosses unit square", {
  sq <- ob_ngon(n = 4, radius = 1, angle = 45)  # axis-aligned square, apothem ≈ sqrt(2)/2
  s  <- ob_segment(ob_point(-2, 0), ob_point(2, 0))
  r <- intersection(sq, s)
  expect_s3_class(r, "ggdiagram::ob_point")
  expect_equal(r@length, 2)
  expect_equal(r@y, c(0, 0), tolerance = 1e-6)
  expect_equal(sort(r@x), c(-cos(pi / 4), cos(pi / 4)), tolerance = 1e-6)
})

test_that("segment × ngon intersection", {
  sq <- ob_ngon(n = 4, radius = 1, angle = 0)
  s  <- ob_segment(ob_point(-2, 0), ob_point(2, 0))
  expect_equal(intersection(s, sq)@x, c(1, -1))
})



# intersection_angle ----

test_that("intersection_angle: line × line — 45-degree crossing", {
  l1 <- ob_line(slope = 0, intercept = 0)  # horizontal
  l2 <- ob_line(slope = 1, intercept = 0)  # 45°
  ia <- intersection_angle(l1, l2)
  expect_s3_class(ia, "ggdiagram::ob_angle")
  expect_equal(ia@degree, 45, tolerance = 1e-10)
})

test_that("intersection_angle: line × line — perpendicular lines", {
  l1 <- ob_line(slope = 0,   intercept = 0)
  l2 <- ob_line(slope = Inf, xintercept = 1)  # vertical
  ia <- intersection_angle(l1, l2)
  expect_equal(abs(ia@degree), 90, tolerance = 1e-10)
})

test_that("intersection_angle: segment × segment — crossing segments", {
  s1 <- ob_segment(ob_point(0, 0), ob_point(2, 2))  # 45°
  s2 <- ob_segment(ob_point(0, 2), ob_point(2, 0))  # -45°
  ia <- intersection_angle(s1, s2)
  expect_s3_class(ia, "ggdiagram::ob_angle")
  expect_equal(abs(ia@degree), 90, tolerance = 1e-6)
})

test_that("intersection_angle: segment × segment — non-intersecting returns NA", {
  s1 <- ob_segment(ob_point(0, 0), ob_point(1, 0))
  s2 <- ob_segment(ob_point(0, 1), ob_point(1, 1))  # parallel, no intersection
  ia <- intersection_angle(s1, s2)
  expect_true(is.na(ia))
})






test_that("intersection_angle(line, segment)", {
  l <- ob_line(slope = 0, intercept = 0)
  s <- ob_segment(ob_point(0, -1), ob_point(0, 1))
  expect_identical(intersection_angle(l, s), degree(90))
})

test_that("intersection_angle(segment, line)", {
  l <- ob_line(slope = 0, intercept = 0)
  s <- ob_segment(ob_point(0, -1), ob_point(0, 1))
  expect_identical(intersection_angle(s, l), degree(-90))
})

o1 <- c(
        # ob_point(1:2,1:2),
        # ob_line(1,1),
        # ob_segment(ob_point(1,1), ob_point(2,3)),
        # ob_circle(),
        ob_arc(start = 0, end = 90, center = ob_point(3,4))
        # ob_ellipse(x = 0, y = .5, a = 1, b = 1.5)
        )

o1 <-   purrr::map(o1, \(x) {
  if (length(x) > 0) {
    set_props(x, color = "blue")
  }
  x
})

o2 <- c(
        # ob_point(3:2,c(0,.1)),
        # ob_line(2,-1),
        # ob_segment(ob_point(0,1), ob_point(2,0)),
        # ob_circle(ob_point(1,0), 2),
        ob_arc(start = 90, end = 180, center = ob_point(3,4))
        # ob_ellipse(x = 0, y = -.5)
        )


i <- tibble::tibble(x = o1) |>
  dplyr::mutate(y = list(o2)) |>
  tidyr::unnest(y) |>
  dplyr::mutate(i = purrr::map2(x,y, intersection)) |>
  dplyr::pull(i) |>
  bind() |>
  map_ob(\(x) {
    if (length(x) > 0) {
      set_props(x, color = "blue")
    }
    x
  })

map_ob(ep, identity)

ggdiagram() + o1 + o2 + i
ggdiagram() + ob_ellipse(x = 0, y = .5) + ob_circle(ob_point(1,0), 2)
intersection(ob_ellipse(x = 0, y = .5),
             ob_circle(ob_point(1,0), 2))
