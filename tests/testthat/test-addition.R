test_that("add angles", {
  purrr::map(list(degree, radian, turn), \(.f) {
    expect_equal(prop(.f(.1), S7_class(.f(0))@name), .1)
    expect_equal(.f(.1) + .f(.1), .f(.2))
    expect_equal(.f(.1) - .f(.1), .f(0))
    expect_equal(.f(.1) * .1, .f(.01))
    expect_equal(.f(.1) / 2, .f(.05))
    expect_equal(.f(.1) ^ 2, .f(.01))
  })
  })

  test_that("trig", {
    expect_equal(cos(turn(1)), cospi(2))
    expect_equal(sin(turn(.25)), sinpi(.5))
    expect_equal(tan(turn(.5)), tanpi(1))
  })

test_that("equality", {
  tidyr::crossing(tibble::tibble(
    .f = list(degree, radian, turn),
    ratio = c(360, 2 * pi, 1)),
    .g = list(`>`, `<`, `<=`, `>=`, `==`, `!=`)) %>%
    dplyr::mutate(t1 = .2,
                  t2 = .3,
                  a1 = purrr::map2(.f, t1, \(f,t) f(t)),
                  a2 = purrr::map2(.f, t2, \(f,t) f(t)),
                  n1 = ratio * t1,
                  n2 = ratio * t2,
                  ta = purrr::pmap_lgl(list(.g, a1,a2), \(g, aa1, aa2) g(aa1,aa2)),
                  tn = purrr::pmap_lgl(list(.g, n1,n2), \(g, aa1, aa2) g(aa1,aa2)),
                  test = ta == tn
                  ) %>%
    dplyr::pull(test) %>%
    all() %>%
    expect_true()
})

test_that("trig angles", {
  my_turn <- 1 / 8
  purrr::map2(list(degree, radian, turn), c(360, 2*pi, 1), \(.f, ratio) {
    expect_equal(sin(.f(ratio * my_turn)),  sinpi(2 * my_turn))
    expect_equal(cos(.f(ratio * my_turn)),  cospi(2 * my_turn))
    expect_equal(tan(.f(ratio * my_turn)),  tanpi(2 * my_turn))
  })
})



test_that("constructor", {
  # Line
  l111 <- line(a = 1, b = 1, c = 1)
  expect_equal(l111, line(slope = -1, intercept = -1))
  v1 <- line(a = 1, b = 0, c = -1)
  expect_identical(v1, line(xintercept = 1))
  h1 <- line(a = 0, b = 1, c = -1)
  expect_identical(h1, line(intercept = 1))
  expect_error(line(a = 1, b = 1, c = 1, slope = 3), "Some slopes are inconsistent with a and b parameters.")
  expect_error(line(slope = Inf, intercept = Inf), "There is not enough information to make a line. Specify the x-intercept or the a,b,c parameters.")
  expect_error(line(intercept = Inf), "There is not enough information to make a line. Specify the x-intercept or the a,b,c parameters.")
  expect_error(line(slope = Inf), "There is insufficient information to create a line.")

  # Angle
  t <- degree(1)
  expect_equal(turn(turn = 1)@degree, 0)
  expect_equal(turn(turn = 2)@degree, 0)
  expect_equal(turn(turn = .5)@degree, 180)
  expect_equal(turn(turn = .5)@radian, pi)
  expect_equal(radian(radian = pi)@radian, pi)
  expect_equal(degree(degree = 180)@radian, pi)
  # Rectangle
  p_center <- point(0,0)
  p_northeast <- point(2,1)
  p_northwest <- point(-2,1)
  p_southwest <- point(-2,-1)
  p_southeast <- point(2,-1)
  width <- 4
  height <- 2
  r_center <- rectangle(center = p_center, width = width, height = height)
  expect_identical(r_center, rectangle(center = p_center, northeast = p_northeast))
  expect_identical(r_center, rectangle(center = p_center, northwest = p_northwest))
  expect_identical(r_center, rectangle(center = p_center, southeast = p_southeast))
  expect_identical(r_center, rectangle(center = p_center, southwest = p_southwest))
  expect_identical(r_center, rectangle(width = width, height = height, southwest = p_southwest))
  expect_identical(r_center, rectangle(width = width, height = height, northwest = p_northwest))
  expect_identical(r_center, rectangle(width = width, height = height, southeast = p_southeast))
  expect_identical(r_center, rectangle(width = width, height = height, northeast = p_northeast))
  expect_identical(r_center, rectangle(width = width, southeast = p_southeast, northeast = p_northeast))
  expect_identical(r_center, rectangle(width = width, southwest = p_southwest, northwest = p_northwest))
  expect_identical(r_center, rectangle(southwest = p_southwest, northeast = p_northeast))
  expect_identical(r_center, rectangle(northwest = p_northwest, southeast = p_southeast))
  expect_identical(r_center, rectangle(height = height, southwest = p_southwest, southeast = p_southeast))
  expect_identical(r_center, rectangle(height = height, northwest = p_northwest, northeast = p_northeast))
  expect_error(rectangle(width = width, southwest = p_southwest), "There is not enough information to make a rectangle.")

  # segment
  segment(point(0,2), point(3,4))
})


test_that("adding",{
  p1 <- point(1,1)
  p2 <- point(3,4)
  p3 <- point(4,5)
  expect_identical(p1 + p2, p3)
  expect_identical(p3 - p2, p1)
  expect_identical(segment(p1, p2) + p3, segment(p1 + p3, p2 + p3))
  expect_identical(p3 + segment(p1, p2), segment(p1 + p3, p2 + p3))
  expect_equal(p3 - segment(p1, p2), segment(p3 - p1, p3 - p2))
  expect_identical(circle(p1, 2) + p2, circle(p1 + p2, 2))
  expect_identical(circle(p1, 2) - p2, circle(p1 - p2, 2))
  expect_identical(p2 + circle(p1, 2), circle(p1 + p2, 2))
  expect_identical(p2 - circle(p1, 2), circle(p2 - p1, 2))
  expect_identical(p2 + 2, point(5,6))
  expect_identical(2 + p2, point(5,6))
  expect_identical(p2 - 2, point(1,2))
  expect_identical(2 - p2, point(-1,-2))
})






test_that("distances", {
  p0 <- point(0,3)
  p1 <- point(1,1)
  p2 <- point(4,5)
  n1 <- label(p1, label = "A")
  n2 <- label(p2, label = "B")
  s1 <- segment(p1, p2)
  c1 <- circle(p1, radius = 1)
  c2 <- circle(p2, radius = 2)
  expect_equal(distance(p0), 3)
  expect_equal(distance(p1, p2), 5)
  expect_equal(distance(s1), 5)
  expect_equal(distance(c1, c2), 2)
  expect_equal(distance(c1, c2, center = TRUE), 5)
  expect_equal(distance(c1, c1), 0)
})





test_that("segments", {
  p1 <- point(0,3)
  p2 <- point(1,1)
  s1 <- segment(p1, p2)
  expect_equal(s1@line@intercept, 3)
})

test_that("intersection", {
  s1 <- segment(point(0, 1), point(1, 0))
  s2 <- segment(point(0, 0), point(1, 1))
  s3 <- segment(point(0.1, .1), point(.9, .9))
  s4 <- segment(point(1, 1), point(2, 2))
  p1 <- point(.5, .5)
  expect_equal(intersection(s1,s2), p1)
  expect_equal(intersection(s1@line,s2), p1)
  expect_equal(intersection(s1,s2@line), p1)
  expect_equal(intersection(s1,s3), p1)
  expect_equal(intersection(s1,s4), list())
  expect_equal(intersection(s1@line, s4@line), p1)
  c1 <- circle(center = point(1, 1), radius = 1)
  l1 <- line(intercept = 1)
  p1 <- point(0, 1)
  p2 <- point(2, 1)
  expect_equal(intersection(l1, c1),
               c(p2, p1))
  expect_equal(intersection(c1, l1),
               c(p2, p1))
  expect_equal(intersection(line(intercept = 2), c1),
               point(1,2))
  l1 <- line(xintercept = 1)
  p1 <- point(1, 0)
  p2 <- point(1, 2)
  expect_equal(intersection(l1, c1),
               c(p1, p2))
  expect_equal(intersection(c1, l1),
               c(p1, p2))
  expect_equal(intersection(line(xintercept = 2), c1),
               point(2,1))
  l1 <- line(slope = 1, intercept = 2 * sin(degree(45)))
  c1 <- circle(point(0,0), radius = 1)
  # intersect at tangent
  expect_equal(intersection(l1, c1),
               S7::convert(polar(theta = radian(radian = pi * 3 / 4 ),
                                 r = 1),
                           point))
  l1 <- line(slope = .5, intercept = 0)
  c1 <- circle(point(0,0), radius = 1)
  expect_equal(intersection(l1, c1),
               c(point(x = cos(atan(.5)), y = sin(atan(.5))),
                 point(x = -cos(atan(.5)), y = -sin(atan(.5)))
                 ))
  e1 <- ellipse(a = 1, b = 2)

  s5 <- segment(point(-2,-2), point(2,2))
  # intersection(s2, e1)
  # intersection(s5, e1)
#   intersection(segment(point(0,0), point(2,0)), segment(point(0,1), point(2,1)))
#
#   intersection(segment(point(1,0), point(1,2)), segment(point(0,1), point(2,1)))
#   intersection(segment(point(1,0), point(1,2)), segment(point(1,1), point(1,2)))
#   intersection(line(1,intercept = 0), rectangle(point(0,0), width = 2, height = 2))
#   intersection(segment(point(0,-3), point(0,10)), rectangle(center = point(0,0), width = 2, height = 2))
# x <- segment(point(1,0), point(1,2))
# y <- segment(point(1,1), point(1,2))
})






test_that("rotate", {
  # rotate a line with an angle
  expect_identical(
    rotate(line(xintercept = 2), turn(turn = .5)),
    line(xintercept = -2)
  )
  # rotate a line with a numeric radian
  expect_identical(
    rotate(line(xintercept = 2), turn(turn = .5)),
    rotate(line(xintercept = 2), pi)
  )

  # rotate a point
  expect_identical(
    rotate(point(1,0), turn(turn = .5)),
    point(-1,0)
  )

  # rotate a segment
  expect_identical(
    rotate(segment(point(0,1), point(1,0)), theta = turn(.5)),
    segment(point(0,-1), point(-1,0))
  )

  # rotate a circle
  expect_identical(
    rotate(x = circle(point(1, 2)),
           theta = turn(.25)),
    circle(point(-2, 1)))

  expect_identical(
    rotate(x = circle(point(1, 2), n = 50),
           theta = turn(.25)),
    circle(point(-2, 1), n = 50))

  # rotate an ellipse
  expect_identical(
    rotate(x = ellipse(center = point(1, 2), a = 2, b = 1),
           theta = turn(.25)),
    ellipse(point(-2, 1), a = 2, b = 1, angle = turn(.25)))
})

test_that("resect", {
  s1 <- segment(point(0,0), point(1,0))
  expect_equal(resect(s1, distance(s1) * .05),segment(point(.05,0), point(.95,0)))
  })

test_that("inside", {
  o <- point(0,0)
  r1 <- rectangle(center = o, width = 2, height = 4)
  expect_equal(inside(o,r1), 1)
  expect_equal(inside(point(0,2),r1), 0)
  expect_equal(inside(point(0,1.5),r1), 1)
  expect_equal(inside(point(1.5,1.5),r1), -1)
  expect_equal(inside(point(.5,2.5),r1), -1)

  c1 <- circle(point(1,1), radius = sqrt(2))
  expect_equal(inside(point(1,1), c1), 1)
  expect_equal(inside(o, c1), 0)
  expect_equal(inside(point(1,0), c1), 1)
  expect_equal(inside(point(1,2), c1), 1)
  expect_equal(inside(point(2,1), c1), 1)
  expect_equal(inside(point(2,2), c1), 0)
  expect_equal(inside(point(2,3), c1), -1)

  e1 <- ellipse(o, a = 1, b = 2)
  expect_equal(inside(o, e1), 1)
  re1 <- ellipse(o, a = 1, b = 2, angle = degree(45))

  p1 <- point(0,1.9)
  rp1 <- rotate(p1, degree(45))
  expect_equal(inside(p1, e1), 1)
  expect_equal(inside(p1, re1), -1)
  expect_equal(inside(rp1, e1), -1)
  expect_equal(inside(rp1, re1), 1)


})




test_that("string concatenation", {
  expect_equal("a" + "b", "ab")
  expect_equal(2 + "b", "2b")
  expect_equal("a" + 2, "a2")
  expect_equal(2 + 2, 4)
})


