
library(testthat)
library(ggdiagram)


test_that("constructor", {
  # Angle
  t <- degree(1)
  expect_equal(turn(turn = 1)@degree, 0)
  expect_equal(turn(turn = 2)@degree, 0)
  expect_equal(turn(turn = .5)@degree, 180)
  expect_equal(turn(turn = .5)@radian, pi)
  expect_equal(radian(radian = pi)@radian, pi)
  expect_equal(degree(degree = 180)@radian, pi)
  # Rectangle
  p_center <- ob_point(0, 0)
  p_northeast <- ob_point(2, 1)
  p_northwest <- ob_point(-2, 1)
  p_southwest <- ob_point(-2, -1)
  p_southeast <- ob_point(2, -1)
  width <- 4
  height <- 2
  r_center <- ob_rectangle(center = p_center,
                           width = width,
                           height = height)
  expect_identical(r_center,
                   ob_rectangle(center = p_center, northeast = p_northeast))
  expect_identical(r_center,
                   ob_rectangle(center = p_center, northwest = p_northwest))
  expect_identical(r_center,
                   ob_rectangle(center = p_center, southeast = p_southeast))
  expect_identical(r_center,
                   ob_rectangle(center = p_center, southwest = p_southwest))
  expect_identical(r_center,
                   ob_rectangle(
                     width = width,
                     height = height,
                     southwest = p_southwest
                   ))
  expect_identical(r_center,
                   ob_rectangle(
                     width = width,
                     height = height,
                     northwest = p_northwest
                   ))
  expect_identical(r_center,
                   ob_rectangle(
                     width = width,
                     height = height,
                     southeast = p_southeast
                   ))
  expect_identical(r_center,
                   ob_rectangle(
                     width = width,
                     height = height,
                     northeast = p_northeast
                   ))
  expect_identical(
    r_center,
    ob_rectangle(
      width = width,
      southeast = p_southeast,
      northeast = p_northeast
    )
  )
  expect_identical(
    r_center,
    ob_rectangle(
      width = width,
      southwest = p_southwest,
      northwest = p_northwest
    )
  )
  expect_identical(r_center,
                   ob_rectangle(southwest = p_southwest, northeast = p_northeast))
  expect_identical(r_center,
                   ob_rectangle(northwest = p_northwest, southeast = p_southeast))
  expect_identical(
    r_center,
    ob_rectangle(
      height = height,
      southwest = p_southwest,
      southeast = p_southeast
    )
  )
  expect_identical(
    r_center,
    ob_rectangle(
      height = height,
      northwest = p_northwest,
      northeast = p_northeast
    )
  )



})


test_that("adding", {
  p1 <- ob_point(1, 1)
  p2 <- ob_point(3, 4)
  p3 <- ob_point(4, 5)
  s1 <- ob_segment(p1, p2)
  s2 <- ob_segment(p2, p3)
  ggdiagram() + p1 + p2 + p3  + s1 + s2

  expect_identical(p1 + p2, p3)
  expect_identical(p3 - p2, p1)
  expect_identical(ob_segment(p1, p2) + p3,
                   ob_segment(p1 + p3, p2 + p3))
  expect_identical(p3 + ob_segment(p1, p2),
                   ob_segment(p1 + p3, p2 + p3))
  expect_equal(p3 - ob_segment(p1, p2),
               ob_segment(p3 - p1, p3 - p2))
  expect_identical(ob_circle(p1, 2) + p2, ob_circle(p1 + p2, 2))
  expect_identical(ob_circle(p1, 2) - p2, ob_circle(p1 - p2, 2))
  expect_identical(p2 + ob_circle(p1, 2), ob_circle(p1 + p2, 2))
  expect_identical(p2 - ob_circle(p1, 2), ob_circle(p2 - p1, 2))
  expect_identical(p2 + 2, ob_point(5, 6))
  expect_identical(2 + p2, ob_point(5, 6))
  expect_identical(p2 - 2, ob_point(1, 2))
  expect_identical(2 - p2, ob_point(-1, -2))
  expect_identical(s1 + s2, nudge(s1, s2))
})

test_that("resect", {
  s1 <- ob_segment(ob_polar(0, 0), ob_polar(theta = 0, r = 1))
  expect_equal(resect(s1, distance(s1) * 0.05), ob_segment(ob_polar(0, 0.05), ob_polar(0, 0.95)))
})

test_that("inside", {
  o <- ob_point(0, 0)
  r1 <- ob_rectangle(center = o,
                     width = 2,
                     height = 4)
  expect_equal(inside(o, r1), 1)
  expect_equal(inside(ob_point(0, 2), r1), 0)
  expect_equal(inside(ob_point(0, 1.5), r1), 1)
  expect_equal(inside(ob_point(1.5, 1.5), r1), -1)
  expect_equal(inside(ob_point(0.5, 2.5), r1), -1)

  c1 <- ob_circle(ob_point(1, 1), radius = sqrt(2))
  expect_equal(inside(ob_point(1, 1), c1), 1)
  expect_equal(inside(o, c1), 0)
  expect_equal(inside(ob_point(1, 0), c1), 1)
  expect_equal(inside(ob_point(1, 2), c1), 1)
  expect_equal(inside(ob_point(2, 1), c1), 1)
  expect_equal(inside(ob_point(2, 2), c1), 0)
  expect_equal(inside(ob_point(2, 3), c1), -1)

  e1 <- ob_ellipse(o, a = 1, b = 2)
  expect_equal(inside(o, e1), 1)
  re1 <- ob_ellipse(o,
                    a = 1,
                    b = 2,
                    angle = degree(45))

  p1 <- ob_point(0, 1.9)
  rp1 <- rotate(p1, degree(45))

  expect_equal(inside(p1, e1), 1)
  expect_equal(inside(p1, re1), -1)
  expect_equal(inside(rp1, e1), -1)
  expect_equal(inside(rp1, re1), 1)


})

