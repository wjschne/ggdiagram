library(testthat)
library(ggdiagram)

test_that("rotate", {

  # rotate a line with an angle
  expect_identical(rotate(ob_line(xintercept = 2), turn(turn = 0.5)), ob_line(xintercept = -2))
  # rotate a line with a numeric degree
  expect_equal(rotate(ob_line(xintercept = 2), turn(turn = 0.5)), rotate(ob_line(xintercept = 2), 180))

  # rotate an ob_point
  expect_equal(rotate(ob_point(1, 0), turn(turn = 0.5)), ob_point(-1, 0))

  # rotate a segment
  expect_equal(rotate(ob_segment(ob_point(0, 1), ob_point(1, 0)), theta = turn(0.5)),
               ob_segment(ob_point(0, -1), ob_point(-1, 0)))

  # rotate a circle
  expect_equal(rotate(x = ob_circle(ob_point(1, 2)), theta = turn(0.25)), ob_circle(ob_point(-2, 1)))

  expect_equal(rotate(x = ob_circle(ob_point(1, 2), n = 50), theta = turn(0.25)),
               ob_circle(ob_point(-2, 1), n = 50))

  # rotate an ellipse
  expect_equal(rotate(
    x = ob_ellipse(
      center = ob_point(1, 2),
      a = 2,
      b = 1
    ),
    theta = radian(0.5 * pi)
  ),
  ob_ellipse(
    ob_point(-2, 1),
    a = 2,
    b = 1,
    angle = radian(0.5 * pi)
  ))

  # rotate bezier
  bz <- ob_bezier(ob_point(1:3, c(0,0,1)))
  expect_equal(bz@p[[1]]@x, rotate(bz, theta = 90)@p[[1]]@y)



})
