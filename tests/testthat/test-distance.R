library(testthat)
library(ggdiagram)

test_that("distances", {
  p0 <- ob_point(0, 3)
  p1 <- ob_point(1, 1)
  p2 <- ob_point(1, 5)
  n1 <- ob_label(p1, label = "A")
  n2 <- ob_label(p2, label = "B")
  s1 <- ob_segment(p1, p2)
  c1 <- ob_circle(p1, radius = 1)
  c2 <- ob_circle(p2, radius = 2)
  e1 <- ob_ellipse(ob_point(2,1))
  # Distance of a point is its ditance to the origin
  expect_identical(distance(p0), 3)

  # distance between 2 points
  expect_identical(distance(p1, p2), 4)

  # distance between segment points
  expect_identical(distance(s1), distance(s1@p1, s1@p2))


  # distance between segment points
  ## On endpoint
  expect_identical(distance(s1, p1), 0)
  ## On midpoint
  expect_identical(distance(s1, s1@midpoint()), 0)
  ## To projection point
  expect_identical(distance(s1, s1@midpoint() + ob_point(1,0)), 1)
  ## To endpoint
  expect_identical(distance(s1, s1@p1 + ob_point(0,-1)), 1)
  ## segment with no distance
  expect_identical(distance(ob_segment(p1,p1), p2), distance(p1, p2))



  # distance between point and circle
  ## point at center == radius
  expect_identical(distance(p1, c1), c1@radius)
  ## point inside circle to closest point
  expect_identical(distance(p1 + ob_point(.5,0), c1), .5)
  ## point on circle
  expect_identical(distance(c1@point_at(0), c1), 0)
  ## point outside circle
  expect_identical(distance(c1@point_at(0) + ob_point(1,0), c1), 1)


  # distance between 2 circles
  ## Non-overlappying circles
  expect_identical(distance(c1, c2), 1)

  ## intersecting circles
  expect_identical(distance(ob_point(.5, 0) + c1, c1), 0)

  ## identical circles
  expect_identical(distance(c1, c1), 0)

  ## Concentric circles
  expect_identical(distance(ob_circle(p1, radius = 2), c1), 0)

  # distance between circle and ellipse
  ## Intersecting
  expect_identical(distance(e1, c1), 0)
  ## Touching
  expect_identical(distance(e1, ob_point(-1, 0) + c1), 0)
  ## Non-Intersecting
  expect_identical(distance(e1, ob_point(-2, 0) + c1), 1)

})
