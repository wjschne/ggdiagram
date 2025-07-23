library(testthat)
library(ggdiagram)

test_that(desc = "polarjust", {
  expect_no_error(polar2just(degree(45), multiplier = 1))
})


test_that(desc = "class_margin", {
  expect_identical(class_margin(1), class_margin(grid::unit(1, "pt")))
  # expect_identical(class_margin(3), class_margin(ggplot2::margin(3,3,3,3)))
  expect_identical(class_margin(c(1,2,1,2)), class_margin(grid::unit(1:2, "pt")))
  expect_identical(class_margin(c(1,2,3,4)), class_margin(grid::unit(1:4, "pt")))
  expect_error(class_margin(grid::unit(1:5, "pt")), "Margins can have 1 \\(all sides\\), 2 \\(horiztonal vs vertical)\\, or 4 \\(top right bottom left\\) elements\\.")
  expect_error(class_margin("3"), "Margins can be of class margin, unit, or numeric")
})

test_that(desc = "class_arrowhead", {
  m1 <- matrix(1:6, ncol = 2)
  m2 <- matrix(1:8, ncol = 2)
  expect_identical(class_arrowhead(class_arrowhead(m1)), class_arrowhead(m1))
  expect_identical(class_arrowhead(list(m1, m2)), list(class_arrowhead(m1), class_arrowhead(m2)))
  expect_error(class_arrowhead(1), "Arrowheads must be a 2-column matrix of numbers\\.")
  expect_error(class_arrowhead(matrix(1)), "Arrowheads must be a 2-column matrix of numbers\\.")
})


test_that(desc = "assign_data", {
  p <- ob_point(1, c(2, 2.4))
  p2 <- ob_point(1, 2.4)
  expect_identical(p[2]@y, p2@y)
  identical(ob_circle(center = p)[2], ob_circle(p2))
  expect_identical(ob_circle(center = p)[2]@center, ob_circle(p2)@center)
})

test_that(desc = "distances", {
  p1 <- ob_point(0, 0)
  p2 <- ob_point(3, 4)
  l1 <- ob_line(slope = 1, intercept = 1)
  c1 <- ob_circle()
  c2 <- ob_circle(center = ob_point(2,0))
  c3 <- ob_circle(center = ob_point(0.5,0))
  expect_identical(distance(p1, p2), 5)
  expect_identical(distance(p2), 5)
  expect_identical(distance(l1, p2), 0)
  expect_identical(distance(c1, p1), 1)
  expect_identical(distance(c1, c3), 0)
})
