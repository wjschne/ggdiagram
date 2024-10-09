library(testthat)
library(ggdiagram)

test_that(desc = "circle props", {
  # area
  c1 <- ob_circle(radius = 3)
  expect_equal(c1@area , expected = 9 * pi)
  # perimeter
  expect_equal(c1@circumference, 6 * pi)
  expect_equal(c1@diameter, 6)
})
