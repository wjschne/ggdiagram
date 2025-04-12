library(testthat)
library(ggdiagram)

test_that(desc = "lines", {
  l111 <- ob_line(a = 1, b = 1, c = 1)
  expect_equal(l111, ob_line(slope = -1, intercept = -1))
  v1 <- ob_line(a = 1, b = 0, c = -1)
  expect_identical(v1, ob_line(xintercept = 1))
  h1 <- ob_line(a = 0, b = 1, c = -1)
  expect_identical(h1, ob_line(intercept = 1))
  expect_error(ob_line(
    a = 1,
    b = 1,
    c = 1,
    slope = 3
  ),
  "Some slopes are inconsistent with a and b parameters\\.")
  expect_error(
    ob_line(slope = Inf, intercept = Inf),
    "There is not enough information to make a line\\. Specify the x-intercept or the a,b,c parameters\\."
  )
  expect_error(
    ob_line(intercept = Inf),
    "There is not enough information to make a line\\. Specify the x-intercept or the a,b,c parameters\\."
  )
  expect_error(ob_line(a = 0, b = 0, c = 3), regexp = "If a and b are 0, c must be 0\\.")
  expect_error(ob_line(intercept = 1, a = 2, b = 3, c = -1), regexp = "Some intercepts are inconsistent with b and c parameters\\.")
  ob_line()@a

})
