library(testthat)
library(ggdiagram)

test_that("default constructor is horizontal line y = 0", {
  l <- ob_line()
  expect_equal(l@slope, 0)
  expect_equal(l@intercept, 0)
  expect_equal(l@a, 0)
  expect_equal(l@b, 1)
  expect_equal(l@c, 0)
  expect_no_error(l@aesthetics)

})

test_that("slope and intercept getters", {
  l <- ob_line(slope = 2, intercept = 3)
  expect_equal(l@slope, 2)
  expect_equal(l@intercept, 3)
})

test_that("intercept-only defaults slope to 0", {
  l <- ob_line(intercept = 5)
  expect_equal(l@slope, 0)
  expect_equal(l@intercept, 5)
})

test_that("xintercept gives vertical line", {
  l <- ob_line(xintercept = 4)
  expect_equal(l@xintercept, 4)
  expect_equal(l@slope, -Inf)
})

test_that("angle getter", {
  expect_equal(ob_line(slope = 1, intercept = 0)@angle@degree, 45)
  expect_equal(ob_line(intercept = 0)@angle@degree, 0)
})

test_that("length property", {
  expect_equal(ob_line(slope = 1:3, intercept = 0)@length, 3)
})

test_that("tibble has core columns", {
  nms <- names(ob_line(slope = 1, intercept = 2)@tibble)
  expect_true(all(c("slope", "intercept", "xintercept", "a", "b", "c") %in% nms))
})

test_that("style getter and setter", {
  l <- ob_line(slope = 1, intercept = 0, color = "red", linewidth = 2)
  expect_identical(l@style@color, "red")
  l@style <- ob_style(color = "blue")
  expect_identical(l@color, "blue")
  l@color <- NA_character_
  expect_no_error(get_tibble_defaults(l))
})

test_that("equation y-form", {
  expect_equal(ob_line(slope = 2, intercept = 3)@equation(), "*y* = 2*x* + 3")
  expect_equal(ob_line(intercept = 1)@equation(), "*y* = 1")
  expect_equal(ob_line(xintercept = 2)@equation(), "*x* = 2")
})

test_that("equation general form", {
  expect_equal(
    ob_line(slope = 2, intercept = 3)@equation(type = "general"),
    "\u22122*x* + 1*y* \u2212 3 = 0"
  )
})

test_that("point_at_x returns correct point", {
  l <- ob_line(slope = 2, intercept = 1)
  p <- l@point_at_x(3)
  expect_equal(p@x, 3)
  expect_equal(p@y, 7)
})

test_that("point_at_x errors on vertical line", {
  expect_error(ob_line(xintercept = 1)@point_at_x(0), "Not possible with vertical lines")
})

test_that("point_at_y returns correct point", {
  l <- ob_line(slope = 2, intercept = 1)
  p <- l@point_at_y(5)
  expect_equal(p@x, 2)
  expect_equal(p@y, 5)
})

test_that("point_at_y errors on horizontal line", {
  expect_error(ob_line(intercept = 1)@point_at_y(0), "Not possible with horizontal lines")
})

test_that("projection of point onto line", {
  proj <- projection(ob_point(0, 2), ob_line(slope = 1, intercept = 0))
  expect_equal(proj@x, 1)
  expect_equal(proj@y, 1)
})

test_that("projection via @projection()", {
  l <- ob_line(slope = 1, intercept = 0)
  proj <- l@projection(ob_point(0, 2))
  expect_equal(proj@x, 1)
  expect_equal(proj@y, 1)
})

test_that("equality operator", {
  l1 <- ob_line(slope = 1, intercept = 0)
  l2 <- ob_line(slope = 1, intercept = 0)
  l3 <- ob_line(slope = 2, intercept = 0)
  expect_true(l1 == l2)
  expect_false(l1 == l3)
})

test_that("subsetting by integer index", {
  lv <- ob_line(slope = 1:3, intercept = 0)
  expect_equal(lv[2]@slope, 2)
})

test_that("subsetting by id", {
  lv <- ob_line(slope = 1:3, intercept = 0, id = letters[1:3])
  expect_equal(lv["b"]@slope, 2)
})

test_that("geom no error", {
  expect_no_error(ob_line(slope = 1, intercept = 0)@geom())
  expect_no_error(ob_line(xintercept = 1)@geom())
})

test_that("str no error", {
  expect_no_error(capture.output(str(ob_line(slope = 1, intercept = 2))))
})

test_that(desc = "lines", {
  l111 <- ob_line(a = 1, b = 1, c = 1)
  expect_equal(l111, ob_line(slope = -1, intercept = -1))
  v1 <- ob_line(a = 1, b = 0, c = -1)
  expect_identical(v1, ob_line(xintercept = 1))
  h1 <- ob_line(a = 0, b = 1, c = -1)
  expect_identical(h1, ob_line(intercept = 1))

  expect_no_error(ob_line() == ob_line())

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


})


test_that("Equality operator", {
 expect_true(ob_line(xintercept = 1) == ob_line(a = 1, b = 0, c = -1))
})

