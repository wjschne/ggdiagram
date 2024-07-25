test_that("constructor", {
  # Point
  a <- point(0,0)
  a@x <- 3
  expect_identical(a, point(3,0))
  aa <- point(c(2, 2, 3), c(4, 1, 3))
  bb <- bind_shape(c(point(2, 4), point(2, 1), point(3, 3)))
  cc <- point(x = c(2, 3), y = 1)
  dd <- point(x = 1, y = c(2, 2))
  expect_identical(aa@x, bb@x)
  expect_identical(aa@y, bb@y)
  expect_identical(polar(r = 1, theta = 0)@x, point(x = 1, y = 0)@x)
  expect_identical(bb@x, c(2, 2, 3))
  expect_identical(bb@y, c(4, 1, 3))
  expect_identical(bb@xy, cbind(x = c(2, 2, 3),
                                y = c(4, 1, 3)))
  expect_true(all(abs(bb@theta@radian - atan(c(2, 0.5, 1)) ) < .Machine$double.eps))
  expect_identical(bb@r, c(sqrt(20), sqrt(5), sqrt(18)))
  expect_identical(aa, point(aa@x, aa@y))
})

test_that("multiplication",{
  p1 <- point(3,4)
  expect_identical(p1 * 2, point(6,8))
  expect_identical(2 * p1, point(6,8))
  expect_identical(p1 / 2, point(1.5,2))
  expect_identical(2 / p1, point(2/3,0.5))
})

test_that("perpendicular", {
  expect_identical(point(0,0) %-|% point(2,2), point(2,0))
  expect_identical(point(0,0) %|-% point(2,2), point(0,2))
})


test_that("dotproduct", {
  p1 <- point(0, 1)
  p2 <- point(1, 0)
  expect_equal(p1 %*% p2, c(1,0) %*% c(0,1))
})

