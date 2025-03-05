test_that("multiplication",{
  p1 <- ob_point(3, 4)
  expect_identical(p1 * 2, ob_point(6, 8))
  expect_identical(2 * p1, ob_point(6, 8))
  expect_identical(p1 / 2, ob_point(1.5, 2))
  expect_identical(2 / p1, ob_point(2 / 3, 0.5))
})

test_that("perpendicular", {
  expect_identical(ob_point(0, 0) %-|% ob_point(2, 2), ob_point(2, 0))
  expect_identical(ob_point(0, 0) %|-% ob_point(2, 2), ob_point(0, 2))
})


test_that("dotproduct", {
  p1 <- ob_point(0, 1)
  p2 <- ob_point(1, 0)
  expect_equal(p1 %*% p2, c(1, 0) %*% c(0, 1))
})
