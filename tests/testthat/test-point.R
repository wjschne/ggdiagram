library(testthat)
library(ggdiagram)

test_that("multiplication", {
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


test_that("matrix input", {
  m <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
  expect_no_error(ob_point(m))
  expect_error(ob_point(t(m)))
})

test_that("rotate point", {
  p1 <- ob_point(0, 1)
  expect_no_error(rotate(p1, 45))
})

# ob_polar
test_that("no radius", {
  expect_no_error(ob_polar())
  expect_no_error(ob_polar())
  expect_no_error(ob_polar(theta = "east"))

})

test_that("point misc", {
  p <- ob_point()

  expect_identical(p@place(ob_point(1, 1), "left"), ob_point(0, 1))

  expect_identical(p@label()@label, "(0,0)")
  expect_identical(p@label(1)@label, "1")
  expect_identical(p@label(.245)@label, ".2")
  expect_identical(p@label("A")@label, "A")
  expect_identical(p@auto_label, label_object(p))
  p@r <- 1
  expect_identical(p, ob_point(1, 0))
  p@theta <- degree(90)
  expect_identical(p, ob_point(0, 1))
  p <- ob_point(c(1, 3), c(3, 5))
  expect_identical(p@centroid, ob_point(2, 4))
  expect_no_error(p@geom())
  expect_no_error(capture.output(print(ob_polar()), file = nullfile()))

  p1 <- ob_point()
  p2 <- ob_point(1, 1)

  expect_equal(polar2just(0, numeric(0)), -0.1)
  expect_equal(p1 == ob_point(), TRUE)
  expect_equal(p1 %*% p2, matrix(0))
  expect_identical(label_object(ob_point(1.2, 1.2)), "(1.2,1.2)")

  expect_no_error(connect(p1, p2))
  expect_identical(place(p1, p2), ob_point(2, 1))
  expect_identical(nudge(p1, x = 1, y = 1), p2)
  expect_identical(nudge(p1, x = 1), ob_point(1, 0))
  expect_identical(nudge(p1, y = 1), ob_point(0, 1))
  expect_identical(ob_array(p1, 2, anchor = "east"), ob_point(c(-1, 0), 0))

  expect_no_error(ob_covariance(
    p1,
    p2,
    where = "west",
    bend = 10,
    label = "A"
  ))
  expect_no_error(ob_covariance(p1, p2))

  p <- ob_point(1:3, 4:6, id = letters[1:3])
  expect_identical(p["b"], p[2])


})
