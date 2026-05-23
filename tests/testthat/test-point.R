library(testthat)
library(ggdiagram)

test_that("constructor defaults", {
  p <- ob_point()
  expect_identical(p@x, 0)
  expect_identical(p@y, 0)
})

test_that("constructor with style arguments", {
  p <- ob_point(1, 2, color = "red", size = 3)
  expect_identical(p@color, "red")
  expect_identical(p@size, 3)
})

test_that("constructor from data.frame", {
  df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  p <- ob_point(df)
  expect_equal(p@x, c(1, 2, 3))
  expect_equal(p@y, c(4, 5, 6))
})

test_that("length property", {
  expect_equal(ob_point(1:3, 4:6)@length, 3)
})

test_that("r getter", {
  expect_equal(ob_point(3, 4)@r, 5)
  expect_equal(ob_point(0, 0)@r, 0)
})

test_that("theta getter", {
  expect_equal(ob_point(1, 0)@theta@degree, 0)
  expect_equal(ob_point(0, 1)@theta@degree, 90)
})

test_that("xy property", {
  expect_equal(ob_point(1, 2)@xy, cbind(x = 1, y = 2))
})

test_that("bounding_box", {
  bb <- ob_point(c(1, 3), c(2, 4))@bounding_box
  expect_equal(bb@southwest, ob_point(1, 2, color = "black", fill = NA))
  expect_equal(bb@northeast, ob_point(3, 4, color = "black", fill = NA))
})

test_that("tibble property", {
  t <- ob_point(1, 2)@tibble
  expect_equal(t$x, 1)
  expect_equal(t$y, 2)
})

test_that("style getter and setter", {
  p <- ob_point(1, 1, color = "blue")
  expect_identical(p@style@color, "blue")
  p@style <- ob_style(color = "red")
  expect_identical(p@color, "red")
})

test_that("midpoint", {
  p1 <- ob_point(0, 0)
  p2 <- ob_point(2, 4)
  expect_identical(midpoint(p1, p2), ob_point(1, 2))
  expect_identical(midpoint(p1, p2, position = 0.25), ob_point(0.5, 1))
})

test_that("subsetting assignment", {
  p <- ob_point(c(1, 2, 3), c(4, 5, 6))
  p[2] <- ob_point(10, 20)
  expect_equal(p@x[2], 10)
  expect_equal(p@y[2], 20)
  expect_equal(p@x[1], 1)
  expect_equal(p@x[3], 3)
})

test_that("ob_polar coordinates", {
  expect_equal(ob_polar(degree(0), r = 1)@x, 1)
  expect_equal(ob_polar(degree(0), r = 1)@y, 0, tolerance = 1e-10)
  expect_equal(ob_polar(degree(90), r = 1)@x, 0, tolerance = 1e-10)
  expect_equal(ob_polar(degree(90), r = 1)@y, 1)
  expect_equal(ob_polar(degree(0), r = 3)@r, 3)
})

test_that("matrix input error message", {
  m <- matrix(1:9, ncol = 3)
  expect_error(ob_point(m), "The input matrix must have 2 columns, not 3")
})

test_that("str method no error", {
  expect_no_error(suppressMessages(capture.output(str(ob_point(1, 2)))))
  expect_no_error(suppressMessages(capture.output(str(ob_polar(degree(45))))))
})

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

test_that("ob_polar defaults to r=1 and theta=0", {
  p <- ob_polar()
  expect_equal(p@r, 1)
  expect_equal(p@theta@degree, 0)
  expect_equal(p@x, 1)
  expect_equal(p@y, 0, tolerance = 1e-10)
})

test_that("ob_polar r defaults to 1 when only theta given", {
  expect_equal(ob_polar(degree(90))@r, 1)
})

test_that("ob_polar x/y at cardinal angles", {
  expect_equal(ob_polar(degree(0),   r = 1)@x,  1,  tolerance = 1e-10)
  expect_equal(ob_polar(degree(0),   r = 1)@y,  0,  tolerance = 1e-10)
  expect_equal(ob_polar(degree(90),  r = 1)@x,  0,  tolerance = 1e-10)
  expect_equal(ob_polar(degree(90),  r = 1)@y,  1,  tolerance = 1e-10)
  expect_equal(ob_polar(degree(180), r = 1)@x, -1,  tolerance = 1e-10)
  expect_equal(ob_polar(degree(180), r = 1)@y,  0,  tolerance = 1e-10)
  expect_equal(ob_polar(degree(270), r = 1)@x,  0,  tolerance = 1e-10)
  expect_equal(ob_polar(degree(270), r = 1)@y, -1,  tolerance = 1e-10)
})

test_that("ob_polar theta getter roundtrip", {
  expect_equal(ob_polar(degree(0),   r = 1)@theta@degree,   0)
  expect_equal(ob_polar(degree(90),  r = 1)@theta@degree,  90)
  expect_equal(ob_polar(degree(180), r = 1)@theta@degree, 180)
  expect_equal(ob_polar(degree(45),  r = 2)@theta@degree,  45)
  # 270 deg normalises to -90 via atan2
  expect_equal(ob_polar(degree(270), r = 1)@theta@degree, -90)
})

test_that("ob_polar r getter matches input", {
  expect_equal(ob_polar(degree(0),  r = 2)@r, 2)
  expect_equal(ob_polar(degree(45), r = 5)@r, 5)
})

test_that("ob_polar accepts radian and turn inputs", {
  expect_equal(ob_polar(radian(pi), r = 1)@x, -1, tolerance = 1e-10)
  expect_equal(ob_polar(radian(pi), r = 1)@y,  0, tolerance = 1e-10)
  expect_equal(ob_polar(turn(0.25), r = 1)@x,  0, tolerance = 1e-10)
  expect_equal(ob_polar(turn(0.25), r = 1)@y,  1, tolerance = 1e-10)
})

test_that("ob_polar accepts cardinal direction strings", {
  expect_equal(ob_polar("east",  r = 1)@x,  1, tolerance = 1e-10)
  expect_equal(ob_polar("west",  r = 1)@x, -1, tolerance = 1e-10)
  expect_equal(ob_polar("north", r = 1)@y,  1, tolerance = 1e-10)
  expect_equal(ob_polar("south", r = 1)@y, -1, tolerance = 1e-10)
})

test_that("ob_polar accepts vector theta and r", {
  p <- ob_polar(degree(c(0, 90, 180, 270)), r = 2)
  expect_equal(p@length, 4)
  expect_equal(p@x, c(2, 0, -2, 0), tolerance = 1e-10)
  expect_equal(p@y, c(0, 2, 0, -2), tolerance = 1e-10)
})

test_that("ob_polar accepts style arguments", {
  p <- ob_polar(degree(45), r = 1, color = "red", size = 3)
  expect_identical(p@color, "red")
  expect_identical(p@size, 3)
})

test_that("ob_polar inherits from ob_point", {
  expect_true(S7::S7_inherits(ob_polar(), ob_point))
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
  expect_no_error(suppressMessages(capture.output(print(ob_polar()))))

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

  expect_error(ob_point(id = "a")["b"], "There are no objects with an id equal to the value specified.")


})
