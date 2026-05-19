library(testthat)
library(ggdiagram)

# circle ----
test_that(desc = "circle", {
  # area
  c1 <- ob_circle(radius = 3)
  c1@style <- ob_style(fill = "red")
  expect_equal(c1@fill, "red")
  expect_no_error(c1@bounding_box)
  expect_no_error(c1@polygon)
  expect_no_error(c1@geom())
  expect_no_error(c1@arc(3,4))
  expect_no_error(c1@angle_at(4))
  expect_no_error(c1@normal_at(4))
  expect_no_error(c1@normal_at(ob_point(3,4)))
  expect_no_error(c1@tangent_at(4))
  expect_equal(c1@tangent_at(0)@xintercept, 3)
  expect_no_error(c1@tangent_at(ob_point(5,5)))
  expect_equal(c1@tangent_at(ob_point(5,0))@xintercept, 3)
  expect_no_error(c1@point_at(4))
  expect_no_error(capture.output(print(c1), file = nullfile()))
  expect_no_error(place(ob_line(3,4), c1))
  expect_no_error(ob_array(c1, 4))
  expect_equal(c1@length, 1)
  expect_equal(c1@area , expected = 9 * pi)
  # perimeter
  expect_equal(c1@circumference, 6 * pi)
  expect_equal(c1@diameter, 6)
  expect_no_error(ob_circle(x = 0, y = 0))
  expect_no_error(ob_circle(x = 0))
  expect_no_error(ob_circle(y = 0))
  expect_no_error(ob_circle(color = 1, label = ob_label(c("A", "B"))))
  p1 <- ob_point(0, 3)
  p2 <- ob_point(1, 1)
  p3 <- ob_point(1, 7)
  expect_no_error(circle_from_3_points(p1, p2, p3))
  expect_no_error(circle_from_3_points(bind(c(p1, p2, p3))))
  expect_error(circle_from_3_points(bind(c(ob_point(1, 2), p2, p3))),
               "Points on the same line cannot lie on a circle\\.")

  expect_error(circle_from_3_points(bind(c(ob_point(1, 1:2), p2, p3))),
               "p1 must be of length 3 or p1, p2, and p2 must be of length 1\\.")
  expect_no_error(as.geom(ob_circle(color = "red")))

  c1 <- ob_circle(label = ob_label("A"))
  c2 <- ob_circle(x = 1, y = 1, label = ob_label("A"))
  expect_equal(c1@xy[1,1], c(x = 0))
  expect_no_error(as.geom(c1))
  expect_identical(c1 + ob_point(), c1)
  expect_identical(c1 - ob_point(), c1)
  expect_identical(ob_point() + c1, c1)
  expect_identical(ob_point() - c1, c1)
  expect_identical(c1 %|-% ob_point(1, 1),
                   ob_point(x = 0, y = 1))
  expect_identical(c1 %-|% ob_point(1, 1),
                   ob_point(x = 1, y = 0))
  expect_identical(ob_point(1, 1) %|-% c1,
                   ob_point(x = 1, y = 0))
  expect_identical(ob_point(1, 1) %-|% c1,
                   ob_point(x = 0, y = 1))
  expect_identical(c2 %|-% c1,
                   ob_point(x = 1, y = 0))
  expect_identical(c2 %-|% c1,
                   ob_point(x = 0, y = 1))

  expect_identical(nudge(ob_circle(), 1, 1),
                   ob_circle(x = 1, y = 1))
  expect_identical(nudge(ob_circle(), x = 1),
                   ob_circle(x = 1, y = 0))
  expect_identical(nudge(ob_circle(), y = 1),
                   ob_circle(x = 0, y = 1))

  expect_identical(data2shape(data.frame(x  = 1, y = 2), ob_circle),
                   ob_circle(x = 1, y = 2))
})

test_that("circle compass points", {
  c1 <- ob_circle()
  expect_equal(c1@east@x, 1)
  expect_equal(c1@east@y, 0)
  expect_equal(c1@west@x, -1)
  expect_equal(c1@west@y, 0)
  expect_equal(c1@north@x, 0)
  expect_equal(c1@north@y, 1)
  expect_equal(c1@south@x, 0)
  expect_equal(c1@south@y, -1)
  expect_equal(c1@northeast@x, sqrt(2) / 2, tolerance = 1e-6)
  expect_equal(c1@northeast@y, sqrt(2) / 2, tolerance = 1e-6)
  expect_equal(c1@northwest@x, -sqrt(2) / 2, tolerance = 1e-6)
  expect_equal(c1@northwest@y, sqrt(2) / 2, tolerance = 1e-6)
  expect_equal(c1@southeast@x, sqrt(2) / 2, tolerance = 1e-6)
  expect_equal(c1@southeast@y, -sqrt(2) / 2, tolerance = 1e-6)
  expect_equal(c1@southwest@x, -sqrt(2) / 2, tolerance = 1e-6)
  expect_equal(c1@southwest@y, -sqrt(2) / 2, tolerance = 1e-6)
})

test_that("circle derived values", {
  c2 <- ob_circle(center = ob_point(1, 2), radius = 3)

  # point_at
  expect_equal(c2@point_at(0)@x, 4)
  expect_equal(c2@point_at(0)@y, 2)

  # bounding_box
  expect_equal(c2@bounding_box@center@x, 1)
  expect_equal(c2@bounding_box@center@y, 2)
  expect_equal(c2@bounding_box@width, 6)
  expect_equal(c2@bounding_box@height, 6)

  # polar_line_at: point on east edge of unit circle -> vertical line x = 1
  c1 <- ob_circle()
  l <- c1@polar_line_at(ob_point(1, 0))
  expect_equal(l@a, 1)
  expect_equal(l@b, 0)
  expect_equal(l@c, -1)

  # get_tibble_defaults
  td <- get_tibble_defaults(ob_circle())
  expect_true(all(c("x0", "y0", "r") %in% names(td)))

  # circle_from_3_points return values
  c3p <- circle_from_3_points(ob_point(1, 1), ob_point(2, 4), ob_point(5, 3))
  expect_true(S7::S7_inherits(c3p, ob_circle))
  expect_equal(c3p@center@x, 3)
  expect_equal(c3p@center@y, 2)
  expect_equal(c3p@radius, sqrt(5), tolerance = 1e-6)
})

test_that("circle equation", {
  c1 <- ob_circle()
  expect_equal(
    equation(c1),
    "*x*<sup>2</sup> + *y*<sup>2</sup> = 1<sup>2</sup>"
  )
  expect_equal(equation(c1, type = "parametric"), "cos(*t*)<br>sin(*t*)")

  c2 <- ob_circle(center = ob_point(1, 2), radius = 3)
  expect_equal(
    equation(c2),
    "(*x* \u2212 1)<sup>2</sup> + (*y* \u2212 2)<sup>2</sup> = 3<sup>2</sup>"
  )
  expect_equal(
    equation(c2, type = "parametric"),
    "3cos(*t*) + 1<br>3sin(*t*) + 2"
  )
})


test_that("subsetting circles", {
  x <- ob_circle(x = 0:1, y = 2)
  x1 <- ob_circle(x = 0, y = 2)
  x2 <- ob_circle(x = c(0,0), y = 2)
  x11 <- x[1]
  expect_equal(x11@tibble, x1@tibble)
  x[2] <- ob_circle(x = 0, y = 2)
  expect_equal(x@tibble, x2@tibble)

})



