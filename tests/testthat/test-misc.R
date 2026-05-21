library(testthat)
library(ggdiagram)

test_that(desc = "polarjust", {
  expect_no_error(polar2just(degree(45), multiplier = 1))
})


test_that(desc = "class_margin", {
  expect_identical(class_margin(list(grid::unit(1, "pt"))), list(class_margin(grid::unit(1,"pt"))))
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
  expect_no_error(set_default_arrowhead())
  expect_no_error(set_default_arrowhead(matrix(c(1,0,0,1), nrow = 2, byrow = TRUE)))
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

# str ----

test_that("str no error", {
  expect_no_error(capture.output(str(ob_point())))
  expect_no_error(capture.output(str(ob_line())))
  expect_no_error(capture.output(str(ob_circle())))
  expect_no_error(capture.output(str(ob_ellipse())))
  expect_no_error(capture.output(str(ob_rectangle())))
  expect_no_error(capture.output(str(ob_segment(ob_point(0, 1:3)))))
  expect_no_error(capture.output(str(ob_arc(start = 0, end = 90, radius = 1, label = "A"))))
  expect_no_error(capture.output(str(ob_bezier(ob_point(c(0, 1, 2), c(0, 2, 0))))))
  expect_no_error(capture.output(str(ob_path(ob_point(c(0, 1, 2), c(0, 2, 0))))))
  expect_no_error(capture.output(str(ob_polygon(ob_point(c(0, 1, 2), c(0, 2, 0))))))
  expect_no_error(capture.output(str(ob_ngon())))
  expect_no_error(capture.output(str(ob_reuleaux())))
})


# bind ----
test_that("bind", {
  # length 0 point bound should be length 0
  p0 <- ob_point(double(0), double(0))
  expect_identical(bind(c(p0))@length, 0L)

   # length 0 point bound to length 1 point should be length 1
  p1 <- ob_point(1, 1)
  expect_identical(bind(c(p0, p1))@length, 1L)
  expect_identical(length(unbind(bind(c(p0, p1)))), 1L)
  expect_identical(length(unbind(ob_shape_list(c(p1, ob_circle())))), 2L)
  expect_identical(length(bind((c(p1, ob_circle())))), 2L)

  # map_ob
  p2 <- map2_ob(ob_circle(), degree(c(0,60)), \(cc, dd) {
    cc@point_at(dd)
  })
  expect_identical(p2@length, 2L)
})

# ob_shape_list ----
test_that("ob_shape_list", {
  expect_no_error(ob_shape_list(list(ob_point(), ob_circle())))
  expect_error(ob_shape_list(list(3)), "All objects must be ggdiagram objects that can be converted to geoms.")
  sl <- ob_shape_list(list(ob_point(), ob_circle()))
  expect_no_error(get_tibble(sl))
  expect_length(bind(ob_shape_list(list(ob_point(), ob_point(1,1), ob_circle()))), 2L)
  expect_no_error(get_tibble(list(ob_point(), ob_circle())))

  expect_no_error(as.geom(sl))

  # Binding a uniform shape list is now ob_point
  expect_true(S7::S7_inherits(bind(ob_shape_list(list(ob_point(), ob_point())))), ob_point)

  # map_ob with shape list

  sl <- map_ob(sl,\(x) {
    S7::set_props(x, color = "blue")
  })
  sl <- map2_ob(sl, sl, \(x, y) {
    S7::set_props(x, color = "blue")
  })

  expect_identical(sl$ob_point@color, "blue")




})

test_that("ob_array", {
  expect_error(ob_array(ob_circle(x = 0, y = 1:2), k = 3), "The shape must start with an object of length 1.")
  expect_no_error(ob_array(ob_circle(x = 0, y = 1, label = "a"), k = 3, anchor = "south"))
  expect_no_error(ob_array(ob_circle(x = 0, y = 1), k = 3, anchor = "south", label = c("a", "b", "c")))
  expect_no_error(ob_array(ob_circle(x = 0, y = 1), k = 3, anchor = "south", label = ob_label(letters[1:3])))

})

# cardinalpoint ----
test_that("cardinalpoint", {
  expect_identical(cardinalpoint("south"), 270)
  expect_error(cardinalpoint("a"), "Position must be an angle, numeric, or one of these named positions:\neast, right, east-northeast, northeast, top right, above right, north-northeast,\nnorth, top, above, north-northwest, northwest, top left, above left,\nwest-northwest, west, left, west-southwest, southwest, bottom left, below left,\nsouth-southwest, south, bottom, below, south-southeast, southeast, bottom right,\nbelow right, east-southeast")
})

# helpers ----
test_that("helpers", {
  # some but not all of a property is missing. Fill with default.
  expect_identical(
    ggdiagram:::get_tibble_defaults_helper(
      ob_point(1:2, 2, alpha = c(.5,NA_integer_)),
      default_style = ob_style(alpha = 1))[,"alpha", drop = TRUE] , c(0.5, 1.0))

  expect_identical(subscript("a"), "a<sub>1</sub>")
  expect_identical(subscript("a", output = "latex"), "a_{1}")
  expect_identical(superscript("a", output = "markdown"), "a<sup>1</sup>")
  expect_identical(superscript("a", output = "latex"), "a^{1}")
  expect_identical(signs_centered(-1), "\u22121\u2007")

  expect_identical(round_probability(.0002, digits = 2, max_digits = 3), "0")
  expect_identical(round_probability(0, digits = 2, max_digits = 3, round_zero_one = F), ".000")
  expect_identical(round_probability(1,phantom_text = "+"), "1<span style='color: white'>+</span>")

  expect_identical(ggdiagram:::.simpleCap("aa"), "Aa")
  expect_identical(ggdiagram:::.between(1, 0,2), TRUE)

  expect_no_error(ggdiagram())
  expect_identical(get_depth("X", "X =~ X1 + X2"), 2L)
  expect_identical(get_depth("X", lavaan::cfa("X =~ X1 + X2")), 2L)
  expect_error(get_depth("X", data.frame(x = 2)), "model must be a lavaan fit object, a lavaan parameter table, or a character vector specifying a lavaan model.")
  expect_error(get_depth("X", "X =~ X1 + X2\nX1 =~ X"), "Maximum depth reached. May be nonrecursive.")


})


test_that("unique", {
  p <- ob_point()
  expect_identical(unique(bind(c(p,p))), p)
  c1 <- ob_circle(label = "a")
  expect_identical(unique(bind(c(c1,c1))), c1)
  c2 <- ob_circle(label = NA_character_)
  expect_equal(unique(bind(c(c2,c2))), c2)
})

test_that("lead and lag cycle", {
  expect_identical(lead_cycle(1:3), c(2L, 3L, 1L))
  expect_identical(lag_cycle(1:3), c(3L, 1L, 2L))

  # too short to cycle
  expect_identical(lead_cycle(1), 1)
  expect_identical(lead_cycle(integer(0)), integer(0))
  expect_identical(lag_cycle(1), 1)
  expect_identical(lag_cycle(integer(0)), integer(0))

  # n too high
  expect_error(lead_cycle(1:2, n = 2), "n must be a positive integer less than length of x.")
  expect_error(lag_cycle(1:2, n = 2), "n must be a positive integer less than length of x.")

  # n non-integer
  expect_error(lead_cycle(1:2, n = 2.1), "n must be a positive integer less than length of x.")
  expect_error(lag_cycle(1:2, n = 2.1), "n must be a positive integer less than length of x.")

  # n negative
  expect_error(lead_cycle(1:2, n = -1), "n must be a positive integer less than length of x.")
  expect_error(lag_cycle(1:2, n = -1), "n must be a positive integer less than length of x.")

})


