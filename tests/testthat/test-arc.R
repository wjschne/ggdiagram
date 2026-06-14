library(testthat)
library(ggdiagram)

# construction ----
test_that("ob_arc construction", {
  expect_no_error(ob_arc())
  expect_no_error(ob_arc(center = ob_point(1, 2), radius = 3, start = 0, end = 90))
  expect_no_error(ob_arc(x = 0, y = 0))
  expect_no_error(ob_arc(x = 0))
  expect_no_error(ob_arc(y = 0))

  # x/y coords stored correctly
  a <- ob_arc(x = 1, y = 2)
  expect_equal(a@center@x, 1)
  expect_equal(a@center@y, 2)

  a@n <- double(0)
  a@type <- "wedge"
  expect_identical(nrow(a@polygon), 361L)

  # start_point / end_point override center
  p1 <- ob_point(1, 2)
  expect_no_error(ob_arc(start_point = p1))
  expect_no_error(ob_arc(end_point = p1))

  # labels
  expect_no_error(ob_arc(label = ob_label(c("A", "B"))))
  expect_identical(ob_arc(label = ob_label("A")), ob_arc(label = "A"))
  expect_identical(
    ob_arc(label = ob_label(degree(34))),
    ob_arc(label = degree(34))
  )

  # style args
  expect_no_error(ob_arc(color = "red", linewidth = 1.5))

  # empty arc
  ea <- ob_arc(center = ob_point(double(0), double(0)))
  expect_length(ea, 0)
  expect_identical(list(ea), unbind(ea))


})

# angle input types ----
test_that("ob_arc angle input types", {
  a_start <- ob_arc(start = 180)
  expect_identical(a_start, ob_arc(start = degree(180)))
  expect_identical(a_start@start@turn, ob_arc(start = radian(pi))@start@turn)
  expect_no_error(ob_arc(start = turn(0.5)))
  expect_no_error(ob_arc(end   = turn(0.5)))

  a_end <- ob_arc(end = 180)
  expect_identical(a_end, ob_arc(end = degree(180)))
  expect_identical(a_end@end@turn, ob_arc(end = radian(pi))@end@turn)
})

# type property ----
test_that("ob_arc type property", {
  expect_equal(ob_arc()@type, "arc")
  expect_no_error(ob_arc(type = "wedge"))
  expect_no_error(ob_arc(type = "segment"))
  expect_error(ob_arc(type = "invalid"))
})

# derived: theta ----
test_that("ob_arc theta", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  expect_equal(a@theta@degree,  90,    tolerance = 1e-10)
  expect_equal(a@theta@radian,  pi / 2, tolerance = 1e-10)

  # negative sweep (going clockwise)
  a_neg <- ob_arc(start = 90, end = 0)
  expect_equal(a_neg@theta@degree, -90, tolerance = 1e-10)
})

# derived: arc_length ----
test_that("ob_arc arc_length", {
  # 90-degree arc with radius 2: length = 2 * pi/2 = pi
  a <- ob_arc(start = 0, end = 90, radius = 2)
  expect_equal(a@arc_length, pi, tolerance = 1e-10)

  # three-quarter arc (270°): length = r * 3*pi/2
  a_3q <- ob_arc(start = 0, end = 270, radius = 2)
  expect_equal(a_3q@arc_length, 2 * 3 * pi / 2, tolerance = 1e-10)

  # half circle
  a_half <- ob_arc(start = 0, end = 180, radius = 1)
  expect_equal(a_half@arc_length, pi, tolerance = 1e-10)
})

# derived: midpoint ----
test_that("ob_arc midpoint", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  tol <- 1e-6

  # default position = 0.5  ->  45° on radius-2 circle = (sqrt(2), sqrt(2))
  m <- a@midpoint()
  expect_equal(m@x, sqrt(2), tolerance = tol)
  expect_equal(m@y, sqrt(2), tolerance = tol)

  # position = 0  ->  start point (2, 0)
  m0 <- a@midpoint(0)
  expect_equal(m0@x, 2, tolerance = tol)
  expect_equal(m0@y, 0, tolerance = tol)

  # position = 1  ->  end point (0, 2)
  m1 <- a@midpoint(1)
  expect_equal(m1@x, 0, tolerance = tol)
  expect_equal(m1@y, 2, tolerance = tol)

  # identical to midpoint() generic
  expect_identical(midpoint(a), a@midpoint())
})

# derived: start_point and end_point ----
test_that("ob_arc start_point and end_point", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  tol <- 1e-10

  expect_equal(a@start_point@x, 2, tolerance = tol)
  expect_equal(a@start_point@y, 0, tolerance = tol)
  expect_equal(a@end_point@x,   0, tolerance = tol)
  expect_equal(a@end_point@y,   2, tolerance = tol)

  # 180-degree arc: start at east (r,0), end at west (-r,0)
  a2 <- ob_arc(start = 0, end = 180, radius = 3)
  expect_equal(a2@start_point@x,  3,  tolerance = tol)
  expect_equal(a2@end_point@x,   -3,  tolerance = tol)
})

# derived: chord ----
test_that("ob_arc chord", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  ch <- a@chord
  tol <- 1e-10

  expect_true(S7::S7_inherits(ch, ob_segment))
  expect_equal(ch@p1@x, 2, tolerance = tol)
  expect_equal(ch@p1@y, 0, tolerance = tol)
  expect_equal(ch@p2@x, 0, tolerance = tol)
  expect_equal(ch@p2@y, 2, tolerance = tol)

  # chord midpoint
  cm <- ch@midpoint()
  expect_equal(cm@x, 1, tolerance = tol)
  expect_equal(cm@y, 1, tolerance = tol)
})

# derived: apothem ----
test_that("ob_arc apothem", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  ap <- a@apothem
  tol <- 1e-10

  expect_true(S7::S7_inherits(ap, ob_segment))
  # runs from center (0,0) to chord midpoint (1,1)
  expect_equal(ap@p1@x, 0, tolerance = tol)
  expect_equal(ap@p1@y, 0, tolerance = tol)
  expect_equal(ap@p2@x, 1, tolerance = tol)
  expect_equal(ap@p2@y, 1, tolerance = tol)
  expect_equal(ap@distance, sqrt(2), tolerance = tol)
})

# derived: sagitta ----
test_that("ob_arc sagitta", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  sg <- a@sagitta
  tol <- 1e-6

  expect_true(S7::S7_inherits(sg, ob_segment))
  # runs from chord midpoint (1,1) to arc midpoint (sqrt(2), sqrt(2))
  expect_equal(sg@p1@x, 1,      tolerance = tol)
  expect_equal(sg@p1@y, 1,      tolerance = tol)
  expect_equal(sg@p2@x, sqrt(2), tolerance = tol)
  expect_equal(sg@p2@y, sqrt(2), tolerance = tol)
  # sagitta length = r - apothem = 2 - sqrt(2)
  expect_equal(sg@distance, 2 - sqrt(2), tolerance = tol)
})

# derived: circle ----
test_that("ob_arc circle", {
  a <- ob_arc(center = ob_point(1, 2), radius = 3, start = 0, end = 90)
  c1 <- a@circle
  expect_true(S7::S7_inherits(c1, ob_circle))
  expect_equal(c1@radius,   3)
  expect_equal(c1@center@x, 1)
  expect_equal(c1@center@y, 2)
})

# derived: bounding_box ----
test_that("ob_arc bounding_box", {
  # 0-90° arc, radius 2, center at origin: box spans (0,0)-(2,2)
  a <- ob_arc(start = 0, end = 90, radius = 2)
  bb <- a@bounding_box
  tol <- 1e-3
  expect_true(S7::S7_inherits(bb, ob_rectangle))
  expect_equal(bb@center@x, 1, tolerance = tol)
  expect_equal(bb@center@y, 1, tolerance = tol)
  expect_equal(bb@width,  2, tolerance = tol)
  expect_equal(bb@height, 2, tolerance = tol)
})

# derived: polygon ----
test_that("ob_arc polygon", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  poly <- a@polygon
  expect_true(is.data.frame(poly))
  expect_true(all(c("x", "y", "group") %in% names(poly)))
})

# length ----
test_that("ob_arc length", {
  expect_equal(ob_arc()@length, 1L)
  a_two <- bind(c(ob_arc(start = 0, end = 90), ob_arc(start = 45, end = 135)))
  expect_equal(a_two@length, 2L)

  # vectorised construction
  a_vec <- ob_arc(start = c(0, 45), end = c(90, 135))
  expect_equal(a_vec@length, 2L)
})

# ob_wedge ----
test_that("ob_wedge", {
  w <- ob_wedge()
  expect_equal(w@type, "wedge")
  expect_equal(w@fill, "black")
  expect_no_error(ob_wedge(end = degree(60), fill = "steelblue"))
  expect_no_error(as.geom(ob_wedge(end = degree(90))))
  expect_no_error(ob_wedge(end = degree(90))@geom())
})

# ob_circular_segment ----
test_that("ob_circular_segment", {
  cs <- ob_circular_segment()
  expect_equal(cs@type, "segment")
  expect_no_error(ob_circular_segment(end = degree(60)))
  expect_no_error(as.geom(ob_circular_segment(end = degree(90))))
})

# point_at ----
test_that("ob_arc point_at", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  tol <- 1e-6

  p45 <- a@point_at(45)
  expect_equal(p45@x, sqrt(2), tolerance = tol)
  expect_equal(p45@y, sqrt(2), tolerance = tol)

  expect_equal(a@point_at(0)@x,  2, tolerance = tol)
  expect_equal(a@point_at(90)@y, 2, tolerance = tol)

  # accepts ob_angle
  expect_no_error(a@point_at(degree(45)))
  expect_no_error(a@point_at(radian(pi / 4)))
})

# angle_at ----
test_that("ob_arc angle_at", {
  a <- ob_arc(center = ob_point(0, 0), radius = 2)
  # point due east of center -> angle = 0°
  expect_equal(a@angle_at(ob_point(2, 0))@degree, 0, tolerance = 1e-10)
  # point due north of center -> angle = 90°
  expect_equal(a@angle_at(ob_point(0, 2))@degree, 90, tolerance = 1e-10)
})

# normal_at and tangent_at ----
test_that("ob_arc normal_at and tangent_at", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  expect_no_error(a@normal_at(0))
  expect_no_error(a@normal_at(45))
  expect_no_error(a@normal_at(degree(90)))
  expect_no_error(a@tangent_at(0))
  expect_no_error(a@tangent_at(45))
  expect_true(S7::S7_inherits(a@tangent_at(0), ob_line))
})

# style ----
test_that("ob_arc style", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  a@style <- ob_style(color = "red", linewidth = 2)
  expect_equal(a@color,     "red")
  expect_equal(a@linewidth, 2)

  a2 <- ob_arc(start = 0, end = 90, color = "blue", alpha = 0.5)
  expect_equal(a2@color, "blue")
  expect_equal(a2@alpha, 0.5)
})

# geom and aesthetics ----
test_that("ob_arc geom and aesthetics", {
  a  <- ob_arc(start = 0, end = 90, radius = 2)
  al <- ob_arc(start = 0, end = 90, radius = 2, label = "A")

  expect_no_error(a@geom())
  expect_no_error(as.geom(a))
  expect_no_error(as.geom(al))          # includes label geom
  expect_no_error(a@aesthetics)
  expect_no_error(get_tibble(a))
  expect_no_error(get_tibble_defaults(a))
  td <- get_tibble_defaults(a)
  expect_true(all(c("x0", "y0", "r", "start", "end") %in% names(td)))
})

# print ----
test_that("ob_arc print", {
  expect_no_error(suppressMessages(capture.output(print(ob_arc(start = 0, end = 90)))))
})

# subsetting ----
test_that("ob_arc subsetting", {
  # vectorised construction gives two arcs
  a_vec <- ob_arc(start = c(0, 45), end = c(90, 135), radius = c(1, 2))
  expect_equal(a_vec[1]@radius,      1)
  expect_equal(a_vec[2]@radius,      2)
  expect_equal(a_vec[1]@start@degree, 0)
  expect_equal(a_vec[2]@end@degree, 135)
  expect_equal(a_vec[1]@length, 1L)

  # id-based subsetting
  a_id <- ob_arc(start = c(0, 45), end = c(90, 135), id = c("A", "B"))
  expect_equal(a_id["B"]@start@degree, 45)
  expect_equal(a_id["A"]@end@degree,   90)

  # bound arcs
  a1 <- ob_arc(start = 0,  end = 90)
  a2 <- ob_arc(start = 45, end = 135)
  ab <- bind(c(a1, a2))
  expect_equal(ab@length, 2L)
  expect_equal(ab[2]@start@degree, 45)
})

# hatch ----
test_that("ob_arc hatch", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  h1 <- a@hatch(k = 1)
  expect_true(S7::S7_inherits(h1, ob_segment))
  expect_equal(h1@length, 1L)

  h3 <- a@hatch(k = 3)
  expect_equal(h3@length, 3L)

  expect_no_error(a@hatch(k = 2, sep = 0.1, height = 0.2))
})

# auto_label ----
test_that("ob_arc auto_label", {
  a <- ob_arc(start = 0, end = 90, radius = 2)
  al <- a@auto_label()

  expect_true(S7::S7_inherits(al, ob_label))
  # label is placed at arc midpoint (sqrt(2), sqrt(2))
  expect_equal(al@center@x, sqrt(2), tolerance = 1e-6)
  expect_equal(al@center@y, sqrt(2), tolerance = 1e-6)

  # custom label text and position
  expect_no_error(a@auto_label(label = "90°", position = 0.25))
})

# set_label_x and set_label_y ----
test_that("ob_arc set_label_x and set_label_y", {
  a_lbl <- ob_arc(start = 0, end = 90, radius = 1, label = "A")
  a_no  <- ob_arc(start = 0, end = 90, radius = 1)

  # error when no label
  expect_error(a_no@set_label_x(), "The ob_arc does not have a label\\.")
  expect_error(a_no@set_label_y(), "The ob_arc does not have a label\\.")

  # set_label_x by explicit x: intersection of unit arc with vertical x=0.5
  lx <- a_lbl@set_label_x(x = 0.5)
  expect_equal(lx@label@center@x, 0.5,       tolerance = 1e-6)
  expect_equal(lx@label@center@y, sqrt(0.75), tolerance = 1e-6)

  # set_label_y by explicit y
  ly <- a_lbl@set_label_y(y = 0.5)
  expect_equal(ly@label@center@y, 0.5, tolerance = 1e-6)
  expect_identical(a_lbl@set_label_x()@label@center@x, a_lbl@label@center@x)
  expect_identical(a_lbl@set_label_y()@label@center@y, a_lbl@label@center@y)

  # set_label_x by position (x comes from midpoint along first arc)
  expect_no_error(a_lbl@set_label_x(position = 0.25))
  expect_no_error(a_lbl@set_label_y(position = 0.75))

  a <- ob_arc(start = 0, end = 90, radius = 1, label = ob_label("A", hjust = 0.5))
  b <- ob_arc(start = 0, end = 90, radius = 1, label = ob_label("B", vjust = 0.5))
  expect_identical(a@label@center@x, b@label@center@x)

  as.geom(ob_arc(arrowhead_length = 5, arrow_head = arrowhead(), label = "A", label_sloped = TRUE))

})


test_that("No arc bends of 0.", {
  expect_error(connect(ob_point(), ob_point(1,1:2), arc_bend = c(0,1)), "An arc cannot have an arc_bend of 0.")
})

test_that("Arc type length == 1", {
  a <- ob_arc(end = c(0,1))

  expect_error({a@type <- c("arc", "wedge")}, "The type property must be of length 1.")
  expect_error({a@type <- c("fred")}, "The type property must be \"arc\", \"wedge\", or \"segment\".")
})

test_that("wedge", {
  expect_no_error(ob_wedge(end = 60))
  expect_equal(ob_wedge(start = 10, end = 20)@bounding_box@southwest@x, 0)
})

test_that("set end_point start_point", {
  a <- ob_arc(center = ob_point(4,3), end = 90, radius = 2)
  a2 <- a
  a2@end_point <- ob_point(-6,10)
  a3 <- a
  a3@start_point <- ob_point(1,2)
  expect_equal(a2@end_point@x, -6)
  expect_equal(a3@start_point@x, 1)
  a@start_point <- ob_point(1,2:3)
  expect_equal(a@length, 2)
  expect_error(a@start_point <- ob_circle(), "start_point must be of class ob_point or ob_polar")
  expect_error(a@end_point <- ob_circle(), "end_point must be of class ob_point or ob_polar")
  a4 <- ob_arc(ob_point(1:4,0), end = 90)
  expect_error(a4@start_point <- ob_point(2,1:5), "The number of points in start_point")
  a5 <- ob_arc(ob_point(1:4,0), end = 90)
  expect_error(a5@end_point <- ob_point(2,1:5), "The number of points in end_point")

})
