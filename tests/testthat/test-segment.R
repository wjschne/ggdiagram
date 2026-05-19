library(testthat)
library(ggdiagram)

# construction ----
test_that("ob_segment construction", {
  p1 <- ob_point(0, 0)
  p2 <- ob_point(3, 4)

  expect_no_error(ob_segment(p1, p2))
  expect_no_error(ob_segment(x = 1, xend = 2, y = 3, yend = 5))
  expect_no_error(ob_segment(x = 1, y = 3, yend = 5))
  expect_no_error(ob_segment(xend = 1, y = 3, yend = 5))
  expect_no_error(ob_segment(x = 1, xend = 2, yend = 5))
  expect_no_error(ob_segment(x = 1, y = 3, xend = 2))

  # p1 as multi-point chain constructs consecutive segments
  expect_no_error(ob_segment(ob_point(1:3, 1:3)))

  # labels
  expect_no_error(ob_segment(p1, p2, label = ob_label("A")))
  expect_no_error(ob_segment(p1, p2, label = ob_label(c("A", "B"))))
  expect_no_error(ob_segment(p1, p2, label = "A"))

  # style args
  expect_no_error(ob_segment(p1, p2, color = "red", linewidth = 2))

  # error cases
  expect_error(
    ob_segment(),
    "p1 must be a ob_point object with one or more points\\."
  )
  expect_error(
    ob_segment(p1),
    "If p2 is missing, p1 must be a ob_point object with multiple points\\."
  )

  # x/xend/y/yend build correct endpoints
  s_xy <- ob_segment(x = 1, y = 2, xend = 4, yend = 6)
  expect_equal(s_xy@p1@x, 1)
  expect_equal(s_xy@p1@y, 2)
  expect_equal(s_xy@p2@x, 4)
  expect_equal(s_xy@p2@y, 6)
})

# derived values ----
test_that("ob_segment derived values", {
  # 3-4-5 right triangle
  s <- ob_segment(ob_point(0, 0), ob_point(3, 4))
  expect_equal(s@distance, 5)
  expect_equal(s@length, 1L)

  # vectorised length
  p_chain <- ob_point(1:4, 1:4)
  s_chain <- ob_segment(p_chain)
  expect_equal(s_chain@length, 3L)

  # tibble has the required columns
  tib <- s@tibble
  expect_true(all(c("x", "y", "xend", "yend") %in% names(tib)))
})

# midpoint ----
test_that("ob_segment midpoint", {
  s <- ob_segment(ob_point(0, 0), ob_point(3, 4))

  # default (position = 0.5)
  m <- s@midpoint()
  expect_equal(m@x, 1.5, tolerance = 1e-10)
  expect_equal(m@y, 2.0, tolerance = 1e-10)

  # quarter-way
  m25 <- s@midpoint(0.25)
  expect_equal(m25@x, 0.75, tolerance = 1e-10)
  expect_equal(m25@y, 1.0,  tolerance = 1e-10)

  # at endpoints
  expect_equal(s@midpoint(0)@x, 0, tolerance = 1e-10)
  expect_equal(s@midpoint(1)@x, 3, tolerance = 1e-10)

  # identical to midpoint() generic
  expect_identical(s@midpoint(), midpoint(s))
})

# line ----
test_that("ob_segment line property", {
  # slope = -2, intercept = 3  (from (0,3) to (1,1))
  s1 <- ob_segment(ob_point(0, 3), ob_point(1, 1))
  expect_equal(s1@line@intercept, 3)
  expect_equal(s1@line@slope, -2)

  # segment through origin with slope 4/3
  s2 <- ob_segment(ob_point(0, 0), ob_point(3, 4))
  expect_equal(s2@line@slope,     4 / 3, tolerance = 1e-10)
  expect_equal(s2@line@intercept, 0,     tolerance = 1e-10)

  # horizontal segment: slope = 0
  sh <- ob_segment(ob_point(0, 2), ob_point(4, 2))
  expect_equal(sh@line@slope,     0, tolerance = 1e-10)
  expect_equal(sh@line@intercept, 2, tolerance = 1e-10)

  # line is an ob_line object
  expect_true(S7::S7_inherits(s1@line, ob_line))
})

# bounding_box ----
test_that("ob_segment bounding_box", {
  s <- ob_segment(ob_point(1, 2), ob_point(5, 4))
  bb <- s@bounding_box
  expect_true(S7::S7_inherits(bb, ob_rectangle))
  expect_equal(bb@center@x, 3, tolerance = 1e-10)
  expect_equal(bb@center@y, 3, tolerance = 1e-10)
  expect_equal(bb@width,  4, tolerance = 1e-10)
  expect_equal(bb@height, 2, tolerance = 1e-10)

  # bounding box of a horizontal segment has zero height
  sh <- ob_segment(ob_point(0, 2), ob_point(4, 2))
  expect_equal(sh@bounding_box@height, 0, tolerance = 1e-10)
})

# resect ----
test_that("ob_segment resect", {
  # horizontal segment: trim 1 unit from each end
  s <- ob_segment(ob_point(0, 0), ob_point(4, 0))
  sr <- resect(s, 1)
  expect_equal(sr@p1@x, 1, tolerance = 1e-10)
  expect_equal(sr@p1@y, 0, tolerance = 1e-10)
  expect_equal(sr@p2@x, 3, tolerance = 1e-10)
  expect_equal(sr@p2@y, 0, tolerance = 1e-10)
  expect_equal(sr@distance, 2, tolerance = 1e-10)

  # asymmetric resect
  sr2 <- resect(s, distance = 1, distance_end = 2)
  expect_equal(sr2@p1@x, 1, tolerance = 1e-10)
  expect_equal(sr2@p2@x, 2, tolerance = 1e-10)
})

# arithmetic ----
test_that("ob_segment arithmetic", {
  p1 <- ob_point(0, 0)
  p2 <- ob_point(3, 4)
  s  <- ob_segment(p1, p2)

  # + ob_point shifts both endpoints
  s_shifted <- s + ob_point(1, 1)
  expect_equal(s_shifted@p1@x, 1, tolerance = 1e-10)
  expect_equal(s_shifted@p1@y, 1, tolerance = 1e-10)
  expect_equal(s_shifted@p2@x, 4, tolerance = 1e-10)
  expect_equal(s_shifted@p2@y, 5, tolerance = 1e-10)

  # - ob_point shifts back
  s_back <- s_shifted - ob_point(1, 1)
  expect_equal(s_back@p1@x, 0, tolerance = 1e-10)
  expect_equal(s_back@p2@x, 3, tolerance = 1e-10)

  # identity with zero-point
  expect_identical(s + ob_point(0, 0), s)

  # commutativity of +
  expect_identical(ob_point(1, 1) + s, s + ob_point(1, 1))

  # + between two segments
  s2 <- ob_segment(ob_point(1, 0), ob_point(0, 1))
  expect_no_error(s + s2)
})

# nudge ----
test_that("ob_segment nudge", {
  p1 <- ob_point(0, 3)
  p2 <- ob_point(1, 1)
  p3 <- ob_point(2, 5)
  s1 <- ob_segment(p1, p2)

  expect_identical(nudge(s1, 1, 1), ob_segment(ob_point(1, 4), ob_point(2, 2)))
  expect_identical(nudge(s1, x = 1), s1 + ob_point(1, 0))
  expect_identical(nudge(s1, y = 1), s1 + ob_point(0, 1))
  expect_identical(nudge(s1), s1)
  expect_identical(nudge(s1, p3, 1), s1 + (p3 + 1))
  expect_identical(nudge(s1, p3), s1 + p3)
  expect_identical(nudge(s1, p3), s1@nudge(p3))
})

# equality ----
test_that("ob_segment == operator", {
  p1 <- ob_point(0, 0)
  p2 <- ob_point(3, 4)
  s  <- ob_segment(p1, p2)

  expect_identical(`==`(s, s), TRUE)
  expect_identical(`==`(s, ob_segment(p1, ob_point(3, 5))), FALSE)
  expect_identical(`==`(s, ob_segment(ob_point(1, 0), p2)), FALSE)
})

# style ----
test_that("ob_segment style", {
  p1 <- ob_point(0, 0)
  p2 <- ob_point(3, 4)
  s  <- ob_segment(p1, p2)

  s@style <- ob_style(color = "red", linewidth = 2)
  expect_equal(s@color, "red")
  expect_equal(s@linewidth, 2)

  # style at construction
  s2 <- ob_segment(p1, p2, color = "blue", alpha = 0.5)
  expect_equal(s2@color, "blue")
  expect_equal(s2@alpha, 0.5)
})

# geom and aesthetics ----
test_that("ob_segment geom and aesthetics", {
  s  <- ob_segment(ob_point(0, 0), ob_point(3, 4))
  sl <- ob_segment(ob_point(0, 0), ob_point(3, 4), label = ob_label("A"))

  expect_no_error(s@geom())
  expect_no_error(as.geom(s))
  expect_no_error(as.geom(sl))   # includes label geom
  expect_no_error(s@aesthetics)
  expect_no_error(get_tibble(s))
  expect_no_error(get_tibble_defaults(s))

  td <- get_tibble_defaults(s)
  expect_true(all(c("x", "y", "xend", "yend") %in% names(td)))
})

# print ----
test_that("ob_segment print", {
  s <- ob_segment(ob_point(0, 0), ob_point(3, 4))
  expect_no_error(capture.output(print(s)))
})

# hatch ----
test_that("ob_segment hatch", {
  s <- ob_segment(ob_point(0, 0), ob_point(4, 0))
  expect_no_error(s@hatch())
  h1 <- s@hatch(k = 1)
  expect_true(S7::S7_inherits(h1, ob_segment))
  expect_equal(h1@length, 1L)

  h3 <- s@hatch(k = 3)
  expect_equal(h3@length, 3L)

  expect_no_error(s@hatch(k = 2, sep = 0.1, height = 0.2))
})

# rotate ----
test_that("ob_segment rotate", {
  p1 <- ob_point(0, 0)
  p2 <- ob_point(1, 0)
  s  <- ob_segment(p1, p2)

  # rotate 90°: horizontal segment becomes vertical
  sr <- rotate(s, theta = degree(90))
  expect_equal(sr@p2@x, 0, tolerance = 1e-10)
  expect_equal(sr@p2@y, 1, tolerance = 1e-10)

  expect_no_error(rotate(s, theta = 45))
})

# subsetting ----
test_that("ob_segment subsetting", {
  p1 <- ob_point(0, 3)
  p2 <- ob_point(1, 1)
  p3 <- ob_point(2, 5)
  s1 <- ob_segment(p1, p2)
  s2 <- ob_segment(p2, p3, label = ob_label("A"))
  s  <- bind(c(s1, s2))

  expect_equal(s[2], s2)
  expect_equal(s[1]@p1@x, 0)
  expect_equal(s[1]@length, 1L)

  # id-based subsetting
  s_id <- ob_segment(ob_point(1:2, 1), ob_point(1:2, 2), id = c("A", "B"))
  sB   <- ob_segment(ob_point(2, 1), ob_point(2, 2), id = "B")
  expect_equal(s_id["B"], sB)
})

# label positioning ----
test_that("ob_segment set_label_x and set_label_y", {
  s <- ob_segment(
    ob_point(0, 0),
    ob_point(4, 4),
    label = ob_label("A")
  )

  # set_label_x by position
  s_lx <- s@set_label_x(position = 0.25)
  expect_equal(s_lx@label@center@x, 1, tolerance = 1e-6)

  # set_label_x by explicit x-value
  s_lx2 <- s@set_label_x(x = 2)
  expect_equal(s_lx2@label@center@x, 2, tolerance = 1e-6)

  # set_label_y by position
  s_ly <- s@set_label_y(position = 0.75)
  expect_equal(s_ly@label@center@y, 3, tolerance = 1e-6)

  # set_label_y by explicit y-value
  s_ly2 <- s@set_label_y(y = 1)
  expect_equal(s_ly2@label@center@y, 1, tolerance = 1e-6)

  # errors when no label
  s_no_label <- ob_segment(ob_point(0, 0), ob_point(4, 4))
  expect_error(s_no_label@set_label_x(), "The ob_segment does not have a label\\.")
  expect_error(s_no_label@set_label_y(), "The ob_segment does not have a label\\.")
})

# equation ----
test_that("ob_segment equation", {
  s <- ob_segment(ob_point(0, 0), ob_point(1, 1))
  expect_no_error(equation(s))
  expect_no_error(equation(s, type = "general"))
  expect_no_error(equation(s, type = "parametric"))
})

# str ----

test_that("str no error", {
  expect_no_error(capture.output(str(ob_segment(ob_point(c(0,0), c(0,1))))))
})

