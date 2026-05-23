library(testthat)
library(ggdiagram)

# construction ----
test_that("ob_ellipse construction", {
  expect_no_error(ob_ellipse())
  expect_no_error(ob_ellipse(center = ob_point(1, 2), a = 3, b = 2))
  expect_no_error(ob_ellipse(x = 0, y = 0))
  expect_no_error(ob_ellipse(x = 0))
  expect_no_error(ob_ellipse(y = 0))
  expect_no_error(ob_ellipse(a = 1, label = ob_label(c("A", "B"))))
  expect_no_error(ob_ellipse(color = "red"))
  expect_no_error(ob_ellipse(label = c("a", "b")))
  expect_no_error(ob_ellipse(label = degree(45)))
  expect_no_error(ob_ellipse(label = 45))
  expect_no_error(ob_ellipse(label = 45, color = c("red", "blue")))
  expect_no_error(ob_ellipse(angle = 45))
  expect_no_error(ob_ellipse(angle = degree(45)))
  expect_no_error(ob_ellipse(x = 0:2, y = 0, a = 3, b = 2))
  # Incompatible label length should error
  expect_error(ob_ellipse(label = ob_label(1:3), color = c("red", "blue")))
})

# superellipse ----
test_that("ob_ellipse superellipse (m1 != 2)", {
  expect_no_error(ob_ellipse(m1 = 4))
  expect_no_error(ob_ellipse(m1 = 4, m2 = 6))
  e <- ob_ellipse(a = 2, b = 2, m1 = 4)
  # m2 defaults to m1 when unspecified
  expect_equal(e@m1, 4)
  expect_equal(e@m2, 4)
  expect_no_error(e@point_at(45))
  expect_no_error(e@normal_at(45))
  expect_no_error(e@bounding_box)
  expect_no_error(e@point_at(45, definitional = TRUE))
})

# derived values ----
test_that("ob_ellipse derived values", {
  e <- ob_ellipse(a = 3, b = 2)

  # area
  expect_equal(e@area, 6 * pi)

  # perimeter: Ramanujan – for a circle (a == b) it's exact
  e_circle <- ob_ellipse(a = 3, b = 3)
  expect_equal(e_circle@perimeter, 6 * pi, tolerance = 1e-10)

  # length
  expect_equal(e@length, 1L)
  expect_equal(ob_ellipse(x = 0:2, y = 0)@length, 3L)

  # bounding_box (unrotated: box should match axes exactly)
  ec <- ob_ellipse(center = ob_point(1, 2), a = 3, b = 2)
  bb <- ec@bounding_box
  expect_equal(bb@center@x, 1, tolerance = 1e-4)
  expect_equal(bb@center@y, 2, tolerance = 1e-4)
  expect_equal(bb@width,  6, tolerance = 1e-4)
  expect_equal(bb@height, 4, tolerance = 1e-4)

  # tibble columns
  tib <- e@tibble
  expect_true(all(c("x", "y", "a", "b") %in% names(tib)))
})

# foci ----
test_that("ob_ellipse foci", {
  # horizontal ellipse (a > b)
  e <- ob_ellipse(a = 5, b = 4)
  c_val <- sqrt(5^2 - 4^2)   # = 3
  expect_equal(e@focus_1@x, -c_val, tolerance = 1e-10)
  expect_equal(e@focus_1@y,  0,     tolerance = 1e-10)
  expect_equal(e@focus_2@x,  c_val, tolerance = 1e-10)
  expect_equal(e@focus_2@y,  0,     tolerance = 1e-10)

  # vertical ellipse (b > a): foci should be on the y-axis
  e2 <- ob_ellipse(a = 3, b = 5)
  c_val2 <- sqrt(5^2 - 3^2)  # = 4
  expect_equal(e2@focus_1@x, 0,      tolerance = 1e-10)
  expect_equal(e2@focus_1@y, c_val2, tolerance = 1e-10)
  expect_equal(e2@focus_2@x, 0,      tolerance = 1e-10)
  expect_equal(e2@focus_2@y, -c_val2, tolerance = 1e-10)

  # circle: both foci at center
  ec <- ob_ellipse(a = 3, b = 3)
  expect_equal(ec@focus_1@x, 0, tolerance = 1e-10)
  expect_equal(ec@focus_1@y, 0, tolerance = 1e-10)
  expect_equal(ec@focus_2@x, 0, tolerance = 1e-10)
  expect_equal(ec@focus_2@y, 0, tolerance = 1e-10)

  # offset center
  ec2 <- ob_ellipse(center = ob_point(1, 2), a = 5, b = 4)
  expect_equal(ec2@focus_1@x, 1 - c_val, tolerance = 1e-10)
  expect_equal(ec2@focus_1@y, 2,         tolerance = 1e-10)
})

# point_at ----
test_that("ob_ellipse point_at", {
  e <- ob_ellipse(a = 3, b = 2)

  expect_equal(e@point_at(0)@x,   3, tolerance = 1e-6)
  expect_equal(e@point_at(0)@y,   0, tolerance = 1e-6)
  expect_equal(e@point_at(90)@x,  0, tolerance = 1e-6)
  expect_equal(e@point_at(90)@y,  2, tolerance = 1e-6)
  expect_equal(e@point_at(180)@x, -3, tolerance = 1e-6)
  expect_equal(e@point_at(180)@y,  0, tolerance = 1e-6)
  expect_equal(e@point_at(270)@x,  0, tolerance = 1e-6)
  expect_equal(e@point_at(270)@y, -2, tolerance = 1e-6)

  # accepts ob_angle objects
  expect_no_error(e@point_at(degree(45)))
  expect_no_error(e@point_at(radian(pi / 4)))

  # offset center
  ec <- ob_ellipse(center = ob_point(1, 2), a = 3, b = 2)
  expect_equal(ec@point_at(0)@x, 4, tolerance = 1e-6)
  expect_equal(ec@point_at(0)@y, 2, tolerance = 1e-6)
})

# angle_at ----
test_that("ob_ellipse angle_at", {
  e <- ob_ellipse(a = 3, b = 2)
  # point to the right: angle should be ~0
  p_east <- ob_point(3, 0)
  a <- e@angle_at(p_east)
  expect_equal(a@degree, 0, tolerance = 1e-6)
  # point above: angle should be ~90
  p_north <- ob_point(0, 2)
  a2 <- e@angle_at(p_north)
  expect_equal(a2@degree, 90, tolerance = 1e-6)
})

# normal_at and tangent_at ----
test_that("ob_ellipse normal_at and tangent_at", {
  e <- ob_ellipse(a = 3, b = 2)
  expect_no_error(e@normal_at(0))
  expect_no_error(e@normal_at(45))
  expect_no_error(e@normal_at(degree(90)))
  expect_no_error(e@normal_at(ob_point(3, 0)))
  expect_no_error(e@tangent_at(0))
  expect_no_error(e@tangent_at(45))
  expect_no_error(e@tangent_at(degree(90)))
  expect_no_error(e@tangent_at(ob_point(3, 0)))
  # tangent at east edge (theta = 0) should be vertical (undefined slope)
  tl <- e@tangent_at(0)
  expect_true(S7::S7_inherits(tl, ob_line))
})

# polar_line_at ----
test_that("ob_ellipse polar_line_at", {
  e <- ob_ellipse(a = 3, b = 2)
  # polar line at the east vertex of a standard ellipse (3, 0):
  # should give a vertical line at x = a (x = 3), i.e. 1*x + 0*y - 3 = 0
  # From the formula: a = x0*b^m2, b_coef = y0*a^m1, c = -(b^m2 * a^m1) - cx*x0 - cy*y0
  # x0=3, y0=0, a=3, b=2, m1=m2=2, center=(0,0)
  # a_coef = 3 * 4 = 12, b_coef = 0, c = -(4*9) - 0 - 0 = -36
  # Normalized (divide by 12): x - 3 = 0 -> vertical line x = 3
  pl <- e@polar_line_at(ob_point(3, 0))
  expect_true(S7::S7_inherits(pl, ob_line))
  # a*3 + b*y + c = 0 should hold (point lies on the line)
  expect_equal(pl@a * 3 + pl@b * 0 + pl@c, 0, tolerance = 1e-10)
})

# polygon ----
test_that("ob_ellipse polygon", {
  e <- ob_ellipse(a = 3, b = 2)
  expect_no_error(e@polygon)
  poly <- e@polygon
  expect_true(is.data.frame(poly))
  expect_true(all(c("x", "y") %in% names(poly)))
  e <- ob_ellipse(a = 3, b = 2, n = 10)
  expect_identical(nrow(e@polygon), 11L)
})

# compass points ----
test_that("ob_ellipse compass points", {
  e <- ob_ellipse(a = 3, b = 2)

  expect_equal(e@east@x,  3, tolerance = 1e-6)
  expect_equal(e@east@y,  0, tolerance = 1e-6)
  expect_equal(e@west@x, -3, tolerance = 1e-6)
  expect_equal(e@west@y,  0, tolerance = 1e-6)
  expect_equal(e@north@x, 0, tolerance = 1e-6)
  expect_equal(e@north@y, 2, tolerance = 1e-6)
  expect_equal(e@south@x, 0, tolerance = 1e-6)
  expect_equal(e@south@y,-2, tolerance = 1e-6)

  # diagonal compass points are on the ellipse boundary
  expect_no_error(e@northeast)
  expect_no_error(e@northwest)
  expect_no_error(e@southeast)
  expect_no_error(e@southwest)
})

# style ----
test_that("ob_ellipse style", {
  e <- ob_ellipse(a = 2, b = 1)
  e@style <- ob_style(fill = "blue", color = "red", linewidth = 2)
  expect_equal(e@fill, "blue")
  expect_equal(e@color, "red")
  expect_equal(e@linewidth, 2)

  # style passed at construction
  e2 <- ob_ellipse(fill = "green", alpha = 0.5)
  expect_equal(e2@fill, "green")
  expect_equal(e2@alpha, 0.5)
})

# geom and aesthetics ----
test_that("ob_ellipse geom and aesthetics", {
  e <- ob_ellipse(a = 2, b = 1, color = "blue")
  expect_no_error(e@geom())
  expect_no_error(as.geom(e))
  expect_no_error(e@aesthetics)
  expect_no_error(get_tibble(e))
  expect_no_error(get_tibble_defaults(e))
  td <- get_tibble_defaults(e)
  expect_true(all(c("x0", "y0", "a", "b", "angle") %in% names(td)))
})

# print ----
test_that("ob_ellipse print", {
  e <- ob_ellipse(a = 3, b = 2)
  expect_no_error(suppressMessages(capture.output(print(e))))
})

# subsetting ----
test_that("ob_ellipse subsetting", {
  e <- ob_ellipse(x = 0:2, y = 0, a = 3, b = 2)
  expect_equal(e[1]@center@x, 0)
  expect_equal(e[2]@center@x, 1)
  expect_equal(e[3]@center@x, 2)
  expect_equal(e[1]@length, 1L)

  # replacement
  e[2] <- ob_ellipse(x = 5, y = 0, a = 3, b = 2)
  expect_equal(e[2]@center@x, 5)
})

# arithmetic (+ / - with ob_point) ----
test_that("ob_ellipse arithmetic with ob_point", {
  e <- ob_ellipse(a = 3, b = 2)
  expect_identical(e + ob_point(), e)
  expect_identical(e - ob_point(), e)
  e_shifted <- e + ob_point(1, 2)
  expect_equal(e_shifted@center@x, 1)
  expect_equal(e_shifted@center@y, 2)
  e_back <- e_shifted - ob_point(1, 2)
  expect_equal(e_back@center@x, 0)
  expect_equal(e_back@center@y, 0)
})

# ob_array ----
test_that("ob_ellipse ob_array", {
  e <- ob_ellipse(a = 2, b = 1)
  arr <- ob_array(e, k = 4)
  expect_equal(arr@length, 4L)
  expect_no_error(ob_ellipse() |> ob_array(2))
})

# place ----
test_that("ob_ellipse place", {
  e1 <- ob_ellipse(a = 2, b = 1)
  e2 <- ob_ellipse(a = 1, b = 1)
  expect_no_error(place(e2, e1, where = "right"))
  expect_no_error(place(ob_point(0, 0), e1))
  expect_no_error(place(e2, ob_point(0, 0)))
  expect_no_error(place(ob_line(1, 0), e1))
})

# connect ----
test_that("ob_ellipse connect", {
  e1 <- ob_ellipse(x = -2, y = 0, a = 1, b = 0.5)
  e2 <- ob_ellipse(x =  2, y = 0, a = 1, b = 0.5)
  # centerpoint to centerpoint
  expect_no_error(connect(e1, e2))
  expect_no_error(connect(e1, ob_point(2, 0)))
  expect_no_error(connect(ob_point(-2, 0), e2))
  ## arc
  expect_no_error(connect(e1, e2, arc_bend = .1))
  expect_no_error(connect(e1, e2@center, arc_bend = .1))
  expect_no_error(connect(e1@center, e2, arc_bend = .1))
  ## bezier
  expect_no_error(connect(e1, e2, from_offset = ob_point(0,1), to_offset = ob_point(0, -1)))
  expect_no_error(connect(e1, e2@center, from_offset = ob_point(0,1), to_offset = ob_point(0, -1)))
  expect_no_error(connect(e1@center, e2, from_offset = ob_point(0,1), to_offset = ob_point(0, -1)))
  ## 1 to many
  expect_no_error(connect(e1, ob_ellipse(x =  2:3, y = 0, a = 1, b = 0.5)))
  expect_no_error(connect(ob_ellipse(x =  2:3, y = 0, a = 1, b = 0.5), e1))
  expect_no_error(connect(e1, ob_ellipse(x =  2:3, y = 0, a = 1, b = 0.5), arc_bend = .5))
  expect_no_error(connect(ob_ellipse(x =  2:3, y = 0, a = 1, b = 0.5), e1, arc_bend = .5))

  # centerpoint to line
  expect_no_error(connect(e1, ob_line(slope = 3)))
  expect_no_error(connect(ob_line(slope = 3), e1))


  # list
  expect_no_error(connect(c(e1, e2), ob_circle()))
  expect_no_error(connect(ob_shape_list(c(e1, e2)), ob_circle()))
  expect_error(connect(list(e1, 2), ob_circle()), "List must contain shape objects.")



  expect_no_error(connect(c(ob_line(slope = 3)), e1))

  expect_equal(midpoint(e1, e2)@x, ob_point(0,0)@x)

  expect_no_error(ob_variance(e1, theta = 20, where = 90, bend = 10, label = "hello"))
  expect_no_error(ob_variance(e1, label = "hello", linewidth = .2, arrowhead_length = 2))

  expect_no_error(ob_covariance(e1, e2, where = 90, bend = 10, label = "hello", linewidth = .2, arrowhead_length = 2))
  expect_no_error(ob_covariance(e1, e2, label = "hello", linewidth = .2, arrowhead_length = 2))

  expect_no_error(place(ob_ellipse(label = "e"), ob_circle(label = "c")))
  expect_no_error(place(ob_circle(label = "a"), ob_point()))
  expect_error(e1[1] <- ob_circle(), "Replacement value must be of the same type.")
  expect_no_error(e1[1] <- ob_ellipse(label = "e"))
  e3 <- ob_ellipse(label = "e", id = "id3")
  e3["id3"] <- ob_ellipse()
  expect_equal(e3@id, "id3")
  e4 <- ob_ellipse(label = "e")
  e4[1] <- ob_ellipse(id = "id4")
  expect_equal(e4@id, "id4")
  e4[1] <- ob_ellipse(id = "id5")
  expect_equal(e4@id, "id5")



})

# data2shape ----
test_that("ob_ellipse data2shape", {
  d <- data.frame(x = 1, y = 2, a = 3, b = 2)
  e <- data2shape(d, ob_ellipse)
  expect_true(S7::S7_inherits(e, ob_ellipse))
  expect_equal(e@center@x, 1)
  expect_equal(e@center@y, 2)
  expect_equal(e@a, 3)
  expect_equal(e@b, 2)
})
