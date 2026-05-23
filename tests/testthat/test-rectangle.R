library(testthat)
library(ggdiagram)

# construction ----
test_that("ob_rectangle construction", {
  a <- ob_point(1, 2)

  expect_no_error(ob_rectangle())
  expect_no_error(ob_rectangle(center = ob_point(1, 2), width = 3, height = 2))
  expect_no_error(ob_rectangle(angle = 45))
  expect_no_error(ob_rectangle(angle = degree(45)))
  expect_no_error(ob_rectangle(x = 3))
  expect_no_error(ob_rectangle(y = 3))
  expect_no_error(ob_rectangle(vertex_radius = 0.1))
  expect_no_error(ob_rectangle(label = ob_label(c("a", "b"))))

  # anchor-based construction
  expect_no_error(ob_rectangle(north = a))
  expect_no_error(ob_rectangle(north = a, width = 3))
  expect_no_error(ob_rectangle(north = a, height = 3))
  expect_no_error(ob_rectangle(south = a))
  expect_no_error(ob_rectangle(south = a, width = 3))
  expect_no_error(ob_rectangle(south = a, height = 3))
  expect_no_error(ob_rectangle(west = a))
  expect_no_error(ob_rectangle(west = a, width = 3))
  expect_no_error(ob_rectangle(west = a, height = 3))
  expect_no_error(ob_rectangle(east = a))
  expect_no_error(ob_rectangle(east = a, width = 3))
  expect_no_error(ob_rectangle(east = a, height = 3))
  expect_no_error(ob_rectangle(northeast = ob_point(2, 1), southwest = ob_point(-2, -1)))
  expect_no_error(ob_rectangle(northwest = ob_point(-2, 1), southeast = ob_point(2, -1)))

  # vertex_radius length > 1 errors
  expect_error(
    ob_rectangle(vertex_radius = c(0.1, 0.2)),
    "The vertex_radius property must be of length 1\\."
  )
})

# construction from corners ----
test_that("ob_rectangle construction from corners recovers dimensions", {
  # northeast + southwest fully define the box
  r <- ob_rectangle(
    northeast = ob_point(3, 2),
    southwest = ob_point(-1, -2)
  )
  expect_equal(r@width,  4)
  expect_equal(r@height, 4)
  expect_equal(r@center@x, 1)
  expect_equal(r@center@y, 0)

  # northwest + southeast
  r2 <- ob_rectangle(
    northwest = ob_point(-2, 3),
    southeast = ob_point(4, -1)
  )
  expect_equal(r2@width,  6)
  expect_equal(r2@height, 4)
  expect_equal(r2@center@x, 1)
  expect_equal(r2@center@y, 1)

  # north anchor + height
  r3 <- ob_rectangle(north = ob_point(0, 3), width = 4, height = 2)
  expect_equal(r3@center@y, 2)
  expect_equal(r3@height, 2)

  # south anchor + height
  r4 <- ob_rectangle(south = ob_point(0, -1), width = 4, height = 2)
  expect_equal(r4@center@y, 0)

  # east anchor + width: center is east@x - width/2
  r5 <- ob_rectangle(east = ob_point(4, 0), width = 4, height = 2)
  expect_equal(r5@center@x, 2)
})

# equality operator ----
test_that("ob_rectangle == operator", {
  r <- ob_rectangle()
  expect_identical(`==`(r, ob_rectangle()), TRUE)
  expect_identical(`==`(r, ob_rectangle(width = 3)), FALSE)
  expect_identical(`==`(r, ob_rectangle(height = 3)), FALSE)
  expect_identical(`==`(r, ob_rectangle(angle = 10)), FALSE)
  expect_identical(`==`(r, ob_rectangle(x = 1, y = 0)), FALSE)
})

# derived values ----
test_that("ob_rectangle derived values", {
  r <- ob_rectangle(width = 3, height = 2)

  expect_equal(r@area, 6)
  expect_equal(r@perimeter, 10)
  expect_equal(r@length, 1L)

  r_unit <- ob_rectangle()
  expect_identical(r_unit@area, 1)
  expect_identical(r_unit@perimeter, 4)

  # vectorised length
  expect_equal(ob_rectangle(height = 1:3)@length, 3L)
})

# compass points ----
test_that("ob_rectangle compass points", {
  r <- ob_rectangle(width = 4, height = 2)

  expect_equal(r@east@x,  2, tolerance = 1e-10)
  expect_equal(r@east@y,  0, tolerance = 1e-10)
  expect_equal(r@west@x, -2, tolerance = 1e-10)
  expect_equal(r@west@y,  0, tolerance = 1e-10)
  expect_equal(r@north@x, 0, tolerance = 1e-10)
  expect_equal(r@north@y, 1, tolerance = 1e-10)
  expect_equal(r@south@x, 0, tolerance = 1e-10)
  expect_equal(r@south@y,-1, tolerance = 1e-10)

  expect_equal(r@northeast@x,  2, tolerance = 1e-10)
  expect_equal(r@northeast@y,  1, tolerance = 1e-10)
  expect_equal(r@northwest@x, -2, tolerance = 1e-10)
  expect_equal(r@northwest@y,  1, tolerance = 1e-10)
  expect_equal(r@southwest@x, -2, tolerance = 1e-10)
  expect_equal(r@southwest@y, -1, tolerance = 1e-10)
  expect_equal(r@southeast@x,  2, tolerance = 1e-10)
  expect_equal(r@southeast@y, -1, tolerance = 1e-10)

  # offset center
  rc <- ob_rectangle(center = ob_point(1, 2), width = 4, height = 2)
  expect_equal(rc@east@x, 3, tolerance = 1e-10)
  expect_equal(rc@east@y, 2, tolerance = 1e-10)
  expect_equal(rc@north@x, 1, tolerance = 1e-10)
  expect_equal(rc@north@y, 3, tolerance = 1e-10)
})

# bounding_box ----
test_that("ob_rectangle bounding_box", {
  r <- ob_rectangle(width = 4, height = 2)
  bb <- r@bounding_box
  expect_equal(bb@center@x, 0, tolerance = 1e-10)
  expect_equal(bb@center@y, 0, tolerance = 1e-10)
  expect_equal(bb@width,  4, tolerance = 1e-10)
  expect_equal(bb@height, 2, tolerance = 1e-10)

  # rotated rectangle: bounding box is strictly larger
  r45 <- ob_rectangle(width = 4, height = 2, angle = 45)
  bb45 <- r45@bounding_box
  expect_gt(bb45@width,  4)
  expect_gt(bb45@height, 2)
})

# point_at ----
test_that("ob_rectangle point_at", {
  r <- ob_rectangle(width = 4, height = 2)

  # cardinal directions land on the correct edge midpoints
  expect_equal(r@point_at(0)@x,    2, tolerance = 1e-10)
  expect_equal(r@point_at(0)@y,    0, tolerance = 1e-10)
  expect_equal(r@point_at(90)@x,   0, tolerance = 1e-10)
  expect_equal(r@point_at(90)@y,   1, tolerance = 1e-10)
  expect_equal(r@point_at(180)@x, -2, tolerance = 1e-10)
  expect_equal(r@point_at(180)@y,  0, tolerance = 1e-10)
  expect_equal(r@point_at(270)@x,  0, tolerance = 1e-10)
  expect_equal(r@point_at(270)@y, -1, tolerance = 1e-10)

  # accepts ob_angle and degree objects
  expect_no_error(r@point_at(degree(45)))
  expect_no_error(r@point_at(radian(pi / 4)))
  expect_no_error(r@point_at(NA))
  expect_no_error(r@point_at("east"))
  expect_no_error(r@point_at("north"))
  expect_no_error(r@point_at("west"))
  expect_no_error(r@point_at("south"))

  # angles with NA become 0
  r@point_at(degree(c(0,45, NA, 60)))
})

# normal_at ----
test_that("ob_rectangle normal_at", {
  r <- ob_rectangle(width = 4, height = 2)
  expect_no_error(r@normal_at(0))
  expect_no_error(r@normal_at(45))
  expect_no_error(r@normal_at(90))
  expect_no_error(r@normal_at(degree(45)))
  # normal at east edge: x should be > east edge (distance = 1 by default → x = 3)
  n_east <- r@normal_at(0)
  expect_equal(n_east@x, 3, tolerance = 1e-10)
  expect_equal(n_east@y, 0, tolerance = 1e-10)
  # rotated rectangle
  expect_no_error(ob_rectangle(width = 4, height = 2, angle = 45)@normal_at(0))
})

# side ----
test_that("ob_rectangle side", {
  r <- ob_rectangle(width = 4, height = 2)
  s <- r@side
  expect_true(S7::S7_inherits(s@east,  ob_segment))
  expect_true(S7::S7_inherits(s@north, ob_segment))
  expect_true(S7::S7_inherits(s@west,  ob_segment))
  expect_true(S7::S7_inherits(s@south, ob_segment))
  # east side length = height
  expect_equal((s@east@p2 - s@east@p1)@r, 2, tolerance = 1e-10)
  # north side length = width
  expect_equal((s@north@p2 - s@north@p1)@r, 4, tolerance = 1e-10)
})

# style ----
test_that("ob_rectangle style", {
  r <- ob_rectangle()
  r@style <- ob_style(color = "red", fill = "blue", linewidth = 2)
  expect_equal(r@color, "red")
  expect_equal(r@fill, "blue")
  expect_equal(r@linewidth, 2)

  # style at construction
  r2 <- ob_rectangle(fill = "green", alpha = 0.5)
  expect_equal(r2@fill, "green")
  expect_equal(r2@alpha, 0.5)
})

# geom and aesthetics ----
test_that("ob_rectangle geom and aesthetics", {
  r <- ob_rectangle(width = 3, height = 2, color = "blue")
  expect_no_error(r@geom())
  expect_no_error(as.geom(r))
  expect_no_error(r@aesthetics)
  expect_no_error(r@tibble)
  expect_no_error(get_tibble(r))
  expect_no_error(get_tibble_defaults(r))
  td <- get_tibble_defaults(r)
  expect_true(all(c("x", "y", "group") %in% names(td)))
})

# print ----
test_that("ob_rectangle print", {
  r <- ob_rectangle(width = 3, height = 2)
  expect_no_error(suppressMessages(capture.output(print(r))))
})

# rotate ----
test_that("ob_rectangle rotate", {
  expect_no_error(rotate(ob_rectangle(), theta = degree(45)))
  expect_no_error(rotate(ob_rectangle(), theta = 45))
  r45 <- rotate(ob_rectangle(width = 2, height = 1), theta = degree(90))
  # rotation is stored in @angle
  expect_equal(r45@angle@degree, 90)
  # after 90° rotation the local "east" direction now points upward
  expect_equal(r45@east@x,  0, tolerance = 1e-10)
  expect_equal(r45@east@y,  1, tolerance = 1e-10)
})

# subsetting ----
test_that("ob_rectangle subsetting", {
  r <- ob_rectangle(x = 0:2, y = 0, width = 2, height = 1)
  expect_equal(r[1]@center@x, 0)
  expect_equal(r[2]@center@x, 1)
  expect_equal(r[3]@center@x, 2)
  expect_equal(r[1]@length, 1L)

  # by height vector
  rh <- ob_rectangle(height = 1:3)
  expect_identical(rh[2]@height, 2L)

  # replacement
  r[2] <- ob_rectangle(x = 9, y = 0, width = 2, height = 1)
  expect_equal(r[2]@center@x, 9)
})

# arithmetic (+ / - with ob_point) ----
test_that("ob_rectangle arithmetic with ob_point", {
  r <- ob_rectangle(width = 2, height = 1)
  expect_identical(r + ob_point(), r)
  expect_identical(r - ob_point(), r)
  r_shifted <- r + ob_point(3, 4)
  expect_equal(r_shifted@center@x, 3)
  expect_equal(r_shifted@center@y, 4)
  r_back <- r_shifted - ob_point(3, 4)
  expect_equal(r_back@center@x, 0)
  expect_equal(r_back@center@y, 0)
})

# place ----
test_that("ob_rectangle place", {
  r1 <- ob_rectangle(width = 2, height = 1)
  r2 <- ob_rectangle(width = 1, height = 1)
  p  <- ob_point(3, 3)
  expect_no_error(place(r2, r1, where = "right"))
  expect_no_error(place(r2, r1, where = "north"))
  expect_no_error(place(p,  r1))
  expect_no_error(place(r1, p))
  # label-carrying rectangle placement
  expect_no_error(place(ob_rectangle(label = ob_label("A")), ob_point(2, 3)))
  expect_no_error(place(ob_point(2, 3), ob_rectangle()))
})

# ob_array ----
test_that("ob_rectangle ob_array", {
  r <- ob_rectangle(width = 2, height = 1)
  arr <- ob_array(r, k = 4)
  expect_equal(arr@length, 4L)
  # dimensions preserved
  expect_equal(arr@width,  rep(2, 4))
  expect_equal(arr@height, rep(1, 4))
})

# bind ----
test_that("ob_rectangle bind", {
  r1 <- ob_rectangle()
  r2 <- ob_rectangle(angle = 2)
  r  <- bind(c(r1, r2))
  expect_equal(r@length, 2L)
  expect_true(S7::S7_inherits(r, ob_rectangle))
})

# data2shape ----
test_that("ob_rectangle data2shape", {
  d <- data.frame(x = 1, y = 2, width = 4, height = 3)
  r <- data2shape(d, ob_rectangle)
  expect_true(S7::S7_inherits(r, ob_rectangle))
  expect_equal(r@center@x, 1)
  expect_equal(r@center@y, 2)
  expect_equal(r@width, 4)
  expect_equal(r@height, 3)
})



