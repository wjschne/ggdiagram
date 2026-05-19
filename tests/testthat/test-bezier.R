library(testthat)
library(ggdiagram)

# Helpers used across tests
bz_quad <- function(...) {
  ob_bezier(ob_point(c(0, 1, 2), c(0, 2, 0)), ...)
}
bz_lin <- function(...) {
  ob_bezier(ob_point(c(0, 2), c(0, 4)), ...)
}

# Constructor ----

test_that("constructor wraps single ob_point in list", {
  bz <- bz_lin()
  expect_equal(bz@length, 1)
  expect_true(inherits(bz@p, "list"))
})

test_that("constructor with list of ob_points", {
  bz <- ob_bezier(list(
    ob_point(c(0, 1, 2), c(0, 2, 0)),
    ob_point(c(0, 1, 2), c(1, 3, 1))
  ))
  expect_equal(bz@length, 2)
})

test_that("constructor defaults: n = 100 and label_sloped = TRUE", {
  bz <- bz_quad(n = 100)
  expect_equal(bz@n, 100)
  expect_true(bz@label_sloped)
})

test_that("constructor error when p is missing", {
  expect_error(ob_bezier(), "Must specify 2 or more control points")
  expect_error(ob_bezier(list(NA, ob_point(1,1))), "Each item in list p must be an ob_point object of length 2 or more.")
  expect_error(ob_bezier(ob_point(1,1)), "Each item in list p must be an ob_point object of length 2 or more.")
})



test_that("constructor with style arguments", {
  bz <- bz_quad(color = "red", linewidth = 2)
  expect_identical(bz@color, "red")
  expect_identical(bz@linewidth, 2)
})

# Properties ----

test_that("tibble has group and p columns", {
  t <- bz_quad()@tibble
  expect_true(all(c("group", "p") %in% names(t)))
  expect_equal(nrow(t), 1)
})

test_that("tibble has one row per curve for multi-curve object", {
  bz <- ob_bezier(list(
    ob_point(c(0, 1, 2), c(0, 2, 0)),
    ob_point(c(0, 1, 2), c(1, 3, 1))
  ))
  expect_equal(nrow(bz@tibble), 2)
})

test_that("style getter and setter", {
  bz <- bz_quad(color = "red")
  expect_identical(bz@style@color, "red")
  bz@style <- ob_style(color = "blue")
  expect_identical(bz@color, "blue")
})

test_that("bounding_box is an ob_rectangle", {
  bb <- bz_quad()@bounding_box
  expect_true(S7::S7_inherits(bb, ob_rectangle))
  expect_equal(bb@southwest@x, 0)
  expect_equal(bb@southwest@y, 0)
  expect_equal(bb@northeast@x, 2)
})

test_that("path is an ob_path", {
  expect_true(S7::S7_inherits(bz_quad()@path, ob_path))
})

# midpoint ----

test_that("midpoint at t = 0 returns start point", {
  mp <- bz_quad()@midpoint(0)
  expect_equal(mp@x, 0, tolerance = 1e-6)
  expect_equal(mp@y, 0, tolerance = 1e-6)
})

test_that("midpoint at t = 0.5 returns curve midpoint", {
  mp <- midpoint(bz_quad())
  expect_equal(mp@x, 1, tolerance = 1e-6)
  expect_equal(mp@y, 1, tolerance = 1e-6)
})

test_that("midpoint at t = 1 returns end point", {
  mp <- bz_quad()@midpoint(1)
  expect_equal(mp@x, 2, tolerance = 1e-6)
  expect_equal(mp@y, 0, tolerance = 1e-6)
})

test_that("functional @midpoint() matches midpoint() generic", {
  bz <- bz_quad()
  expect_equal(bz@midpoint()@x, midpoint(bz)@x)
  expect_equal(bz@midpoint()@y, midpoint(bz)@y)
})

# point_at_x / point_at_y ----

test_that("point_at_x returns ob_point at given x", {
  p <- bz_lin()@point_at_x(1)
  expect_true(S7::S7_inherits(p, ob_point))
  expect_equal(p@x, 1, tolerance = 0.01)
  expect_equal(p@y, 2, tolerance = 0.01)
})

test_that("point_at_x returns NULL when x is outside the curve range", {
  expect_null(bz_lin()@point_at_x(-1))
})

test_that("point_at_y returns ob_point at given y", {
  p <- bz_lin()@point_at_y(2)
  expect_true(S7::S7_inherits(p, ob_point))
  expect_equal(p@x, 1, tolerance = 0.01)
  expect_equal(p@y, 2, tolerance = 0.01)
})

test_that("point_at_y returns NULL when y is outside the curve range", {
  expect_null(bz_lin()@point_at_y(10))
})

# Label ----

test_that("character label is converted to ob_label", {
  bz <- bz_quad(label = "A")
  expect_true(S7::S7_inherits(bz@label, ob_label))
  expect_identical(bz@label@label, "A")
})

test_that("ob_label passed directly is stored", {
  bz <- bz_quad(label = ob_label("B"))
  expect_identical(bz@label@label, "B")
})

test_that("1 bezier with multiple labels", {
  bz <- bz_quad(label = ob_label(letters[1:2]))
  expect_identical(bz@length, 2L)
  expect_identical(bz@label@label, letters[1:2])
})

# set_label_x / set_label_y ----

test_that("set_label_x errors when curve has no label", {
  expect_error(bz_quad()@set_label_x(0.5), "does not have a label")
})

test_that("set_label_y errors when curve has no label", {
  expect_error(bz_quad()@set_label_y(0.5), "does not have a label")
})

test_that("set_label_x with label does not error", {
  expect_no_error(bz_quad(label = "X")@set_label_x(0.5))
})

# Subsetting ----

test_that("[ by integer extracts the correct curve", {
  bz <- ob_bezier(list(
    ob_point(c(0, 1, 2), c(0, 2, 0)),
    ob_point(c(0, 1, 2), c(1, 3, 1))
  ))
  bz2 <- bz[2]
  expect_equal(bz2@length, 1)
  expect_equal(bz2@p[[1]]@y[1], 1)
})

test_that("[ by id extracts the correct curve", {
  bz <- ob_bezier(
    list(
      ob_point(c(0, 1, 2), c(0, 2, 0)),
      ob_point(c(0, 1, 2), c(1, 3, 1))
    ),
    id = c("a", "b")
  )
  expect_equal(bz["b"]@length, 1)
  expect_equal(bz["b"]@p[[1]]@y[1], 1)
})

# geom ----

test_that("geom no error for plain curve", {
  expect_no_error(bz_quad()@geom())
})

test_that("geom no error with sloped label", {
  expect_no_error(bz_quad(label = "A", label_sloped = TRUE)@geom())
})

test_that("geom no error with non-sloped label", {
  expect_no_error(bz_quad(label = "A", label_sloped = FALSE)@geom())
})

test_that("from and to offset with direction words", {
  bz <- connect(ob_point(), ob_point(1,0), from_offset = "south", to_offset = "south")
  expect_equal(bz@p[[1]]@y, c(0,-1, -1, 0))
  # p setter
  bz@p <- ob_point(c(1,2,3,1), c(0,2,1,0))
  expect_length(bz@p, 1)
  bz2 <- bz
  bz2@p <- list(ob_point(c(1,2,3,1), c(0,2,1,0)))
  expect_equal(bz2@p[[1]]@y, bz@p[[1]]@y)
  expect_no_error(bz@point_at_y(2))
  expect_no_error(bz@point_at_x(0))
  bz2@p <- list(ob_point(c(0,2,1,0), c(1,2,3,1)))
  expect_no_error(bz2@point_at_x(0))

  bz <- connect(ob_point(), ob_point(1,0), from_offset = "south", to_offset = "south", label = "a")
  # ggdiagram() +
    # ob_segment(ob_point(c(1,-1), c(1,-1))) +
    # ob_line(slope = 2, intercept = 0, color = "black", linewidth = 1) +
    # bz@set_label_y() +
    # bz@set_label_y(position = .1) +
    # bz@set_label_x() +
    # bz@set_label_x(position = .1) +
    # bz@set_label_x(x = .1) +
    # bz@set_label_y(y = .1) +
    # bz@set_label_x(position = -1) +
    # theme_minimal()
# bz@midpoint()@y
  # No values
  expect_no_error(bz@set_label_x())
  expect_no_error(bz@set_label_y())
  # Positions
  expect_no_error(bz@set_label_x(position = .2))
  expect_no_error(bz@set_label_y(position = .1))
  # Position outside of range
  expect_true(is.na(bz@set_label_x(position = 2)@label@center@x))
  expect_true(is.na(bz@set_label_y(position = -1)@label@center@x))

  # x and y

  expect_equal(bz@set_label_y(y = -.2)@label@center@y, -.2, tolerance = 1e-04)
  expect_no_error(bz@set_label_y(y = .2))
  # x and y out of range
  expect_true(is.na(bz@set_label_x(x = 2)@label@center@x))
  expect_true(is.na(bz@set_label_y(y = .2)@label@center@y))

})

# as.geom

test_that("as.geom", {
  bz <- bz_quad(label = ob_label("a"))
  expect_no_error(as.geom(bz))

  l <- ob_label("a", label.padding = ggplot2::margin(1,1,1,1))
  l@position <- NA_real_

  bz <- bz_quad(label = l,
                arrowhead_length = 2, label_sloped = FALSE)
  expect_no_error(as.geom(bz))
})


