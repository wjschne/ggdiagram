library(testthat)
library(ggdiagram)

# ob_point ----

test_that("rescale ob_point from origin", {
  p <- rescale(ob_point(2, 4), scale = 2)
  expect_equal(p@x, 4)
  expect_equal(p@y, 8)
})

test_that("rescale ob_point with custom origin", {
  p <- rescale(ob_point(3, 4), scale = 2, origin = ob_point(1, 2))
  expect_equal(p@x, 4)
  expect_equal(p@y, 4)
})

test_that("rescale ob_point scale = 1 is identity", {
  p <- rescale(ob_point(5, 7), scale = 1)
  expect_equal(p@x, 5)
  expect_equal(p@y, 7)
})

test_that("rescale ob_point scale = 0 collapses to (0, 0)", {
  p <- rescale(ob_point(5, 3), scale = 0)
  expect_equal(p@x, 0)
  expect_equal(p@y, 0)
})

test_that("rescale ob_point works on a vector of points", {
  p <- rescale(ob_point(c(1, 2, 3), c(4, 5, 6)), scale = 2)
  expect_equal(p@x, c(2, 4, 6))
  expect_equal(p@y, c(8, 10, 12))
})

# ob_circle ----

test_that("rescale ob_circle scales center and radius", {
  c1 <- rescale(ob_circle(center = ob_point(2, 0), radius = 1), scale = 3)
  expect_equal(c1@center@x, 6)
  expect_equal(c1@center@y, 0)
  expect_equal(c1@radius, 3)
})

test_that("rescale ob_circle with custom origin", {
  c1 <- rescale(ob_circle(ob_point(2, 0), radius = 2), scale = 2, origin = ob_point(1, 0))
  expect_equal(c1@center@x, 2)
  expect_equal(c1@center@y, 0)
  expect_equal(c1@radius, 4)
})

# ob_ngon ----

test_that("rescale ob_ngon scales center and radius", {
  ng <- rescale(ob_ngon(n = 4, center = ob_point(2, 0), radius = 1), scale = 3)
  expect_equal(ng@center@x, 6)
  expect_equal(ng@center@y, 0)
  expect_equal(ng@radius, 3)
})

# ob_ellipse ----

test_that("rescale ob_ellipse scales center, a, and b", {
  e1 <- rescale(ob_ellipse(center = ob_point(2, 0), a = 2, b = 1), scale = 3)
  expect_equal(e1@center@x, 6)
  expect_equal(e1@center@y, 0)
  expect_equal(e1@a, 6)
  expect_equal(e1@b, 3)
})

# ob_rectangle ----

test_that("rescale ob_rectangle scales center, width, and height", {
  r1 <- rescale(ob_rectangle(center = ob_point(1, 1), width = 4, height = 2), scale = 2)
  expect_equal(r1@center@x, 2)
  expect_equal(r1@center@y, 2)
  expect_equal(r1@width, 8)
  expect_equal(r1@height, 4)
})

# ob_segment ----

test_that("rescale ob_segment scales both endpoints", {
  s1 <- rescale(ob_segment(ob_point(1, 0), ob_point(3, 0)), scale = 2)
  expect_equal(s1@p1@x, 2)
  expect_equal(s1@p1@y, 0)
  expect_equal(s1@p2@x, 6)
  expect_equal(s1@p2@y, 0)
})

test_that("rescale ob_segment with custom origin", {
  s1 <- rescale(
    ob_segment(ob_point(0, 0), ob_point(2, 0)),
    scale = 2,
    origin = ob_point(1, 0)
  )
  expect_equal(s1@p1@x, -2)
  expect_equal(s1@p2@x, 2)
})

# ob_polygon (ob_point_list) ----

test_that("rescale ob_polygon scales all vertices", {
  poly <- ob_polygon(ob_point(c(0, 1, 1, 0), c(0, 0, 1, 1)))
  poly2 <- rescale(poly, scale = 2)
  expect_equal(poly2@p[[1]]@x, c(0, 2, 2, 0))
  expect_equal(poly2@p[[1]]@y, c(0, 0, 2, 2))
})

# ob_shape_list ----

test_that("rescale ob_shape_list scales each element", {
  sl <- ob_shape_list(list(ob_point(1, 2), ob_circle(ob_point(1, 1), radius = 1)))
  sl2 <- rescale(sl, scale = 2)
  expect_equal(sl2[[1]]@x, 2)
  expect_equal(sl2[[1]]@y, 4)
  expect_equal(sl2[[2]]@center@x, 2)
  expect_equal(sl2[[2]]@center@y, 2)
  expect_equal(sl2[[2]]@radius, 2)
})
