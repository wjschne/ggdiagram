library(ggdiagram)
library(testthat)

# ── inside ────────────────────────────────────────────────────────────────────

# circle ----
test_that("inside: ob_point in ob_circle", {
  c1 <- ob_circle(center = ob_point(0, 0), radius = 1)

  expect_identical(inside(ob_point(0,  0), c1),  1)   # center → inside
  expect_identical(inside(ob_point(1,  0), c1),  0)   # on boundary
  expect_identical(inside(ob_point(2,  0), c1), -1)   # outside
})

test_that("inside: ob_point in ob_circle with non-default center", {
  c2 <- ob_circle(center = ob_point(3, 4), radius = 2)

  expect_identical(inside(ob_point(3, 4), c2),  1)
  expect_identical(inside(ob_point(5, 4), c2),  0)   # east boundary
  expect_identical(inside(ob_point(6, 4), c2), -1)
})

test_that("inside: vectorised ob_point in ob_circle", {
  c1 <- ob_circle(radius = 1)
  pts <- ob_point(c(0, 1, 2), c(0, 0, 0))

  expect_identical(inside(pts, c1), c(1, 0, -1))
})

# rectangle ----
test_that("inside: ob_point in ob_rectangle", {
  r1 <- ob_rectangle(center = ob_point(0, 0), width = 4, height = 2)

  expect_identical(inside(ob_point(0,  0), r1),  1)   # center → inside
  expect_identical(inside(ob_point(2,  0), r1),  0)   # east edge → on boundary
  expect_identical(inside(ob_point(0,  1), r1),  0)   # north edge → on boundary
  expect_identical(inside(ob_point(3,  0), r1), -1)   # outside
})

test_that("inside: rotated ob_rectangle accounts for rotation", {
  # 45° rotation: axis-aligned point (1.5, 0) is outside the rotated rectangle
  r_rot <- ob_rectangle(center = ob_point(0, 0), width = 4, height = 2, angle = degree(45))

  expect_identical(inside(ob_point(0, 0), r_rot),   1)
  expect_identical(inside(ob_point(1.5, 0), r_rot), -1)
})

# ellipse ----
test_that("inside: ob_point in ob_ellipse", {
  e1 <- ob_ellipse(a = 2, b = 1)

  expect_identical(inside(ob_point(0, 0), e1),  1)   # center → inside
  expect_identical(inside(ob_point(2, 0), e1),  0)   # east vertex → on boundary
  expect_identical(inside(ob_point(3, 0), e1), -1)   # outside
})

test_that("inside: vectorised ob_point in ob_ellipse", {
  e1 <- ob_ellipse(a = 2, b = 1)
  pts <- ob_point(c(0, 2, 3), c(0, 0, 0))

  expect_identical(inside(pts, e1), c(1, 0, -1))
})

# polygon ----
test_that("inside: ob_point in ob_polygon", {
  sq <- ob_polygon(ob_point(c(-1, 1, 1, -1), c(-1, -1, 1, 1)))

  expect_identical(inside(ob_point(0,  0), sq),  1)   # center → inside
  expect_identical(inside(ob_point(1,  0), sq),  0)   # on edge → boundary
  expect_identical(inside(ob_point(2,  2), sq), -1)   # outside

  expect_error(
    inside(ob_point(1:3,0), sq[c(1,1)]),
    "ob_point and ob_polygon with length > 1 need to be of the same size.")

})

test_that("inside: vectorised ob_point in ob_polygon (single polygon recycled)", {
  sq <- ob_polygon(ob_point(c(-1, 1, 1, -1), c(-1, -1, 1, 1)))
  pts <- ob_point(c(0, 1, 2), c(0, 0, 0))

  expect_identical(inside(pts, sq), c(1, 0, -1))
})

# ngon ----
test_that("inside: ob_point in ob_ngon", {
  sq4 <- ob_ngon(n = 4, radius = 1)

  expect_identical(inside(ob_point(0, 0), sq4),  1)   # center → inside
  expect_identical(inside(ob_point(2, 2), sq4), -1)   # clearly outside
})

test_that("inside: ob_ngon length mismatch raises error", {
  ng2 <- ob_ngon(center = ob_point(c(0, 5), c(0, 0)), n = 4, radius = 1)
  pts3 <- ob_point(c(0, 1, 2), c(0, 0, 0))

  expect_error(inside(pts3, ng2))
})

# reuleaux ----
test_that("inside: ob_point in ob_reuleaux", {
  rl <- ob_reuleaux(radius = 1)

  expect_identical(inside(ob_point(0,   0), rl),  1L)   # center → inside
  expect_identical(inside(ob_point(0.9, 0), rl),  1L)   # near boundary but inside
  expect_identical(inside(ob_point(1.5, 0), rl), -1L)   # outside
})

test_that("inside: ob_reuleaux length mismatch raises error", {
  rl2 <- ob_reuleaux(center = ob_point(c(0, 5), c(0, 0)), radius = 1)
  pts3 <- ob_point(c(0, 1, 2), c(0, 0, 0))

  expect_error(inside(pts3, rl2))
})


test_that("inside helpers", {
  d <- matrix(c(1,1,1,1), ncol = 2, dimnames = list(NULL,c("x", "y")))
  expect_identical(ggdiagram:::point_in_polygon(x = 0, y = 0, d), -1)
  expect_error(ggdiagram:::point_in_polygon(x = 0, y = 0, c(2,2)), "vertices must be a matrix or data.frame")
  d <- matrix(c(1,1,1,1), ncol = 2)
  expect_error(ggdiagram:::point_in_polygon(x = 0, y = 0, d), "vertices must have two columns for x and y coordinates")



})
