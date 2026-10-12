test_that("connect uses a gradient when colors differ", {
  c1 <- ob_circle(color = "blue", fill = "white")
  c2 <- ob_circle(center = ob_point(5, 0), color = NA, fill = "red")
  c3 <- ob_circle(center = ob_point(0, -4))
  c4 <- ob_circle(center = ob_point(5, -4), color = "black")

  # color, else fill
  expect_equal(connect(c1, c2)@gradient, list(c("blue", "red")))
  # color and fill both NA is black
  expect_equal(connect(c1, c3)@gradient, list(c("blue", "black")))
  # arcs get gradients too
  expect_equal(connect(c2, c4, arc_bend = 0.5)@gradient, list(c("red", "black")))
  # identical colors: no gradient, one color
  s <- connect(c3, c4)
  expect_length(s@gradient, 0)
  expect_equal(s@color, "black")
  # explicit color or gradient is respected
  s <- connect(c1, c2, color = "green")
  expect_length(s@gradient, 0)
  expect_equal(s@color, "green")
  expect_equal(connect(c1, c2, gradient = c("pink", "gold"))@gradient, list(c("pink", "gold")))
})

test_that("connect gradients are per row", {
  c1 <- ob_circle(center = ob_point(c(0, 0), c(0, 3)), color = c("blue", "red"))
  c2 <- ob_circle(center = ob_point(c(5, 5), c(0, 3)), color = c("red", "red"))
  s <- connect(c1, c2)
  expect_equal(s@gradient, list(c("blue", "red"), character(0)))
  expect_equal(s@color, c("blue", "red"))
})
