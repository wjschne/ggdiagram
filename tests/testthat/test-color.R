library(ggdiagram)
library(testthat)

# ── class_color ───────────────────────────────────────────────────────────────

# construction ----
test_that("class_color: construction from R color name", {
  cc <- class_color("red")
  expect_s3_class(cc, "ggdiagram::class_color")
  expect_identical(cc@color, "#FF0000FF")
})

test_that("class_color: construction from hex code", {
  cc <- class_color("#0000FF")
  expect_s3_class(cc, "ggdiagram::class_color")
  expect_identical(cc@color, "#0000FFFF")
})

test_that("class_color: default constructor produces red", {
  cc <- class_color()
  expect_identical(cc@color, "#FF0000FF")
})

test_that("class_color: vectorised construction", {
  cc <- class_color(c("red", "blue"))
  expect_length(cc, 2)
  expect_identical(cc@color, c("#FF0000", "#0000FF"))
})

test_that("class_color: hue override in constructor", {
  cc <- class_color("red", hue = 120)
  expect_equal(cc@hue, 120)
})

test_that("class_color: saturation override in constructor", {
  cc <- class_color("red", saturation = 0)
  expect_equal(cc@saturation, 0)
})

test_that("class_color: brightness override in constructor", {
  cc <- class_color("red", brightness = 0.5)
  expect_equal(cc@brightness, 0.5, tolerance = 0.01)
})

test_that("class_color: alpha override in constructor", {
  cc <- class_color("red", alpha = 0)
  expect_equal(cc@alpha, 0)
})

test_that("class_color: constructor errors on wrong-length HSV arguments", {
  expect_error(class_color(c("red", "blue"), hue        = c(0, 120, 240)), "Hue must be of same length")
  expect_error(class_color(c("red", "blue"), saturation = c(0, 0.5, 1)),   "Saturation must be of same length")
  expect_error(class_color(c("red", "blue"), brightness = c(0, 0.5, 1)),   "Brightness must be of same length")
  expect_error(class_color(c("red", "blue"), alpha      = c(0, 0.5, 1)),   "Alpha must be of same length")
})

# HSV properties ----
test_that("class_color: HSV getters for red", {
  cc <- class_color("red")
  expect_equal(cc@hue,        0)
  expect_equal(cc@saturation, 1)
  expect_equal(cc@brightness, 1)
  expect_equal(cc@alpha,      1)
})

test_that("class_color: hue setter", {
  cc <- class_color("red")
  cc@hue <- 240L
  expect_equal(cc@hue, 240)
})

test_that("class_color: saturation setter", {
  cc <- class_color("red")
  cc@saturation <- 0L
  expect_equal(cc@saturation, 0)
})

test_that("class_color: brightness setter", {
  cc <- class_color("red")
  cc@brightness <- 0L
  expect_equal(cc@brightness, 0)
})

test_that("class_color: alpha setter", {
  cc <- class_color("red")
  cc@alpha <- 0L
  expect_equal(cc@alpha, 0)
})

# RGB properties ----
test_that("class_color: RGB getters for red", {
  cc <- class_color("red")
  expect_equal(cc@red,   255)
  expect_equal(cc@green,   0)
  expect_equal(cc@blue,    0)
})

test_that("class_color: red setter", {
  cc <- class_color("black")
  cc@red <- 255L
  expect_equal(cc@red, 255)
})

test_that("class_color: green setter", {
  cc <- class_color("black")
  cc@green <- 255L
  expect_equal(cc@green, 255)
})

test_that("class_color: blue setter", {
  cc <- class_color("black")
  cc@blue <- 255L
  expect_equal(cc@blue, 255)
})

# tex property ----
test_that("class_color: tex property produces TeX color command", {
  cc <- class_color("red")
  expect_identical(cc@tex, "\\color[HTML]{FF0000}")
})

# transparentize ----
test_that("class_color: transparentize returns class_color with lower alpha", {
  cc <- class_color("blue")
  t  <- cc@transparentize(0.5)
  expect_s3_class(t, "ggdiagram::class_color")
  expect_lt(t@alpha, 1)
})

test_that("class_color: transparentize(1) preserves full opacity", {
  cc <- class_color("blue")
  t  <- cc@transparentize(1)
  expect_equal(t@alpha, 1)
})

# lighten / darken ----
test_that("class_color: lighten returns class_color", {
  cc <- class_color("blue")
  expect_s3_class(cc@lighten(0.2), "ggdiagram::class_color")
})

test_that("class_color: lighten(0) returns white", {
  cc <- class_color("blue")
  expect_identical(cc@lighten(0)@color, "#FFFFFFFF")
})

test_that("class_color: darken returns class_color with lower brightness", {
  cc <- class_color("blue")
  d  <- cc@darken(0.2)
  expect_s3_class(d, "ggdiagram::class_color")
  expect_lt(d@brightness, cc@brightness)
})

test_that("class_color: darken(0) returns original color", {
  cc <- class_color("blue")
  expect_identical(cc@darken(0)@color, cc@color)
})

# mean property and mean() method ----
test_that("class_color: mean property returns class_color", {
  cc <- class_color(c("red", "blue"))
  expect_s3_class(cc@mean, "ggdiagram::class_color")
})

test_that("class_color: mean() method returns class_color", {
  cc <- class_color(c("red", "blue"))
  expect_s3_class(mean(cc), "ggdiagram::class_color")
})

test_that("class_color: mean of identical colors returns same color", {
  cc <- class_color(c("red", "red"))
  expect_equal(mean(cc)@hue,        cc@hue[1])
  expect_equal(mean(cc)@saturation, cc@saturation[1])
  expect_equal(mean(cc)@brightness, cc@brightness[1])
})

# [ subsetting ----
test_that("class_color: [ subsetting by position", {
  cc <- class_color(c("red", "blue"))
  expect_identical(cc[1]@color, "#FF0000")
  expect_identical(cc[2]@color, "#0000FF")
})

test_that("class_color: [ subsetting by id", {
  cc <- class_color(c("red", "blue"), id = c("a", "b"))
  expect_identical(cc["b"]@color, "#0000FF")
})

# str ----
test_that("class_color: str produces no error", {
  expect_no_error(suppressMessages(capture.output(str(class_color("red")))))
})

# ── mean_color ────────────────────────────────────────────────────────────────
test_that("mean_color: returns a single color string", {
  result <- mean_color(c("red", "blue"))
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("mean_color: midpoint of identical colors is the same color", {
  result <- mean_color(c("red", "red"))
  # Both endpoints are red, so the middle is also red
  cc_mid    <- class_color(result)
  cc_target <- class_color("red")
  expect_equal(cc_mid@hue, cc_target@hue, tolerance = 1)
})

# ── latex_color ───────────────────────────────────────────────────────────────
test_that("latex_color: wraps expression in TeX color command", {
  result <- latex_color("X^2", "red")
  expect_identical(result, "{\\color[HTML]{FF0000} X^2}")
})

test_that("latex_color: accepts a class_color object directly", {
  cc <- class_color("red")
  expect_identical(latex_color("Y", cc), "{\\color[HTML]{FF0000} Y}")
})
