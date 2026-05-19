library(ggdiagram)
library(testthat)

# ── str methods ───────────────────────────────────────────────────────────────
# Helper: collapse capture.output() into one searchable string
str_out <- function(x) paste(capture.output(str(x)), collapse = "\n")

# ── ob_point ──────────────────────────────────────────────────────────────────
test_that("str ob_point: no error", {
  expect_no_error(str_out(ob_point()))
})


test_that("str ob_point: output shows @x and @y properties", {
  out <- str_out(ob_point())
  expect_true(grepl("@ x", out))
  expect_true(grepl("@ y", out))
})

test_that("str ob_point: output reflects actual coordinate values", {
  out <- str_out(ob_point(3, 4))
  expect_true(grepl("3", out))
  expect_true(grepl("4", out))
})

test_that("str ob_point: omitted properties listed under 'Other props'", {
  expect_true(grepl("Other props", str_out(ob_point())))
})

test_that("str ob_point: returns invisibly", {
  expect_false(withVisible(str(ob_point()))$visible)
})

# ── ob_polar ──────────────────────────────────────────────────────────────────
test_that("str ob_polar: no error", {
  expect_no_error(str_out(ob_polar(r = 1, theta = degree(45))))
})


test_that("str ob_polar: output shows r and theta", {
  out <- str_out(ob_polar(r = 2, theta = degree(90)))
  expect_true(grepl("@ r", out))
  expect_true(grepl("@ theta", out))
})

test_that("str ob_polar: returns invisibly", {
  expect_false(withVisible(str(ob_polar(r = 1, theta = degree(45))))$visible)
})

# ── degree / radian / turn ────────────────────────────────────────────────────
test_that("str degree: no error", {
  expect_no_error(str_out(degree(45)))
})

test_that("str degree: returns the degree object invisibly", {
  wv <- withVisible(str(degree(45)))
  expect_false(wv$visible)
  expect_s3_class(wv$value, "ggdiagram::degree")
})

test_that("str degree: output contains a numeric value", {
  # degree(45) stores 0.125 turns internally; c(degree(45)) gives that number
  out <- str_out(degree(45))
  expect_true(grepl("0.125", out))
})

test_that("str radian: no error", {
  expect_no_error(str_out(radian(pi / 2)))
})

test_that("str radian: returns the radian object invisibly", {
  wv <- withVisible(str(radian(pi / 2)))
  expect_false(wv$visible)
  expect_s3_class(wv$value, "ggdiagram::radian")
})

test_that("str radian: output contains pi symbol", {
  expect_true(grepl("\u03c0", str_out(radian(pi / 4))))
})

test_that("str turn: no error", {
  expect_no_error(str_out(turn(0.5)))
})

test_that("str turn: returns the turn object invisibly", {
  wv <- withVisible(str(turn(0.5)))
  expect_false(wv$visible)
  expect_s3_class(wv$value, "ggdiagram::turn")
})

test_that("str turn: output contains the turn fraction", {
  expect_true(grepl(".25", str_out(turn(0.25))))
})

# ── class_color ───────────────────────────────────────────────────────────────
test_that("str class_color: no error", {
  expect_no_error(str_out(class_color("blue")))
})

test_that("str class_color: output shows @color property", {
  expect_true(grepl("@ color", str_out(class_color("red"))))
})

test_that("str class_color: output reflects the hex value of the color", {
  expect_true(grepl("#FF0000", str_out(class_color("red"))))
})

test_that("str class_color: computed properties listed under 'Other props'", {
  expect_true(grepl("Other props", str_out(class_color("red"))))
})

test_that("str class_color: returns invisibly", {
  expect_false(withVisible(str(class_color("blue")))$visible)
})

# ── ob_style ──────────────────────────────────────────────────────────────────
test_that("str ob_style: no error", {
  expect_no_error(str_out(ob_style()))
  expect_no_error(str_out(ob_style(color = "red", fill = "blue")))
})


test_that("str ob_style: output shows only non-empty properties", {
  out <- str_out(ob_style(color = "red"))
  expect_true(grepl("@ color", out))
  expect_false(grepl("@ fill", out))   # fill was not set
})

test_that("str ob_style: multiple set properties all appear", {
  out <- str_out(ob_style(color = "red", fill = "blue", alpha = 0.5))
  expect_true(grepl("@ color", out))
  expect_true(grepl("@ fill",  out))
  expect_true(grepl("@ alpha", out))
})

test_that("str ob_style: returns invisibly", {
  expect_false(withVisible(str(ob_style(color = "red")))$visible)
})

# ── ob_label ──────────────────────────────────────────────────────────────────
test_that("str ob_label: no error", {
  expect_no_error(str_out(ob_label("Hello")))
})


test_that("str ob_label: output shows @label and @center properties", {
  out <- str_out(ob_label("Hello"))
  expect_true(grepl("@ label",  out))
  expect_true(grepl("@ center", out))
})

test_that("str ob_label: output reflects label text", {
  expect_true(grepl("Hello", str_out(ob_label("Hello"))))
})

test_that("str ob_label: returns invisibly", {
  expect_false(withVisible(str(ob_label("A")))$visible)
})

# ── class_arrowhead ───────────────────────────────────────────────────────────
test_that("str class_arrowhead: no error", {
  ah <- ggdiagram:::class_arrowhead(matrix(c(0, 1, 0, 0, 0, 1), ncol = 2))
  expect_no_error(str_out(ah))
})



test_that("str class_arrowhead: returns object invisibly", {
  ah <- class_arrowhead(matrix(c(0, 1, 0, 0, 0, 1), ncol = 2))
  wv <- withVisible(str(ah))
  expect_false(wv$visible)
  expect_s3_class(wv$value, "ggdiagram::class_arrowhead")
})

# ── class_margin ──────────────────────────────────────────────────────────────
test_that("str class_margin: no error", {
  expect_no_error(str_out(class_margin(c(1, 2, 3, 4))))
})


test_that("str class_margin: output reflects the unit values", {
  out <- str_out(class_margin(c(1, 2, 3, 4)))
  expect_true(grepl("1", out))
  expect_true(grepl("points", out))
})

test_that("str class_margin: returns object invisibly", {
  wv <- withVisible(str(class_margin(1)))
  expect_false(wv$visible)
  expect_s3_class(wv$value, "ggdiagram::class_margin")
})

# ── class_aesthetics_list ─────────────────────────────────────────────────────
test_that("str class_aesthetics_list: no error", {
  expect_no_error(str_out(ob_circle()@aesthetics))
})


test_that("str class_aesthetics_list: output shows @required_aes property", {
  expect_true(grepl("@ required_aes", str_out(ob_circle()@aesthetics)))
})

test_that("str class_aesthetics_list: output shows @geom property", {
  expect_true(grepl("@ geom", str_out(ob_circle()@aesthetics)))
})

test_that("str class_aesthetics_list: returns invisibly", {
  expect_false(withVisible(str(ob_circle()@aesthetics))$visible)
})

# ── ob_shape_list ─────────────────────────────────────────────────────────────
test_that("str ob_shape_list: no error", {
  expect_no_error(str_out(ob_shape_list(list(ob_point(), ob_circle()))))
})



test_that("str ob_shape_list: returns object invisibly", {
  sl  <- ob_shape_list(list(ob_point(), ob_circle()))
  wv  <- withVisible(str(sl))
  expect_false(wv$visible)
  expect_s3_class(wv$value, "ggdiagram::ob_shape_list")
})

# ── already-covered classes: content tests ────────────────────────────────────
# test-misc.R already checks no-error for these; here we add content checks.

test_that("str ob_circle: output contains class header, center, and radius", {
  out <- str_out(ob_circle(radius = 3))
  expect_true(grepl("@ center", out))
  expect_true(grepl("@ radius", out))
  expect_true(grepl("3", out))
})

test_that("str ob_rectangle: output contains class header and dimensions", {
  out <- str_out(ob_rectangle(width = 4, height = 2))
  expect_true(grepl("@ width",  out))
  expect_true(grepl("@ height", out))
})

test_that("str ob_ellipse: output contains class header and semi-axes", {
  out <- str_out(ob_ellipse(a = 3, b = 2))
  expect_true(grepl("@ a", out))
  expect_true(grepl("@ b", out))
})

test_that("str ob_segment: output contains class header and endpoints", {
  out <- str_out(ob_segment(ob_point(0, 0), ob_point(1, 1)))
  expect_true(grepl("@ p1", out))
  expect_true(grepl("@ p2", out))
})

test_that("str ob_arc: output contains class header", {
  out <- str_out(ob_arc(start = 0, end = 90, radius = 1))
  expect_no_error(out)
})

test_that("str ob_bezier: output contains class header", {
  out <- str_out(ob_bezier(ob_point(c(0, 1, 2), c(0, 2, 0))))
  expect_no_error(out)
})

test_that("str ob_path: output contains class header", {
  out <- str_out(ob_path(ob_point(c(0, 1, 2), c(0, 2, 0))))
  expect_no_error(out)
})

test_that("str ob_polygon: output contains class header", {
  out <- str_out(ob_polygon(ob_point(c(0, 1, 2), c(0, 2, 0))))
  expect_no_error(out)
})

test_that("str ob_ngon: output contains class header", {
  out <- str_out(ob_ngon())
  expect_no_error(out)
})

test_that("str ob_reuleaux: output contains class header", {
  out <- str_out(ob_reuleaux())
  expect_no_error(out)
})

