library(ggdiagram)
library(testthat)

# ── equation ──────────────────────────────────────────────────────────────────
# Covers: ob_line, ob_circle, ob_segment
# All three types (y, general, parametric) × markdown/latex outputs.

# ── ob_line ───────────────────────────────────────────────────────────────────

# type = "y" ----
test_that("equation ob_line: y-form slope/intercept, markdown", {
  eq <- equation(ob_line(slope = 2, intercept = 4))
  expect_identical(eq, "*y* = 2*x* + 4")
})

test_that("equation ob_line: y-form slope/intercept, latex", {
  eq <- equation(ob_line(slope = 2, intercept = 4), output = "latex")
  expect_identical(eq, "y = 2x + 4")
})

test_that("equation ob_line: y-form negative intercept uses minus sign", {
  eq <- equation(ob_line(slope = 1, intercept = -3))
  expect_true(grepl("\u2212", eq))            # unicode minus
  expect_true(grepl("3", eq))
})

test_that("equation ob_line: horizontal line (slope = 0) renders as y = c", {
  eq <- equation(ob_line(slope = 0, intercept = 5))
  expect_identical(eq, "*y* = 5")
})

test_that("equation ob_line: vertical line (xintercept) renders as x = c", {
  eq <- equation(ob_line(xintercept = 3))
  expect_identical(eq, "*x* = 3")
})

test_that("equation ob_line: vertical line, latex output", {
  eq <- equation(ob_line(xintercept = 3), output = "latex")
  expect_identical(eq, "x = 3")
})

test_that("equation ob_line: digits argument controls rounding", {
  l <- ob_line(slope = 1 / 3, intercept = 2 / 3)
  eq4 <- equation(l, digits = 4)
  expect_true(grepl("0.3333", eq4))
  expect_true(grepl("0.6667", eq4))
})

test_that("equation ob_line: returns character vector", {
  expect_type(equation(ob_line(slope = 1, intercept = 0)), "character")
})

# type = "general" ----
test_that("equation ob_line: general form ends with '= 0'", {
  eq <- equation(ob_line(slope = 2, intercept = 4), type = "general")
  expect_true(endsWith(eq, "= 0"))
})

test_that("equation ob_line: general form, latex", {
  eq <- equation(ob_line(slope = 2, intercept = 4), type = "general", output = "latex")
  expect_true(endsWith(eq, "= 0"))
  expect_false(grepl("\\*", eq))    # no markdown asterisks
})

# type = "parametric" ----
test_that("equation ob_line: parametric form contains linebreak and t variable", {
  eq <- equation(ob_line(slope = 2, intercept = 4), type = "parametric")
  expect_true(grepl("<br>", eq, fixed = TRUE))
  expect_true(grepl("*t*", eq, fixed = TRUE))
})

test_that("equation ob_line: parametric form latex uses LaTeX linebreak", {
  eq <- equation(ob_line(slope = 2, intercept = 4), type = "parametric", output = "latex")
  expect_true(grepl("\\\\ ", eq, fixed = TRUE))
  expect_false(grepl("<br>", eq, fixed = TRUE))
})

test_that("equation ob_line: parametric vertical renders x = constant", {
  eq <- equation(ob_line(xintercept = 3), type = "parametric")
  expect_true(grepl("*x*", eq, fixed = TRUE))
  expect_true(grepl("3", eq))
})

# ── ob_circle ─────────────────────────────────────────────────────────────────

# type = "general" / "y" (identical for circles) ----
test_that("equation ob_circle: general form at origin contains x² + y² = r²", {
  eq <- equation(ob_circle())
  expect_true(grepl("*x*<sup>2</sup>", eq, fixed = TRUE))
  expect_true(grepl("*y*<sup>2</sup>", eq, fixed = TRUE))
  expect_true(grepl("= 1<sup>2</sup>", eq, fixed = TRUE))
})

test_that("equation ob_circle: general form, latex uses ^{2} notation", {
  eq <- equation(ob_circle(), output = "latex")
  expect_true(grepl("x^{2}", eq, fixed = TRUE))
  expect_true(grepl("y^{2}", eq, fixed = TRUE))
  expect_false(grepl("<sup>", eq, fixed = TRUE))
})

test_that("equation ob_circle: radius value appears in equation", {
  eq <- equation(ob_circle(radius = 3))
  expect_true(grepl("= 3<sup>2</sup>", eq, fixed = TRUE))
})

test_that("equation ob_circle: non-origin center shifts x and y terms", {
  eq <- equation(ob_circle(center = ob_point(2, 3), radius = 1))
  expect_true(grepl("(*x*", eq, fixed = TRUE))
  expect_true(grepl("(*y*", eq, fixed = TRUE))
  expect_true(grepl("2", eq))
  expect_true(grepl("3", eq))
})

test_that("equation ob_circle: type = 'y' gives same result as default", {
  c1 <- ob_circle(radius = 2)
  expect_identical(equation(c1, type = "y"), equation(c1))
})

test_that("equation ob_circle: digits argument controls rounding of radius", {
  eq <- equation(ob_circle(radius = sqrt(2)), digits = 4)
  expect_true(grepl("1.414", eq))
})

test_that("equation ob_circle: vectorised circles return one string per circle", {
  c2 <- bind(c(ob_circle(radius = 1), ob_circle(radius = 2)))
  eq <- equation(c2)
  expect_length(eq, 2)
  expect_true(grepl("1<sup>2</sup>", eq[1], fixed = TRUE))
  expect_true(grepl("2<sup>2</sup>", eq[2], fixed = TRUE))
})

# type = "parametric" ----
test_that("equation ob_circle: parametric r=1 omits coefficient prefix", {
  eq <- equation(ob_circle(radius = 1), type = "parametric")
  expect_true(grepl("cos(*t*)", eq, fixed = TRUE))
  expect_true(grepl("sin(*t*)", eq, fixed = TRUE))
  # radius 1 → no numeric prefix before cos/sin
  expect_false(grepl("^1cos", eq))
})

test_that("equation ob_circle: parametric r=2 prefixes coefficient", {
  eq <- equation(ob_circle(radius = 2), type = "parametric")
  expect_true(grepl("2cos(*t*)", eq, fixed = TRUE))
  expect_true(grepl("2sin(*t*)", eq, fixed = TRUE))
})

test_that("equation ob_circle: parametric with non-zero center includes offset", {
  eq <- equation(ob_circle(center = ob_point(3, 4), radius = 2), type = "parametric")
  expect_true(grepl("3", eq))
  expect_true(grepl("4", eq))
})

test_that("equation ob_circle: parametric latex uses \\cos and \\sin", {
  eq <- equation(ob_circle(), type = "parametric", output = "latex")
  expect_true(grepl("\\cos", eq, fixed = TRUE))
  expect_true(grepl("\\sin", eq, fixed = TRUE))
  expect_false(grepl("*t*", eq, fixed = TRUE))   # no markdown italics in latex
})

test_that("equation ob_circle: parametric latex uses \\\\ linebreak", {
  eq <- equation(ob_circle(), type = "parametric", output = "latex")
  expect_true(grepl("\\\\ ", eq, fixed = TRUE))
  expect_false(grepl("<br>", eq, fixed = TRUE))
})

# ── ob_segment ────────────────────────────────────────────────────────────────

test_that("equation ob_segment: delegates to its underlying line", {
  seg <- ob_segment(ob_point(0, 0), ob_point(2, 4))
  expect_identical(equation(seg), equation(seg@line))
})

test_that("equation ob_segment: general form matches line general form", {
  seg <- ob_segment(ob_point(0, 0), ob_point(1, 2))
  expect_identical(
    equation(seg, type = "general"),
    equation(seg@line, type = "general")
  )
})

test_that("equation ob_segment: parametric form matches line parametric form", {
  seg <- ob_segment(ob_point(0, 0), ob_point(1, 2))
  expect_identical(
    equation(seg, type = "parametric"),
    equation(seg@line, type = "parametric")
  )
})

test_that("equation ob_segment: vertical segment renders as x = constant", {
  vseg <- ob_segment(ob_point(3, 0), ob_point(3, 5))
  expect_identical(equation(vseg), "*x* = 3")
})

test_that("equation ob_segment: horizontal segment renders as y = constant", {
  hseg <- ob_segment(ob_point(0, 2), ob_point(4, 2))
  expect_identical(equation(hseg), "*y* = 2")
})

test_that("equation ob_segment: latex output suppresses markdown", {
  seg <- ob_segment(ob_point(0, 0), ob_point(2, 4))
  eq <- equation(seg, output = "latex")
  expect_false(grepl("\\*", eq))
  expect_false(grepl("<sup>", eq, fixed = TRUE))
})

test_that("equation ob_segment: returns a character vector", {
  seg <- ob_segment(ob_point(0, 0), ob_point(1, 1))
  expect_type(equation(seg), "character")
})

