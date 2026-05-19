library(testthat)
library(ggdiagram)

# construction ----
test_that("ob_style construction: empty default", {
  s <- ob_style()
  # All properties are empty by default
  expect_equal(length(s@color),     0)
  expect_equal(length(s@fill),      0)
  expect_equal(length(s@alpha),     0)
  expect_equal(length(s@linewidth), 0)
  expect_equal(length(s@linetype),  0)
  expect_equal(length(s@hjust),     0)
  expect_equal(length(s@vjust),     0)
  expect_true(S7::S7_inherits(s, ob_style))
})

test_that("ob_style construction: scalar properties", {
  # numeric properties
  s <- ob_style(alpha = 0.5, linewidth = 2, linetype = 2, n = 100)
  expect_equal(s@alpha,     0.5)
  expect_equal(s@linewidth, 2)
  expect_equal(s@linetype,  2)
  expect_equal(s@n,         100)

  # character properties
  s2 <- ob_style(color = "red", fill = "blue", family = "serif", fontface = "bold")
  expect_equal(s2@color,    "red")
  expect_equal(s2@fill,     "blue")
  expect_equal(s2@family,   "serif")
  expect_equal(s2@fontface, "bold")

  # logical property
  s3 <- ob_style(straight = TRUE)
  expect_equal(s3@straight, TRUE)

  # justification
  s4 <- ob_style(hjust = 0.5, vjust = 0)
  expect_equal(s4@hjust, 0.5)
  expect_equal(s4@vjust, 0)

  # nudge
  s5 <- ob_style(nudge_x = 0.1, nudge_y = -0.2)
  expect_equal(s5@nudge_x,  0.1)
  expect_equal(s5@nudge_y, -0.2)

  # stroke
  s6 <- ob_style(stroke = 0.5, stroke_color = "black", stroke_width = 0.25)
  expect_equal(s6@stroke,       0.5)
  expect_equal(s6@stroke_color, "black")
  expect_equal(s6@stroke_width, 0.25)
})

test_that("ob_style construction: edge cases", {
  expect_no_error(ob_style(linetype = NA))
  expect_no_error(ob_style(linewidth = NA))
  expect_no_error(ob_style(angle = degree(45)))
  expect_no_error(ob_style(label.padding = 5))
  expect_no_error(ob_style(label.margin = 3))
  expect_no_error(ob_style(arrow_fins = arrowheadr::arrow_head_deltoid()))
  expect_no_error(ob_style(arrow_mid  = arrowheadr::arrow_head_deltoid()))
})

# colour aliases ----
test_that("ob_style colour aliases", {
  # British spelling aliases map to their canonical properties
  expect_equal(ob_style(colour = "red")@color,              "red")
  expect_equal(ob_style(label.colour = "navy")@label.color, "navy")
  expect_equal(ob_style(text.colour  = "grey")@text.color,  "grey")
  expect_equal(ob_style(stroke_colour = "black")@stroke_color, "black")

  # canonical spelling is unchanged
  expect_equal(ob_style(color = "green")@color, "green")
})

# + merge operator ----
test_that("ob_style + merge: e2 property wins over e1", {
  s1 <- ob_style(color = "red")
  s2 <- ob_style(color = "blue")
  expect_equal((s1 + s2)@color, "blue")
})

test_that("ob_style + merge: non-overlapping properties combined", {
  result <- ob_style(color = "red") + ob_style(fill = "blue")
  expect_equal(result@color, "red")
  expect_equal(result@fill,  "blue")
  # identical to constructing with both at once
  expect_identical(result, ob_style(color = "red", fill = "blue"))
})

test_that("ob_style + merge: empty right side preserves left", {
  s <- ob_style(color = "red", linewidth = 2)
  expect_equal((s + ob_style())@color,     "red")
  expect_equal((s + ob_style())@linewidth, 2)
})

test_that("ob_style + merge: empty left side returns right", {
  s <- ob_style(color = "red")
  expect_equal((ob_style() + s)@color, "red")
})

test_that("ob_style + merge: multiple properties, right side wins for shared", {
  s1 <- ob_style(color = "red",  linewidth = 1, fill = "white")
  s2 <- ob_style(color = "blue", fill = "green")
  r  <- s1 + s2
  expect_equal(r@color,     "blue")   # e2 wins
  expect_equal(r@linewidth, 1)        # only in e1, preserved
  expect_equal(r@fill,      "green")  # e2 wins
})

test_that("ob_style + identity with S7::class_missing and non-style", {
  s <- ob_style(color = "red")
  expect_identical(`+`(s, S7::class_missing), s)
  expect_identical(`+`(S7::class_missing, s), s)
  # adding non-style scalar returns style unchanged
  expect_identical(`+`(s, 3), s)
  expect_identical(`+`(3, s), s)
})

# polar_just ----
test_that("ob_style polar_just constructor computes hjust/vjust", {
  tol <- 1e-10

  # east (0°): hjust = -0.1, vjust = 0.5
  s_east <- ob_style(polar_just = 0)
  expect_equal(s_east@hjust, -0.1, tolerance = tol)
  expect_equal(s_east@vjust,  0.5, tolerance = tol)

  # north (90°): hjust = 0.5, vjust = -0.1
  s_north <- ob_style(polar_just = 90)
  expect_equal(s_north@hjust, 0.5,  tolerance = tol)
  expect_equal(s_north@vjust, -0.1, tolerance = tol)

  # west (180°): hjust = 1.1, vjust = 0.5
  s_west <- ob_style(polar_just = 180)
  expect_equal(s_west@hjust, 1.1, tolerance = tol)
  expect_equal(s_west@vjust, 0.5, tolerance = tol)

  # south (270°): hjust = 0.5, vjust = 1.1
  s_south <- ob_style(polar_just = 270)
  expect_equal(s_south@hjust, 0.5, tolerance = tol)
  expect_equal(s_south@vjust, 1.1, tolerance = tol)

  # polar_just is not stored
  expect_equal(length(s_east@polar_just), 0)

  # ob_angle input works the same way as numeric degrees
  s_deg <- ob_style(polar_just = degree(0))
  expect_equal(s_deg@hjust, -0.1, tolerance = tol)

  # angle character
  s_deg_character <- ob_style(angle = "south")
  expect_equal(s_deg_character@angle, 270)

  # ob_polar input (explicit multiplier)
  expect_no_error(ob_style(polar_just = ob_polar(theta = degree(45), r = 3)))
})

test_that("ob_style polar_just setter computes hjust/vjust", {
  tol <- 1e-10
  s <- ob_style()

  s@polar_just <- degree(0)    # east
  expect_equal(s@hjust, -0.1, tolerance = tol)
  expect_equal(s@vjust,  0.5, tolerance = tol)

  s@polar_just <- 90           # north (numeric degrees)
  expect_equal(s@hjust, 0.5,  tolerance = tol)
  expect_equal(s@vjust, -0.1, tolerance = tol)

  s@polar_just <- ob_point(1, 0)  # east via ob_point (r=1 → multiplier=1)
  expect_equal(s@hjust, 0,   tolerance = tol)  # multiplier 1, not 1.2
  expect_equal(s@vjust, 0.5, tolerance = tol)
})

# polar2just ----
test_that("polar2just dispatch types agree", {
  tol <- 1e-10

  # numeric (radians)
  h_num <- polar2just(0, 1.2, "h")
  expect_equal(h_num, -0.1, tolerance = tol)

  # ob_angle
  h_ang <- polar2just(degree(0), 1.2, "h")
  expect_equal(h_ang, h_num, tolerance = tol)

  # character compass direction
  h_chr <- polar2just("east", 1.2, "h")
  expect_equal(h_chr, h_num, tolerance = tol)

  # ob_point: multiplier comes from @r
  h_pt <- polar2just(ob_point(1.2, 0), axis = "h")  # r=1.2
  expect_equal(h_pt, h_num, tolerance = tol)

  # ob_polar: multiplier comes from @r
  h_pol <- polar2just(ob_polar(theta = degree(0), r = 1.2), axis = "h")
  expect_equal(h_pol, h_num, tolerance = tol)

  # cardinal directions
  expect_equal(polar2just(degree(90),  1.2, "v"), -0.1, tolerance = tol)
  expect_equal(polar2just(degree(180), 1.2, "h"),  1.1, tolerance = tol)
  expect_equal(polar2just(degree(270), 1.2, "v"),  1.1, tolerance = tol)
})

# get_tibble ----
test_that("ob_style get_tibble", {
  # empty style → 0-column tibble
  expect_equal(ncol(get_tibble(ob_style())), 0)

  # single property
  t1 <- get_tibble(ob_style(color = "red"))
  expect_equal(t1$color, "red")
  expect_false("fill" %in% names(t1))

  # multiple properties
  t2 <- get_tibble(ob_style(color = "red", fill = "blue", linewidth = 2))
  expect_true(all(c("color", "fill", "linewidth") %in% names(t2)))
  expect_equal(t2$color,     "red")
  expect_equal(t2$fill,      "blue")
  expect_equal(t2$linewidth, 2)

  # vectorised: 2-row tibble
  t3 <- get_tibble(ob_style(color = c("red", "blue")))
  expect_equal(nrow(t3), 2)
  expect_equal(t3$color, c("red", "blue"))
})

# [ subsetting ----
test_that("ob_style [ subsetting", {
  s <- ob_style(color = c("red", "green", "blue"))
  expect_equal(s[1]@color, "red")
  expect_equal(s[2]@color, "green")
  expect_equal(s[3]@color, "blue")
  expect_identical(s[2], ob_style(color = "green"))
})

# print ----
test_that("ob_style print", {
  expect_no_error(capture.output(print(ob_style(color = "red"))))
  # empty style also prints without error
  expect_no_error(capture.output(print(ob_style())))
})

# data2shape round-trip ----
test_that("ob_style data2shape round-trip", {
  s <- ob_style(color = "red", fill = "blue", linewidth = 2)
  d <- get_tibble(s)
  s2 <- data2shape(d, ob_style)
  expect_true(S7::S7_inherits(s2, ob_style))
  expect_equal(s2@color,     "red")
  expect_equal(s2@fill,      "blue")
  expect_equal(s2@linewidth, 2)
})

# applied to shapes ----
test_that("ob_style applied to shapes via style argument", {
  my_style <- ob_style(fill = "red", color = "navy", linewidth = 2)

  c1 <- ob_circle(style = my_style)
  expect_equal(c1@fill,      "red")
  expect_equal(c1@color,     "navy")
  expect_equal(c1@linewidth, 2)

  e1 <- ob_ellipse(a = 2, b = 1, style = my_style)
  expect_equal(e1@fill,  "red")
  expect_equal(e1@color, "navy")

  r1 <- ob_rectangle(width = 3, height = 2, style = my_style)
  expect_equal(r1@fill,  "red")
  expect_equal(r1@color, "navy")
})

test_that("ob_style @style setter on shapes", {
  c1 <- ob_circle()
  c1@style <- ob_style(fill = "green", alpha = 0.7)
  expect_equal(c1@fill,  "green")
  expect_equal(c1@alpha, 0.7)

  # merging: existing properties not in new style are preserved
  c2 <- ob_circle(color = "red")
  c2@style <- ob_style(fill = "blue")
  expect_equal(c2@fill,  "blue")
  expect_equal(c2@color, "red")
})
