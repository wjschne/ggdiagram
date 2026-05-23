library(testthat)
library(ggdiagram)

# label ----
test_that(desc = "label", {
  expect_no_error(ob_label(x = 1, label = "a"))
  expect_no_error(ob_label(y = 1, label = "a"))

  expect_no_error(ob_label(label = "a", polar_just = degree(10)))
  expect_no_error(ob_label(label = "a", polar_just = 4))
  expect_no_error(ob_label(label = "a", angle = 4))
  expect_no_error(ob_label(label = "a", angle = degree(c(0,30))))
  expect_no_error(ob_label(label = "a", center = ob_segment(ob_point(2,3), ob_point(3,4))))
  expect_no_error(ob_label(label = "a", center = ob_arc()))
  expect_no_error(ob_label(center = ob_arc()))
  expect_no_error(ob_label(label = c("a", "b"), angle = degree(1)))
  expect_no_error(ob_label(label = c("a", "b"), angle = 1))
  expect_no_error(ob_label(label = 1))
  l1 <- ob_label(center = ob_point(1,2))
  l2 <- ob_label(center = ob_point(1,0))
  l3 <- ob_label(center = ob_point(0,2))
  expect_identical(ob_label(x = 1, y = 2), l1)
  expect_identical(ob_label(x = 1), l2)
  expect_identical(ob_label(y = 2), l3)
  expect_equal(ob_label(label = 1, center = ob_point(2,1), x = 5)@center@x, 5)
  expect_equal(ob_label(label = 1, center = ob_point(2,1), y = 5)@center@y, 5)
  expect_no_error(l1@auto_label)
  expect_equal(l1@geom(), as.geom(l1))
  expect_no_error(ob_label("A", angle = degree(45))@tibble)
  l <- ob_label("A")
  l@angle <- degree(45)
  expect_no_error(l@tibble)
  expect_error(l@plot_point <- c(TRUE, TRUE), "The plot_point property must be a TRUE/FALSE value of length 1\\.")
  expect_error(
    ob_label(
      "A",
      plot_point = c(TRUE, FALSE),
      "The plot_point property must be a TRUE/FALSE value of length 1\\."
    )
  )
  expect_no_error(suppressMessages(capture.output(print(l), file = nullfile())))
  l@label.margin <- ggdiagram:::class_margin(1)
  l@label.padding <- ggdiagram:::class_margin(1)
  expect_no_error(get_tibble(l))



  expect_no_error(as.geom(ob_label("A", plot_point = TRUE), size = 12))

  expect_identical(ggdiagram::label_object(l), "(0,0)")

  expect_identical(
    unbind(ob_label(c("A", "B"))),
    list(ob_label("A"),
         ob_label("B")))

  expect_identical(nudge(ob_label("A"), 1, 1),
                   ob_label("A", x = 1, y = 1))
  expect_identical(nudge(ob_label("A"), x = 1),
                   ob_label("A", x = 1, y = 0))
  expect_identical(nudge(ob_label("A"), y = 1),
                   ob_label("A", x = 0, y = 1))
  l <- ob_label("A")
  expect_identical(place(x = l, from = ob_point(1,2), where = "left", sep = 1), ob_label("A", x = 0, y = 2))
  expect_identical(place(x = l, from = l, where = "left", sep = 1), ob_label("A", x = -1, y = 0))

  l2 <- ob_label(c("A", "B"), id = c("A", "B"))
  expect_identical(l2["A"], ob_label("A", id = "A"))
})


test_that("constructor defaults", {
  lb <- ob_label()
  expect_identical(lb@label, "(0,0)")
  expect_identical(lb@center@x, 0)
  expect_identical(lb@center@y, 0)
})

test_that("constructor with label and center", {
  lb <- ob_label("Hello", center = ob_point(1, 2))
  expect_identical(lb@label, "Hello")
  expect_identical(lb@center@x, 1)
  expect_identical(lb@center@y, 2)
})

test_that("constructor with x and y instead of center", {
  lb <- ob_label("B", x = 3, y = 4)
  expect_identical(lb@center@x, 3)
  expect_identical(lb@center@y, 4)
})

test_that("constructor overrides center x with x argument", {
  lb <- ob_label("A", center = ob_point(1, 2), x = 5)
  expect_identical(lb@center@x, 5)
  expect_identical(lb@center@y, 2)
})

test_that("constructor with ob_segment center uses midpoint", {
  seg <- ob_segment(ob_point(0, 0), ob_point(2, 0))
  lb <- ob_label(center = seg)
  expect_identical(lb@center@x, 1)
  expect_identical(lb@center@y, 0)
})

test_that("constructor with style arguments", {
  lb <- ob_label("X", color = "blue", size = 14)
  expect_identical(lb@color, "blue")
  expect_identical(lb@size, 14)
})

test_that("length property", {
  lb <- ob_label(c("A", "B", "C"), center = ob_point(c(1, 2, 3), c(4, 5, 6)))
  expect_equal(lb@length, 3)
})

test_that("auto_label returns center coordinates", {
  lb <- ob_label("X", center = ob_point(3, 4))
  expect_identical(lb@auto_label, "(3,4)")
})

test_that("tibble property has required columns", {
  t <- ob_label("A", center = ob_point(1, 2))@tibble
  expect_true(all(c("x", "y", "label") %in% names(t)))
  expect_equal(t$x, 1)
  expect_equal(t$y, 2)
  expect_equal(t$label, "A")
})

test_that("style getter and setter", {
  lb <- ob_label("X", color = "blue")
  expect_identical(lb@style@color, "blue")
  lb@style <- ob_style(color = "red")
  expect_identical(lb@color, "red")
})

test_that("polar_just sets hjust and vjust", {
  lb <- ob_label("X", polar_just = degree(0))
  expect_equal(lb@hjust, -0.1)
  expect_equal(lb@vjust, 0.5)
})

test_that("nudge shifts center", {
  lb <- ob_label("A", center = ob_point(1, 2))
  expect_identical(nudge(lb, x = 1, y = 2)@center, ob_point(2, 4))
  expect_identical(nudge(lb, x = 1)@center, ob_point(2, 2))
  expect_identical(nudge(lb, y = 1)@center, ob_point(1, 3))
})

test_that("place positions center relative to point", {
  lb <- ob_label("A")
  placed <- place(lb, ob_point(0, 0), where = "right", sep = 1)
  expect_equal(placed@center@x, 1, tolerance = 1e-10)
  expect_equal(placed@center@y, 0, tolerance = 1e-10)
})

test_that("subsetting with integer index", {
  lb <- ob_label(c("A", "B", "C"), center = ob_point(c(1, 2, 3), c(4, 5, 6)))
  expect_identical(lb[2]@label, "B")
  expect_identical(lb[2]@center@x, 2)
})

test_that("subsetting with id", {
  lb <- ob_label(c("A", "B"), center = ob_point(c(1, 2), c(3, 4)), id = c("x", "y"))
  expect_identical(lb["y"]@label, "B")
})

test_that("subsetting assignment", {
  lb <- ob_label(c("A", "B", "C"), center = ob_point(c(1, 2, 3), c(4, 5, 6)))
  lb[2] <- ob_label("Z", center = ob_point(9, 9))
  expect_identical(lb@label[2], "Z")
  expect_identical(lb@center@x[2], 9)
})

test_that("subsetting assignment error for non-ob_label", {
  lb <- ob_label(c("A", "B"), center = ob_point(c(1, 2), c(3, 4)))
  expect_error(lb[1] <- "not a label", "value must be of class ob_label")
})

test_that("geom no error", {
  expect_no_error(ob_label("A", center = ob_point(1, 2))@geom())
})

test_that("geom with plot_point no error", {
  expect_no_error(ob_label("A", center = ob_point(1, 2), plot_point = TRUE)@geom())
})

test_that("str no error", {
  expect_no_error(suppressMessages(capture.output(str(ob_label("A", center = ob_point(1, 2))))))
})

test_that(desc = "does set_label_x work on arcs, segments, and bezier curves?", {
a <- ob_point()
b <- ob_point(1,c(0,1))
# segment
ab_seg <- connect(a, b, label = c("1","2"))@set_label_x(.2)
expect_all_equal(ab_seg@label@center@x, ab_seg@label@center@x[1])

ab_seg2 <- connect(a, b, label = c("1","2"))
ab_seg3 <- connect(a, b, label = c("1","2"))@set_label_x()
expect_equal(ab_seg2@label@center@x, ab_seg3@label@center@x)


ab_seg4 <- connect(a, b[2], label = c("2"))@set_label_y()
expect_equal(ab_seg2[2]@label@center@y, ab_seg4@label@center@y)


# arc
ab_arc <- connect(a, b, label = c("1","2"), arc_bend = .5)@set_label_x(.2)
expect_all_equal(ab_arc@label@center@x, ab_arc@label@center@x[1])

# bezier
ab_bez <- connect(a, b, label = c("1","2"), to_offset = ob_point(1, 1))@set_label_x(.2)
expect_all_equal(ab_bez@label@center@x, ab_bez@label@center@x[1])
})


test_that("Return empty string",{
    l <- ob_label(c("A", NA))
    l[1] <- ob_label(NA)
    expect_equal(l, character(0))
  })
