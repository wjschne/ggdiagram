library(testthat)
library(ggdiagram)

# circle ----
test_that(desc = "circle", {
  # area
  c1 <- ob_circle(radius = 3)
  c1@style <- ob_style(fill = "red")
  expect_equal(c1@fill, "red")
  expect_no_error(c1@bounding_box)
  expect_no_error(c1@polygon)
  expect_no_error(c1@geom())
  expect_no_error(c1@arc(3,4))
  expect_no_error(c1@angle_at(4))
  expect_no_error(c1@normal_at(4))
  expect_no_error(c1@normal_at(ob_point(3,4)))
  expect_no_error(c1@tangent_at(4))
  expect_no_error(c1@point_at(4))
  expect_no_error(capture.output(print(c1), file = nullfile()))
  expect_no_error(place(ob_line(3,4), c1))
  expect_no_error(ob_array(c1, 4))
  expect_equal(c1@length, 1)
  expect_equal(c1@area , expected = 9 * pi)
  # perimeter
  expect_equal(c1@circumference, 6 * pi)
  expect_equal(c1@diameter, 6)
  expect_no_error(ob_circle(x = 0, y = 0))
  expect_no_error(ob_circle(x = 0))
  expect_no_error(ob_circle(y = 0))
  expect_no_error(ob_circle(color = 1, label = ob_label(c("A", "B"))))
  p1 <- ob_point(0, 3)
  p2 <- ob_point(1, 1)
  p3 <- ob_point(1, 7)
  expect_no_error(circle_from_3_points(p1, p2, p3))
  expect_no_error(circle_from_3_points(bind(c(p1, p2, p3))))
  expect_error(circle_from_3_points(bind(c(ob_point(1, 2), p2, p3))),
               "Points on the same line cannot lie on a circle\\.")

  expect_error(circle_from_3_points(bind(c(ob_point(1, 1:2), p2, p3))),
               "p1 must be of length 3 or p1, p2, and p2 must be of length 1\\.")
  expect_no_error(as.geom(ob_circle(color = "red")))

  c1 <- ob_circle(label = ob_label("A"))
  c2 <- ob_circle(x = 1, y = 1, label = ob_label("A"))
  expect_no_error(as.geom(c1))
  expect_identical(c1 + ob_point(), c1)
  expect_identical(c1 - ob_point(), c1)
  expect_identical(ob_point() + c1, c1)
  expect_identical(ob_point() - c1, c1)
  expect_identical(c1 %|-% ob_point(1, 1),
                   ob_point(x = 0, y = 1))
  expect_identical(c1 %-|% ob_point(1, 1),
                   ob_point(x = 1, y = 0))
  expect_identical(ob_point(1, 1) %|-% c1,
                   ob_point(x = 1, y = 0))
  expect_identical(ob_point(1, 1) %-|% c1,
                   ob_point(x = 0, y = 1))
  expect_identical(c2 %|-% c1,
                   ob_point(x = 1, y = 0))
  expect_identical(c2 %-|% c1,
                   ob_point(x = 0, y = 1))

  expect_identical(nudge(ob_circle(), 1, 1),
                   ob_circle(x = 1, y = 1))
  expect_identical(nudge(ob_circle(), x = 1),
                   ob_circle(x = 1, y = 0))
  expect_identical(nudge(ob_circle(), y = 1),
                   ob_circle(x = 0, y = 1))

  expect_identical(data2shape(data.frame(x  = 1, y = 2), ob_circle),
                   ob_circle(x = 1, y = 2))
})

test_that(desc = "arc", {
  expect_no_error(ob_arc(x = 0, y = 0))
  expect_no_error(ob_arc(x = 0))
  expect_no_error(ob_arc(y = 0))
  expect_no_error(ob_arc(label = ob_label(c("A", "B"))))
  expect_identical(ob_arc(label = ob_label("A")), ob_arc(label = "A"))
  expect_identical(ob_arc(label = ob_label(degree(34))), ob_arc(label = degree(34)))
  p1 <- ob_point(1,2)
  expect_no_error(ob_arc(start_point = p1))
  expect_no_error(ob_arc(end_point = p1))
  a1 <- ob_arc(start = 180)
  a2 <- ob_arc(end = 180)
  expect_identical(a1, ob_arc(start = degree(180)))
  expect_no_error(ob_arc(start = turn(.5)))
  expect_no_error(ob_arc(end = turn(.5)))
  expect_identical(a1@start@turn, ob_arc(start = radian(pi))@start@turn)
  expect_identical(a2, ob_arc(end = degree(180)))
  expect_identical(a2@end@turn, ob_arc(end = radian(pi))@end@turn)


})

# ellipse ----
test_that(desc = "ellipse", {
  expect_no_error(ob_ellipse(x = 0, y = 0))
  expect_no_error(ob_ellipse(x = 0))
  expect_no_error(ob_ellipse(y = 0))
  expect_no_error(ob_ellipse(a = 1, label = ob_label(c("A", "B"))))
  expect_no_error(ob_ellipse(color = "red"))
  expect_no_error(ob_ellipse(label = c("a", "b")))
  expect_no_error(ob_ellipse(label = degree(45)))
  expect_no_error(ob_ellipse(label = 45))
  expect_no_error(ob_ellipse(label = 45, color = c("red", "blue")))
  expect_error(ob_ellipse(label = ob_label(1:3), color = c("red", "blue")), "Label length is 3\\. It must be either of length 1 or compatible with the length of the ob_ellipse \\(length = 2\\)\\.")

  expect_no_error(ob_ellipse() %>% ob_array(2))
})

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
  expect_no_error(capture.output(print(l), file = nullfile()))
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


# segment ----
test_that(desc = "segment", {
  # segment
  p1 <- ob_point(0, 3)
  p2 <- ob_point(1, 1)
  p3 <- ob_point(2, 5)
  s1 <- ob_segment(p1, p2)
  s2 <- ob_segment(p2, p3, label = ob_label("A"))
  s <- bind(c(s1,s2))
  expect_identical(nudge(s1, 1, 1),
                   ob_segment(ob_point(1, 4),
                              ob_point(2, 2)))
  expect_identical(nudge(s1, x =  1), s1 + ob_point(1, 0))
  expect_identical(nudge(s1, y =  1), s1 + ob_point(0, 1))
  expect_identical(nudge(s1), s1)
  expect_identical(nudge(s1, p3, 1), s1 + (p3 + 1))
  expect_identical(nudge(s1, p3), s1 + p3)
  expect_no_error(`+`(s1,s2))
  expect_identical(`==`(s1,s1), TRUE)
  expect_no_error(capture.output(print(s1), file = nullfile()))
  expect_identical(s1@length, 1L)
  expect_no_error(s1@bounding_box)
  expect_no_error(s2@geom())
  expect_no_error(s1@hatch())
  expect_identical(s1@midpoint(), midpoint(s1))
  expect_no_error(s1@nudge(1,1))
  expect_no_error(s1@aesthetics)
  expect_no_error(s1@tibble)
  expect_no_error(get_tibble_defaults(s1))
  expect_no_error(get_tibble(s1))
  expect_no_error(as.geom(s1))
  expect_equal(s1@length, 1)
  expect_equal(s1@distance, distance(s1))
  expect_equal(s1@line@intercept, 3)
  expect_equal(s[2], s2)
  expect_no_error(ob_segment(p1, p2))
  expect_no_error(ob_segment(x = 1, xend = 2, y = 3, yend = 5))
  expect_no_error(ob_segment(x = 1, y = 3, yend = 5))
  expect_no_error(ob_segment(xend = 1, y = 3, yend = 5))
  expect_no_error(ob_segment(x = 1, xend = 2, yend = 5))
  expect_no_error(ob_segment(x = 1, y = 3, xend = 2))
  expect_error(ob_segment(), "p1 must be a ob_point object with one or more points\\.")
  expect_error(ob_segment(p1), "If p2 is missing, p1 must be a ob_point object with multiple points\\.")
  expect_no_error(ob_segment(ob_point(1:3,3)))
  expect_no_error(ob_segment(p1, p2, label = ob_label("3")))
  expect_no_error(ob_segment(p1, p2, label = ob_label(c("3", "A"))))
  expect_no_error(rotate(ob_segment(p1, p2), theta = 45))

  s2 <- ob_segment(ob_point(1:2, 1), ob_point(1:2, 2), id = c("A", "B"))
  sB <- ob_segment(ob_point(2, 1), ob_point(2, 2), id = "B")
  expect_equal(s2["B"], sB)


})


# rectangle ----
test_that(desc = "rectangle", {
  r2 <- ob_rectangle()
  r3 <- ob_rectangle(angle = 2)
  r <- bind(c(r2,r3))
  a <- ob_point(1,2)
  expect_identical(`==`(r2,ob_rectangle()), TRUE)
  expect_identical(`==`(r2, ob_rectangle(width = 3)), FALSE)
  expect_no_error(ob_rectangle())
  expect_no_error(r2@normal_at(degree(45)))
  expect_no_error(ob_rectangle(angle = 45))
  expect_no_error(ob_rectangle(x = 3))
  expect_no_error(ob_rectangle(y = 3))
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
  expect_no_error(ob_rectangle(label = ob_label(c("a", 'b'))))
  expect_no_error(rotate(ob_rectangle(), theta = degree(45)))
  expect_no_error(rotate(ob_rectangle(), theta = 45))
  expect_error(ob_rectangle(vertex_radius = c(.1, .2)), "The vertex_radius property must be of length 1\\.")
  expect_identical(r2@area, 1)
  expect_identical(r2@perimeter, 4)
  expect_no_error(r2@north)
  expect_no_error(r2@south)
  expect_no_error(r2@east)
  expect_no_error(r2@west)
  expect_no_error(r2@side)
  r2@style <- ob_style(color = "red")
  expect_identical(r2@style@color, "red")
  expect_no_error(r2@normal_at(theta = 45))
  expect_no_error(r2@point_at(theta = 45))
  expect_no_error(r2@point_at(theta = NA))
  expect_no_error(r2@geom())
  expect_no_error(r2@aesthetics)
  expect_no_error(r2@bounding_box)
  expect_no_error(r2@tibble)
  expect_no_error(get_tibble(r2))
  expect_no_error(get_tibble_defaults(r2))
  expect_no_error(capture.output(print(r2), file = nullfile()))
  expect_identical(ob_rectangle(height = 1:3)[2]@height, 2L)
  expect_no_error(place(ob_rectangle(label = ob_label("A")), ob_point(2,3)))
  expect_no_error(place(ob_point(2,3), ob_rectangle()))
  expect_identical(ob_array(ob_rectangle(), k = 4)@length, 4L)

})


test_that("subsetting circles", {
  x <- ob_circle(x = 0:1, y = 2)
  x1 <- ob_circle(x = 0, y = 2)
  x2 <- ob_circle(x = c(0,0), y = 2)
  x11 <- x[1]
  expect_equal(x11@tibble, x1@tibble)
  x[2] <- ob_circle(x = 0, y = 2)
  expect_equal(x@tibble, x2@tibble)

})
