library(testthat)
library(ggdiagram)

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
