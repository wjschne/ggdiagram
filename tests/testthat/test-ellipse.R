library(testthat)
library(ggdiagram)


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
