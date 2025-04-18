library(ggdiagram)
library(testthat)


test_that("add, subtract, multiply, divide, and exponentiate degrees, radians, and turns",
          {
            purrr::map(list(degree, radian, turn), \(.f) {
              expect_equal(S7::prop(.f(.1), S7::S7_class(.f(0))@name), .1)
              expect_equal(.f(.1) + .f(.1), .f(.2))
              expect_equal(.f(.1) - .f(.1), .f(0))
              expect_equal(.f(.1) * .1, .f(.01))
              expect_equal(.f(.1) / 2, .f(.05))
              expect_equal(.f(.1)^2, .f(.01))
            })
          })



test_that("trig functions work with ob_angle", {
  expect_equal(cos(turn(1)), cospi(2))
  expect_equal(sin(turn(.25)), sinpi(.5))
  expect_equal(tan(turn(.5)), tanpi(1))
})

test_that("equality with ob_angle", {
  tidyr::crossing(tibble::tibble(
    .f = list(degree, radian, turn),
    ratio = c(360, 2 * pi, 1)
  ),
  .g = list(`>`, `<`, `<=`, `>=`, `==`, `!=`)) |>
    dplyr::mutate(
      t1 = .2,
      t2 = .3,
      a1 = purrr::map2(.f, t1, \(f, t) f(t)),
      a2 = purrr::map2(.f, t2, \(f, t) f(t)),
      n1 = ratio * t1,
      n2 = ratio * t2,
      ta = purrr::pmap_lgl(list(.g, a1, a2), \(g, aa1, aa2) g(aa1, aa2)),
      tn = purrr::pmap_lgl(list(.g, n1, n2), \(g, aa1, aa2) g(aa1, aa2)),
      test = ta == tn
    ) |>
    dplyr::pull(test) |>
    all() |>
    expect_true()
})

test_that("trig angles", {
  my_turn <- 1 / 8
  purrr::map2(list(degree, radian, turn), c(360, 2 * pi, 1), \(.f, ratio) {
    expect_equal(sin(.f(ratio * my_turn)), sinpi(2 * my_turn))
    expect_equal(cos(.f(ratio * my_turn)), cospi(2 * my_turn))
    expect_equal(tan(.f(ratio * my_turn)), tanpi(2 * my_turn))
  })
})


test_that("negative x: x@positive becomes positive", {
  expect_equal(degree(-90)@positive@degree, degree(270)@degree)
  expect_equal(degree(-90 - 360)@positive@degree, degree(270)@degree)
  expect_equal(degree(-90 - 360 * 2)@positive@degree, degree(270)@degree)
})

test_that("positive x: x@positive stays positive", {
  expect_equal(degree(90)@positive@degree, degree(90)@degree)
  expect_equal(degree(90 + 360)@positive@degree, degree(90)@degree)
  expect_equal(degree(90 + 360 * 2)@positive@degree, degree(90)@degree)
})

test_that("negative x: x@negative stays negative", {
  expect_equal(degree(-90)@negative@degree, degree(-90)@degree)
  expect_equal(degree(-90 - 360)@negative@degree, degree(-90)@degree)
  expect_equal(degree(-90 - 360 * 2)@negative@degree, degree(-90)@degree)
})

test_that("positive x: x@negative becomes negative", {
  expect_equal(degree(90)@negative@degree, degree(-270)@degree)
  expect_equal(degree(90 + 360)@negative@degree, degree(-270)@degree)
  expect_equal(degree(90 + 360 * 2)@negative@degree, degree(-270)@degree)
})


test_that("misc angle", {
  expect_equal(degree("north")@degree, radian("north")@degree)
  expect_equal(degree("north")@degree, turn("north")@degree)
  expect_equal(degree("north")@degree, turn(turn(.25))@degree)
  expect_identical(unbind(degree(1:2)), list(degree(1), degree(2)))
  expect_identical(degree(1:2)[1], degree(1))
  expect_identical(as.character(degree(2)), "2°")
  expect_identical(as.character(radian(2)), "0.64π")
  expect_identical(as.character(radian(pi)), "π")
  expect_identical(as.character(radian(2 * pi)), "0π")
  expect_identical(as.character(turn(.5)), ".50")
  a <- degree(0)
  a@degree <- 20
  expect_identical(a@degree, 20)
  a@radian <- pi
  expect_identical(a@radian, pi)
  a@turn <- 0.5
  expect_identical(a@turn, 0.5)

  expect_identical(convert(2,degree), degree(720))
  expect_identical(convert(2,turn), turn(2))
  expect_identical(convert(2,radian), radian(2 * 2 * pi))
  expect_no_error(capture.output(print(a), file = nullfile()))
ob_angle(.5)
ob_angle@constructor
})
