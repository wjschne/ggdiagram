library(ggdiagram)
library(testthat)


test_that("add, subtract, multiply, divide, and exponentiate degrees, radians, and turns",
          {
            purrr::map(list(degree, radian, turn), \(.f) {
              expect_equal(S7::prop(.f(0.1), S7::S7_class(.f(0))@name), 0.1)
              expect_equal(.f(0.1) + .f(0.1), .f(0.2))
              expect_equal(.f(0.1) - .f(0.1), .f(0))
              expect_equal(.f(0.1) * 0.1, .f(0.01))
              expect_equal(.f(0.1) / 2, .f(0.05))
              expect_equal(.f(0.1)^2, .f(0.01))
            })
          })

test_that("ob_angle equality", {
  expect_equal(c(degree(90)), c(ob_angle(degree = 90)))
  expect_equal(c(radian(1)), c(ob_angle(radian = 1)))
  expect_equal(c(turn(0.1)), c(ob_angle(turn = 0.1)))
})



test_that("trig functions work with ob_angle", {
  expect_equal(cos(turn(1)), cospi(2))
  expect_equal(sin(turn(0.25)), sinpi(0.5))
  expect_equal(tan(turn(0.5)), tanpi(1))
})

test_that("cardinal point", {
  expect_identical(ob_angle(0.25), ob_angle("north"))
})

test_that("equality with ob_angle", {
  tidyr::crossing(tibble::tibble(
    .f = list(degree, radian, turn, ob_angle),
    ratio = c(360, 2 * pi, 1, 1)
  ),
  .g = list(`>`, `<`, `<=`, `>=`, `==`, `!=`)) |>
    dplyr::mutate(
      t1 = 0.2,
      t2 = 0.3,
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
  purrr::map2(list(degree, radian, turn, ob_angle), c(360, 2 * pi, 1, 1), \(.f, ratio) {
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
  expect_identical(as.character(degree(0), digits = 2), "0°")
  expect_identical(as.character(degree(c(1,2))), c("1°", "2°"))
  expect_identical(as.character(degree(-2), digits = 2), "−2°")
  expect_identical(as.character(degree(2.45), digits = 2), "2.45°")
  expect_identical(as.character(radian(2)), "0.64π")
  expect_identical(as.character(radian(0)), "0π")
  expect_identical(as.character(radian(pi)), "π")
  expect_identical(as.character(radian(-pi)), "−π")
  expect_identical(as.character(radian(2 * pi)), "0π")
  expect_identical(as.character(radian(-2 * pi)), "0π")
  expect_identical(as.character(radian(-0.1 * pi)), "−0.1π")
  expect_identical(as.character(turn(0.5)), ".50")
  expect_identical(as.character(turn(0)), "0")
  expect_identical(as.character(turn(1)), "0")
  expect_identical(as.character(turn(-0.5)), "−.50")
  a <- degree(0)
  a@degree <- 20
  expect_identical(a@degree, 20)
  a@radian <- pi
  expect_identical(a@radian, pi)
  a@turn <- 0.5
  expect_identical(a@turn, 0.5)

  expect_no_error(capture.output(print(a)))

})

test_that(
  "selection and assignment with angles", {
    x <- degree(1:20)
    expect_identical(x[1:10], degree(1:10))
    x[2] <- degree(100)
    expect_identical(x, degree(c(1,100, 3:20)))
  }
)

test_that("cross-unit conversions", {
  expect_equal(degree(180)@radian, pi)
  expect_equal(degree(180)@turn, 0.5)
  expect_equal(radian(pi)@degree, 180)
  expect_equal(radian(pi)@turn, 0.5)
  expect_equal(turn(0.5)@degree, 180)
  expect_equal(turn(0.5)@radian, pi)
})

test_that("ob_angle subclasses accept each other as input", {
  expect_equal(degree(radian(pi))@degree, 180)
  expect_equal(radian(degree(180))@radian, pi)
  expect_equal(turn(degree(180))@turn, 0.5)
})

test_that("numeric arithmetic respects the unit of the angle class", {
  expect_equal((degree(10) + 10)@degree, 20)
  expect_equal((radian(1) + 1)@radian, 2)
  expect_equal((turn(0.25) + 0.25)@turn, 0.5)
})

test_that("upright flips angles in the 91-270 range", {
  expect_equal(degree(45)@upright@degree,  45)
  expect_equal(degree(135)@upright@degree, 315)
  expect_equal(degree(270)@upright@degree,  90)
  expect_equal(degree(315)@upright@degree, 315)
})

test_that("ob_angle default constructor returns empty", {
  expect_equal(length(c(ob_angle())), 0)
})

test_that("ob_angle .data constructor takes turns", {
  expect_equal(ob_angle(0.5)@degree, 180)
  expect_equal(ob_angle(0.25)@radian, pi / 2)
})

test_that("as.character with type argument converts units", {
  expect_identical(as.character(degree(90), type = "radian"), "0.5\u03C0")
  expect_identical(as.character(degree(90), type = "turn"), ".25")
  expect_identical(as.character(turn(0.5), type = "degree"), "180\u00B0")
  expect_identical(as.character(radian()), character(0))
})

test_that("trig at specific values", {
  expect_equal(sin(degree(30)), 0.5)
  expect_equal(cos(degree(60)), 0.5)
  expect_equal(tan(degree(45)), 1)
})

test_that("!= operator", {
  expect_true(degree(90) != degree(45))
  expect_false(degree(90) != degree(90))
  bind(degree(0))
})

test_that("str and print no error", {
  expect_no_error(capture.output(str(degree(45))))
  expect_no_error(capture.output(str(radian(pi))))
  expect_no_error(capture.output(str(turn(0.5))))
  expect_no_error(capture.output(print(degree(45))))
})

test_that("bind angles", {
  expect_true(S7::S7_inherits(bind(list(turn(0), degree(60))), turn), TRUE)
  expect_true(S7::S7_inherits(bind(list(radian(0), degree(60))), radian), TRUE)
  expect_true(S7::S7_inherits(bind(list(degree(0), radian(60))), degree), TRUE)
  length(radian())
  bind(list(radian()))
  unlist(list(radian()))
  radian(numeric(0) * pi * 2)
  expect_true(S7::S7_inherits(bind(list(degree(0), radian(60))), degree), TRUE)

})
