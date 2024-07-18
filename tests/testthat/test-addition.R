test_that("add angles", {
  purrr::map(list(degree, radian, turn), \(.f) {
    expect_equal(prop(.f(.1), S7_class(.f(0))@name), .1)
    expect_equal(.f(.1) + .f(.1), .f(.2))
    expect_equal(.f(.1) - .f(.1), .f(0))
    expect_equal(.f(.1) * .1, .f(.01))
    expect_equal(.f(.1) / 2, .f(.05))
    expect_equal(.f(.1) ^ 2, .f(.01))
  })
  })

test_that("equality", {
  tidyr::crossing(tibble::tibble(
    .f = list(degree, radian, turn),
    ratio = c(360, 2 * pi, 1)),
    .g = list(`>`, `<`, `<=`, `>=`, `==`, `!=`)) %>%
    dplyr::mutate(t1 = .2,
                  t2 = .3,
                  a1 = purrr::map2(.f, t1, \(f,t) f(t)),
                  a2 = purrr::map2(.f, t2, \(f,t) f(t)),
                  n1 = ratio * t1,
                  n2 = ratio * t2,
                  ta = purrr::pmap_lgl(list(.g, a1,a2), \(g, aa1, aa2) g(aa1,aa2)),
                  tn = purrr::pmap_lgl(list(.g, n1,n2), \(g, aa1, aa2) g(aa1,aa2)),
                  test = ta == tn
                  ) %>%
    dplyr::pull(test) %>%
    all() %>%
    expect_true()
})

test_that("trig angles", {
  my_turn <- 1 / 8
  purrr::map2(list(degree, radian, turn), c(360, 2*pi, 1), \(.f, ratio) {
    expect_equal(sin(.f(ratio * my_turn)),  sinpi(2 * my_turn))
    expect_equal(cos(.f(ratio * my_turn)),  cospi(2 * my_turn))
    expect_equal(tan(.f(ratio * my_turn)),  tanpi(2 * my_turn))
  })
})
