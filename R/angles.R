num2turn <- function(x, object_name) {
  e2_class <- match.arg(arg = object_name,
                        choices = c("degree", "radian", "turn", "ob_angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, turn = 1, ob_angle = 2 * pi)
  x / denominator
}

turn2angle <- function(x, object_name) {
  e2_class <- match.arg(arg = object_name,
                        choices = c("degree", "radian", "turn", "ob_angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, turn = 1, ob_angle = 2 * pi)
  x * denominator
}

# Angle ----
#' ob_angle
#'
#' Creates an angle in the metric of radians, degrees, and turns.
#'
#' Angles turns can be any real number, but degrees are displayed as values between -360 and +360, and radians are between -2pi and +2pi.
#' @param .data a real number indicating the number of turns.
#' @param radian radians
#' @param degree degrees
#' @param turn proportion of full turns of a circle (1 turn = 2 * pi radians)
#' @slot positive if angle is negative, adds a full turn to ensure the angle is positive
#' @slot negative if angle is positive, subtracts a full turn to ensure the angle is negative
#' @export
#' @return ob_angle
#' @examples
#' # Three Different ways to make a right angle
#' ## 90 degrees
#' degree(90)
#'
#' ## half pi radians
#' radian(.5 * pi)
#'
#' ## A quarter turn
#' turn(.25)
#'
#' # Operations
#' degree(30) + degree(20)
#' degree(350) + degree(20)
#' degree(30) - degree(30)
#' degree(30) - degree(50)
#'
#' degree(30) * 2
#' degree(30) / 3
#'
#' radian(1) + 1 # added or subtracted numbers are radians
#' degree(10) + 10 # added or subtracted numbers are degrees
#' turn(.25) + .25 # added or subtracted numbers are turns
#'
#' # Trigonometric functions work as normal
#' sin(degree(30))
#' cos(degree(30))
#' tan(degree(30))
ob_angle <- S7::new_class(
  name = "ob_angle",
  parent = S7::class_double,
  properties = list(
    degree = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        (c(self) %% ifelse(c(self) < 0, -1, 1)) * 360
      },
      setter = function(self, value) {
        S7::S7_data(self, TRUE) <- value / 360
        self
      }
    ),
    radian = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        (c(self) %% ifelse(c(self) < 0, -1, 1)) * (2 * pi)
      },
      setter = function(self, value) {
        S7::S7_data(self, TRUE) <- value / (2 * pi)
        self
      }
    ),
    turn = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        (c(self) %% ifelse(c(self) < 0, -1, 1))
      },
      setter = function(self, value) {
        S7::S7_data(self, TRUE) <- value
        self
      }
    ),
    positive = S7::new_property(getter = \(self) {
      trn <- rep(0, length(c(self)))
      trn[self@turn < 0] <- floor(c(self))[self@turn < 0]
      turn(abs(trn)) + self
    }),
    negative = S7::new_property(getter = \(self) {
      trn <- rep(0, length(c(self)))
      trn[self@turn > 0] <- floor(c(self))[self@turn > 0] + 1
      turn(-abs(trn)) + self
    })
    ))


ob_angle_or_numeric <- S7::new_union(S7::class_numeric, ob_angle)
ob_angle_or_character <- S7::new_union(S7::class_character, ob_angle)





# Angle wrappers ----
#' degree class
#'
#' @rdname ob_angle
#' @export
degree <- S7::new_class(
  name = "degree",
  parent = ob_angle,
  constructor = function(degree = numeric(0)) {
    if (is.character(degree)) degree <- cardinalpoint(degree)
    if (S7::S7_inherits(degree, ob_angle)) degree <- c(degree) * 360
    S7::new_object(degree / 360)
  })

#' degree class
#'
#' @rdname ob_angle
#' @export
radian <- S7::new_class(
  name = "radian",
  parent = ob_angle,
  constructor = function(radian = numeric(0)) {
    if (is.character(radian)) radian <- cardinalpoint(radian) * pi / 180
    if (S7::S7_inherits(radian, ob_angle)) radian <- c(radian) * 2 * pi
    S7::new_object(radian / (2 * pi))
  })


#' degree class
#'
#' @rdname ob_angle
#' @export
turn <- S7::new_class(
  name = "turn",
  parent = ob_angle,
  constructor = function(turn = numeric(0)) {
    if (is.character(turn)) turn <- cardinalpoint(turn) / 360
    if (S7::S7_inherits(turn, ob_angle)) turn <- c(turn)
    S7::new_object(turn)
  })

# arithmetic ----
purrr::walk(list(`+`, `-`, `*`, `/`, `^`), \(.f) {
  S7::method(.f, list(ob_angle, ob_angle)) <- function(e1, e2) {
    S7::convert(
      .f(c(e1), c(e2)),
      S7::S7_class(e2))
  }

  S7::method(.f, list(ob_angle, S7::class_numeric)) <- function(e1, e2) {
    S7::convert(
      num2turn(
        .f(
          S7::prop(
            e1,
            S7::S7_class(e1)@name),
          e2),
        S7::S7_class(e1)@name),
      S7::S7_class(e1))
  }

  S7::method(.f, list(S7::class_numeric, ob_angle)) <- function(e1, e2) {
    S7::convert(
      num2turn(
        .f(
          e1,
          S7::prop(
            e2, S7::S7_class(e2)@name)),
        S7::S7_class(e2)@name),
      S7::S7_class(e2))
  }
})

# equality ----
S7::method(`==`, list(ob_angle, ob_angle)) <- function(e1, e2) {
  abs(c(e1) - c(e2)) <= .Machine$double.eps
}

S7::method(`==`, list(ob_angle, S7::class_numeric)) <- function(e1, e2) {
  abs(c(e1) - num2turn(e2, e1)) <= .Machine$double.eps
}

S7::method(`==`, list(S7::class_numeric, ob_angle)) <- function(e1, e2) {
  abs(num2turn(e1, e2) - c(e2)) <= .Machine$double.eps
}


S7::method(`!=`, list(ob_angle, ob_angle)) <- function(e1, e2) {
  abs(c(e1) - c(e2)) > .Machine$double.eps
}

purrr::walk(list(`<`, `<=`, `>`, `>=`), \(.f) {
  S7::method(.f, list(ob_angle, ob_angle)) <- function(e1, e2) {
    .f(c(e1), c(e2))
  }
  S7::method(.f, list(ob_angle, S7::class_numeric)) <- function(e1, e2) {
    .f(c(e1), num2turn(e2, e1))
  }
  S7::method(.f, list(S7::class_numeric, ob_angle)) <- function(e1, e2) {
    .f(num2turn(e1, e2), c(e2))
  }
})

# Trigonometry ----
S7::method(cos, ob_angle) <- function(x) {
  cospi(c(x) * 2)
}
S7::method(sin, ob_angle) <- function(x) {
  sinpi(c(x) * 2)
}
S7::method(tan, ob_angle) <- function(x) {
  tanpi(c(x) * 2)
}

# Angle Conversions ----
S7::method(convert, list(S7::class_numeric, degree)) <- function(from, to) {
  degree(from * 360)
}
S7::method(convert, list(S7::class_numeric, turn)) <- function(from, to) {
  turn(from)
}
#
S7::method(convert, list(S7::class_numeric, radian)) <- function(from, to) {
  radian(from * 2 * pi)
}

# str print ----

S7::method(print, ob_angle) <- function(x, ...) {
 cat(as.character(x), "\n")
  invisible(x)
}

S7::method(str, ob_angle) <- function(object,
                                     nest.lev = 0,
                                     additional = FALSE,
                                     omit = c(".data", "positive", "negative")) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
}

S7::method(str, degree) <- function(object,
                                nest.lev = 0,
                                additional = FALSE,
                                omit = c(".data", "turn", "radian", "positive", "negative")) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
}

S7::method(str, radian) <- function(object,
                                nest.lev = 0,
                                additional = FALSE,
                                omit = c(".data", "turn", "degree", "positive", "negative")) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
}

S7::method(str, turn) <- function(object,
                                nest.lev = 0,
                                additional = FALSE,
                                omit = c(".data", "degree", "radian", "positive", "negative")) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
}

# as.character ----

S7::method(as.character, ob_angle) <- function(x,
  ...,
  digits = NULL,
  type = NULL) {
  if (is.null(type)) {
    a_class <- match.arg(
      arg = class(x)[1],
      choices = c("ggdiagram::degree", "ggdiagram::radian", "ggdiagram::turn", "ggdiagram::ob_angle"))
  } else {
    a_class <- rlang::arg_match(type, c("degree", "radian", "turn"))
  }


  if (a_class == "ggdiagram::ob_angle") a_class <- "ggdiagram::degree"

  if (is.null(digits)) {
    digits <- unname(c(`ggdiagram::degree` = 0, `ggdiagram::radian` = 2, `ggdiagram::turn` = 2)[a_class])
  }
  switch(
    a_class,
    `ggdiagram::degree` = paste0(signs::signs(round(x@degree, digits)), "\u00B0"),
    `ggdiagram::radian` = ifelse(
      abs(x@radian - pi) < .Machine$double.eps,
      "\u03C0",
      paste0(round(x@turn * 2, digits), "\u03C0")),
    `ggdiagram::turn` = paste0(round_probability(x@turn, digits = digits)),
    ob_angle = 2 * pi)
}

# subset ----
S7::method(`[`, ob_angle) <- function(x, y) {
  S7::S7_data(x) <-  c(x)[y]
  x
}

S7::method(`[<-`, ob_angle) <- function(x, y, value) {
  d <- c(x)
  d[y] <- c(value)
  S7::S7_data(x) <-  d
  x
}

# unbind ----
S7::method(unbind, ob_angle) <- function(x) {
  purrr::map(seq(length(c(x))), \(i) x[i])
}



