num2turn <- function(x, object) {
  e2_class <- match.arg(arg = class(object)[1],
                        choices = c("degree", "radian", "gradian", "turn", "angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, gradian = 400, turn = 1, angle = 2 * pi)
  x / denominator
}

turn2angle <- function(x, object) {
  e2_class <- match.arg(arg = class(object)[1],
                        choices = c("degree", "radian", "gradian", "turn", "angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, gradian = 400, turn = 1, angle = 2 * pi)
  x * denominator
}

# Angle ----
#' angle class
#'
#' Creates an angle in the metric of radians, degrees, and turns.
#'
#' Angles turns can be any real number, but degrees are displayed as values between -360 and +360, and radians are between -2pi and +2pi.
#' @param .data a real number indicating the number of turns.
#' @param radian radians
#' @param degree degrees
#' @param turn proportion of full turns of a circle (1 turn = 2 * pi radians)
#' @export
#' @examples
#' # Three Different ways to make a right angle
#' ## A quarter turn
#' turn(.25)
#'
#' ## half pi radians
#' radian(.5 * pi)
#'
#' ## 90 degrees
#' degree(90)
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
angle <- new_class(
  name = "angle",
  parent = class_double,
  properties = list(
    degree = new_property(
      class_numeric,
      getter = function(self) {
        (S7_data(self) %% ifelse(S7_data(self) < 0, -1, 1)) * 360
      },
      setter = function(self, value) {
        S7_data(self, TRUE) <- value / 360
        self
      }
    ),
    radian = new_property(
      class_numeric,
      getter = function(self) {
        (S7_data(self) %% ifelse(S7_data(self) < 0, -1, 1)) * (2 * pi)
      },
      setter = function(self, value) {
        S7_data(self, TRUE) <- value / (2 * pi)
        self
      }
    ),
    turn = new_property(
      class_numeric,
      getter = function(self) {
        (S7_data(self) %% ifelse(S7_data(self) < 0, -1, 1))
      },
      setter = function(self, value) {
        S7_data(self, TRUE) <- value
        self
      }
    )))
class_angle_or_numeric <- new_union(class_numeric, angle)
class_angle_or_character <- new_union(class_character, angle)





# Angle wrappers ----
#' degree class
#'
#' @rdname angle
#' @export
degree <- new_class("degree", angle,
                    constructor = function(degree = class_missing) {
                      new_object(degree / 360)
                    })

# Angle wrappers ----
#' degree class
#'
#' @rdname angle
#' @export
radian <- new_class("radian", angle,
                    constructor = function(radian = class_missing) {
                      new_object(radian / (2 * pi))
                    })

# Angle wrappers ----
#' degree class
#'
#' @rdname angle
#' @export
turn <- new_class("turn", angle,
                    constructor = function(turn = class_missing) {
                      new_object(turn)
                    })

# arithmetic ----
purrr::walk(list(`+`, `-`, `*`, `/`, `^`), \(.f) {
  method(.f, list(angle, angle)) <- function(e1, e2) {
    convert(.f(S7_data(e1), S7_data(e2)), S7_class(e2))
  }
  method(.f, list(angle, class_numeric)) <- function(e1, e2) {
    convert(num2turn(.f(prop(e1, S7_class(e1)@name), e2), e1), S7_class(e1))
  }
  method(.f, list(class_numeric, angle)) <- function(e1, e2) {
    convert(num2turn(.f(e1, prop(e2, S7_class(e2)@name)), e2), S7_class(e2))
  }
})


method(`==`, list(angle, angle)) <- function(e1, e2) {
  abs(S7_data(e1) - S7_data(e2)) <= .Machine$double.eps
}

method(`==`, list(angle, class_numeric)) <- function(e1, e2) {
  abs(S7_data(e1) - num2turn(e2, e1)) <= .Machine$double.eps
}

method(`==`, list(class_numeric, angle)) <- function(e1, e2) {
  abs(num2turn(e1, e2) - S7_data(e2)) <= .Machine$double.eps
}


method(`!=`, list(angle, angle)) <- function(e1, e2) {
  abs(S7_data(e1) - S7_data(e2)) > .Machine$double.eps
}

purrr::walk(list(`<`, `<=`, `>`, `>=`), \(.f) {
  method(.f, list(angle, angle)) <- function(e1, e2) {
    .f(S7_data(e1), S7_data(e2))
  }
  method(.f, list(angle, class_numeric)) <- function(e1, e2) {
    .f(S7_data(e1), num2turn(e2, e1))
  }
  method(.f, list(class_numeric, angle)) <- function(e1, e2) {
    .f(num2turn(e1, e2), S7_data(e2))
  }
})

method(cos, angle) <- function(x) {
  cospi(S7_data(x) * 2)
}
method(sin, angle) <- function(x) {
  sinpi(S7_data(x) * 2)
}
method(tan, angle) <- function(x) {
  tanpi(S7_data(x) * 2)
}



method(convert, list(class_numeric, degree)) <- function(from, to) {
  degree(from * 360)
}
method(convert, list(class_numeric, turn)) <- function(from, to) {
  turn(from)
}

method(convert, list(class_numeric, radian)) <- function(from, to) {
  radian(from * 2 * pi)
}

# str print ----
method(str, angle) <- function(object,
                               nest.lev = 0,
                               additional = FALSE,
                               omit = ".data") {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev)
}

method(print, angle) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}
