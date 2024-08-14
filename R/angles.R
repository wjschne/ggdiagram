num2turn <- function(x, object) {
  e2_class <- match.arg(arg = class(object)[1],
                        choices = c("degree", "radian", "gradian", "turn", "class_angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, gradian = 400, turn = 1, class_angle = 2 * pi)
  x / denominator
}

turn2angle <- function(x, object) {
  e2_class <- match.arg(arg = class(object)[1],
                        choices = c("degree", "radian", "gradian", "turn", "class_angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, gradian = 400, turn = 1, class_angle = 2 * pi)
  x * denominator
}

# Angle ----
#' class_angle
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
class_angle <- new_class(
  name = "class_angle",
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
class_angle_or_numeric <- new_union(class_numeric, class_angle)
class_angle_or_character <- new_union(class_character, class_angle)





# Angle wrappers ----
#' degree class
#'
#' @rdname class_angle
#' @export
degree <- new_class(
  name = "degree",
  parent = class_angle,
  constructor = function(degree = class_missing) {
    if (is.character(degree)) degree <- cardinalpoint(degree)
    if (S7_inherits(degree, class_angle)) degree <- c(degree) * 360
    new_object(degree / 360)
  })




#' degree class
#'
#' @rdname class_angle
#' @export
radian <- new_class(
  name = "radian",
  parent = class_angle,
  constructor = function(radian = class_missing) {
    if (is.character(radian)) radian <- cardinalpoint(radian) * pi / 180
    if (S7_inherits(radian, class_angle)) radian <- c(radian) * 2 * pi
    new_object(radian / (2 * pi))
  })


#' degree class
#'
#' @rdname class_angle
#' @export
turn <- new_class(
  name = "turn",
  parent = class_angle,
  constructor = function(turn = class_missing) {
    if (is.character(turn)) turn <- cardinalpoint(turn) / 360
    if (S7_inherits(turn, class_angle)) turn <- c(turn)
    new_object(turn)
  })

# arithmetic ----
purrr::walk(list(`+`, `-`, `*`, `/`, `^`), \(.f) {
  method(.f, list(class_angle, class_angle)) <- function(e1, e2) {
    convert(.f(S7_data(e1), S7_data(e2)), S7_class(e2))
  }
  method(.f, list(class_angle, class_numeric)) <- function(e1, e2) {
    convert(num2turn(.f(prop(e1, S7_class(e1)@name), e2), e1), S7_class(e1))
  }
  method(.f, list(class_numeric, class_angle)) <- function(e1, e2) {
    convert(num2turn(.f(e1, prop(e2, S7_class(e2)@name)), e2), S7_class(e2))
  }
})


method(`==`, list(class_angle, class_angle)) <- function(e1, e2) {
  abs(S7_data(e1) - S7_data(e2)) <= .Machine$double.eps
}

method(`==`, list(class_angle, class_numeric)) <- function(e1, e2) {
  abs(S7_data(e1) - num2turn(e2, e1)) <= .Machine$double.eps
}

method(`==`, list(class_numeric, class_angle)) <- function(e1, e2) {
  abs(num2turn(e1, e2) - S7_data(e2)) <= .Machine$double.eps
}


method(`!=`, list(class_angle, class_angle)) <- function(e1, e2) {
  abs(S7_data(e1) - S7_data(e2)) > .Machine$double.eps
}

purrr::walk(list(`<`, `<=`, `>`, `>=`), \(.f) {
  method(.f, list(class_angle, class_angle)) <- function(e1, e2) {
    .f(S7_data(e1), S7_data(e2))
  }
  method(.f, list(class_angle, class_numeric)) <- function(e1, e2) {
    .f(S7_data(e1), num2turn(e2, e1))
  }
  method(.f, list(class_numeric, class_angle)) <- function(e1, e2) {
    .f(num2turn(e1, e2), S7_data(e2))
  }
})

method(cos, class_angle) <- function(x) {
  cospi(S7_data(x) * 2)
}
method(sin, class_angle) <- function(x) {
  sinpi(S7_data(x) * 2)
}
method(tan, class_angle) <- function(x) {
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

method(print, class_angle) <- function(x, ...) {
 cat(as.character(x), "\n")
  invisible(x)
}

method(str, class_angle) <- function(object,
                                     nest.lev = 0,
                                     additional = FALSE,
                                     omit = ".data") {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
}

method(str, degree) <- function(object,
                                nest.lev = 0,
                                additional = FALSE,
                                omit = c(".data", "turn", "radian")) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
}

method(str, radian) <- function(object,
                                nest.lev = 0,
                                additional = FALSE,
                                omit = c(".data", "turn", "degree")) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
}

method(str, turn) <- function(object,
                                nest.lev = 0,
                                additional = FALSE,
                                omit = c(".data", "degree", "radian")) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
}

method(as.character, class_angle) <- function(x,
  ...,
  digits = NULL,
  type = NULL) {
  if (is.null(type)) {
    a_class <- match.arg(
      arg = class(x)[1],
      choices = c("degree", "radian", "turn", "class_angle"))
  } else {
    a_class <- rlang::arg_match(type, c("degree", "radian", "turn"))
  }


  if (a_class == "class_angle") a_class <- "degree"

  if (is.null(digits)) {
    digits <- c(degree = 0, radian = 2, turn = 2)[a_class]
  }
  switch(
    a_class,
    degree = paste0(signs::signs(round(x@degree, digits)), "\u00B0"),
    radian = ifelse(
      abs(x@radian - pi) < .Machine$double.eps,
      "\u03C0",
      paste0(round(x@turn * 2, digits), "\u03C0")),
    gradian = paste0(round(x@gradian, digits), ""),
    turn = paste0(round_probability(x@turn, digits = digits)),
    class_angle = 2 * pi)
}


method(`[`, class_angle) <- function(x, y) {
  S7::S7_data(x) <-  c(x)[y]
  x
}


