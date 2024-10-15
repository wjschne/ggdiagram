num2turn <- function(x, object) {
  e2_class <- match.arg(arg = class(object)[1],
                        choices = c("degree", "radian", "gradian", "turn", "ob_angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, gradian = 400, turn = 1, ob_angle = 2 * pi)
  x / denominator
}

turn2angle <- function(x, object) {
  e2_class <- match.arg(arg = class(object)[1],
                        choices = c("degree", "radian", "gradian", "turn", "ob_angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, gradian = 400, turn = 1, ob_angle = 2 * pi)
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
ob_angle <- new_class(
  name = "ob_angle",
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


ob_angle_or_numeric <- new_union(class_numeric, ob_angle)
ob_angle_or_character <- new_union(class_character, ob_angle)





# Angle wrappers ----
#' degree class
#'
#' @rdname ob_angle
#' @export
degree <- new_class(
  name = "degree",
  parent = ob_angle,
  constructor = function(degree = class_missing) {
    if (is.character(degree)) degree <- cardinalpoint(degree)
    if (S7_inherits(degree, ob_angle)) degree <- c(degree) * 360
    new_object(degree / 360)
  })




#' degree class
#'
#' @rdname ob_angle
#' @export
radian <- new_class(
  name = "radian",
  parent = ob_angle,
  constructor = function(radian = class_missing) {
    if (is.character(radian)) radian <- cardinalpoint(radian) * pi / 180
    if (S7_inherits(radian, ob_angle)) radian <- c(radian) * 2 * pi
    new_object(radian / (2 * pi))
  })


#' degree class
#'
#' @rdname ob_angle
#' @export
turn <- new_class(
  name = "turn",
  parent = ob_angle,
  constructor = function(turn = class_missing) {
    if (is.character(turn)) turn <- cardinalpoint(turn) / 360
    if (S7_inherits(turn, ob_angle)) turn <- c(turn)
    new_object(turn)
  })

# arithmetic ----
purrr::walk(list(`+`, `-`, `*`, `/`, `^`), \(.f) {
  method(.f, list(ob_angle, ob_angle)) <- function(e1, e2) {
    S7::convert(.f(S7_data(e1), S7_data(e2)), S7_class(e2))
  }
  method(.f, list(ob_angle, class_numeric)) <- function(e1, e2) {
    S7::convert(num2turn(.f(prop(e1, S7_class(e1)@name), e2), e1), S7_class(e1))
  }
  method(.f, list(class_numeric, ob_angle)) <- function(e1, e2) {
    S7::convert(num2turn(.f(e1, prop(e2, S7_class(e2)@name)), e2), S7_class(e2))
  }
})

# equality ----
method(`==`, list(ob_angle, ob_angle)) <- function(e1, e2) {
  abs(S7_data(e1) - S7_data(e2)) <= .Machine$double.eps
}

method(`==`, list(ob_angle, class_numeric)) <- function(e1, e2) {
  abs(S7_data(e1) - num2turn(e2, e1)) <= .Machine$double.eps
}

method(`==`, list(class_numeric, ob_angle)) <- function(e1, e2) {
  abs(num2turn(e1, e2) - S7_data(e2)) <= .Machine$double.eps
}


method(`!=`, list(ob_angle, ob_angle)) <- function(e1, e2) {
  abs(S7_data(e1) - S7_data(e2)) > .Machine$double.eps
}

purrr::walk(list(`<`, `<=`, `>`, `>=`), \(.f) {
  method(.f, list(ob_angle, ob_angle)) <- function(e1, e2) {
    .f(S7_data(e1), S7_data(e2))
  }
  method(.f, list(ob_angle, class_numeric)) <- function(e1, e2) {
    .f(S7_data(e1), num2turn(e2, e1))
  }
  method(.f, list(class_numeric, ob_angle)) <- function(e1, e2) {
    .f(num2turn(e1, e2), S7_data(e2))
  }
})

# Trigonometry ----
method(cos, ob_angle) <- function(x) {
  cospi(S7_data(x) * 2)
}
method(sin, ob_angle) <- function(x) {
  sinpi(S7_data(x) * 2)
}
method(tan, ob_angle) <- function(x) {
  tanpi(S7_data(x) * 2)
}


# Angle Conversions ----

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

method(print, ob_angle) <- function(x, ...) {
 cat(as.character(x), "\n")
  invisible(x)
}

method(str, ob_angle) <- function(object,
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

# as.character ----

method(as.character, ob_angle) <- function(x,
  ...,
  digits = NULL,
  type = NULL) {
  if (is.null(type)) {
    a_class <- match.arg(
      arg = class(x)[1],
      choices = c("degree", "radian", "turn", "ob_angle"))
  } else {
    a_class <- rlang::arg_match(type, c("degree", "radian", "turn"))
  }


  if (a_class == "ob_angle") a_class <- "degree"

  if (is.null(digits)) {
    digits <- unname(c(degree = 0, radian = 2, turn = 2)[a_class])
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
    ob_angle = 2 * pi)
}

# subset ----
method(`[`, ob_angle) <- function(x, y) {
  S7::S7_data(x) <-  c(x)[y]
  x
}

method(`[<-`, ob_angle) <- function(x, y, value) {
  d <- c(x)
  d[y] <- c(value)
  S7::S7_data(x) <-  d
  x
}

# unbind ----
method(unbind, ob_angle) <- function(x, ...) {
  purrr::map(seq(length(c(x))), \(i) x[i])
}


