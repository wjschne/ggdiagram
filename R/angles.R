num2turn <- function(x, object_name) {
  e2_class <- match.arg(arg = object_name,
                        choices = c("degree", "radian", "turn", "ob_angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, turn = 1, ob_angle = 2 * pi)
  x / denominator
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
#' @returns ob_angle
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
  package = "ggdiagram",
  parent = class_double,
  properties = list(
    degree = new_property(
      class_numeric,
      getter = function(self) {
        (c(self) %% ifelse(c(self) < 0, -1, 1)) * 360
      },
      setter = function(self, value) {
        S7_data(self, TRUE) <- value / 360
        self
      }
    ),
    radian = new_property(
      class_numeric,
      getter = function(self) {
        (c(self) %% ifelse(c(self) < 0, -1, 1)) * (2 * pi)
      },
      setter = function(self, value) {
        S7_data(self, TRUE) <- value / (2 * pi)
        self
      }
    ),
    turn = new_property(
      class_numeric,
      getter = function(self) {
        (c(self) %% ifelse(c(self) < 0, -1, 1))
      },
      setter = function(self, value) {
        S7_data(self, TRUE) <- value
        self
      }
    ),
    positive = new_property(getter = \(self) {
      trn <- rep(0, length(c(self)))
      trn[self@turn < 0] <- floor(c(self))[self@turn < 0]
      turn(abs(trn)) + self
    }),
    negative = new_property(getter = \(self) {
      trn <- rep(0, length(c(self)))
      trn[self@turn > 0] <- floor(c(self))[self@turn > 0] + 1
      turn(-abs(trn)) + self
    })
    ), constructor = function(
    .data = numeric(0),
    degree = numeric(0),
    radian = numeric(0),
    turn = numeric(0)) {
      if (is.character(.data)) {
        .data <- cardinalpoint(.data) / 360
      }

      if (length(.data) == 0) {
        if (length(degree) > 0) {
          .data = degree / 360
        } else if (length(radian) > 0) {
          .data = 0.5 * radian / pi
        } else if (length(turn) > 0) {
          .data = turn
        }
      }
      new_object(.data)
    })


ob_angle_or_numeric <- S7::new_union(S7::class_numeric, ob_angle)
ob_angle_or_character <- S7::new_union(S7::class_character, ob_angle)





# Angle wrappers ----
# degree ----
#' degree class
#'
#' @rdname ob_angle
#' @export
degree <- S7::new_class(
  name = "degree",
  package = "ggdiagram",
  parent = ob_angle,
  constructor = function(degree = numeric(0)) {
    if (is.character(degree)) degree <- cardinalpoint(degree)
    if (S7::S7_inherits(degree, ob_angle)) degree <- c(degree) * 360
    S7::new_object(degree / 360)
  })

# radian ----
#' radian class
#'
#' @rdname ob_angle
#' @export
radian <- S7::new_class(
  name = "radian",
  package = "ggdiagram",
  parent = ob_angle,
  constructor = function(radian = numeric(0)) {
    if (is.character(radian)) radian <- cardinalpoint(radian) * pi / 180
    if (S7::S7_inherits(radian, ob_angle)) radian <- c(radian) * 2 * pi
    S7::new_object(radian / (2 * pi))
  })

# turn ----
#' turn class
#'
#' @rdname ob_angle
#' @export
turn <- S7::new_class(
  name = "turn",
  package = "ggdiagram",
  parent = ob_angle,
  constructor = function(turn = numeric(0)) {
    if (is.character(turn)) turn <- cardinalpoint(turn) / 360
    if (S7::S7_inherits(turn, ob_angle)) turn <- c(turn)
    S7::new_object(turn)
  })

# arithmetic ----
purrr::walk(list(`+`, `-`, `*`, `/`, `^`), \(.f) {
  S7::method(.f, list(ob_angle, ob_angle)) <- function(e1, e2) {
    d <- .f(c(e1), c(e2))
    S7::S7_data(e2) <- d
    e2
  }

  S7::method(.f, list(ob_angle, S7::class_numeric)) <- function(e1, e2) {
    d <- num2turn(
      .f(
        S7::prop(
          e1,
          S7::S7_class(e1)@name),
        e2),
      S7::S7_class(e1)@name)

    S7::S7_data(e1) <- d
    e1
  }

  S7::method(.f, list(S7::class_numeric, ob_angle)) <- function(e1, e2) {
    d <- num2turn(
      .f(
        e1,
        S7::prop(
          e2, S7::S7_class(e2)@name)),
      S7::S7_class(e2)@name)

    S7::S7_data(e2) <- d
    e2
  }
})

# equality ----
S7::method(`==`, list(ob_angle, ob_angle)) <- function(e1, e2) { # nocov start
  abs(c(e1) - c(e2)) <= .Machine$double.eps
} # nocov end

S7::method(`==`, list(ob_angle, S7::class_numeric)) <- function(e1, e2) { # nocov start
  abs(c(e1) - num2turn(e2, e1)) <= .Machine$double.eps
} # nocov end

S7::method(`==`, list(S7::class_numeric, ob_angle)) <- function(e1, e2) { # nocov start
  abs(num2turn(e1, e2) - c(e2)) <= .Machine$double.eps
} # nocov end


S7::method(`!=`, list(ob_angle, ob_angle)) <- function(e1, e2) { # nocov start
  abs(c(e1) - c(e2)) > .Machine$double.eps
} # nocov end

purrr::walk(list(`<`, `<=`, `>`, `>=`), \(.f) { # nocov start
  S7::method(.f, list(ob_angle, ob_angle)) <- function(e1, e2) {
    .f(c(e1), c(e2))
  }
  S7::method(.f, list(ob_angle, S7::class_numeric)) <- function(e1, e2) {
    .f(c(e1), num2turn(e2, e1))
  }
  S7::method(.f, list(S7::class_numeric, ob_angle)) <- function(e1, e2) {
    .f(num2turn(e1, e2), c(e2))
  }
} # nocov end
)

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
# S7::method(convert, list(S7::class_numeric, degree)) <- function(from, to) {
#   degree(from * 360)
# }
# S7::method(convert, list(S7::class_numeric, turn)) <- function(from, to) {
#   turn(from)
# }
# #
# S7::method(convert, list(S7::class_numeric, radian)) <- function(from, to) {
#   radian(from * 2 * pi)
# }

S7::method(str, ob_angle) <- function(object,
                                     nest.lev = 0,
                                     additional = FALSE,
                                     omit = c(".data", "positive", "negative")) {
  print(c(object))
  invisible(object)
}

S7::method(print, degree) <- function(x, ...) {
  print(as.character(x))
  # invisible(object)
}

S7::method(str, radian) <- function(object,
                                nest.lev = 0,
                                additional = FALSE,
                                omit = c(".data", "turn", "degree", "positive", "negative")) {
  print(as.character(object))
  invisible(object)
}

S7::method(str, turn) <- function(object,
                                nest.lev = 0,
                                additional = FALSE,
                                omit = c(".data", "degree", "radian", "positive", "negative")) {
  print(as.character(object))
  invisible(object)
}

# as.character ----

S7::method(as.character, ob_angle) <- function(x,
  ...,
  digits = NULL,
  type = NULL) {

  if (!is.null(type)) {
    if (type == "degree" || type == "ggdiagram::degree") x <- degree(x)
    if (type == "radian" || type == "ggdiagram::radian") x <- radian(x)
    if (type == "turn" || type == "ggdiagram::turn") x <- turn(x)
  }




  if (inherits(x, "ggdiagram::radian")) {
    if (is.null(digits)) digits <- 2
    x <- paste0(ifelse(
      abs(x@radian - pi) < .Machine$double.eps * 2,
      "",
      ifelse(
        abs(x@radian + pi) < .Machine$double.eps * 2,
        "\u2212",
        signs::signs(
          x@turn * 2,
          accuracy = 10 ^ (-1 * digits),
          drop0trailing = TRUE)
      )
                  ),
      "\u03C0")
  }

  if (inherits(x, "ggdiagram::turn")) {
    if (is.null(digits)) digits <- 2
    x <- round_probability(x@turn, accuracy = 10 ^ (-1 * digits))
}

  if (inherits(x, "ggdiagram::ob_angle") || inherits(x, "ggdiagram::degree")) {
    if (is.null(digits)) digits <- 0
    x <- paste0(signs::signs(x@degree, accuracy = 10 ^ (-1 * digits), drop0trailing = TRUE), "\u00B0")
  }

  x

}

# subset ----
S7::method(`[`, ob_angle) <- function(x, i) {
  S7::S7_data(x) <-  c(x)[i]
  x
}

# unbind ----
S7::method(unbind, ob_angle) <- function(x) {
  purrr::map(seq(length(c(x))), \(i) x[i])
}

#' @export
`[<-.ggdiagram::ob_angle` <- function(x, i, value) {
  d <- c(x)
  d[i] <- c(value)
  S7::S7_data(x) <-  d
  x
}
