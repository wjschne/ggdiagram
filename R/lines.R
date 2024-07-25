ln_styles <- c("alpha", "color", "lineend", "linejoin", "linewidth", "linetype")
ln_props <- list(
  primary = list(
    # ax + by + c = 0
    a = new_property(class = class_numeric, default = 0),
    b = new_property(class = class_numeric, default = 0),
    c = new_property(class = class_numeric, default = 0)
  ),
  styles = style@properties[ln_styles],
  derived = list(
    slope = new_property(
      getter = function(self) {
        -self@a / self@b
      }
    ),
    intercept = new_property(
      getter = function(self) {
        -self@c / self@b
      }
    ),
    xintercept = new_property(
      getter = function(self) {
        -self@c / self@a
      }
    ),
    angle = new_property(
      getter = function(self) {
        degree(radian(atan(self@slope)))
      }
    ),
    equation = new_property(getter = function(self) {
      equation(self)
    }),
    length = new_property(
      getter = function(self) {
        length(self@a)
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- `names<-`(purrr::map(ln_styles,
                                   prop, object = self), ln_styles)
        rlang::inject(style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        line(a = self@a,
             b = self@b,
             c = self@c ,
             style = self@style + value)
      }),
    tibble = new_property(getter = function(self) {
      d <- list(
        slope = self@slope,
        intercept = self@intercept,
        xintercept = self@xintercept,
        a = self@a,
        b = self@b,
        c = self@c,
        alpha = self@alpha,
        color = self@color,
        lineend = self@lineend,
        linejoin = self@linejoin,
        linewidth = self@linewidth,
        linetype = self@linetype
      )
      get_non_empty_tibble(d)
    })
  ),
  funs = list(
    point_at_x = new_property(class_function, getter = function(self) {
      \(x = 0, ...) {
        if (any(self@b == 0)) stop("Not possible with verical lines")
        point(x = x, y = x * self@slope + self@intercept, style = self@style, ...)
      }
    }),
    point_at_y = new_property(class_function, getter = function(self) {
      \(y = 0, ...) {
        if (any(self@a == 0)) stop("Not possible with horizontal lines")
        point(x = -1 * y * self@b / self@a - self@c / self@a, y = y, style = self@style, ...)
      }
    }),
    projection = new_property(class_function, getter = function(self) {
      \(point, ...) {
        projection(point, self, ...)
      }
    })
)

)



# Line----

#' line class
#'
#' @param a coefficient in general form: a * x + b * y + c = 0
#' @param b coefficient in general form: a * x + b * y + c = 0
#' @param a constant in general form: a * x + b * y + c = 0
#' @param slope coefficient in y = slope * x + intercept
#' @param intercept value of y when x is 0
#' @param xintercept value of x when y is 0
#' @param style a style list
#' @param ... properties passed to style
#' @export
line <- new_class(
  "line",
  parent = shape,
  properties =  rlang::inject(list(
    !!!ln_props$primary,
    !!!ln_props$styles,
    !!!ln_props$derived,
    !!!ln_props$funs)),
  constructor = function(slope = class_missing,
                         intercept = class_missing,
                         xintercept = class_missing,
                         a = class_missing,
                         b = class_missing,
                         c = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         lineend = class_missing,
                         linejoin = class_missing,
                         linewidth = class_missing,
                         linetype = class_missing,
                         style = class_missing,
                         ...) {

    l_style <- style + style(
      alpha = alpha,
      color = color,
      lineend = lineend,
      linejoin = linejoin,
      linewidth = linewidth,
      linetype = linetype
    ) + style(...)

    d <- get_non_empty_tibble(list(
      slope = slope,
      intercept = intercept,
      xintercept = xintercept,
      a = a,
      b = b,
      c = c
    ))

    d_names <- colnames(d)

    if (all(c("a", "b", "c") %in% d_names)) {
      check_ab0 <- d %>%
        dplyr::mutate(ab0 = (a == 0) & (b == 0) & (c != 0)) %>%
        dplyr::pull(ab0) %>%
        any()
      if (check_ab0) stop("If a and b are 0, c must be 0.")
      

      
      if (length(slope) > 0 && all(slope != -b / a)) stop("Some slopes are inconsistent with a and b parameters.")
      if (length(intercept) > 0 && all(intercept != -c / a)) stop("Some intercepts are inconsistent with b and c parameters.")

      # Prevent a and b from being both negative
      ab_both_negative = -2 * ((d$a <= 0) & (d$b <= 0)) + 1
      d$a <- d$a * ab_both_negative
      d$b <- d$b * ab_both_negative
      d$c <- d$c * ab_both_negative


    } else if ("intercept" %in% d_names) {
      if (!("slope" %in% d_names)) {
        d$slope <- 0
      }

      if (any(is.infinite(d$slope)) || any(is.infinite(d$intercept))) {
        stop("There is not enough information to make a line. Specify the x-intercept or the a,b,c parameters.")
      }

      d$a <- d$slope * -1
      d$b <- 1
      d$c <- d$intercept * -1

    } else if (length(xintercept) == 1) {

      d$a <- 1
      d$b <- 0
      d$c <- d$xintercept * -1

    } else {
      stop("There is insufficient information to create a line. Specify a slope and intercept OR an xintercept OR a, b, and c.")
    }
    non_empty_list <- get_non_empty_props(l_style)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }
    new_object(S7_object(),
                      a = d$a,
                      b = d$b,
                      c = d$c,
                      alpha = d[["alpha"]] %||% alpha,
                      color = d[["color"]] %||% color ,
                      lineend = d[["lineend"]]  %||% lineend,
                      linejoin = d[["linejoin"]] %||% linejoin,
                      linewidth = d[["linewidth"]] %||% linewidth,
                      linetype = d[["linetype"]] %||% linetype)

  }
)


method(str, line) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("slope","intercept", "xintercept"))) {
str_properties(object,
               omit = omit,
               nest.lev = nest.lev,
               additional = FALSE)
}

method(get_tibble, line) <- function(x) {
  x@tibble %>%
    dplyr::mutate(geom = ifelse(is.infinite(slope), "v", "ab"))
}

method(get_tibble_defaults, line) <- function(x) {
  sp <- style(
    alpha = 1,
    color = "black",
    lineend = "black",
    linejoin = 16,
    linewidth = 1.5,
    linetype = 0.5
  )
  d <- get_tibble(x)
  for (n in setdiff(colnames(d), c("slope", "intercept", "xintercept", "geom", "a", "b", "c"))) {
    d[is.na(dplyr::pull(d, n)), n] <- prop(sp, n)
  }
  d
}



v_line_helper <- function(d, ...) {
  f_geom <- ggplot2::geom_vline
  req <- "xintercept"
  omit <- c("slope", "intercept", "a", "b", "c")
  make_geom_helper(
    d = d,
    .geom_x = f_geom,
    user_overrides = get_non_empty_props(style(...)),
    mappable_bare = character(0),
    mappable_identity = c("color", "linewidth", "linetype", "alpha"),
    not_mappable = character(0),
    required_aes = req,
    omit_names = c(omit,"group")
  )
}
ab_line_helper <- function(d, ...) {
  f_geom <- ggplot2::geom_abline
  req <- c("intercept", "slope")
  omit <- c("xintercept", "a", "b", "c")
  make_geom_helper(
    d = d,
    .geom_x = f_geom,
    user_overrides = get_non_empty_props(style(...)),
    mappable_bare = "",
    not_mappable = "",
    required_aes = req,
    omit_names = c(omit,"group")
  )
}

method(as.geom, line) <- function(x, ...) {
get_tibble_defaults(x) %>%
  tidyr::nest(.by = geom, .key = "d") %>%
  dplyr::mutate(.f = ifelse(geom == "ab", c(ab_line_helper), c(v_line_helper)),
                output = purrr::map(d, .f)) %>%
  dplyr::pull(output)
}

method(equation, line) <- function(
    x,
    type = c("y", "general", "parametric"),
    digits = 2) {

  type <- match.arg(type)

  eq <- rep("", x@length)

  if (type == "y") {
    eq[x@b == 0] <- trimmer(
      paste0("*x* = ",
             rounder(x@xintercept, digits = digits)))

    eq[x@a == 0 & x@b != 0] <- trimmer(
      paste0("*y* = ",
             rounder(x@intercept, digits = digits)))

    eq[x@a != 0 & x@b != 0] <- trimmer(
      paste0("*y* = ",
             rounder(x@slope, digits = digits, add = FALSE),
             "*x*",
             rounder(x@intercept, digits = digits, add = TRUE)
    ))

  } else if (type == "general") {
    eq <- trimmer(paste0(
      rounder(x@a, digits = digits),
      "*x*",
      rounder(x@b, digits = digits, add = TRUE),
      "*y*",
      rounder(x@c, digits = digits, add = TRUE),
      " = 0"
    ))

  } else if (type == "parametric") {
    eq <- trimmer(paste0(
      "*y* = ",
      rounder(x@slope, digits = digits),
      "*t*",
      rounder(x@intercept, digits = digits, add = TRUE),
      "<br>*x* = *t*"
    ))
  }
  eq
}



method(projection, list(point, line)) <- function(point,object, ...) {
ab <- object@a * object@a + object@b * object@b
xp <- (object@b * object@b * point@x - object@b * object@a * point@y - object@a * object@c) / ab
yp <- (object@a * object@a * point@y - object@a * object@b * point@x - object@b * object@c) / ab
point(xp, yp, style = point@style, ...)
}
