ln_styles <- c("alpha", "color", "lineend", "linejoin", "linewidth", "linetype")
vline_aesthetics_list <- class_aesthetics_list(
  geom = ggplot2::geom_vline,
  required_aes = "xintercept",
  omit_names = c("slope", "intercept", "a", "b", "c", "group", "id"),
  mappable_bare = character(0),
  mappable_identity = c("color", "linewidth", "linetype", "alpha"),
  not_mappable = character(0),
  style = ln_styles
)

abline_aesthetics_list <- class_aesthetics_list(
  geom = ggplot2::geom_abline,
  mappable_bare = character(0),
  mappable_identity = c("color", "linewidth", "linetype", "alpha"),
  not_mappable = character(0),
  required_aes = c("intercept", "slope"),
  omit_names = c("xintercept", "a", "b", "c", "group"),
  style = ln_styles
)

abline_aesthetics <-
ln_props <- list(
  # primary ----
  primary = list(
    # ax + by + c = 0
    a = S7::new_property(class = S7::class_numeric, default = 0),
    b = S7::new_property(class = S7::class_numeric, default = 0),
    c = S7::new_property(class = S7::class_numeric, default = 0)
  ),
  styles = ob_style@properties[ln_styles],
  # derived ----
  derived = list(
    slope = S7::new_property(
      getter = function(self) {
        -self@a / self@b
      }
    ),
    intercept = S7::new_property(
      getter = function(self) {
        -self@c / self@b
      }
    ),
    xintercept = S7::new_property(
      getter = function(self) {
        -self@c / self@a
      }
    ),
    angle = S7::new_property(
      getter = function(self) {
        degree(radian(atan(self@slope)))
      }
    ),
    length = S7::new_property(
      getter = function(self) {
        length(self@a)
      }
    ),
    style = S7::new_property(
      getter = function(self) {
        pr <- `names<-`(purrr::map(ln_styles,
                                   prop, object = self), ln_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% ln_styles]
        self <- rlang::inject(S7::set_props(self, !!!s_list))
        self
      }),
    tibble = S7::new_property(getter = function(self) {
      d <- list(
        slope = self@slope,
        intercept = self@intercept,
        xintercept = self@xintercept,
        a = self@a,
        b = self@b,
        c = self@c,
        alpha = self@alpha,
        color = c(self@color),
        lineend = self@lineend,
        linejoin = self@linejoin,
        linewidth = self@linewidth,
        linetype = self@linetype,
        id = self@id
      )
      get_non_empty_tibble(d)
    })
  ),
  # functions ----
  funs = list(
    equation = S7::new_property(S7::class_function, getter = function(self) {
      \(type = c("y", "general", "parametric"), output = c("markdown", "latex"), digits = 2) {
        type <- match.arg(type)
        output <- match.arg(output)
        equation(self, type = type, output = output, digits = digits)
      }
    }),
    geom = S7::new_property(S7::class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    place = pr_place,
    point_at_x = S7::new_property(S7::class_function, getter = function(self) {
      \(x = 0, ...) {
        if (any(self@b == 0)) stop("Not possible with verical lines")
        ob_point(x = x, y = x * self@slope + self@intercept, style = self@style, ...)
      }
    }),
    point_at_y = S7::new_property(S7::class_function, getter = function(self) {
      \(y = 0, ...) {
        if (any(self@a == 0)) stop("Not possible with horizontal lines")
        ob_point(x = -1 * y * self@b / self@a - self@c / self@a, y = y, style = self@style, ...)
      }
    }),
    projection = S7::new_property(S7::class_function, getter = function(self) {
      \(ob_point, ...) {
        projection(ob_point, self, ...)
      }
    })
),
# information ----
info = list(
  aesthetics = S7::new_property(getter = function(self) {
    abline_aesthetics_list
  }))

)



# ob_line----

#' ob_line class
#'
#' Creates a line
#'
#' @param a coefficient in general form: a * x + b * y + c = 0
#' @param b coefficient in general form: a * x + b * y + c = 0
#' @param c constant in general form: a * x + b * y + c = 0
#' @param slope coefficient in y = slope * x + intercept
#' @param intercept value of y when x is 0
#' @param xintercept value of x when y is 0
#' @param style an [ob_style] object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @inheritParams ob_style
#' @export
#' @returns ob_line object
ob_line <- S7::new_class(
  "ob_line",
  parent = shape,
  properties = rlang::list2(
    !!!ln_props$primary,
    !!!ln_props$styles,
    !!!ln_props$derived,
    !!!ln_props$funs,
    !!!ln_props$info),
  constructor = function(slope = numeric(0),
                         intercept = numeric(0),
                         xintercept = numeric(0),
                         a = numeric(0),
                         b = numeric(0),
                         c = numeric(0),
                         alpha = numeric(0),
                         color = character(0),
                         lineend = numeric(0),
                         linejoin = numeric(0),
                         linewidth = numeric(0),
                         linetype = numeric(0),
                         style = S7::class_missing,
                         id = character(0),
                         ...) {

    id <- as.character(id)

    l_style <- style + ob_style(
      alpha = alpha,
      color = c(color),
      lineend = lineend,
      linejoin = linejoin,
      linewidth = linewidth,
      linetype = linetype,
      id = id
    ) + ob_style(...)

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
      check_ab0 <- d |>
        dplyr::mutate(ab0 = (a == 0) & (b == 0) & (c != 0)) |>
        dplyr::pull(.data$ab0) |>
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
      d <- tibble::tibble(a = 0, b = 1, c = 0)
    }
    non_empty_list <- get_non_empty_props(l_style)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }
    S7::new_object(S7::S7_object(),
                      a = d$a,
                      b = d$b,
                      c = d$c,
                      alpha = d[["alpha"]] %||% alpha,
                      color = d[["color"]] %||% color ,
                      lineend = d[["lineend"]]  %||% lineend,
                      linejoin = d[["linejoin"]] %||% linejoin,
                      linewidth = d[["linewidth"]] %||% linewidth,
                      linetype = d[["linetype"]] %||% linetype,
                   id = d[["id"]] %||% id)

  }
)


S7::method(str, ob_line) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("slope","intercept", "xintercept"))) {
str_properties(object,
               omit = omit,
               nest.lev = nest.lev,
               additional = FALSE)
}

S7::method(get_tibble, ob_line) <- function(x) {
  x@tibble |>
    dplyr::mutate(geom = ifelse(is.infinite(slope), "v", "ab"))
}

S7::method(get_tibble_defaults, ob_line) <- function(x) {
  sp <- ob_style(
    alpha = 1,
    color = "black",
    lineend = "black",
    linejoin = 16,
    linewidth = 0.5,
    linetype = 0.5
  )
  d <- get_tibble(x)
  for (n in setdiff(colnames(d), c("slope", "intercept", "xintercept", "geom", "a", "b", "c"))) {
    d[is.na(dplyr::pull(d, n)), n] <- S7::prop(sp, n)
  }
  d
}

# v_line_helper <- function(d, ...) {
#
#
#   make_geom_helper(
#     d = d,
#     user_overrides = get_non_empty_props(ob_style(...)),
#     aesthetics = vline_aesthetics
#   )
# }
# ab_line_helper <- function(d, ...) {
#   make_geom_helper(
#     d = d,
#     aesthetics = abline_aesthetics,
#     user_overrides = get_non_empty_props(ob_style(...))
#   )
# }

S7::method(as.geom, ob_line) <- function(x, ...) {
  overrides <- get_non_empty_props(ob_style(...))
get_tibble_defaults(x) |>
  tidyr::nest(.by = geom, .key = "d") |>
  dplyr::mutate(a = ifelse(geom == "ab", c(abline_aesthetics_list), c(vline_aesthetics_list)),
                output = purrr::map2(d,a,
                                     make_geom_helper,
                                     user_overrides = overrides)) |>
  dplyr::pull(output)
}

S7::method(equation, ob_line) <- function(
    x,
    type = c("y", "general", "parametric"),
    output = c("markdown", "latex"),
    digits = 2) {

  type <- match.arg(type)
  output <- match.arg(output)
  myrounder <- redefault(rounder, digits = digits, output = output)
  myemphasis <- redefault(emphasis, output = output)

  eq <- rep("", x@length)

  if (type == "y") {
    eq[x@b == 0] <- trimmer(paste0(
      myemphasis("x"),
      " = ",
      myrounder(x@xintercept[x@b == 0])
    ))

    eq[x@a == 0 & x@b != 0] <- trimmer(paste0(
      myemphasis("y"),
      " = ",
      myrounder(x@intercept[x@a == 0 & x@b != 0])
    ))

    eq[x@a != 0 & x@b != 0] <- trimmer(paste0(
      myemphasis("y"),
      " = ",
      myrounder(
        x@slope[x@a != 0 & x@b != 0],
        add = FALSE
      ),
      myemphasis("x"),
      myrounder(
        x@intercept[x@a != 0 & x@b != 0],
        add = TRUE)
    ))

  } else if (type == "general") {
    eq <- trimmer(paste0(
      myrounder(x@a),
      myemphasis("x"),
      myrounder(x@b, add = TRUE),
      myemphasis("y"),
      myrounder(x@c, add = TRUE),
      " = 0"
    ))

  } else if (type == "parametric") {
    eq <- trimmer(paste0(
      emphasis("y", output),
      " = ",
      rounder(x@slope, digits = digits, output = output),
      myemphasis("t"),
      rounder(x@intercept, digits = digits, add = TRUE, output = output),
      ifelse(output == "markdown", "<br>", "\\\\ "),
      myemphasis("x"),
      " = ",
      myemphasis("t")
    ))
    eq[x@b == 0] <- trimmer(
      paste0(
        myemphasis("y"),
        " = ",
        myemphasis("t"),
        ifelse(output == "markdown", "<br>", "\\\\ "),
        myemphasis("x"),
        " = ",
        rounder(x@xintercept, digits = digits, output = output)
      )
    )
  }
  eq
}



S7::method(projection, list(ob_point, ob_line)) <- function(p,object, ...) {
ab <- object@a * object@a + object@b * object@b
xp <- (object@b * object@b * p@x - object@b * object@a * p@y - object@a * object@c) / ab
yp <- (object@a * object@a * p@y - object@a * object@b * p@x - object@b * object@c) / ab
ob_point(xp, yp, style = p@style, ...)
}

S7::method(`[`, ob_line) <- function(x, i) {
  i <- character_index(i, x@id)
  x@tibble[i, ] |>
    dplyr::select(-c(slope, intercept, xintercept)) |>
    data2shape(ob_line)
}

S7::method(`==`, list(ob_line, ob_line)) <- function(e1, e2) {
  (e1@a == e2@a) & (e1@b == e2@b) & (e1@c == e2@c)
}

