cr_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype",
  "n",
  "id"
)

compass_props <- list(
  east = S7::new_property(getter = function(self) self@point_at("east")),
  northeast = S7::new_property(getter = function(self) self@point_at("northeast")),
  north = S7::new_property(getter = function(self) self@point_at("north")),
  northwest = S7::new_property(getter = function(self) self@point_at("northwest")),
  west = S7::new_property(getter = function(self) self@point_at("west")),
  southwest = S7::new_property(getter = function(self) self@point_at("southwest")),
  south = S7::new_property(getter = function(self) self@point_at("south")),
  southeast = S7::new_property(getter = function(self) self@point_at("southeast"))
)

cr_props <- list(
  # primary ----
  primary = list(
    radius = S7::new_property(class = S7::class_numeric, default = 1)
  ),
  styles = ob_style@properties[cr_styles],
  # derived ----
  derived = list(
    area = S7::new_property(getter = function(self) {
      pi * self@radius ^ 2
    }),
    bounding_box = S7::new_property(getter = function(self) {
      ob_rectangle(
          southwest = ob_point(x = min(self@center@x - self@radius),
                            y = min(self@center@y - self@radius)),
          northeast = ob_point(x = max(self@center@x + self@radius),
                            y = max(self@center@y + self@radius)))
    }),
    circumference = S7::new_property(getter = function(self) {
      pi * self@radius * 2
    }),
    diameter = S7::new_property(getter = function(self) {
      self@radius * 2
    }),
    length = S7::new_property(
      getter = function(self) {
        nrow(self@tibble)
      }
    ),
    polygon = S7::new_property(getter = function(self) {
      d <- self@tibble
      if (!("n" %in% colnames(d))) {
        d$n <- 360
      }
      d |>
        dplyr::mutate(group = factor(dplyr::row_number())) |>
        tidyr::uncount(.data$n, .remove = FALSE) |>
        dplyr::mutate(theta = 2 * pi * (dplyr::row_number() - 1) / n,
                      x = x + r * cos(theta),
                      y = y + r * sin(theta),
                      .by = group)
    }),
    style = S7::new_property(
      getter = function(self) {
        pr <- purrr::map(cr_styles,
                         prop, object = self) |>
          `names<-`(cr_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% cr_styles]
        self <- rlang::inject(S7::set_props(self, !!!s_list))
        self
      }
    ),
    tibble = S7::new_property(getter = function(self) {
      d <- list(
        x = self@center@x,
        y = self@center@y,
        r = self@radius,
        alpha = self@alpha,
        color = self@color,
        fill = self@fill,
        linewidth = self@linewidth,
        linetype = self@linetype,
        n = self@n,
        id = self@id
        )
      get_non_empty_tibble(d)
    })
  ),
  # functions ----
  funs = list(
    geom = S7::new_property(S7::class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    arc = S7::new_property(S7::class_function, getter = \(self) {
      \(start, end, type = "arc", ...) {
        dots <- rlang::list2(...)
        if (is.null(dots$radius)) {
          dots$radius <- self@radius
          }

        rlang::inject(ob_arc(self@center, start = start, end = end, type = type, style = self@style, !!!dots))
      }
    }),
    angle_at = S7::new_property(S7::class_function, getter = function(self) {
      \(point) {
        if (S7::S7_inherits(point, ob_point)) {
          dp <- point - self@center
          dp@theta
        } else {
          return(NULL)
        }
      }
    }),
    normal_at = S7::new_property(S7::class_function, getter = function(self) {
      \(theta = degree(0), distance = 1, ...) {
        if (S7::S7_inherits(theta, ob_point)) theta <- projection(theta, self)@theta
        if (!S7::S7_inherits(theta, ob_angle)) theta <- degree(theta)
        self@center + ob_polar(theta, self@radius + distance, ...)
      }
    }),
    tangent_at = S7::new_property(
      class = S7::class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (!S7::S7_inherits(theta, ob_angle)) theta <- degree(theta)
          x <- self@center@x
          y <- self@center@y
          x1 <- cos(theta) * self@radius + self@center@x
          y1 <- sin(theta) * self@radius + self@center@y
          ob_line(
            a = x1 - x,
            b = y1 - y,
            c = x ^ 2 - (x1 * x) + y ^ 2 - (y1 * y) - self@radius ^ 2,
            style = self@style,
            ...
          )
        }
      }
    ),
    place = pr_place,
    point_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (!S7::S7_inherits(theta, ob_angle)) {
            theta <- degree(theta)
            }
          self@center + ob_polar(theta = theta, r = self@radius, style = self@style, ...)
          }
      }
    )),
  # info ----
  info = list(
  aesthetics = S7::new_property(getter = function(self) {
    class_aesthetics_list(
      geom = ggforce::geom_circle,
      mappable_bare = character(0),
      mappable_identity = c(
        "linewidth",
        "linetype",
        "alpha",
        "color",
        "fill"
      ),
      not_mappable = c("n"),
      required_aes = c("x0", "y0", "r", "group"),
      omit_names = c("linejoin", "rule", "id"),
      inherit.aes = FALSE,
      style = cr_styles
    )}
  )))

# Circle----

#' ob_circle class
#' @param center point at center of the circle
#' @param radius distance between center and edge circle
#' @param label A character, angle, or label object
#' @param x x-coordinate of center point. If specified, overrides x-coordinate of `@center`.
#' @param y x-coordinate of center point. If specified, overrides y-coordinate of `@center`.
#' @param n number of points in circle (default = 360)
#' @param style an ob_style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style object
#' @inherit ob_style params
#' @slot aesthetics A list of information about the circle's aesthetic properties
#' @slot angle_at A function that finds the angle of the specified point in relation to the circle's center
#' @slot area area of the circle
#' @slot bounding_box a rectangle that contains all the circles
#' @slot circumference circumference of the circle
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to `ggforce::geom_circle`.
#' @slot length The number of circles in the circle object
#' @slot normal_at A function that finds a point that is perpendicular from the circle and at a specified distance
#' @slot point_at A function that finds a point on the circle at the specified angle.
#' @slot polygon a tibble containing information to create all the polygon points in a circle.
#' @slot tangent_at A function that finds the tangent line at the specified angle.
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggforce::geom_cirlce`.
#' @export
#' @returns ob_circle object
#' @examples
#' # specify center point and radius
#' ob_circle(center = ob_point(0,0), radius = 6)
ob_circle <- S7::new_class(
  name = "ob_circle",
  parent = centerpoint,
  properties = rlang::list2(
    !!!cr_props$primary,
    !!!cr_props$styles,
    !!!cr_props$derived,
    !!!compass_props,
    !!!cr_props$funs,
    !!!cr_props$info),
  constructor = function(center = ob_point(0,0),
                         radius = 1,
                         label = character(0),
                         alpha = numeric(0),
                         color = character(0),
                         fill = character(0),
                         linewidth = numeric(0),
                         linetype = numeric(0),
                         n = numeric(0),
                         style = S7::class_missing,
                         x = numeric(0),
                         y = numeric(0),
                         id = character(0),
                         ...) {
    id <- as.character(id)
    c_style <- style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype,
        n = n
      ) +
      ob_style(...)

    if ((length(x) > 0) || (length(y) > 0)) {
      if (length(x) == 0) {
        x <- 0
      }
      if (length(y) == 0) {
        y <- 0
      }
      center <- ob_point(tibble::tibble(x = x, y = y))
    }

    non_empty_list <- get_non_empty_props(c_style)
    d <- tibble::tibble(x = center@x, y = center@y, radius = radius)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }


    center = set_props(center, x = d$x, y = d$y)

    if (S7::S7_inherits(label, ob_label)) {
      if (all(label@center == ob_point(0,0))) {
        label@center <- center
      }
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_circle")

    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }

     S7::new_object(centerpoint(center = center,
                                label = label),
                 radius = d$radius,
                 alpha = d[["alpha"]] %||% alpha,
                 color = d[["color"]] %||% color ,
                 fill = d[["fill"]]  %||% fill,
                 linewidth = d[["linewidth"]]  %||% linewidth,
                 linetype = d[["linetype"]]  %||% linetype,
                 n = d[["n"]]  %||% n,
                 id = d[["id"]] %||% id)
  }
)


S7::method(str, ob_circle) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","radius"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

S7::method(get_tibble, ob_circle) <- function(x) {
  x@tibble
}


S7::method(get_tibble_defaults, ob_circle) <- function(x) {
  sp <- ob_style(
    alpha = replace_na(as.double(ggforce::GeomCircle$default_aes$alpha), 1),
    color = replace_na(ggforce::GeomCircle$default_aes$colour, "black"),
    fill = replace_na(ggforce::GeomCircle$default_aes$fill, "black"),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggforce::GeomCircle$default_aes$linewidth, 0.5),
    linetype = replace_na(ggforce::GeomCircle$default_aes$default_aes$linetype, 1),
    n = 360
  )
  get_tibble_defaults_helper(x, sp,required_aes = c("x0", "y0", "r", "n"))
}

S7::method(`[`, ob_circle) <- function(x, i) {
  i <- character_index(i, x@id)
  z <- data2shape(x@tibble[i,], ob_circle)
  z@label <- na2zero(x@label[i])
  z
}

S7::method(`==`, list(ob_circle, ob_circle)) <- function(e1, e2) { # nocov start
  (e1@center == e2@center) & (e1@radius == e1@radius)
} # nocov end

# Place ----

S7::method(place, list(ob_line, ob_circle)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  from@radius <- sep + from@radius
  from@tangent_at(where)
}

S7::method(ob_array, ob_circle) <- function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {

  sa <- ob_array_helper(x = x, k = k, sep = sep, where = where, anchor = anchor, ...)

  rlang::inject(ob_circle(center = sa$p_center,
                        radius = x@radius,
                        style = x@style,
                        !!!sa$dots))
}

S7::method(equation, ob_circle) <- function(
    x,
    type = c("y", "general", "parametric"),
    output = c("markdown", "latex"),
    digits = 2) {
  if (length(type) == 3) type <- "general"
  type <- match.arg(type)
  output <- match.arg(output)
  myrounder <- redefault(rounder, digits = digits, output = output)
  myemphasis <- redefault(emphasis, output = output)
  mysuperscript <- redefault(superscript, output = output)
  minus <- ifelse(output == "markdown", "\u2212", "-")
  linebreak <- ifelse(output == "markdown", "<br>", "\\\\ ")
  mysine <- ifelse(output == "markdown", "sin", "\\sin")
  mycosine <- ifelse(output == "markdown", "cos", "\\cos")


  eq <- rep("", x@length)

  if (type == "general" || type == "y") {
    eq <- trimmer(paste0(
      ifelse(x@center@x == 0,
      paste0(mysuperscript(myemphasis("x"), "2")),
      mysuperscript(paste0(
        "(",
        myemphasis("x"),
        " ",
        minus,
        " ",
        myrounder(x@center@x),
        ")"
    ), "2")),
    " + ",
    ifelse(x@center@y == 0,
           paste0(mysuperscript(myemphasis("y"), "2")),
           mysuperscript(paste0(
             "(",
             myemphasis("y"),
             " ",
             minus,
             " ",
             myrounder(x@center@y),
             ")"
           ), "2")),
    " = ",
    mysuperscript(myrounder(x@radius), "2")
    ))
  }

  if (type == "parametric") {
    eq <- paste0(
      ifelse(x@radius == 1, "", myrounder(x@radius)),
      mycosine,
      "(",
      myemphasis("t"),")",
      ifelse(x@center@x == 0,
             "",
             myrounder(x@center@x, add = TRUE)),
      linebreak,
      ifelse(x@radius == 1, "", myrounder(x@radius)),
      mysine,
      "(",
      myemphasis("t"),
      ")",
      ifelse(x@center@y == 0,
             "",
             myrounder(x@center@y, add = TRUE))
    )
  }

eq
}

#' Get a circle from 3 points
#'
#' @param p1 an ob_point of length 1 or length 3
#' @param p2 an ob_point of length 1 or NULL
#' @param p3 an ob_point of length 1 or NULL
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Pass arguments to ob_circle
#' @returns ob_point object
#' @export
#'
#' @examples
#' circle_from_3_points(ob_point(1,1),
#'                      ob_point(2,4),
#'                      ob_point(5,3))
circle_from_3_points <- function(p1, p2 = NULL, p3 = NULL, ...) {
  # from https://math.stackexchange.com/a/1460096

  if (p1@length == 3 && is.null(p2) && is.null(p3)) {
    p <- p1
  } else if (p1@length == 1 && p2@length == 1 && p3@length == 1) {
    p <- bind(c(p1, p2, p3))
  } else {
    stop("p1 must be of length 3 or p1, p2, and p2 must be of length 1.")
  }

  # Minor M11
  m11 <- det(cbind(p@x, p@y, rep(1,3)))
  if (m11 == 0) stop("Points on the same line cannot lie on a circle.")

  # Minor m12
  m12 <- det(cbind(p@x ^ 2 + p@y ^ 2, p@y, rep(1,3)))
  # Minor m13
  m13 <- det(cbind(p@x ^ 2 + p@y ^ 2, p@x, rep(1,3)))

  x <- 0.5 * m12 / m11
  y <- -0.5 * m13 / m11
  center <- ob_point(x,y)
  ob_circle(center, radius = distance(center, p1), ...)
}



