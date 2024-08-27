el_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype",
  "n"
)

el_props <- list(
  # primary ----
  primary = list(
    a = new_property(class = class_numeric, default = 1),
    b = new_property(class = class_numeric, default = 1),
    angle = new_property(class_angle_or_numeric, default = 0),
    m1 = new_property(class = class_numeric, default = 2),
    m2 = new_property(class = class_numeric, default = 2)
  ),
  styles = style@properties[el_styles],
  # derived ----
  derived = list(
    focus_1 = new_property(point, getter = function(self) {
      self@center + point(ifelse(self@a > self@b, -sqrt(self@a ^ 2 - self@b ^ 2), 0),
                          ifelse(self@b > self@a, sqrt(self@b ^ 2 - self@a ^ 2), 0))
    }),
    focus_2 = new_property(point, getter = function(self) {
      self@center + point(ifelse(self@a > self@b, sqrt(self@a ^ 2 - self@b ^ 2), 0),
                          ifelse(self@b > self@a, -sqrt(self@b ^ 2 - self@a ^ 2), 0))
    }),
    length = new_property(
      getter = function(self) {
        length(self@a)
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(el_styles,
                         prop, object = self) |>
          `names<-`(el_styles)
        rlang::inject(style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        point(self@x, self@y, style = self@style + value)
      }
    ),
    tibble = new_property(getter = function(self) {
      d <- list(
        x0 = self@center@x,
        y0 = self@center@y,
        a = self@a,
        b = self@b,
        angle = self@angle@radian,
        m1 = self@m1,
        m2 = self@m2,
        alpha = self@alpha,
        color = self@color,
        fill = self@fill,
        linewidth = self@linewidth,
        linetype = self@linetype,
        n = self@n)
      get_non_empty_tibble(d)
    })
  ),
  # functions ----
  funs = list(
    geom = new_property(class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    normal_at = new_property(class_function, getter = function(self) {
      \(theta = degree(0), distance = 1, definitional = FALSE) {
        if (!S7_inherits(theta, class_angle)) theta <- degree(theta)
          p <- self@point_at(theta, definitional) - self@center
          p1 <- point(self@m1 * ((p@x) / self@a) ^ (self@m1 - 1), self@m2 * (p@y / self@b) ^ (self@m2 - 1))
          self@center + p + (distance * p1 / p1@r) * point(sign(cos(theta)), sign(sin(theta)))
      }
    }),
    point_at = new_property(class_function, getter = function(self) {
      \(theta = degree(0), definitional = FALSE, ...) {
        if (!S7_inherits(theta, class_angle)) theta <- degree(theta)

        if (definitional) {
          self@center + rotate(point(cos(t) * self@a, sin(t) * self@b, ...), self@angle)
        } else {

        rtheta <- theta - radian(self@angle)

        t <- radian(dplyr::if_else(abs(cos(rtheta)) < .Machine$double.eps, rtheta@radian, atan((
          abs(suppressWarnings(tan(rtheta))) * self@a / self@b
        ) ^ (self@m1 / 2))))

        rp <- rotate(point((abs(cos(t)) ^ (2 / self@m1)) *
                             self@a * ((cos(rtheta) >= 0) * 2 - 1),
                           (abs(sin(t)) ^ (2 / self@m1)) *
                             self@b * ((sin(rtheta) >= 0) * 2 - 1), ...),
                     self@angle)


        self@center + rp
        }
      }
    }),
    tangent_at = new_property(class_function, getter = function(self) {
      \(theta = degree(0), definitional = FALSE, ...) {
        if (!S7_inherits(theta, class_angle)) theta <- degree(theta)
        p <- self@point_at(theta, definitional)
        p_normal <- self@normal_at(theta, definitional)
        l <- segment(p, rotate(p_normal, degree(90), origin = p))@line
        s <- rlang::list2(...)
        rlang::inject(set_props(l, !!!s))
      }
    })

  ),
  info = list(
    aesthetics = new_property(getter = function(self) {
      class_aesthetics_list(
    geom = ggforce::geom_ellipse,
    mappable_bare = c(
      "m1",
      "m2"),
    mappable_identity = c(
      "linewidth",
      "linetype",
      "alpha",
      "color",
      "fill"),
    not_mappable = c("n"),
    required_aes = c(
      "x0",
      "y0",
      "a",
      "b",
      "angle",
      "group"),
    omit_names = c(
      "linejoin",
      "rule",
      "label"),
    inherit.aes = FALSE,
    style = el_styles
  )}))
)

# ellipse----

#' ellipse class
#'
#' Makes ellipses and superellipses
#' @param center point at center of ellipse. *Settable.*
#' @param a distance of semi-major axis. *Settable.*
#' @param b distance of semi-minor axis. *Settable.*
#' @param m1 exponent of semi-major axis. *Settable.* Controls roundedness of superellipse
#' @param m2 exponent of semi-minor axis. *Settable.* By default equal to `m1`. If different, some functions may not work as expected (e.g., `point_at`).
#' @param angle ellipse rotation. *Settable.*
#' @param label A character, angle, or label object
#' @param n number of points in ellipse (default = 360). *Settable.*
#' @slot length Gets the number of ellipses
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggforce::geom_ellipse`.
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to `ggforce::geom_ellipse`.
#' @slot normal_at A function that finds a point perpendicular to the ellipse at angle `theta` at the specified `distance`. The `definitional` parameter is passed to the `point_at` function.
#' @slot point_at A function that finds a point on the ellipse at an angle `theta`. If `definitional` is `FALSE` (default), then `theta` is interpreted as an angle. If `TRUE`, then `theta` is the parameter in the definition of the ellipse in polar coordinates.
#' @slot tangent_at A function that finds a tangent line on the ellipse. Uses `point_at` to find the tangent point at angle `theta` and then returns the tangent line at that point.
#' @inherit style params
#' @param style gets and sets style parameters
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object
#' @examples
#' # specify center point and semi-major axes
#' p <- point(0,0)
#' ellipse(p, a = 2, b = 3)
#' @export
ellipse <- new_class(
  name = "ellipse",
  parent = centerpoint,
  properties = rlang::inject(list(
    !!!el_props$primary,
    !!!el_props$styles,
    !!!el_props$derived,
    !!!el_props$funs,
    !!!el_props$info)),
  constructor = function(center = point(0,0),
                         a = 1,
                         b = 1,
                         angle = 0,
                         m1 = class_missing,
                         m2 = class_missing,
                         label = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         linewidth = class_missing,
                         linetype = class_missing,
                         n = class_missing,
                         style = class_missing,
                         x0 = class_missing,
                         y0 = class_missing,
                         ...) {
    if (!S7_inherits(angle, class_angle)) angle <- degree(angle)


    if (length(m1) == 0) m1 <- 2

    if (length(m2) == 0) m2 <- m1

    el_style <- center@style + style +
      style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype,
        n = n
      ) +
      style(...)

    if (length(x0) > 0 | length(y0) > 0) {
      if (length(x0) == 0) {
        x0 <- 0
      }
      if (length(y0) == 0) {
        y0 <- 0
      }
      center <- point(tibble::tibble(x = x0, y = y0))
    }

    non_empty_list <- get_non_empty_props(el_style)
    d <- tibble::tibble(x0 = center@x, y0 = center@y, a = a, b = b, angle = angle@radian, m1 = m1, m2 = m2)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }

    center = set_props(center, x = d$x0, y = d$y0)

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ellipse")






     new_object(centerpoint(center = center, label = label),
                 a = d$a,
                 b = d$b,
                 angle = radian(d$angle),
                 m1 = d$m1,
                 m2 = d$m2,
                 alpha = d[["alpha"]] %||% alpha,
                 color = d[["color"]] %||% color ,
                 fill = d[["fill"]]  %||% fill,
                 linewidth = d[["linewidth"]]  %||% linewidth,
                 linetype = d[["linetype"]]  %||% linetype,
                 n = d[["n"]]  %||% n)
  }
)


method(str, ellipse) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","a", "b", "angle", "m1", "m2"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

circle_or_ellipse <- new_union(circle, ellipse)

method(projection, list(point, circle_or_ellipse)) <- function(p,object, ...) {
  d <- p - object@center
  object@point_at(d@theta, ...)
}

 method(get_tibble, ellipse) <- function(x) {
  x@tibble
}

method(get_tibble_defaults, ellipse) <- function(x) {
  # ggforce::geom_ellipse uses GeomCircle
  sp <- style(
    alpha = replace_na(as.double(ggforce::GeomCircle$default_aes$alpha), 1),
    color = replace_na(ggforce::GeomCircle$default_aes$colour, "black"),
    fill = replace_na(ggforce::GeomCircle$default_aes$fill, "black"),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggforce::GeomCircle$default_aes$linewidth, 0.5),
    linetype = replace_na(ggforce::GeomCircle$default_aes$default_aes$linetype, 1),
    n = 360
  )
  get_tibble_defaults_helper(x, sp,required_aes = c("x0", "y0", "a", "b", "m1", "m2", "angle"))
}

method(`[`, ellipse) <- function(x, y) {
  d <- x@tibble[y,]
  dl <- as.list(dplyr::select(d, -.data$x0, -.data$y0))
  z <- rlang::inject(ellipse(center = point(d$x0, d$y0), !!!dl))
  z@label <- x@label[y]
  z
}

method(connect, list(centerpoint, centerpoint)) <- function(x,y, ...) {
  theta <- radian(atan2(y@center@y - x@center@y, y@center@x - x@center@x))
  connect(x@point_at(theta), y@point_at(theta + degree(180)), ...)
}

method(connect, list(centerpoint, point)) <- function(x,y, ...) {
  theta <- radian(atan2(y@y - x@center@y, y@x - x@center@x))
  connect(x@point_at(theta), y, ...)
}

method(connect, list(point, centerpoint)) <- function(x,y, ...) {
  theta <- radian(atan2(y@center@y - x@y, y@center@x - x@x))
  connect(x, y@point_at(theta + degree(180)), ...)
}

method(connect, list(centerpoint, line)) <- function(x,y, ...) {
  p2 <- projection(x@center, y)
  connect(x, p2, ...)
}

method(connect, list(line, centerpoint)) <- function(x,y, ...) {
  p1 <- projection(y@center, x)
  connect(p1, y, ...)
}




# Placing ----
method(place, list(ellipse, ellipse)) <- function (x, from, where = "right", sep = 1) {
  where <- degree(where)
  p <- from@point_at(where)
  p_sep <- polar((p - from@center)@theta, sep)

  xp <- x@point_at(where + degree(180)) - x@center
  x@center <- (p + p_sep) - xp
  if (S7_inherits(x@label)) x@label@p <- x@center
  x
}


method(place, list(point, circle_or_ellipse)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p <- from@point_at(where)
  p_sep <- polar((p - from@center)@theta, sep)
  x@x <- p@x + p_sep@x
  x@y <- p@y + p_sep@y
  x

}

method(place, list(circle_or_ellipse, point)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p_sep <- polar(where, sep)
  p <- x@center - x@point_at(where + degree(180))
  x@center@x <- from@x + p@x + p_sep@x
  x@center@y <- from@y + p@y + p_sep@y
  if (S7_inherits(x@label)) x@label@p <- x@center
  x

}

method(place, list(line, ellipse)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p1 <- from@point_at(where)
  p2 <- from@normal_at(where, distance = sep)
  p3 <- rotate(p1, theta = degree(90), origin = p2)
  segment(p2, p3)@line
}


