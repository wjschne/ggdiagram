cr_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype",
  "n"
)

cr_props <- list(
  # primary ----
  primary = list(
    # center = new_property(class = ob_point, default = ob_point(0,0)),
    radius = new_property(class = class_numeric, default = 1)
  ),
  styles = ob_style@properties[cr_styles],
  # derived ----
  derived = list(
    area = new_property(getter = function(self) {
      pi + self@radius ^ 2
    }),
    bounding_box = new_property(getter = function(self) {
      ob_rectangle(
          southwest = ob_point(x = min(self@center@x - self@radius),
                            y = min(self@center@y - self@radius)),
          northeast = ob_point(x = max(self@center@x + self@radius),
                            y = max(self@center@y + self@radius)))
    }),
    circumference = new_property(getter = function(self) {
      pi + self@radius * 2
    }),
    diameter = new_property(getter = function(self) {
      self@radius * 2
    }),
    length = new_property(
      getter = function(self) {
        nrow(self@tibble)
      }
    ),
    polygon = new_property(getter = function(self) {
      d <- self@tibble
      if (!("n" %in% colnames(d))) {
        d$n <- 360
      }
      d |>
        dplyr::mutate(group = factor(dplyr::row_number())) |>
        tidyr::uncount(n, .remove = FALSE) |>
        dplyr::mutate(theta = 2 * pi * (dplyr::row_number() - 1) / n,
                      x = x0 + r * cos(theta),
                      y = y0 + r * sin(theta),
                      .by = group)
    }),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(cr_styles,
                         prop, object = self) |>
          `names<-`(cr_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        ob_point(self@x, self@y, style = self@style + value)
      }
    ),
    tibble = new_property(getter = function(self) {
      d <- list(
        x0 = self@center@x,
        y0 = self@center@y,
        r = self@radius,
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
    angle_at = new_property(class_function, getter = function(self) {
      \(point) {
        dp <- point - self@center
        dp@theta
      }
    }),
    normal_at = new_property(class_function, getter = function(self) {
      \(theta = degree(0), distance = 1) {
        if (S7_inherits(theta, ob_point)) theta <- projection(theta, self)@theta
        if (!S7_inherits(theta, ob_angle)) theta <- degree(theta)
        ob_polar(theta, self@radius + distance) + self@center
      }
    }),
    tangent_at = new_property(
      class = class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (!S7_inherits(theta, ob_angle)) theta <- degree(theta)
          x0 <- self@center@x
          y0 <- self@center@y
          x1 <- cos(theta) * self@radius + self@center@x
          y1 <- sin(theta) * self@radius + self@center@y
          ob_line(
            a = x1 - x0,
            b = y1 - y0,
            c = x0 ^ 2 - (x1 * x0) + y0 ^ 2 - (y1 * y0) - self@radius ^ 2,
            style = self@style,
            ...
          )
        }
      }
    ),
    point_at = new_property(
      class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (!S7_inherits(theta, ob_angle)) theta <- degree(theta)
          self@center + ob_polar(theta = theta, r = self@radius, style = self@style, ...)
          }
      }
    )),
  # info ----
  info = list(
  aesthetics = new_property(getter = function(self) {
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
      omit_names = c("linejoin", "rule"),
      inherit.aes = FALSE,
      style = cr_styles
    )}
  )))

# Circle----

#' ob_circle class
#' @param center point at center of the circle
#' @param radius distance between center and edge circle
#' @param label A character, angle, or label object
#' @param x0 x-coordinate of circle's center. Overrides `@center@x`
#' @param y0 y-coordinate of circle's center. Overrides `@center@y
#' @param n number of points in circle (default = 360)
#' @param style an ob_style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object
#' @inherit ob_style params
#' @slot aesthetics A list of information about the circle's aesthetic properties
#' @slot angle_at A function that finds the angle of the specified point in relation to the circle's center
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to `ggforce::geom_circle`.
#' @slot length The number of circles in the circle object
#' @slot point_at A function that finds a point on the circle at the specified angle.
#' @slot tangent_at A function that finds the tangent line at the specified angle.
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggforce::geom_cirlce`.
#' @examples
#' # specify center point and radius
#' p <- ob_point(0,0)
#' ob_circle(p, radius = 6)
#' @export
ob_circle <- new_class(
  name = "ob_circle",
  parent = centerpoint,
  properties = rlang::inject(list(
    !!!cr_props$primary,
    !!!cr_props$styles,
    !!!cr_props$derived,
    !!!cr_props$funs,
    !!!cr_props$info)),
  constructor = function(center = ob_point(0,0),
                         radius = 1,
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

    if ((length(x0) > 0) || (length(y0) > 0)) {
      if (length(x0) == 0) {
        x0 <- 0
      }
      if (length(y0) == 0) {
        y0 <- 0
      }
      center <- ob_point(tibble::tibble(x = x0, y = y0))
    }

    non_empty_list <- get_non_empty_props(c_style)
    d <- tibble::tibble(x0 = center@x, y0 = center@y, radius = radius)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }


    center = set_props(center, x = d$x0, y = d$y0)

    if (S7_inherits(label, ob_label)) {
      if (all(label@p == ob_point(0,0))) {
        label@p <- center
      }
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_circle")





     new_object(centerpoint(center = center, label = label),
                 radius = d$radius,
                 alpha = d[["alpha"]] %||% alpha,
                 color = d[["color"]] %||% color ,
                 fill = d[["fill"]]  %||% fill,
                 linewidth = d[["linewidth"]]  %||% linewidth,
                 linetype = d[["linetype"]]  %||% linetype,
                 n = d[["n"]]  %||% n)
  }
)


method(str, ob_circle) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","radius"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

method(get_tibble, ob_circle) <- function(x) {
  x@tibble
}


method(get_tibble_defaults, ob_circle) <- function(x) {
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

method(`[`, ob_circle) <- function(x, y) {
  d <- x@tibble[y,]
  dl <- as.list(dplyr::select(d, -.data$x0, -.data$y0))
  z <- rlang::inject(ob_circle(center = ob_point(d$x0, d$y0), !!!dl))
  z@label <- x@label[y]
  z
}

method(`==`, list(ob_circle, ob_circle)) <- function(e1, e2) {
  (e1@center == e2@center) & (e1@radius == e1@radius)
}

# Place ----

# method(place, list(ob_circle, ob_circle)) <- function(x, from, where = "right", sep = 1) {
#
#   where <- degree(where)
#   p <- ob_polar(where, sep + x@radius + from@radius)
#   x@center@x <- from@center@x + p@x
#   x@center@y <- from@center@y + p@y
#
#   if (S7_inherits(x@label, ob_label)) {
#     x@label@p <- x@center
#   }
#   x
#
# }

method(place, list(ob_line, ob_circle)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  from@radius <- sep + from@radius
  from@tangent_at(where)
}

method(ob_array, ob_circle) <- function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {
  # s <- seq(0, (sep + x@radius) * (k - 1), sep + x@radius)
  # px <- cos(degree(where)) * s
  # py <- sin(degree(where)) * s
  # p <- ob_circle(ob_point(px, py), radius = x@radius)
  # bb <- p@bounding_box
  # if (anchor == "center") {
  #   p_anchor <- bb@center
  # } else {
  #   p_anchor <- bb@point_at(anchor)
  # }
  # ob_circle(p@center - p_anchor + x@center, style = x@style, ...)

  sa <- ob_array_helper(x = x, k = k, sep = sep, where = where, anchor = anchor, ...)

  rlang::inject(ob_circle(center = sa$p_center,
                        radius = x@radius,
                        style = x@style,
                        !!!sa$dots))
}
