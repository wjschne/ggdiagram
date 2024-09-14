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
    # center = new_property(class = point, default = point(0,0)),
    radius = new_property(class = class_numeric, default = 1)
  ),
  styles = style@properties[cr_styles],
  # derived ----
  derived = list(
    area = new_property(getter = function(self) {
      pi + self@radius ^ 2
    }),
    bounding_box = new_property(getter = function(self) {
        rectangle(
          southwest = point(x = min(self@center@x - self@radius),
                            y = min(self@center@y - self@radius)),
          northeast = point(x = max(self@center@x + self@radius),
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
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(cr_styles,
                         prop, object = self) |>
          `names<-`(cr_styles)
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
        if (S7_inherits(theta, point)) theta <- projection(theta, self)@theta
        if (!S7_inherits(theta, class_angle)) theta <- degree(theta)
        polar(theta, self@radius + distance) + self@center
      }
    }),
    tangent_at = new_property(
      class = class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (!S7_inherits(theta, class_angle)) theta <- degree(theta)
          x0 <- self@center@x
          y0 <- self@center@y
          x1 <- cos(theta) * self@radius + self@center@x
          y1 <- sin(theta) * self@radius + self@center@y
          line(
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
          if (!S7_inherits(theta, class_angle)) theta <- degree(theta)
          self@center + polar(theta = theta, r = self@radius, style = self@style, ...)
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

#' circle class
#' @param center point at center of the circle
#' @param radius distance between center and edge circle
#' @param label A character, angle, or label object
#' @param x0 x-coordinate of circle's center. Overrides `@center@x`
#' @param y0 y-coordinate of circle's center. Overrides `@center@y
#' @param n number of points in circle (default = 360)
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object
#' @inherit style params
#' @slot aesthetics A list of information about the circle's aesthetic properties
#' @slot angle_at A function that finds the angle of the specified point in relation to the circle's center
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to `ggforce::geom_circle`.
#' @slot length The number of circles in the circle object
#' @slot point_at A function that finds a point on the circle at the specified angle.
#' @slot tangent_at A function that finds the tangent line at the specified angle.
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggforce::geom_cirlce`.
#' @examples
#' # specify center point and radius
#' p <- point(0,0)
#' circle(p, radius = 6)
#' @export
circle <- new_class(
  name = "circle",
  parent = centerpoint,
  properties = rlang::inject(list(
    !!!cr_props$primary,
    !!!cr_props$styles,
    !!!cr_props$derived,
    !!!cr_props$funs,
    !!!cr_props$info)),
  constructor = function(center = point(0,0),
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
      style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype,
        n = n
      ) +
      style(...)

    if ((length(x0) > 0) || (length(y0) > 0)) {
      if (length(x0) == 0) {
        x0 <- 0
      }
      if (length(y0) == 0) {
        y0 <- 0
      }
      center <- point(tibble::tibble(x = x0, y = y0))
    }

    non_empty_list <- get_non_empty_props(c_style)
    d <- tibble::tibble(x0 = center@x, y0 = center@y, radius = radius)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }


    center = set_props(center, x = d$x0, y = d$y0)

    if (S7_inherits(label, ggdiagram::label)) {
      if (all(label@p == point(0,0))) {
        label@p <- center
      }
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "circle")





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


method(str, circle) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","radius"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

method(get_tibble, circle) <- function(x) {
  x@tibble
}


method(get_tibble_defaults, circle) <- function(x) {
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
  get_tibble_defaults_helper(x, sp,required_aes = c("x0", "y0", "r", "n"))
}

method(`[`, circle) <- function(x, y) {
  d <- x@tibble[y,]
  dl <- as.list(dplyr::select(d, -.data$x0, -.data$y0))
  z <- rlang::inject(circle(center = point(d$x0, d$y0), !!!dl))
  z@label <- x@label[y]
  z
}

method(`==`, list(circle, circle)) <- function(e1, e2) {
  (e1@center == e2@center) & (e1@radius == e1@radius)
}

# Place ----

# method(place, list(circle, circle)) <- function(x, from, where = "right", sep = 1) {
#
#   where <- degree(where)
#   p <- polar(where, sep + x@radius + from@radius)
#   x@center@x <- from@center@x + p@x
#   x@center@y <- from@center@y + p@y
#
#   if (S7_inherits(x@label, label)) {
#     x@label@p <- x@center
#   }
#   x
#
# }

method(place, list(line, circle)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  from@radius <- sep + from@radius
  from@tangent_at(where)
}

method(shape_array, circle) <- function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {
  # s <- seq(0, (sep + x@radius) * (k - 1), sep + x@radius)
  # px <- cos(degree(where)) * s
  # py <- sin(degree(where)) * s
  # p <- circle(point(px, py), radius = x@radius)
  # bb <- p@bounding_box
  # if (anchor == "center") {
  #   p_anchor <- bb@center
  # } else {
  #   p_anchor <- bb@point_at(anchor)
  # }
  # circle(p@center - p_anchor + x@center, style = x@style, ...)

  sa <- shape_array_helper(x = x, k = k, sep = sep, where = where, anchor = anchor, ...)

  rlang::inject(circle(center = sa$p_center,
                        radius = x@radius,
                        style = x@style,
                        !!!sa$dots))
}
