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
    a = S7::new_property(class = S7::class_numeric, default = 1),
    b = S7::new_property(class = S7::class_numeric, default = 1),
    angle = S7::new_property(ob_angle_or_numeric, default = 0),
    m1 = S7::new_property(class = S7::class_numeric, default = 2),
    m2 = S7::new_property(class = S7::class_numeric, default = 2)
  ),
  styles = ob_style@properties[el_styles],
  # derived ----
  derived = list(
    area = S7::new_property(getter = function(self) {
      pi * self@a * self@b
    }),
    bounding_box = S7::new_property(getter = function(self) {
      d_rect <- self@tibble |>
        dplyr::mutate(group = dplyr::row_number(),
                      d = purrr::pmap(list(x0 = x,
                                           y0 = y,
                                           a = a,
                                           b = b,
                                           angle = angle,
                                           m1 = m1,
                                           m2 = m2),
                                      \(x0, y0,a,b,angle, m1,m2) {
                                        th <- degree(seq(0,359.5,.5))
                                        cs <- cos(th)
                                        sn <- sin(th)
                                        x <- a * (abs(cs) ^ (2 / m1)) * sign(cs)
                                        y <- b * (abs(sn) ^ (2 / m2)) * sign(sn)
                                        xy <- rotate2columnmatrix(
                                          cbind(x,y), angle)
                                        xr <- xy[,1] + x0
                                        yr <- xy[,2] + y0
                                        tibble::tibble(
                                          xmin = min(xr),
                                          xmax = max(xr),
                                          ymin = min(yr),
                                          ymax = max(yr)
                                        )
                                      })) |>
        tidyr::unnest(d) |>
        dplyr::summarise(xmin = min(xmin),
                         xmax = max(xmax),
                         ymin = min(ymin),
                         ymax = max(ymax))
      ob_rectangle(southwest = ob_point(d_rect$xmin, d_rect$ymin),
                northeast = ob_point(d_rect$xmax, d_rect$ymax))
    }),
    focus_1 = S7::new_property(ob_point, getter = function(self) {
      ab_df <- self@a ^ 2 - self@b ^ 2
      ab_df[ab_df < 0] <- 0
      x <- sqrt(ab_df) * -1
      ab_df <- self@a ^ 2 - self@b ^ 2
      ab_df[ab_df > 0] <- 0
      y <- sqrt(abs(ab_df))
      self@center + ob_point(x,y)
    }),
    focus_2 = S7::new_property(ob_point, getter = function(self) {
      ab_df <- self@a ^ 2 - self@b ^ 2
      ab_df[ab_df < 0] <- 0
      x <- sqrt(ab_df)
      ab_df <- self@a ^ 2 - self@b ^ 2
      ab_df[ab_df > 0] <- 0
      y <- sqrt(abs(ab_df)) * -1
      self@center + ob_point(x, y)
    }),
    length = S7::new_property(
      getter = function(self) {
        length(self@a)
      }
    ),
    perimeter = S7::new_property(getter = function(self) {
      # Ramanujan's approximation
      # https://www.johndcook.com/blog/2024/09/22/ellipse-perimeter-approx/
      ab <- self@a + self@b
      lamba <- (self@a - self@b) / ab
      pi * ab * (1 + (3 * lamba ^ 2) / (10 + sqrt(4 - 3 * lamba ^ 2)))
    }),
    polygon = S7::new_property(getter = function(self) {
      d <- self@tibble |>
        dplyr::mutate(group = factor(dplyr::row_number()))
      if (!("n" %in% colnames(d))) {
        d$n <- 360
      }


      d$xy <- unbind(self) |>
        purrr::map(\(x) {
          if (length(x@n) > 0) n <- x@n else n <- 360
          th <- degree(seq(0, 360, length.out = n + 1))
          xy <- tibble::as_tibble(x@point_at(th)@xy) |>
            dplyr::mutate(degree = th@degree)
        })

      tidyr::unnest(d, xy)
    }),
    style = S7::new_property(
      getter = function(self) {
        pr <- purrr::map(el_styles,
                         prop, object = self) |>
          `names<-`(el_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% el_styles]
        self <- rlang::inject(S7::set_props(self, !!!s_list))
        self
      }
    ),
    tibble = S7::new_property(getter = function(self) {
      d <- list(
        x = self@center@x,
        y = self@center@y,
        a = self@a,
        b = self@b,
        angle = c(self@angle) * 360,
        m1 = self@m1,
        m2 = self@m2,
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
    angle_at = S7::new_property(S7::class_function, getter = function(self) {
      \(point) {
        dp <- point - self@center
        dp@theta
      }
    }),
    geom = S7::new_property(S7::class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    normal_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(theta = degree(0), distance = 1) {
          if (S7::S7_inherits(theta, ob_point)) {
            theta <- projection(theta, self)@theta
            }
          if (!S7::S7_inherits(theta, ob_angle)) {
            theta <- degree(theta)
            }

          p0 <- self@point_at(theta - self@angle) - self@center
          p1 <- ob_point(
            sign(cos(theta)) * abs((self@a ^ (-1 * self@m1)) * self@m1 * (p0@x ^ (self@m1 - 1))),
            sign(sin(theta)) * abs((self@b ^ (-1 * self@m2)) * self@m2 * (p0@y ^ (self@m2 - 1))))
          self@point_at(
            theta) +
            distance / p1@r * rotate(p1, self@angle)
      }
    }),
    place = pr_place,
    point_at = S7::new_property(S7::class_function, getter = function(self) {
      \(theta = degree(0), definitional = FALSE, ...) {
        if (!S7::S7_inherits(theta, ob_angle)) theta <- degree(theta)

        rtheta <- theta - radian(self@angle)

        if (definitional) {
          return(
            self@center + rotate(
              ob_point(abs(cos(theta) ^ (2 / self@m1)) * self@a * sign(cos(theta)),
                    abs(sin(theta) ^ (2 / self@m2)) * self@b * sign(sin(theta)), ...),
              theta = self@angle,
              origin = self@center)
            )
        } else {
        t <- radian(
          dplyr::if_else(
            abs(cos(rtheta)) < .Machine$double.eps,
            rtheta@radian,
            atan(
              (
                abs(suppressWarnings(tan(rtheta))) *
                  self@a / self@b
                ) ^ (self@m1 / 2))
            )
          )

        rp <- rotate(
          ob_point((abs(cos(t)) ^ (2 / self@m1)) *
                  self@a * ((cos(rtheta) >= 0) * 2 - 1),
                (abs(sin(t)) ^ (2 / self@m1)) *
                  self@b * ((sin(rtheta) >= 0) * 2 - 1), ...),
          self@angle)

        self@center + rp
        }
      }
    }),
    tangent_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (S7::S7_inherits(theta, ob_point)) {
            theta <- (projection(theta, self) - self@center)@theta
            }
        if (!S7::S7_inherits(theta, ob_angle)) {
          theta <- degree(theta)
          }

        p <- self@point_at(theta)
        p_normal <- self@normal_at(theta)
        l <- ob_segment(
          p1 = p,
          p2 = rotate(p_normal, degree(90), origin = p))@line

        s <- rlang::list2(...)
        rlang::inject(set_props(l, !!!s))
      }
    }),
    polar_line_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(x) {
          x0 <- x - self@center
          ob_line(a = x0@x * self@b ^ self@m2, b = x0@y * self@a ^ self@m1, c = -1 * (self@b ^ self@m2) * (self@a ^ self@m1) - self@center@x * x0@x - self@center@y * x0@y)
        }
      })
  ),
  info = list(
    aesthetics = S7::new_property(getter = function(self) {
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

# ob_ellipse----

#' ob_ellipse class
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
#' @slot normal_at A function that finds a point perpendicular to the ellipse at angle `theta` at the specified `distance`. The `definitional` parameter is passed to the `point_at` function. If a point is supplied instead of an angle, the point is projected onto the ellipse and then the normal is calculated found from the projected point.
#' @slot point_at A function that finds a point on the ellipse at an angle `theta`. If `definitional` is `FALSE` (default), then `theta` is interpreted as an angle. If `TRUE`, then `theta` is the parameter in the definition of the ellipse in polar coordinates.
#' @slot tangent_at A function that finds a tangent line on the ellipse. Uses `point_at` to find the tangent point at angle `theta` and then returns the tangent line at that point. If a point is supplied instead of an angle, the point is projected onto the ellipse and then the tangent line is found from there.
#' @param x x-coordinate of center point. If specified, overrides x-coordinate of `@center`.
#' @param y x-coordinate of center point. If specified, overrides y-coordinate of `@center`.
#' @inherit ob_style params
#' @param style gets and sets style parameters
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style object
#' @examples
#' # specify center point and semi-major axes
#' e <- ob_ellipse(center = ob_point(0,0), a = 2, b = 3)
#' ggdiagram() +
#'   e
#' @export
#' @returns ob_ellipse object
ob_ellipse <- S7::new_class(
  name = "ob_ellipse",
  package = "ggdiagram",
  parent = centerpoint,
  properties = rlang::list2(
    !!!el_props$primary,
    !!!el_props$styles,
    !!!el_props$derived,
    !!!compass_props,
    !!!el_props$funs,
    !!!el_props$info),
  constructor = function(center = ob_point(0,0),
                         a = 1,
                         b = a,
                         angle = 0,
                         m1 = numeric(0),
                         m2 = numeric(0),
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
    if (!S7::S7_inherits(angle, ob_angle)) angle <- degree(angle)


    if (length(m1) == 0) m1 <- 2

    if (length(m2) == 0) m2 <- m1

    el_style <- center@style + style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype,
        n = n
      ) +
      ob_style(...)


    if (length(x) > 0 | length(y) > 0) {
      if (length(x) == 0) {
        x <- 0
      }
      if (length(y) == 0) {
        y <- 0
      }
      center <- ob_point(tibble::tibble(x = x, y = y))
    }

    non_empty_list <- get_non_empty_props(el_style)
    d <- tibble::tibble(
      x = center@x,
      y = center@y,
      a = a,
      b = b,
      angle = angle@radian,
      m1 = m1,
      m2 = m2)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }

    center = set_props(center, x = d$x, y = d$y)


    label <- centerpoint_label(label = label,
                               center = center,
                               d = d,
                               shape_name = "ob_ellipse",
                               angle = angle)

    # If there is one ellipse but many labels, make multiple ellipses
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }



     S7::new_object(centerpoint(center = center, label = label),
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
                 n = d[["n"]]  %||% n,
                 id = d[["id"]] %||% id)
  }
)


S7::method(str, ob_ellipse) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","a", "b", "angle", "m1", "m2"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

circle_or_ellipse <- S7::new_union(ob_circle, ob_ellipse)

S7::method(projection, list(ob_point, centerpoint)) <- function(p,object, ...) {
  d <- p - object@center
  object@point_at(d@theta, ...)
}

S7::method(get_tibble, ob_ellipse) <- function(x) {
  d <- x@tibble
  if ("angle" %in% colnames(d)) {
    d$angle <- pi * d$angle / 180
  }
    d
}

S7::method(get_tibble_defaults, ob_ellipse) <- function(x) {
  # ggforce::geom_ellipse uses GeomCircle
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
  get_tibble_defaults_helper(x, sp,required_aes = c("x0", "y0", "a", "b", "m1", "m2", "angle"))
}

S7::method(`[`, ob_ellipse) <- function(x, i) {
  i <- character_index(i, x@id)
  d <- x@tibble[i,]

  z <- data2shape(d, ob_ellipse)
  z@label <- na2zero(x@label[i])
  if (!is.null(d$angle)) {
    z@angle <- x@angle[i]
  }
  z
}



S7::method(connect, list(centerpoint, centerpoint)) <- function(
    from,
    to,
    label = character(0),
    arc_bend = NULL,
    from_offset = NULL,
    to_offset = NULL,
    alpha = numeric(0),
    arrow_head = the$arrow_head,
    arrow_fins = list(),
    arrowhead_length = 7,
    length_head = numeric(0),
    length_fins = numeric(0),
    color = character(0),
    lineend = numeric(0),
    linejoin = numeric(0),
    linewidth = numeric(0),
    linewidth_fins = numeric(0),
    linewidth_head = numeric(0),
    linetype = numeric(0),
    resect = numeric(0),
    resect_fins = numeric(0),
    resect_head = numeric(0),
    stroke_color = character(0),
    stroke_width = numeric(0),
    style = S7::class_missing,
    label_sloped = TRUE,
    id = character(0),
    ...) {
  theta <- radian(atan2(to@center@y - from@center@y, to@center@x - from@center@x))

  if (is.null(arc_bend) && is.null(from_offset) && is.null(to_offset)) {
    p_from <- from@point_at(theta)
    p_to <- to@point_at(theta + degree(180))
  } else {
    p_from <- from@center
    p_to <- to@center
  }

  s <- connect(p_from,
          p_to,
          label = label,
          arc_bend = arc_bend,
          from_offset = from_offset,
          to_offset = to_offset,
          alpha = alpha,
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          arrowhead_length = arrowhead_length,
          length_head = length_head,
          length_fins = length_fins,
          color = color,
          lineend = lineend,
          linejoin = linejoin,
          linewidth = linewidth,
          linewidth_fins = linewidth_fins,
          linewidth_head = linewidth_head,
          linetype = linetype,
          resect = resect,
          resect_fins = resect_fins,
          resect_head = resect_head,
          stroke_color = stroke_color,
          stroke_width = stroke_width,
          style = style,
          label_sloped = label_sloped,
          id = id,
          ...)

  if (S7::S7_inherits(s, ob_arc)) {
    i_from <- map_ob(s, \(sss) intersection(sss, from))
    i_to <- map_ob(s, \(sss) intersection(sss, to))

    ss <- s@circle@angle_at(i_from)
    ee <- s@circle@angle_at(i_to)
    ee[ee > ss & arc_bend < 0] <- ee[ee > ss & arc_bend < 0] + turn(-1)
    ee[ee < ss & arc_bend >= 0] <- ee[ee < ss & arc_bend >= 0] + turn(1)

#
    s@start <- ss
    s@end <- ee

  } else if (S7::S7_inherits(s, ob_bezier)) {
    pp <- s@path
    pp@p <- purrr::map2(pp@p, unbind(from), \(p, ff) p[inside(p, ff) != 1])
    pp@p <- purrr::map2(pp@p, unbind(to), \(p, tt) p[inside(p, tt) != 1])

    pp@label <- s@label
    s <- pp


  }

  s
}

S7::method(connect, list(centerpoint, ob_point)) <- function(
    from,
    to,
    label = character(0),
    arc_bend = NULL,
    from_offset = NULL,
    to_offset = NULL,
    alpha = numeric(0),
    arrow_head = the$arrow_head,
    arrow_fins = list(),
    arrowhead_length = 7,
    length_head = numeric(0),
    length_fins = numeric(0),
    color = character(0),
    lineend = numeric(0),
    linejoin = numeric(0),
    linewidth = numeric(0),
    linewidth_fins = numeric(0),
    linewidth_head = numeric(0),
    linetype = numeric(0),
    resect = numeric(0),
    resect_fins = numeric(0),
    resect_head = numeric(0),
    stroke_color = character(0),
    stroke_width = numeric(0),
    style = S7::class_missing,
    label_sloped = TRUE,
    id = character(0),
    ...) {

  theta <- radian(atan2(to@y - from@center@y,
                        to@x - from@center@x))

  if (is.null(arc_bend) && is.null(from_offset) && is.null(to_offset)) {
    p_from <- from@point_at(theta)
  } else {
    p_from <- from@center
  }


  s <- connect(p_from,
          to,
          label = label,
          arc_bend = arc_bend,
          from_offset = from_offset,
          to_offset = to_offset,
          alpha = alpha,
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          arrowhead_length = arrowhead_length,
          length_head = length_head,
          length_fins = length_fins,
          color = color,
          lineend = lineend,
          linejoin = linejoin,
          linewidth = linewidth,
          linewidth_fins = linewidth_fins,
          linewidth_head = linewidth_head,
          linetype = linetype,
          resect = resect,
          resect_fins = resect_fins,
          resect_head = resect_head,
          stroke_color = stroke_color,
          stroke_width = stroke_width,
          style = style,
          label_sloped = label_sloped,
          id = id,
          ...)

  if (S7::S7_inherits(s, ob_arc)) {
    i_from <- map_ob(s, \(sss) intersection(sss, from))
    ss <- s@circle@angle_at(i_from)
    ee <- s@circle@angle_at(to)
    ee[ee > ss & arc_bend < 0] <- ee[ee > ss & arc_bend < 0] + turn(-1)
    ee[ee < ss & arc_bend >= 0] <- ee[ee < ss & arc_bend >= 0] + turn(1)

    #
    s@start <- ss
    s@end <- ee

  } else if (S7::S7_inherits(s, ob_bezier)) {
    pp <- s@path
    pp@p <- purrr::map(pp@p, \(p) p[inside(p, from) != 1])
    pp@label <- s@label
    s <- pp


  }

  s

}

S7::method(connect, list(ob_point, centerpoint)) <- function(
    from,
    to,
    label = character(0),
    arc_bend = NULL,
    from_offset = NULL,
    to_offset = NULL,
    alpha = numeric(0),
    arrow_head = the$arrow_head,
    arrow_fins = list(),
    arrowhead_length = 7,
    length_head = numeric(0),
    length_fins = numeric(0),
    color = character(0),
    lineend = numeric(0),
    linejoin = numeric(0),
    linewidth = numeric(0),
    linewidth_fins = numeric(0),
    linewidth_head = numeric(0),
    linetype = numeric(0),
    resect = numeric(0),
    resect_fins = numeric(0),
    resect_head = numeric(0),
    stroke_color = character(0),
    stroke_width = numeric(0),
    style = S7::class_missing,
    label_sloped = TRUE,
    id = character(0),
    ...) {
  theta <- radian(atan2(to@center@y - from@y,
                        to@center@x - from@x))

  if (is.null(arc_bend) && is.null(from_offset) && is.null(to_offset)) {
    p_to <- to@point_at(theta + degree(180))
  } else {
    p_to <- to@center
  }

  s <- connect(from,
          p_to,
          label = label,
          arc_bend = arc_bend,
          from_offset = from_offset,
          to_offset = to_offset,
          alpha = alpha,
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          arrowhead_length = arrowhead_length,
          length_head = length_head,
          length_fins = length_fins,
          color = color,
          lineend = lineend,
          linejoin = linejoin,
          linewidth = linewidth,
          linewidth_fins = linewidth_fins,
          linewidth_head = linewidth_head,
          linetype = linetype,
          resect = resect,
          resect_fins = resect_fins,
          resect_head = resect_head,
          stroke_color = stroke_color,
          stroke_width = stroke_width,
          style = style,
          label_sloped = label_sloped,
          id = id,
          ...)

  if (S7::S7_inherits(s, ob_arc)) {
    i_to <- map_ob(s, \(sss) intersection(sss, to))

    ss <- s@circle@angle_at(from)
    ee <- s@circle@angle_at(i_to)
    ee[ee > ss & arc_bend < 0] <- ee[ee > ss & arc_bend < 0] + turn(-1)
    ee[ee < ss & arc_bend >= 0] <- ee[ee < ss & arc_bend >= 0] + turn(1)

    #
    s@start <- ss
    s@end <- ee

  } else if (S7::S7_inherits(s, ob_bezier)) {
    pp <- s@path
    pp@p <- purrr::map(pp@p, \(p) p[inside(p, to) != 1])
    pp@label <- s@label
    s <- pp


  }
  s
}

S7::method(connect, list(centerpoint, ob_line)) <- function(
    from,
    to,
    label = character(0),
    arc_bend = NULL,
    from_offset = NULL,
    to_offset = NULL,
    alpha = numeric(0),
    arrow_head = the$arrow_head,
    arrow_fins = list(),
    arrowhead_length = 7,
    length_head = numeric(0),
    length_fins = numeric(0),
    color = character(0),
    lineend = numeric(0),
    linejoin = numeric(0),
    linewidth = numeric(0),
    linewidth_fins = numeric(0),
    linewidth_head = numeric(0),
    linetype = numeric(0),
    resect = numeric(0),
    resect_fins = numeric(0),
    resect_head = numeric(0),
    stroke_color = character(0),
    stroke_width = numeric(0),
    style = S7::class_missing,
    label_sloped = TRUE,
    id = character(0),
    ...) {
  p2 <- projection(from@center, to)
  connect(from = from,
          to = p2,
          label = label,
          arc_bend = arc_bend,
          from_offset = from_offset,
          to_offset = to_offset,
          alpha = alpha,
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          arrowhead_length = arrowhead_length,
          length_head = length_head,
          length_fins = length_fins,
          color = color,
          lineend = lineend,
          linejoin = linejoin,
          linewidth = linewidth,
          linewidth_fins = linewidth_fins,
          linewidth_head = linewidth_head,
          linetype = linetype,
          resect = resect,
          resect_fins = resect_fins,
          resect_head = resect_head,
          stroke_color = stroke_color,
          stroke_width = stroke_width,
          style = style,
          label_sloped = label_sloped,
          id = id,
          ...)
}

S7::method(connect, list(ob_line, centerpoint)) <- function(
    from,
    to,
    label = character(0),
    arc_bend = NULL,
    from_offset = NULL,
    to_offset = NULL,
    alpha = numeric(0),
    arrow_head = the$arrow_head,
    arrow_fins = list(),
    arrowhead_length = 7,
    length_head = numeric(0),
    length_fins = numeric(0),
    color = character(0),
    lineend = numeric(0),
    linejoin = numeric(0),
    linewidth = numeric(0),
    linewidth_fins = numeric(0),
    linewidth_head = numeric(0),
    linetype = numeric(0),
    resect = numeric(0),
    resect_fins = numeric(0),
    resect_head = numeric(0),
    stroke_color = character(0),
    stroke_width = numeric(0),
    style = S7::class_missing,
    label_sloped = TRUE,
    id = character(0),
    ...) {
  p1 <- projection(to@center, from)
  connect(p1,
          to,
          label = label,
          arc_bend = arc_bend,
          from_offset = from_offset,
          to_offset = to_offset,
          alpha = alpha,
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          arrowhead_length = arrowhead_length,
          length_head = length_head,
          length_fins = length_fins,
          color = color,
          lineend = lineend,
          linejoin = linejoin,
          linewidth = linewidth,
          linewidth_fins = linewidth_fins,
          linewidth_head = linewidth_head,
          linetype = linetype,
          resect = resect,
          resect_fins = resect_fins,
          resect_head = resect_head,
          stroke_color = stroke_color,
          stroke_width = stroke_width,
          style = style,
          label_sloped = label_sloped,
          id = id,
          ...)
}

S7::method(connect, list(S7::class_list, centerpoint)) <- function(
    from,
    to,
    label = character(0),
    arc_bend = NULL,
    from_offset = NULL,
    to_offset = NULL,
    alpha = numeric(0),
    arrow_head = the$arrow_head,
    arrow_fins = list(),
    arrowhead_length = 7,
    length_head = numeric(0),
    length_fins = numeric(0),
    color = character(0),
    lineend = numeric(0),
    linejoin = numeric(0),
    linewidth = numeric(0),
    linewidth_fins = numeric(0),
    linewidth_head = numeric(0),
    linetype = numeric(0),
    resect = numeric(0),
    resect_fins = numeric(0),
    resect_head = numeric(0),
    stroke_color = character(0),
    stroke_width = numeric(0),
    style = S7::class_missing,
    label_sloped = TRUE,
    id = character(0),
    ...) {
  purrr::map(unbind(from), \(xx) {
    connect(xx,
            to,
            label = label,
            arc_bend = arc_bend,
            from_offset = from_offset,
            to_offset = to_offset,
            alpha = alpha,
            arrow_head = arrow_head,
            arrow_fins = arrow_fins,
            arrowhead_length = arrowhead_length,
            length_head = length_head,
            length_fins = length_fins,
            color = color,
            lineend = lineend,
            linejoin = linejoin,
            linewidth = linewidth,
            linewidth_fins = linewidth_fins,
            linewidth_head = linewidth_head,
            linetype = linetype,
            resect = resect,
            resect_fins = resect_fins,
            resect_head = resect_head,
            stroke_color = stroke_color,
            stroke_width = stroke_width,
            style = style,
            label_sloped = label_sloped,
            id = id,
            ...)
  }) |>
    bind()

}

S7::method(midpoint, list(centerpoint, centerpoint)) <- function(x,y, position = .5, ...) {
  midpoint(connect(x,y), position = position, ...)
}

#' @name ob_variance
#' @export
#' @rdname ob_variance
#' @examples
#' theta <- degree(seq(0, 360 - 45, 45))
#' ggdiagram() +
#' {x <- ob_circle(ob_polar(theta, r = 3))} +
#' ob_variance(x,
#'             label = ob_label(LETTERS[seq_along(c(theta))]),
#'             where = theta,
#'             looseness = 1.25)
S7::method(ob_variance, centerpoint) <- function(
    x,
    where = "north",
    theta = 50,
    bend = 0,
    looseness = 1,
    arrow_head = the$arrow_head,
    arrow_fins = the$arrow_head,
    resect = 2,
    ...) {
  if (!S7::S7_inherits(where, ob_angle)) where <- degree(where)
  if (!S7::S7_inherits(theta, ob_angle)) theta <- degree(theta)
  if (!S7::S7_inherits(bend, ob_angle)) bend <- degree(bend)


  p <- purrr::pmap(list(el = unbind(x),
                        th = unbind(theta),
                        ww = unbind(where),
                        ll = looseness,
                        bb = unbind(bend)), \(el, th, ww, ll, bb) {

      start_angle <- ww - (th / 2)
      end_angle <- ww + (th / 2)
    s <- el@point_at(start_angle)
    m <- el@point_at(ww)
    e <- el@point_at(end_angle)
    # s_dist <- (s - m)@r * looseness * 2
    # e_dist <- (e - m)@r * looseness * 2
    # m_dist <- (s_dist + e_dist) / 2
    radius <- (s - e)@r  * ll



    bind(c(
      s,
      rotate(
        el@normal_at(
          theta = start_angle,
          distance = (s - el@center)@r * ll),
        theta = bb,
        origin = s),
      el@normal_at(
        theta = ww,
        distance = (m - el@center)@r * ll),
      rotate(
        el@normal_at(
          theta = end_angle,
          distance = (e - el@center)@r * ll),
        theta = bb * -1,
             origin = e),
      e))
  })

  dots <- rlang::list2(...)

  l <- character(0)

  # if (length(x@length) > 0) {
  #   l <- x@label
  # }

  if (!is.null(dots$label)) {
    l <- dots$label
    dots$label <- NULL
  }

  if (is.null(dots$linewidth)) {
    lw <- .5
  } else {
    lw <- dots$linewidth
    dots$linewidth <- NULL
  }

  if (is.null(dots$arrowhead_length)) {
    ahl <- 7
  } else {
    ahl <- dots$arrowhead_length
    dots$arrowhead_length <- NULL
  }



  rlang::inject(ob_bezier(p = p,
          label = l,
          label_sloped = FALSE,
          arrow_head = arrow_head,
          arrow_fins = arrow_fins,
          resect = resect,
          linewidth = lw,
          arrowhead_length = ahl,
          !!!dots))
}

#' @name ob_covariance
#' @export
#' @rdname ob_covariance
#' @examples
#' ggdiagram() +
#'   {x <- ob_circle(ob_point(c(-2, 2), 0))} +
#'   ob_covariance(x = x[1],
#'                 y = x[2],
#'                 label = ob_label("A"))
#'
#' ggdiagram() +
#'   x +
#'   ob_covariance(x = x[1],
#'                 y = x[2],
#'                 label = ob_label("A"),
#'                 where = -45,
#'                 looseness = .75)
S7::method(ob_covariance, list(centerpoint, centerpoint)) <- function(
    x,
    y,
    where = NULL,
    bend = 0,
    looseness = 1,
    arrow_head = the$arrow_head,
    length_head = 7,
    length_fins = 7,
    resect = 2,
    ...) {
  if (!S7::S7_inherits(where, ob_angle) && !is.null(where)) where <- degree(where)
  if (!S7::S7_inherits(bend, ob_angle)) bend <- degree(bend)

  p <- purrr::pmap(
    .l = list(
      xx = unbind(x),
      yy = unbind(y),
      bb = unbind(bend)
      ),
    .f = \(xx, yy, bb) {
      if (is.null(where)) {
        d_xy <- yy@center - xx@center
        x_angle <- d_xy@theta + degree(45)
        y_angle <- degree(135) + (d_xy@theta)
        } else {
          x_angle <- where
          y_angle <- degree(180) - where
          }
      s <- xx@point_at(x_angle)
      e <- yy@point_at(y_angle)
      m_dist <- looseness * (s - e)@r / 2

      bind(c(
        s,
        rotate(
          xx@normal_at(theta = x_angle, distance = m_dist),
          theta = bb,
          origin = s
          ),
        rotate(
          yy@normal_at(theta = y_angle, distance = m_dist),
          theta = bb * -1,
          origin = e),
        e))
      })

  dots <- rlang::list2(...)
  l <- character(0)

  if (!is.null(dots$label)) {
    l <- dots$label
    if (!S7::S7_inherits(l, ob_label)) l <- ob_label(l)
    dots$label <- NULL
    }

  rlang::inject(ob_bezier(p = p,
                        label = l,
                        label_sloped = FALSE,
                        arrow_head = arrow_head,
                        arrow_fins = arrow_head,
                        length_head = length_head,
                        length_fins = length_fins,
                        resect = resect,
                        !!!dots))
}

# Placing ----
S7::method(place, list(centerpoint, centerpoint)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p <- from@point_at(where)
  p_sep <- ob_polar((p - from@center)@theta, sep)

  xp <- x@point_at(where + degree(180)) - x@center
  x@center <- (p + p_sep) - xp
  if (S7::S7_inherits(x@label)) x@label@center <- x@center
  x
}


S7::method(place, list(ob_point, centerpoint)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p <- from@point_at(where)
  p_sep <- ob_polar((p - from@center)@theta, sep)
  x@x <- p@x + p_sep@x
  x@y <- p@y + p_sep@y
  x

}

S7::method(place, list(centerpoint, ob_point)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p_sep <- ob_polar(where, sep)
  p <- x@center - x@point_at(where + degree(180))
  x@center@x <- from@x + p@x + p_sep@x
  x@center@y <- from@y + p@y + p_sep@y
  if (S7::S7_inherits(x@label)) x@label@center <- x@center
  x

}

S7::method(place, list(ob_line, ob_ellipse)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p1 <- from@point_at(where)
  p2 <- from@normal_at(where, distance = sep)
  p3 <- rotate(p1, theta = degree(90), origin = p2)
  ob_segment(p2, p3)@line
}



S7::method(ob_array, ob_ellipse) <- function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {

  sa <- ob_array_helper(x = x, k = k, sep = sep, where = where, anchor = anchor, ...)

  rlang::inject(ob_ellipse(sa$p_center,
                        a = x@a,
                        b = x@b,
                        m1 = x@m1,
                        m2 = x@m2,
                        angle = x@angle,
                        style = x@style,
                        !!!sa$dots))
}

#' @export
`[<-.ggdiagram::shape` <- function(x, i, value) {
  .fn <- S7::S7_class(x)
  if (!S7::S7_inherits(value, .fn)) stop("Replacement value must be of the same type")

  i <- character_index(i, x@id)


  d <- x@tibble |>
    dplyr::bind_rows(dplyr::filter(value@tibble, FALSE))
  d[i,] <- value@tibble


  z <- data2shape(d, S7::S7_class(x))
  z@label <- x@label

  # If original object x has no label and value does,
  # then z needs blank labels
  if (length(z@label) == 0) {
    if (length(value@label) > 0) {
      z@label <- ob_label("", center = z@center)
    }
  } else {
    # if object x has a label, but value does not,
    # then value needs blank labels
    if (length(value@label) == 0) {
      value@label <- ob_label("", center = value@center)
    }
  }


  z@label[i] <- value@label
  if (S7::S7_inherits(z@label, ob_label)) {
    z@label@center <- z@center
  }


  if (length(x@id) > 0) {
    if (length(value@id) > 0) {
      x@id[i] <- value@id
    }
    z@id <- x@id
  }
  if ("angle" %in% colnames(d)) {
    z@angle[i] <- value@angle
  }
  z
}
