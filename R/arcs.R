arc_styles <- c(
  "alpha",
  "arrow_head",
  "arrow_fins",
  "arrowhead_length",
  "length_head",
  "length_fins",
  "color",
  "fill",
  "lineend",
  "linejoin",
  "linewidth",
  "linewidth_fins",
  "linewidth_head",
  "linetype",
  "n",
  "resect",
  "resect_fins",
  "resect_head",
  "stroke_color",
  "stroke_width"
)

wedge_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype"
)

wedge_aesthetics <- class_aesthetics_list(
  geom = ggplot2::geom_polygon,
  mappable_bare = character(0),
  mappable_identity = c(
    "color",
    "fill",
    "linewidth",
    "linetype",
    "alpha"),
  not_mappable = c(
    character(0)
  ),
  required_aes = c(
    "x",
    "y",
    "group"),
  omit_names = c(
    "rule",
    "label",
    "arrow_head",
    "arrow_fins"),
  inherit.aes = FALSE,
  style = wedge_styles
)

# cat(paste0(arc_styles, ' = ', arc_styles, collapse = ",\n"))

arc_props <- list(
  # primary ----
  primary = list(
    radius = new_property(class = class_numeric, default = 1),
    start = new_property(class = ob_angle_or_numeric, default = 0),
    end = new_property(class = ob_angle_or_numeric, default = 0)
  ),
  styles = ob_style@properties[arc_styles],
  extra = list(
    wedge = new_property(class = class_logical)
  ),
  # derived ----
  derived = list(
    apothem = new_property(getter = function(self) {
      self@radius - self@sagitta
    }),
    sagitta = new_property(getter = function(self) {
      l <- self@chord@distance
      r <- self@radius
      r - sqrt(r ^ 2 - (l / 2) ^ 2)
    }),
    bounding_box = new_property(getter = function(self) {

      d_rect <- self@tibble |>
        dplyr::mutate(d = purrr::pmap(
          list(x0 = x0,
               y0 = y0,
               r = r,
               start = start,
               end = end,
               n = n), \(x0,y0,r,start,end, n) {
                 theta <- seq(start, end, length.out = n)
                 d <- tibble::tibble(
                   x = x0 + cos(theta) * r,
                   y = y0 + sin(theta) * r
                 )

                 if (self@wedge) d <- dplyr::add_row(d, x = x0, y = y0)

                   dplyr::summarise(d, xmin = min(x),
                                    xmax = max(x),
                                    ymin = min(y),
                                    ymax = max(y))
               })) |>
        tidyr::unnest(d) |>
        dplyr::summarise(xmin = min(xmin),
                         xmax = max(xmax),
                         ymin = min(ymin),
                         ymax = max(ymax))

      ob_rectangle(southwest = ob_point(d_rect$xmin, d_rect$ymin),
                northeast = ob_point(d_rect$xmax, d_rect$ymax))

    }),
    chord = new_property(getter = function(self) {
      ob_segment(self@midpoint(0), self@midpoint(1), style = self@style)
    }),
    length = new_property(
      getter = function(self) {
        length(self@radius)
      }
    ),
    polygon = new_property(getter = function(self) {
      d <- self@tibble
      if (!("n" %in% colnames(d))) {
        d$n <- 360
      }
      d |>
        dplyr::mutate(group = factor(dplyr::row_number())) |>
        dplyr::mutate(xy = purrr::pmap(
          list(x0, y0, r, start, end, n),
          \(X0, Y0, R, START, END, N) {
            THETA <- seq(c(START), c(END), length.out = N)
            dd <- tibble::tibble(
              x = X0 + cos(THETA) * R,
              y = Y0 + sin(THETA) * R)
            if (self@wedge) {
              dd <- dplyr::bind_rows(
                dd,
                tibble(x = X0,
                       y = Y0))}
            dd
            })) |>
        tidyr::unnest(xy) |>
        dplyr::select(-c(x0, y0, r, start, end, n))
    }),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(arc_styles,
          prop,
          object = self
        ) |>
          `names<-`(arc_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        ob_point(self@x, self@y, style = self@style + value)
      }
    ),
    theta = new_property(getter = function(self) {
      self@end - self@start
    }),
    tibble = new_property(getter = function(self) {
      if (self@wedge) {
        d <- list(
          x0 = self@center@x,
          y0 = self@center@y,
          r = self@radius,
          start = c(self@start) * 2 * pi,
          end = c(self@end) * 2 * pi,
          alpha = self@alpha,
          color = self@color,
          fill = self@fill,
          linewidth = self@linewidth,
          linetype = self@linetype,
          n = self@n
        )

      } else {
      d <- list(
        x0 = self@center@x,
        y0 = self@center@y,
        r = self@radius,
        start = c(self@start) * 2 * pi,
        end = c(self@end) * 2 * pi,
        alpha = self@alpha,
        arrow_head = self@arrow_head,
        arrow_fins = self@arrow_fins,
        arrowhead_length = self@arrowhead_length,
        length_head = self@length_head,
        length_fins = self@length_fins,
        color = self@color,
        fill = self@fill,
        lineend = self@lineend,
        linejoin = self@linejoin,
        linewidth = self@linewidth,
        linewidth_fins = self@linewidth_fins,
        linewidth_head = self@linewidth_head,
        linetype = self@linetype,
        n = self@n,
        resect = self@resect,
        resect_fins = self@resect_fins,
        resect_head = self@resect_head,
        stroke_color = self@stroke_color,
        stroke_width = self@stroke_width
      )
      }
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
    autolabel = new_property(class_function, getter = function(self) {
      \(label = as.character(degree(self@theta)),
        position = .5,
        polar_just_angle = (self@midpoint(position) - self@center)@theta,
        polar_just_distance = 1.4,
        ...) {
        mp <- midpoint(self, position = position, ...)
        ob_label(p = mp,
              label = label,
              polar_just = ob_polar(theta = polar_just_angle,
                                 r = polar_just_distance), ...)
      }
    }),
    midpoint = new_property(class_function, getter = function(self) {
      \(position = .5, ...) midpoint(self, position = position, ...)
    }),
    point_at = new_property(
      class_function,
      getter = function(self) {
        \(theta = degree(0), ...) ob_polar(theta = theta, r = self@radius, style = self@style, ...)
      }
    ),
    tangent_at = new_property(
      class = class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          x0 <- self@center@x
          y0 <- self@center@y
          x1 <- cos(theta) * self@radius + self@center@x
          y1 <- cos(theta) * self@radius + self@center@y
          ob_line(
            a = x1 - x0,
            b = y1 - y0,
            c = x0^2 - (x1 * x0) + y0^2 - (y1 * y0) - self@radius^2,
            style = self@style,
            ...
          )
        }
      }
    )
  ),
  # info ----
  info = list(aesthetics = new_property(
    getter = function(self) {
      class_aesthetics_list(
        geom = ggarrow::geom_arrow,
        mappable_bare = character(0),
        mappable_identity = c(
          "color",
          "linewidth",
          "linetype",
          "alpha"),
        not_mappable = c(
          "n",
          "lineend",
          "linejoin",
          "arrow_head",
          "arrow_fins",
          "length",
          "length_head",
          "length_fins",
          "length_mid",
          "resect",
          "resect_fins",
          "resect_head",
          "linemitre"
        ),
        required_aes = c(
          "x",
          "y",
          "group"),
        omit_names = c(
          "linejoin",
          "rule",
          "x0",
          "y0",
          "r",
          "start",
          "end",
          "label"),
        inherit.aes = FALSE,
        style = arc_styles
      )
    }
  ))
)


# ob_arc----

#' ob_arc class
#'
#' Create arcs and wedges
#' @param center point at center of the arc (default = ob_point(0,0))
#' @param radius distance between center and edge arc (default = 1)
#' @param start start angle (default = 0 degrees)
#' @param end end angle (default = 0 degrees)
#' @param label A character, angle, or label object
#' @param start_point Specify where arc starts. Overrides `@center`
#' @param end_point Specify where arc ends Overrides `@center`
#' @param n number of points in arc (default = 360)
#' @param wedge Draw a wedge instead of an arc when `TRUE`
#' @param style a style object
#' @param x0 x-coordinate of center point. If specified, overrides x-coordinate of `@center`.
#' @param y0 x-coordinate of center point. If specified, overrides y-coordinate of `@center`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object
#' @inherit ob_style params
#' @slot aesthetics A list of information about the arc's aesthetic properties
#' @slot angle_at A function that finds the angle of the specified point in relation to the arc's center
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to `ggarrow::geom_arrow`.
#' @slot length The number of arcs in the arc object
#' @slot point_at A function that finds a point on the arc at the specified angle.
#' @slot tangent_at A function that finds the tangent line at the specified angle.
#' @slot theta interior angle (end - start)
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggarrow::geom_arrow`.
#' @examples
#' library(ggplot2)
#'
#' # center point
#' p_center <- ob_point(0,0)
#'
#' # 90-degree arc
#' a_90 <- ob_arc(
#'  center = p_center,
#'  radius = 6,
#'  start = degree(0),
#'  end = degree(90)
#'  )
#'
#'  # Print arc
#'  a
#'
#' # Plot arc and its center point
#' ggplot() + coord_equal() + theme_minimal() +
#'  p_center +
#'  a_90
#' @export
ob_arc <- new_class(
  name = "ob_arc",
  parent = centerpoint,
  properties = rlang::inject(
    list(
      !!!arc_props$primary,
      !!!arc_props$extra,
      !!!arc_props$styles,
      !!!arc_props$derived,
      !!!arc_props$funs,
      !!!arc_props$info
    )
  ),
  constructor = function(center = ob_point(0,0),
                         radius = 1,
                         start = 0,
                         end = 0,
                         label = class_missing,
                         start_point = class_missing,
                         end_point = class_missing,
                         n = 360,
                         wedge = FALSE,
                         alpha = class_missing,
                         arrow_head = class_missing,
                         arrow_fins = class_missing,
                         arrowhead_length = class_missing,
                         length_head = class_missing,
                         length_fins = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         lineend = class_missing,
                         linejoin = class_missing,
                         linewidth = .25,
                         linewidth_fins = class_missing,
                         linewidth_head = class_missing,
                         linetype = class_missing,
                         resect = class_missing,
                         resect_fins = class_missing,
                         resect_head = class_missing,
                         stroke_color = class_missing,
                         stroke_width = class_missing,
                         style = class_missing,
                         x0 = class_missing,
                         y0 = class_missing,
                         ...) {

    if (!S7_inherits(start, ob_angle)) {
      start <- degree(start)
    }

    if (!S7_inherits(end, ob_angle)) {
      end <- degree(end)
    }

    if (length(x0) > 0 | length(y0) > 0) {
      if (length(x0) == 0) {
        x0 <- 0
      }
      if (length(y0) == 0) {
        y0 <- 0
      }
      center <- ob_point(tibble::tibble(x = x0, y = y0))
    }




    if (S7_inherits(start_point, ob_point)) {
      c1 <- ob_circle(radius = radius)
      p1 <- c1@point_at(start)
      center <- start_point - p1
    } else if (S7_inherits(end_point, ob_point)) {
      c1 <- ob_circle(radius = radius)
      p2 <- c1@point_at(end)
      center <- end_point - p2
    }




    arc_style <- center@style + style +
      ob_style(
        alpha = alpha,
        arrow_head = arrow_head,
        arrow_fins = arrow_fins,
        arrowhead_length = arrowhead_length,
        length_head = length_head,
        length_fins = length_fins,
        color = color,
        fill = fill,
        lineend = lineend,
        linejoin = linejoin,
        linewidth = linewidth,
        linewidth_fins = linewidth_fins,
        linewidth_head = linewidth_head,
        linetype = linetype,
        n = n,
        resect = resect,
        resect_fins = resect_fins,
        resect_head = resect_head,
        stroke_color = stroke_color,
        stroke_width = stroke_width
      ) +
      ob_style(...)





    if (is.character(label) || S7_inherits(label, ob_angle)) {
      label <- ob_label(label = label)
    }



    non_empty_list <- get_non_empty_props(arc_style)
    d <- tibble::tibble(
      x0 = center@x,
      y0 = center@y,
      radius = radius,
      start = c(start),
      end = c(end)
    )
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_arc")



    if (S7_inherits(label, ob_label)) {
      if (all(label@p@x == 0) && all(label@p@y == 0)) {
        m <- start + ((end - start) * label@position)
        label@p <- center + ob_polar(theta = m, r = radius)
        if (all(length(label@hjust) == 0)) {
          label@hjust <- polar2just(m, 1.4, axis = "h")
        }

        if (all(length(label@vjust) == 0)) {
          label@vjust <- polar2just(m, 1.4, axis = "v")
        }



      }

    }

    center = set_props(center, x = d$x0, y = d$y0)
    center@style <- arc_style

    if (S7_inherits(start, degree)) {
      start <- degree(d$start * 360)
    } else if (S7_inherits(start, radian)) {
      start <- radian(d$start * 2 * pi)
    } else {
      start <- turn(d$start)
    }

    if (S7_inherits(end, degree)) {
      end <- degree(d$end * 360)
    } else if (S7_inherits(end, radian)) {
      end <- radian(d$end * 2 * pi)
    } else {
      end <- turn(d$end)
    }





    new_object(
      centerpoint(center = center, label = label),
      radius = d$radius,
      start = start,
      end = end,
      wedge = wedge,
      alpha = d[["alpha"]] %||% alpha,
      arrow_head = d[["arrow_head"]] %||% arrow_head,
      arrow_fins = d[["arrow_fins"]] %||% arrow_fins,
      arrowhead_length = d[["arrowhead_length"]] %||% arrowhead_length,
      length_head = d[["length_head"]] %||% length_head,
      length_fins = d[["length_fins"]] %||% length_fins,
      color = d[["color"]] %||% color,
      fill = d[["fill"]] %||% fill,
      lineend = d[["lineend"]] %||% lineend,
      linejoin = d[["linejoin"]] %||% linejoin,
      linewidth = d[["linewidth"]] %||% linewidth,
      linewidth_fins = d[["linewidth_fins"]] %||% linewidth_fins,
      linewidth_head = d[["linewidth_head"]] %||% linewidth_head,
      linetype = d[["linetype"]] %||% linetype,
      n = d[["n"]] %||% n,
      resect = d[["resect"]] %||% resect,
      resect_fins = d[["resect_fins"]] %||% resect_fins,
      resect_head = d[["resect_head"]] %||% resect_head,
      stroke_color = d[["stroke_color"]] %||% stroke_color,
      stroke_width = d[["stroke_width"]] %||% stroke_width
    )
  }
)


method(str, ob_arc) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","radius", "start", "end", "theta"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

method(as.geom, ob_arc) <- function(x, ...) {

  d <- get_tibble_defaults(x)
  if ("arrowhead_length" %in% colnames(d)) {
    d <- dplyr::rename(d, length = arrowhead_length)
  }

  d <- d |>
    dplyr::mutate(group = factor(dplyr::row_number())) |>
    dplyr::mutate(xy = purrr::pmap(list(x0, y0, r, start, end, n),
                                   \(X0, Y0, R, START, END, N) {
    THETA <- seq(c(START), c(END), length.out = N)
    dd <- tibble::tibble(
      x = X0 + cos(THETA) * R,
      y = Y0 + sin(THETA) * R
    )

    if (x@wedge) {
      dd <- dplyr::bind_rows(
        dd,
        tibble(x = X0,
               y = Y0)
      )

    }
    dd
  })) |>
  tidyr::unnest(xy) |>
  dplyr::select(-c(x0, y0, r, start, end, n))

overrides <- get_non_empty_props(ob_style(...))

  if (all(x@wedge == TRUE)) {
    arc_aesthetics <- wedge_aesthetics
  } else {
    if (!("arrow_head" %in% c(colnames(d), names(overrides)))) {
      overrides$arrow_head <- ggarrow::arrow_head_minimal(90)
    }
    arc_aesthetics <- x@aesthetics
  }



  gc <- make_geom_helper(
    d = d,
    user_overrides = overrides,
    aesthetics = arc_aesthetics)

  if (S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gc <- list(gc, gl)
  }
  gc
}

method(get_tibble, ob_arc) <- function(x) {
  x@tibble
}


method(get_tibble_defaults, ob_arc) <- function(x) {
  sp <- ob_style(
    alpha = replace_na(as.double(ggarrow::GeomArrow$default_aes$alpha), 1),
    arrow_head = ggarrow::arrow_head_minimal(90),
    arrow_fins = ggarrow::arrow_fins_minimal(90),
    color = replace_na(ggarrow::GeomArrow$default_aes$colour, "black"),
    stroke_color = replace_na(ggarrow::GeomArrow$default_aes$colour, "black"),
    stroke_width = replace_na(ggarrow::GeomArrow$default_aes$colour, 0.25),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggarrow::GeomArrow$default_aes$linewidth, .5),
    linewidth_head = replace_na(ggarrow::GeomArrow$default_aes$linewidth, 1),
    linewidth_fins = replace_na(ggarrow::GeomArrow$default_aes$linewidth, 1),
    linetype = replace_na(ggarrow::GeomArrow$default_aes$linetype, 1),
    n = 360
  )
  get_tibble_defaults_helper(x, sp,required_aes = c("x0", "y0", "r", "start", "end", "group"))
}

method(
  midpoint,
  list(ob_arc, class_missing)) <- function(x,y, position = .5, ...) {
  m <- x@start@turn + (x@theta@turn * position)
  x@center + ob_polar(
    theta = turn(m),
    r = x@radius,
    style = x@style + ob_style(...))
  }

method(`[`, ob_arc) <- function(x, y) {
  d <- as.list(x@tibble[y,] |>
                 dplyr::rename(radius = r))
  z <- rlang::inject(ob_arc(!!!d))
  z@start <- x@start[y]
  z@end <- x@end[y]
  z@label <- x@label[y]
  z@wedge <- x@wedge
  z
}


# method(variance, centerpoint) <- function(
#     x,
#     where = "north",
#     theta = 50,
#     bend = 0,
#     label = NULL,
#     looseness = 1,
#     nudge = 0,
#     arrow_head = arrowheadr::arrow_head_deltoid(),
#     arrow_fins = arrowheadr::arrow_head_deltoid(),
#     resect = 1,
#     linewidth = .5,
#     arrowhead_length = unit(10, "pt"),
#     ...) {
#   if (!S7_inherits(where, ob_angle)) where <- degree(where)
#   if (!S7_inherits(theta, ob_angle)) theta <- degree(theta)
#   if (!S7_inherits(bend, ob_angle)) bend <- degree(bend)
#
#
#   angle_start <- where - (theta / 2)
#   angle_end <- where + (theta / 2)
#   p_start <- x@point_at(angle_start)
#   p_end <- x@point_at(angle_end)
#   p_midpoint <- midpoint(p_start, p_end)
#
#   tangent_start <- x@tangent_at(angle_start)
#   tangent_end <- x@tangent_at(angle_end)
#
#   c_p <- tangent_start@a * tangent_end@b - tangent_end@a * tangent_start@b
#   a_p <- (tangent_start@b * tangent_end@c - tangent_end@b * tangent_start@c)
#   b_p <- (tangent_start@c * tangent_end@a - tangent_end@c * tangent_start@a)
#   xx <- ifelse(c_p == 0, p_midpoint@x, a_p / c_p)
#   yy <- ifelse(c_p == 0, p_midpoint@y, b_p / c_p)
#
#   p_center <- ob_point(xx, yy) + ob_polar(where, nudge)
#   v_circle <- ob_circle(p_center,
#                      radius = distance(p_start, p_center))
#   v_start <- v_circle@angle_at(p_start)
#   v_end <- v_circle@angle_at(p_end)
#   v_end <- v_end + turn((c(v_end) < c(v_start)) * 1)
#
#   ob_arc(
#     center = p_center,
#     radius = v_circle@radius,
#     end = v_end,
#     start = v_start,
#     resect = resect,
#     arrow_head = arrow_head,
#     arrow_fins = arrow_fins,
#     arrowhead_length = arrowhead_length,
#     linewidth = linewidth,
#     label = label,
#     ...
#   )
#
#
# }

# ob_wedge ----

#' ob_wedge
#' @rdname ob_arc
#' @export
ob_wedge <- redefault(ob_arc, wedge = TRUE, color = NA, fill = "black")

# circle_pie ----

#' circle_pie
#'
#' pie wedges
#' @param center center point of pie
#' @param radius length of circle radius
#' @param start angle at which pie wedges start
#' @param proportion_list list of proportions
#' @param color_list color of wedge borders
#' @param fill_list color of wedge fills
circle_pie <- new_class(
  name = "circle_pie",
  parent = centerpoint,
  properties = list(
    radius = class_numeric,
    start = new_property(class = ob_angle_or_numeric, default = 0),
    proportion_list = class_list,
    color_list = class_list,
    fill_list = class_list
  ),
  constructor = function(
    center = ob_point(0,0),
    radius = 1,
    proportion_list = list(c(.5,.5)),
    start = degree(0)) {
    if (!S7_inherits(start, ob_angle)) start <- degree(start)

    new_object(parent = centerpoint(center = center),
               radius = radius,
               start = start,
               proportion_list = proportion_list,
               color_list = color_list,
               fill_list = fill_list
               )

  }
                        )
