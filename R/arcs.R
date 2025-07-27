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
    "arrow_fins",
    "id"),
  inherit.aes = FALSE,
  style = wedge_styles
)

arc_props <- list(
  # primary ----
  primary = list(
    radius = S7::new_property(class = S7::class_numeric, default = 1),
    start = S7::new_property(class = ob_angle_or_numeric, default = 0),
    end = S7::new_property(class = ob_angle_or_numeric, default = 0)
  ),
  styles = ob_style@properties[arc_styles],
  extra = list(
    label_sloped = S7::class_logical,
    type = S7::new_property(class = S7::class_character, validator = function(value) {
      if (length(value) != 1) return("The type property must be of length 1.")
      if (!(value %in% c("arc", "wedge", "segment"))) 'The type property must be "arc", "wedge", or "segment".'
    })
  ),
  # derived ----
  derived = list(
    apothem = S7::new_property(getter = function(self) {
      ob_segment(p1 = self@center,
                 p2 = self@chord@midpoint(),
                 style = self@style)
    }),
    arc_length = S7::new_property(getter = function(self) {
      abs(self@radius) * abs(self@theta@radian)
    }),
    sagitta = S7::new_property(getter = function(self) {
      ob_segment(p1 = self@chord@midpoint(),
                 p2 = self@midpoint(),
                 style = self@style)
    }),
    bounding_box = S7::new_property(getter = function(self) {

      d_rect <- self@tibble |>
        dplyr::mutate(d = purrr::pmap(
          list(x = x,
               y = y,
               r = r,
               start = start,
               end = end,
               n = n), \(x,y,r,start,end, n) {
                 theta <- seq(start, end, length.out = n)
                 d <- tibble::tibble(
                   x = x + cos(theta) * r,
                   y = y + sin(theta) * r
                 )

                 if (self@type == "wedge") d <- dplyr::add_row(d, x = x, y = y)

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
    circle = S7::new_property(getter = \(self) {
      ob_circle(center = self@center,
                radius = self@radius,
                label = self@label,
                style  = self@style)
    }),
    chord = S7::new_property(getter = \(self) {
      ob_segment(self@start_point, self@end_point, style = self@style)
    }),
    length = S7::new_property(
      getter = \(self) {
        length(self@radius)
      }
    ),
    end_point = S7::new_property(
      getter = \(self) {
        self@midpoint(1)
      },
      setter = \(self, value) {
        if (S7::S7_inherits(value, ob_point)) {
          if (value@length == self@length) {
            self@center <- value - self@center
          } else {
            stop(paste0(
              "The number of points in end_point  (",
              start_point@length,
              ") differs from the number of ",
              self@type,
              ,"s (",
              self@length,
              ,")."))
          }
        } else stop("end_point must be of class ob_point or ob_polar")
        self
      }),
    polygon = S7::new_property(getter = function(self) {
      d <- self@tibble
      if (!("n" %in% colnames(d))) {
        d$n <- 360
      }
      d |>
        dplyr::mutate(group = factor(dplyr::row_number())) |>
        dplyr::mutate(xy = purrr::pmap(
          list(x, y, radius, start, end, n, type),
          \(X, Y, R, START, END, N, TYPE) {
            THETA <- seq(c(START), c(END), length.out = N)
            dd <- tibble::tibble(
              x = X + cospi(THETA / 180) * R,
              y = Y + sinpi(THETA / 180) * R)
            if (TYPE == "wedge") {
              dd <- dplyr::bind_rows(
                dd,
                tibble(x = X,
                       y = Y))}
            dd
            })) |>
        dplyr::select(-x,-y) |>
        tidyr::unnest(xy) |>
        dplyr::select(-c(radius, start, end, n, type))
    }),
    start_point = S7::new_property(
      getter = function(self) {
        self@midpoint(0)
        },
      setter = function(self, value) {
        if (S7::S7_inherits(value, ob_point)) {
          if (value@length == self@length) {
            self@center <- value - self@center
            } else {
              stop(paste0(
                "The number of points in start_point  (",
                start_point@length,
                ") differs from the number of ",
                self@type,
                "s (",
                self@length,
                ")."))
        }
      } else stop("start_point must be of class ob_point or ob_polar")
        self
    }),
    style = S7::new_property(
      getter = function(self) {
        pr <- purrr::map(arc_styles,
          prop,
          object = self
        ) |>
          `names<-`(arc_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% arc_styles]
        self <- rlang::inject(S7::set_props(self, !!!s_list))
        self
      }
    ),
    theta = S7::new_property(getter = function(self) {
      self@end - self@start
    }),
    tibble = S7::new_property(getter = function(self) {
      if (self@type != "arc") {
        d <- list(
          x = self@center@x,
          y = self@center@y,
          radius = self@radius,
          start = c(self@start) * 360,
          end = c(self@end) * 360,
          alpha = self@alpha,
          color = self@color,
          fill = self@fill,
          linewidth = self@linewidth,
          linetype = self@linetype,
          n = self@n,
          type = self@type,
          id = self@id
        )

      } else {
      d <- list(
        x = self@center@x,
        y = self@center@y,
        radius = self@radius,
        start = c(self@start) * 360,
        end = c(self@end) * 360,
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
        stroke_width = self@stroke_width,
        type = self@type
      )
      }
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
    angle_at = ob_circle@properties$angle_at,
    autolabel = S7::new_property(S7::class_function, getter = function(self) {
      \(label = as.character(degree(self@theta)),
        position = .5,
        polar_just_angle = (self@midpoint(position) - self@center)@theta,
        polar_just_distance = 1.4,
        ...) {
        mp <- midpoint(self, position = position, ...)
        ob_label(
          center = mp,
          label = label,
          polar_just = ob_polar(theta = polar_just_angle, r = polar_just_distance),
          ...
        )
      }
    }),
    hatch = S7::new_property(S7::class_function, getter = function(self) {
      \(k = 1, sep = .05, height = .1, position = .5, ...) {
        h <- map_ob(self, \(s) {
          m <- s@midpoint(position = position)
          m_theta <- (m - s@center)@theta
          theta_sep <- sep / s@radius
          theta_width <- c(theta_sep) * (k - 1)
          h_theta <- turn(seq(0,theta_width, length.out = k) - theta_width / 2) + m_theta


          p_top <- s@normal_at(h_theta, distance = height / 2)
          p_bottom <- s@normal_at(h_theta, distance = height / -2)
          ob_segment(p_top, p_bottom, style = s@style)
        })

        h@style <- h@style + ob_style(...)
        h

      }}),
    midpoint = S7::new_property(S7::class_function, getter = function(self) {
      \(position = .5, ...) {
        m <- self@start@turn + (self@theta@turn * position)
        self@center + ob_polar(
          theta = turn(m),
          r = self@radius,
          style = self@style + ob_style(...))
      }

    }),
    normal_at = ob_circle@properties$normal_at,
    place = pr_place,
    point_at = ob_circle@properties$point_at,
    tangent_at = ob_circle@properties$tangent_at
  ),
  # info ----
  info = list(aesthetics = S7::new_property(
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
          "label",
          "label_sloped"),
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
#' @param center point at center of the arc (default = `ob_point(0,0)`)
#' @param radius distance between center and edge arc (default = 1)
#' @param start start angle. Can be numeric (degrees), [degree], [radian], [turn], or named direction (e.g., "northwest", "east", "below", "left"). Defaults to 0.
#' @param end end angle Can be numeric (degrees), [degree], [radian], [turn], or named direction (e.g., "northwest", "east", "below", "left"). Defaults to 0.
#' @param label A character, angle, or label object
#' @param label_sloped If TRUE, label runs along arc.
#' @param start_point Specify where arc starts. Overrides `@center`
#' @param end_point Specify where arc ends Overrides `@center`
#' @param n number of points in arc (default = 360)
#' @param type Type of object to drawn. Can be "arc", "wedge", or "segment"
#' @param style an [`ob_style`] object
#' @param x x-coordinate of center point. If specified, overrides x-coordinate of `@center`.
#' @param y x-coordinate of center point. If specified, overrides y-coordinate of `@center`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style object
#' @inherit ob_style params
#' @slot aesthetics A list of information about the arc's aesthetic properties
#' @slot angle_at A function that finds the angle of the specified point in relation to the arc's center
#' @slot apothem Distance from center to the chord's midpoint
#' @slot arc_length Distance along arc from `start_point` to `end_point`
#' @slot auto_label Places a label at the arc's midpoint
#' @slot chord [`ob_segment`] from `start_point` to `end_point`
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to [`ggarrow::geom_arrow`].
#' @slot hatch A function that puts hatch (tally) marks on arcs. Often used to indicate which arcs have the same angle. The `k` parameter controls how many hatch marks to display. The `height` parameter controls how long the hatch mark segment is. The `sep` parameter controls the separation between hatch marks when `k > 2`. Additional parameters sent to [`ob_segment`].
#' @slot length The number of arcs in the arc object
#' @slot midpoint A function that selects 1 or more midpoints of the ob_arc. The `position` argument can be between 0 and 1. Additional arguments are passed to [`ob_point`].
#' @slot point_at A function that finds a point on the arc at the specified angle.
#' @slot sagitta [`ob_segment`] from `chord` midpoint to [`ob_arc`] midpoint
#' @slot tangent_at A function that finds the tangent line at the specified angle.
#' @slot theta interior angle (end - start)
#' @slot tibble Gets a [tibble::tibble] or data.frame containing parameters and styles used by [`ggarrow::geom_arrow`].
#' @export
#' @returns ob_arc object
#' @examples
#' # 90-degree arc
#' ggdiagram() +
#'   ob_arc(
#'     radius = 6,
#'     start = degree(0),
#'     end = degree(90)
#'  )
ob_arc <- S7::new_class(
  name = "ob_arc",
  parent = centerpoint,
  properties = rlang::list2(
      !!!arc_props$primary,
      !!!arc_props$extra,
      !!!arc_props$styles,
      !!!arc_props$derived,
      !!!arc_props$funs,
      !!!arc_props$info
    ),
  constructor = function(center = ob_point(0,0),
                         radius = 1,
                         start = 0,
                         end = 0,
                         label = character(0),
                         label_sloped = FALSE,
                         start_point = S7::class_missing,
                         end_point = S7::class_missing,
                         n = 360,
                         type = "arc",
                         alpha = numeric(0),
                         arrow_head = list(),
                         arrow_fins = list(),
                         arrowhead_length = numeric(0),
                         length_head = numeric(0),
                         length_fins = numeric(0),
                         color = character(0),
                         fill = character(0),
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
                         x = numeric(0),
                         y = numeric(0),
                         id = character(0),
                         ...) {
    id <- as.character(id)

    if (!S7::S7_inherits(start, ob_angle)) {
      start <- degree(start)
    }

    if (!S7::S7_inherits(end, ob_angle)) {
      end <- degree(end)
    }

    if (length(x) > 0 | length(y) > 0) {
      if (length(x) == 0) {
        x <- 0
      }
      if (length(y) == 0) {
        y <- 0
      }
      center <- ob_point(tibble::tibble(x = x, y = y))
    }




    if (S7::S7_inherits(start_point, ob_point)) {
      c1 <- ob_circle(radius = radius)
      p1 <- c1@point_at(start)
      center <- start_point - p1
    } else if (S7::S7_inherits(end_point, ob_point)) {
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
        stroke_width = stroke_width,
        id = id
      ) +
      ob_style(...)



    if (length(label) > 0) {

    if (is.character(label) || S7::S7_inherits(label, ob_angle)) {
      label <- ob_label(label = label)
    }
}


    non_empty_list <- get_non_empty_props(arc_style)
    d <- tibble::tibble(
      x = center@x,
      y = center@y,
      radius = radius,
      start = c(start),
      end = c(end)
    )
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }



      label <- centerpoint_label(label,
                                 center = label@center,
                                 d = d,
                                 shape_name = "ob_arc")

      # If there is one object but many labels, make multiple objects
      if (S7::S7_inherits(label, ob_label)) {
        if (label@length > 1 & nrow(d) == 1) {
          d <- dplyr::mutate(d, k = label@length) |>
            tidyr::uncount(.data$k)
        }
      }


    center = set_props(center, x = d$x, y = d$y)
    center@style <- arc_style

    if (S7::S7_inherits(start, degree)) {
      start <- degree(d$start * 360)
    } else if (S7::S7_inherits(start, radian)) {
      start <- radian(d$start * 2 * pi)
    } else {
      start <- turn(d$start)
    }

    if (S7::S7_inherits(end, degree)) {
      end <- degree(d$end * 360)
    } else if (S7::S7_inherits(end, radian)) {
      end <- radian(d$end * 2 * pi)
    } else {
      end <- turn(d$end)
    }

    if (S7::S7_inherits(label, ob_label)) {
      if (all(label@center@x == 0) && all(label@center@y == 0)) {
        m <- start + ((end - start) * label@position)
        label@center <- center + ob_polar(theta = m, r = radius)

        if (all(label_sloped)) {
          if (all(length(label@hjust) == 0)) {
            label@hjust <- 0.5
          }

          if (all(length(label@vjust) == 0)) {
            label@vjust <- 0.5
          }

        } else {
          if (all(length(label@hjust) == 0) & all(length(label@vjust) == 0)) {
            label@hjust <- polar2just(m, 1.4, axis = "h")
            label@vjust <- polar2just(m, 1.4, axis = "v")
          } else {
            if (all(length(label@hjust) == 0)) {
              label@hjust <- 0.5
            }

            if (all(length(label@vjust) == 0)) {
              label@vjust <- 0.5
            }

          }
        }


      }
    }

    S7::new_object(
      centerpoint(center = center, label = label),
      label_sloped = label_sloped,
      radius = d$radius,
      start = start,
      end = end,
      type = type[1],
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
      stroke_width = d[["stroke_width"]] %||% stroke_width,
      id = d[["id"]] %||% id
    )
  }
)


S7::method(str, ob_arc) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","radius", "start", "end", "theta"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

S7::method(as.geom, ob_arc) <- function(x, ...) {

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

    if (x@type == "wedge") {
      dd <- dplyr::bind_rows(
        dd,
        tibble::tibble(x = X0,
               y = Y0)
      )

    }
    dd
  })) |>
  tidyr::unnest(xy) |>
  dplyr::select(-c(x0, y0, r, start, end, n))

overrides <- get_non_empty_props(ob_style(...))

  if (all(x@type != "arc")) {
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

  if (S7::S7_inherits(x@label, ob_label)) {

    if (all(x@label_sloped)) {

      d <- x@polygon |>
        dplyr::select(group, x, y) |>
        tidyr::nest(.by = group) |>
        dplyr::bind_cols(x@label@tibble |>
                           dplyr::select(-c(x,y)))



      if (!("hjust" %in% colnames(d))) {
        d <- dplyr::mutate(d, hjust = x@label@position)
      }

      d <- tidyr::unnest(d, data)

      if ("size" %in% colnames(d)) {
        d <- dplyr::mutate(d, size = size / ggplot2::.pt)
      }

      if (!("boxcolour" %in% colnames(d))) {
        d <- dplyr::mutate(d, boxcolour = NA)
      }

      if (!("label.padding" %in% colnames(d))) {
        d <- dplyr::mutate(d, label.padding = unit(2, "pt"))
      }

      d <- dplyr::mutate(d, label.padding = purrr::map_dbl(label.padding, \(lp) c(lp[1] / 96)))



      gl <- make_geom_helper(d, aesthetics = gtextcurve_aes, user_overrides = NULL)
    } else {
      gl <- as.geom(x@label)
      }



    gc <- list(gc, gl)
  }
  gc
}

S7::method(get_tibble, ob_arc) <- function(x) {
  x@tibble |>
    dplyr::rename(r = radius) |>
    dplyr::mutate(start = pi * start / 180,
                  end = pi * end / 180) |>
    dplyr::select(-type)
}


S7::method(get_tibble_defaults, ob_arc) <- function(x) {
  sp <- ob_style(
    alpha = replace_na(as.double(ggarrow::GeomArrow$default_aes$alpha), 1),
    arrow_head = ggarrow::arrow_head_minimal(90),
    arrow_fins = ggarrow::arrow_fins_minimal(90),
    color = replace_na(ggarrow::GeomArrow$default_aes$colour, "black"),
    stroke_color = replace_na(ggarrow::GeomArrow$default_aes$colour, "black"),
    stroke_width = replace_na(ggarrow::GeomArrow$default_aes$stroke_width, 0.25),
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

S7::method(midpoint,list(ob_arc, S7::class_missing)) <- function(x,y, position = .5, ...) {
    x@midpoint(position = position, ...)
}

S7::method(`[`, ob_arc) <- function(x, i) {
  i <- character_index(i, x@id)
  z <- data2shape(x@tibble[i,], ob_arc)
  z@start <- x@start[i]
  z@end <- x@end[i]
  z@label <- na2zero(x@label[i])
  z@type <- x@type
  z
}

# ob_wedge ----

#' ob_wedge
#' @rdname ob_arc
#' @export
ob_wedge <- redefault(ob_arc, type = "wedge", color = NA, fill = "black")

# ob_circular_segment ----

#' ob_circular_segment
#' @rdname ob_arc
#' @export
ob_circular_segment <- redefault(ob_arc, type = "segment", color = NA, fill = "black")
