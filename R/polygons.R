ob_polygon_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype"
)

ob_polygon_aesthetics <- class_aesthetics_list(
  geom = ggforce::geom_shape,
  mappable_bare = character(0),
  mappable_identity = c(
    "color",
    "fill",
    "linewidth",
    "linetype",
    "alpha"),
  not_mappable = c(
    "radius"
  ),
  required_aes = c(
    "x",
    "y",
    "group"),
  omit_names = c(
    "rule",
    "label"),
  inherit.aes = FALSE,
  style = ob_polygon_styles
)


ob_polygon_props <- list(
  # primary ----
  primary = list(
    p = new_property(class = point_or_list, validator = function(value) {
      if ("list" %in% class(value)) {
        allsameclass(value, "ob_point")
      }

      if (S7_inherits(value, ob_point)) value <- list(value)

      chk_points <- purrr::imap_chr(value, \(x, idx) {
        if (x@length < 3) {
          paste0("Group ", idx, " needs at least 3 points. It has ", x@length, ". ")
        } else {
          ""
        }
      }) |>
        paste0(collapse = "")
      if (nchar(chk_points) > 0) chk_points

    })
  ),
  # extra ----
  extra = list(
    label = label_or_character_or_angle,
    vertex_radius = new_property(class = class_numeric_or_unit, validator = function(value) {
      if (length(value) > 1) stop("The vertex_radius property must be of length 1.")
    })
  ),
  styles = ob_style@properties[ob_polygon_styles],
  # derived ----
  derived = list(
    bounding_box = new_property(getter = function(self) {

      d_rect <- get_tibble(self) |>
        dplyr::summarise(xmin = min(x),
                         xmax = max(x),
                         ymin = min(y),
                         ymax = max(y))

      ob_rectangle(southwest = ob_point(d_rect$xmin, d_rect$ymin),
                northeast = ob_point(d_rect$xmax, d_rect$ymax))

    }),
    center = new_property(ob_point, getter = function(self) {
      d <- self@tibble
      gr <- dplyr::intersect(colnames(d), c("group", pt_styles))
      d <- dplyr::summarise(d, .by = dplyr::any_of(gr),
                       x = mean(x, na.rm = TRUE),
                       y = mean(y, na.rm = TRUE)) |>
        dplyr::select(-.data$group)

      rlang::inject(ob_point(!!!d))
    }),
    length = new_property(
      getter = function(self) {
        if ("list" %in% class(self@p)) {
          l <- length(self@p)
        } else l <- 1
        l
      }
    ),
    segment = new_property(getter = function(self) {
      self@p |>
        purrr::map(\(pp) {
          k <- pp@length
          ob_segment(pp[c(1:k, 1)])
        })
    }),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(ob_polygon_styles,
                         prop,
                         object = self
        ) |>
          `names<-`(ob_polygon_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        setter = function(self, value) {
          s <- self@style + value
          s_list <- get_non_empty_props(s)
          s_list <- s_list[names(s_list) %in% ob_polygon_styles]
          self <- rlang::inject(set_props(self, !!!s_list))
          self
        }}
    ),
    tibble = new_property(getter = function(self) {
      p <- self@p
      if (S7_inherits(self@p, ob_point)) p <- list(p)
      d <- list(
        p = p,
        group = seq(1, self@length),
        alpha = self@alpha,
        color = self@color,
        fill = self@fill,
        linewidth = self@linewidth,
        linetype = self@linetype,
        vertex_radius = self@vertex_radius
      )
      get_non_empty_tibble(d) |>
        dplyr::mutate(p = purrr::map(p, \(x) {x@tibble |> dplyr::select(x,y)})) |>
        tidyr::unnest(p)

    })
  ),
  # functions ----
  funs = list(
    geom = new_property(class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    })
  ),
  # info ----
  info = list(aesthetics = new_property(
    getter = function(self) {
      ob_polygon_aesthetics
    }
  ))
)


# ob_polygon ----

#' The ob_polygon class
#'
#' A polygon is specified with an ob_point that contains at least 3 points, the start and the end. Any number of intermediate points are possible.
#'
#' If you wish to specify multiple polygons, you must supply a list of ob_points. When plotted, the ob_polygon function uses the ggforce::geom_shape function to create the geom.
#' @export
#' @param p ob_point or list of ob_point objects
#' @param label A character, angle, or label object
#' @param vertex_radius A numeric or unit vector of length one, specifying the corner radius
#' @slot length The number of polygons in the ob_polygon object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @param style Gets and sets the styles associated with polygons
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggforce::geom_shape`.
#' @inherit ob_style params
ob_polygon <- new_class(
  name = "ob_polygon",
  parent = has_style,
  properties = rlang::inject(
    list(
      !!!ob_polygon_props$primary,
      !!!ob_polygon_props$extra,
      !!!ob_polygon_props$styles,
      !!!ob_polygon_props$derived,
      !!!ob_polygon_props$funs,
      !!!ob_polygon_props$info
    )
  ),
  constructor = function(p = class_missing,
                         label = character(0),
                         vertex_radius = numeric(0),
                         alpha = numeric(0),
                         color = character(0),
                         fill = character(0),
                         linewidth = numeric(0),
                         linetype = numeric(0),
                         style = class_missing,
                         ...) {



    ob_polygon_style <- style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(ob_polygon_style)

    if (S7_inherits(p, ob_point)) p <- list(p)

    d <- tibble::tibble(
      p = p
    )


    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }

    if (length(label) == 0) {
      label = character(0)
    } else {
      center <- dplyr::bind_rows(
        purrr::imap(p, \(pp, idx) {
          tibble::as_tibble(pp@xy) |>
            dplyr::mutate(group = idx)})) |>
        dplyr::bind_rows() |>
        dplyr::summarise(.by = .data$group,
                         x = mean(.data$x),
                         y = mean(.data$y)) |>
        dplyr::select(-.data$group) |>
        ob_point()

      center@style <- ob_polygon_style
      if (S7_inherits(label, ob_label)) {
        if (all(label@center@x == 0) && all(label@center@y == 0)) {
          label@center <- center
        }
        if (length(label@fill) == 0 || all(label@fill == "white") && all(!is.na(label@fill))) {
          label@fill <- d[["fill"]] %||% fill
        }
      } else {
        label <- ob_label(label, center, fill = d[["fill"]] %||% fill)
      }

      }

    new_object(.parent = S7_object(),
               p =  d$p,
               label = label,
               vertex_radius = vertex_radius,
               alpha = d[["alpha"]] %||% alpha,
               color = d[["color"]] %||% color,
               fill = d[["fill"]] %||% fill,
               linewidth = d[["linewidth"]] %||% linewidth,
               linetype = d[["linetype"]] %||% linetype
    )
  })


method(str, ob_polygon) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c(""))) {

  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
  cat(" <points>\n")
  purrr::walk(object@p,
              \(o) {
                str_properties(
                  o,
                  omit = omit_props(
                    o,
                    include = c("x", "y")),
                  nest.lev = 2,
                  additional = TRUE)
              })

}

method(print, ob_polygon) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(get_tibble, ob_polygon) <- function(x) {
  d <- x@tibble
  if ("vertex_radius" %in% colnames(d)) {
    d <- dplyr::rename(x@tibble, radius = vertex_radius)
  }
  d
}


method(as.geom, ob_polygon) <- function(x, ...) {
  gp <- as.geom(super(x, has_style), ...)
  if (S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gp <- list(gp, gl)
  }
  gp
}


method(`[`, ob_polygon) <- function(x, y) {
  d <- x@tibble[y,]
  dl <- d |>
    dplyr::select(-.data$x, -.data$y, -.data$group) |>
    unique() |>
    unbind()
  z <- rlang::inject(ob_polygon(p = x@p[y], !!!dl))
  z@label <- x@label[y]
  z
}

method(connect, list(ob_polygon, ob_polygon)) <- function(x,y, ...) {
  centroid_segment <- ob_segment(x@center, y@center)
  connect(intersection(x, centroid_segment), intersection(y, centroid_segment), ...)
}

method(connect, list(ob_polygon, ob_point)) <- function(x,y, ...) {
  centroid_segment <- ob_segment(x@center, y)
  connect(intersection(x, centroid_segment), y, ...)
}

method(connect, list(ob_point, ob_polygon)) <- function(x,y, ...) {
  centroid_segment <- ob_segment(x, y@center)
  connect(x, intersection(y, centroid_segment), ...)
}


method(connect, list(centerpoint, ob_polygon)) <- function(x,y, ...) {
  centroid_segment <- ob_segment(x@center, y@center)
  p <- intersection(y, centroid_segment)
  connect(x, p, ...)
}


method(connect, list(ob_polygon, centerpoint)) <- function(x,y, ...) {
  centroid_segment <- ob_segment(x@center, y@center)
  p <- intersection(x, centroid_segment)
  connect(p, y, ...)
}

# Intercepts ----
#' ob_intercept
#'
#' Triangle polygons used in path diagrams.
#' @param center point at center
#' @param width length of side
#' @param label A character, angle, or label object
#' @param vertex_radius A numeric or unit vector of length one, specifying the vertex radius
#' @param top Top vertex of triangle
#' @param left Left vertex of triangle
#' @param right Right vertex of triangle
#' @param x0 overrides x-coordinate in `center@x`
#' @param y0 overrides x-coordinate in `center@y`
#' @slot length The number of polygons in the ob_polygon object
#' @param style Gets and sets the styles associated with polygons
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggplot2::geom_polygon`.
#' @inherit ob_style params
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
ob_intercept <- new_class(
  name = "ob_intercept",
  parent = has_style,
  properties = rlang::inject(
    list(
      center = ob_point,
      width = class_numeric,
      p = new_property(getter = function(self) {
        purrr::map(unbind(self@center), \(cc) {
          cc + ob_polar(degree(c(90, 210, 330)),
                        r = self@width * .5 / cos(degree(30)))
        })
      }),
      polygon = new_property(getter = function(self) {
        ob_polygon(self@p, style = self@style, vertex_radius = self@vertex_radius)
      }),
      top = new_property(getter = function(self) {
        self@center + ob_polar(degree(90),
                               r = self@width * .5 / cos(degree(30)))
      }),
      left = new_property(getter = function(self) {
        self@center + ob_polar(degree(210),
                               r = self@width * .5 / cos(degree(30)))
      }),
      right = new_property(getter = function(self) {
        self@center + ob_polar(degree(330),
                               r = self@width * .5 / cos(degree(30)))
      }),
      !!!ob_polygon_props$extra,
      !!!ob_polygon_props$styles,
      !!!ob_polygon_props$derived[!names(ob_polygon_props$derived) %in% c("center")],
      !!!ob_polygon_props$funs,
      !!!ob_polygon_props$info
    )
  ),
  constructor = function(center = ob_point(0,0),
                         width = 1,
                         label = character(0),
                         top = class_missing,
                         left = class_missing,
                         right = class_missing,
                         vertex_radius = numeric(0),
                         alpha = numeric(0),
                         color = character(0),
                         fill = character(0),
                         linewidth = numeric(0),
                         linetype = numeric(0),
                         x0 = numeric(0),
                         y0 = numeric(0),
                         style = class_missing,
                         ...) {

    if ((length(x0) > 0) || (length(y0) > 0)) {
      if (length(x0) == 0) {
        x0 <- 0
      }
      if (length(y0) == 0) {
        y0 <- 0
      }
      center <- ob_point(x = x0, y = y0)
    }

    ob_polygon_style <- ob_style() + center@style + style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype
      ) +
      ob_style(...)


    non_empty_list <- get_non_empty_props(ob_polygon_style)
    d <- tibble::tibble(x0 = center@x, y0 = center@y, width = width)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }


    center = set_props(center, x = d$x0, y = d$y0)

    if (S7_inherits(label, ob_label)) {
      if (all(label@center == ob_point(0,0))) {
        label@center <- center
      }
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_intercept")



    new_object(.parent = S7_object(),
               center =  center,
               width = d$width %||% width,
               label = label,
               vertex_radius = vertex_radius,
               alpha = d[["alpha"]] %||% alpha,
               color = d[["color"]] %||% color,
               fill = d[["fill"]] %||% fill,
               linewidth = d[["linewidth"]] %||% linewidth,
               linetype = d[["linetype"]] %||% linetype
    )
  })


method(get_tibble, ob_intercept) <- function(x) {
  d <- x@tibble
  if ("vertex_radius" %in% colnames(d)) {
    d <- dplyr::rename(x@tibble, radius = vertex_radius)
  }
  d
}

method(connect, list(ob_intercept, ob_intercept)) <- function(x,y, ...) {
  connect(x@polygon, y@polygon, ...)
}

method(connect, list(ob_intercept, ob_point)) <- function(x,y, ...) {
  centroid_segment <- ob_segment(x@center, y)
  connect(x@polygon, y, ...)
}

method(connect, list(ob_point, ob_intercept)) <- function(x,y, ...) {
  connect(x, y@polygon, ...)
}


method(connect, list(centerpoint, ob_intercept)) <- function(x,y, ...) {
  connect(x, y@polygon, ...)
}


method(connect, list(ob_intercept, centerpoint)) <- function(x,y, ...) {
  connect(x@polygon, y, ...)
}

method(as.geom, ob_intercept) <- function(x, ...) {
  gp <- as.geom(super(x@polygon, has_style), ...)
  if (S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gp <- list(gp, gl)
  }
  gp
}

# ob_ngon ----

ob_ngon_props <- list(
  ## primary ----
  primary = list(
    n = new_property(class_numeric, validator = \(value) {
      if (!all(value >= 3))
        stop("There must be at least three sides to a regular polygon.")
    }),
    radius = class_numeric,
    angle = ob_angle_or_character
  ),
  ## derived----
  derived = list(
    side_length = new_property(getter = \(self) {
      self@radius * 2 * sin(pi / self@n)
    }),
    apothem = new_property(getter = \(self) {
      self@radius * cos(pi / self@n)
    }),
    area = new_property(getter = \(self) {
      self@n * self@side_length * self@apothem / 2
    }),
    bounding_box = new_property(getter = \(self) {
      self@vertices@bounding_box
    }),
    circumscribed = new_property(getter = \(self) {
      ob_circle(self@center, radius = self@radius, style = self@style)
    }),
    inscribed = new_property(getter = \(self) {
      ob_circle(self@center, radius = self@apothem, style = self@style)
    }),
    length = new_property(
      getter = function(self) {
        self@center@length
      }
    ),
    perimeter = new_property(getter = \(self) {
      self@n * self@side_length
    }),
    segments = new_property(getter = \(self) {
      map_ob(self, \(s) {
        theta <- degree(seq(0, 360, length.out = s@n + 1)) + s@angle

        p <- s@center + ob_polar(theta, r = s@radius)

        ob_segment(p[seq(1, s@n)], p[seq(2, s@n + 1)], style = s@style)
      })
    }),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(ob_polygon_styles, prop, object = self) |>
          `names<-`(ob_polygon_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% ob_polygon_styles]
        self <- rlang::inject(set_props(self, !!!s_list))
        self
      }
    ),
    tibble = new_property(
      getter = function(self) {
        d <- list(
          x0 = self@center@x,
          y0 = self@center@y,
          radius = self@radius,
          n = self@n,
          angle = c(self@angle) * 360,
          vertex_radius = self@vertex_radius,
          alpha = self@alpha,
          color = self@color,
          fill = self@fill,
          linewidth = self@linewidth,
          linetype = self@linetype
        )
        get_non_empty_tibble(d)
      }
    ),
    vertices = new_property(getter = \(self) {
      map_ob(self, \(s) {
        theta <- degree(seq(0, 360 - 360 / s@n, length.out = s@n)) + s@angle

        s@center + ob_polar(theta, r = s@radius, color = s@color)
      })
    })
  ),
  # functions ----
  funs = list(
    angle_at = new_property(
      class_function,
      getter = function(self) {
        \(point) {
          dp <- point - self@center
          dp@theta
        }
      }
    ),
    normal_at = new_property(
      class_function,
      getter = function(self) {
        \(theta = degree(0), distance = 1, ...) {
          if (S7_inherits(theta, ob_point)) {
            theta <- projection(theta, self)@theta
          }
          if (!S7_inherits(theta, ob_angle)) {
            theta <- degree(theta)
          }

          if (!S7_inherits(theta, ob_angle))
            theta <- degree(theta)
          p <- purrr::map2(unbind(self), unbind(theta), \(s, th) {
            th_p <- th@turn - s@angle@turn
            th_n <- 1 / s@n
            th_r <- th_p / th_n
            th_floor <- floor(th_r)
            th_normal <- turn(th_floor * th_n + ifelse(th_floor == th_r, 0, th_n / 2)) + s@angle
            s@point_at(th) + ob_polar(th_normal, distance)

          }) %>%
            bind()
          st <- rlang::list2(...)
          rlang::inject(set_props(p, !!!st))
        }
      }
    ),
    point_at = new_property(
      class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          st <- rlang::list2(...)

          if (!S7_inherits(theta, ob_angle))
            theta <- degree(theta)
          p <- purrr::map(unbind(self), \(s) {
            s_radius <- ob_segment(s@center, s@center + ob_polar(theta = theta, r = s@radius))
            intersection(s_radius, s@segments)[1]
          }) %>%
            bind()
          rlang::inject(set_props(p, !!!st))
        }
      }
    ),
    tangent_at = new_property(
      class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (S7_inherits(theta, ob_point)) {
            theta <- projection(theta, self)@theta
          }
          if (!S7_inherits(theta, ob_angle)) {
            theta <- degree(theta)
          }

          p <- self@point_at(theta)
          p_normal <- self@normal_at(theta)
          l <- ob_segment(p1 = p,
                          p2 = rotate(p_normal,
                                      degree(90),
                                      origin = p))@line

          s <- rlang::list2(...)
          rlang::inject(set_props(l, !!!s))
        }
      }
    )
  )
)

#' The ob_ngon (regular polygon) class
#'
#' An ngon is a regular polygon, meaning that each side is of equal length. The `ob_ngon` object can be specified with a center, n (number of sides), radius, and angle. Instead of specifying a radius, one can specify either the `side_length` or the length of the `apothem` (i.e., the distance from the center to a side's midpoint.
#' @export
#' @param center point at center of the ngon
#' @param n Number of sides
#' @param radius Distance from center to a vertex
#' @param side_length Distance of each side
#' @param apothem Distance from center to a side's midpoint
#' @param angle description
#' @param label A character, angle, or label object
#' @param vertex_radius A numeric or unit vector of length one, specifying the corner radius
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @param style Gets and sets the styles associated with ob_ngon
#' @slot area The area of the ngons in the ob_ngon object
#' @slot length The number of ngons in the ob_ngon object
#' @slot normal_at A function that finds a point that is perpendicular from the ngon and at a specified distance
#' @slot perimeter The length of all the side segments
#' @slot point_at A function that finds a point on the ngon at the specified angle.
#' @slot segments side segments of the regular polygon
#' @slot tangent_at A function that finds the tangent line at the specified angle.
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggforce::geom_shape`.
#' @slot vertices points on the regular polygon
#' @inherit ob_style params
ob_ngon <- new_class(
  name = "ob_ngon",
  parent = centerpoint,
  properties = rlang::inject(
    list(
      !!!ob_ngon_props$primary,
      !!!ob_polygon_props$extra,
      !!!ob_polygon_props$styles,
      !!!ob_ngon_props$derived,
      !!!ob_polygon_props$funs,
      !!!ob_ngon_props$funs,
      !!!ob_polygon_props$info
    )
  ),
  constructor = function(center = ob_point(0,0),
                         n = 3L,
                         radius = numeric(0),
                         angle = 0,
                         label = character(0),
                         side_length = numeric(0),
                         apothem = numeric(0),
                         vertex_radius = numeric(0),
                         alpha = numeric(0),
                         color = character(0),
                         fill = character(0),
                         linewidth = numeric(0),
                         linetype = numeric(0),
                         style = class_missing,
                         x0 = numeric(0),
                         y0 = numeric(0),
                         ...) {
    if (!S7_inherits(angle, ob_angle)) angle <- degree(angle)

    if (length(side_length) > 0) {
      radius <- side_length / (2 * sin(turn(1 / (2 * n))))
    }

    if (length(apothem) > 0) {
      radius <- apothem / (2 * cos(turn(1 / (2 * n))))
    }

    if (length(radius) == 0) {
      radius <- 1
    }

    if ((length(x0) > 0) || (length(y0) > 0)) {
      if (length(x0) == 0) {
        x0 <- 0
      }
      if (length(y0) == 0) {
        y0 <- 0
      }
      center <- ob_point(x = x0, y = y0)
    }

    ob_polygon_style <- ob_style() + center@style + style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(ob_polygon_style)
    d <- tibble::tibble(x0 = center@x,
                        y0 = center@y,
                        n = n,
                        radius = radius,
                        angle = c(angle))
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }


    center = set_props(center, x = d$x0, y = d$y0)

    if (S7_inherits(label, ob_label)) {
      if (all(label@center == ob_point(0,0))) {
        label@center <- center
      }
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_intercept")

    new_object(.parent = S7_object(),
               center =  center,
               n = d$n %||% n,
               radius = d$radius %||% radius,
               angle = turn(d$angle),
               label = label,
               vertex_radius = vertex_radius,
               alpha = d[["alpha"]] %||% alpha,
               color = d[["color"]] %||% color,
               fill = d[["fill"]] %||% fill,
               linewidth = d[["linewidth"]] %||% linewidth,
               linetype = d[["linetype"]] %||% linetype
    )
  })

method(get_tibble, ob_ngon) <- function(x) {
  d <- x@tibble
  if ("radius" %in% colnames(d)) {
    d <- dplyr::rename(x@tibble, r = radius)
  }

  if ("vertex_radius" %in% colnames(d)) {
    d <- dplyr::rename(x@tibble, radius = vertex_radius)
  }

  d %>%
    dplyr::mutate(group = dplyr::row_number(),
                  theta = purrr::map2(n, angle, \(nn, aa) {seq(0, 360, length.out = nn + 1) + aa })) %>%
    tidyr::unnest(theta) %>%
    dplyr::mutate(x = cospi(theta / 180) * r + x0,
                  y = sinpi(theta / 180) * r + y0) %>%
    dplyr::select(-x0, -y0, -r, -theta)
}


method(as.geom, ob_ngon) <- function(x, ...) {
  gp <- as.geom(super(x, has_style), ...)
  if (S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gp <- list(gp, gl)
  }
  gp
}

method(str, ob_ngon) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = omit_props(object, include = c("center","radius", "n", "angle"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev)
}

method(`[`, ob_ngon) <- function(x, y) {
  d <- x@tibble[y,]
  dl <- as.list(dplyr::select(d, -.data$x0, -.data$y0))
  z <- rlang::inject(ob_ngon(center = ob_point(d$x0, d$y0), !!!dl))
  z@label <- x@label[y]
  z
}


method(connect, list(ob_ngon, ob_ngon)) <- function(x,y, ...) {
  s <- ob_segment(x@center, y@center)
  connect(intersection(x, s), intersection(y, s), ...)
}

method(connect, list(ob_ngon, ob_point)) <- function(x,y, ...) {
  s <- ob_segment(x@center, y)
  connect(intersection(x, s), y, ...)
}

method(connect, list(ob_point, ob_ngon)) <- function(x,y, ...) {
  s <- ob_segment(x, y@center)
  connect(x, intersection(y, s), ...)
}


method(connect, list(centerpoint, ob_ngon)) <- function(x,y, ...) {
  s <- ob_segment(x@center, y@center)
  p <- intersection(y, s)
  connect(x, p, ...)
}

# ob_reuleaux ----
#' Reuleaux polygon
#'
#' @param center point at center of the rectangle
#' @param n Number of sides. True Reuleaux polygons have an odd number of sides, but Reauleaux-like shapes with an even number of sides are possible.
#' @param radius Distance from center to a vertex
#' @inherit ob_style params
#'
#' @return ob_polygon
#' @export
ob_reuleaux <- new_class(
  name = "ob_reuleaux",
  parent = centerpoint,
  properties = list(
    n = class_integer,
    radius = class_double,
    angle = ob_angle_or_numeric,
    label = label_or_character_or_angle,
    vertex_radius = new_property(class = class_numeric_or_unit, validator = function(value) {
      if (length(value) > 1) stop("The vertex_radius property must be of length 1.")
    }),
    ob_style@properties$alpha,
    ob_style@properties$color,
    ob_style@properties$fill,
    ob_style@properties$linewidth,
    ob_style@properties$linetype,
    ob_polygon@properties$style,
    arc_radius = new_property(getter = \(self) {
      self@chord_length * sin((degree(180) - self@inscribed_angle) / 2) / sin(self@inscribed_angle)
    }),
    arcs = new_property(getter = \(self) {
      map_ob(self, \(s) {
        ntheta <- 360 / s@n
        theta <- seq(0, 360, length.out = s@n + 1)[1:s@n] + s@angle@degree

        purrr::map(theta, \(th) {
          start <- degree(180 + th - ntheta / 2)
          end <- start + degree(ntheta)
          p_start <- ob_polar(start, s@radius)
          p_end <- ob_polar(end, s@radius)
          p_opposite <- ob_polar(degree(th), s@radius)
          start1 <- (p_start - p_opposite)@theta@degree
          end1 <- (p_end - p_opposite)@theta@degree
          if (start1 > end1)
            end1 <- end1 + 360
          ob_arc(
            center = p_opposite + s@center,
            start = start1,
            end = end1,
            radius = (p_start - p_opposite)@r,
          )
        }) %>%
          bind()

      })
    }),
    central_angle = new_property(getter = \(self) {
      degree(360 / self@n)
    }),
    chord_length = new_property(getter = \(self) {
      sqrt((2 * self@radius ^ 2) * (1 - cos(self@inscribed_angle)))
    }),
    circumscribed = new_property(getter = \(self) {
      ob_circle(self@center, radius = self@radius, style = self@style)
    }),
    inscribed_angle = new_property(getter = \(self) {
      degree(180 / self@n)
    }),
    length = ob_ngon@properties$length,
    tibble = new_property(
      getter = function(self) {
        d <- list(
          x0 = self@center@x,
          y0 = self@center@y,
          radius = self@radius,
          n = self@n,
          angle = c(self@angle) * 360,
          vertex_radius = self@vertex_radius,
          alpha = self@alpha,
          color = self@color,
          fill = self@fill,
          linewidth = self@linewidth,
          linetype = self@linetype
        )
        get_non_empty_tibble(d)
      }
    ),
    vertices = ob_ngon@properties$vertices,
    aesthetics = ob_polygon_props$info$aesthetics,
    # functions ----
    angle_at = new_property(class_function, getter = function(self) {
      \(point) {
        dp <- point - self@center
        dp@theta
      }
    }),
    arc_at = new_property(class_function, getter = \(self) {
      \(theta, ...) {
        if (!S7_inherits(theta, ob_angle)) {
          theta <- degree(theta)
        }
        th <- theta - self@angle
        i_arc <- floor(c(th) / c(self@central_angle))
        a_start <- i_arc * self@central_angle + self@angle
        a_end <- a_start + self@central_angle
        a_center <- a_start + (self@central_angle / 2) + turn(.5)
        p_center <- ob_polar(a_center, self@radius) + self@center
        p_start <- ob_polar(a_start, self@radius) + self@center
        p_end <- ob_polar(a_end, self@radius) + self@center
        a_start1 <- (p_start - p_center)@theta
        a_end1 <- (p_end - p_center)@theta
        a_end1 <- a_end1 + ifelse(a_start1 > a_end1, turn(1), turn(0))
        r1 <- (p_start - p_center)@r
        ob_arc(
          center = p_center,
          radius = r1,
          start = a_start1,
          end = a_end1,
          style = self@style,
          ...
        )

      }
    }),
    normal_at = new_property(class_function, getter = function(self) {
      \(theta = degree(0), distance = 1, ...) {
        if (S7_inherits(theta, ob_point)) theta <- self@angle_at(theta)
        if (!S7_inherits(theta, ob_angle)) theta <- degree(theta)
        p <- self@point_at(theta)
        a <- self@arc_at(theta)
        theta_a <- (p - a@center)@theta
        a@normal_at(theta_a, ...)

      }
    }),
    point_at = new_property(
      class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (!S7_inherits(theta, ob_angle)) theta <- degree
          s <- ob_segment(self@center,
                          self@circumscribed@point_at(theta))
          a <- self@arc_at(theta)@circle
          intersection(s,a, ...)



        }
      }
    )
  ), constructor = function(center = ob_point(0, 0),
                            n = 5,
                            radius = 1,
                            angle = 90,
                            label = character(0),
                            vertex_radius = numeric(0),
                            alpha = numeric(0),
                            color = "black",
                            fill = character(0),
                            linewidth = numeric(0),
                            linetype = numeric(0),
                            style = class_missing,
                            ...) {
    if (!S7_inherits(angle, ob_angle)) angle <- degree(angle)
    n <- as.integer(n)

    ob_polygon_style <- ob_style() + center@style + style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(ob_polygon_style)
    d <- tibble::tibble(x0 = center@x,
                        y0 = center@y,
                        n = n,
                        radius = radius,
                        angle = c(angle))
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }


    center = set_props(center, x = d$x0, y = d$y0)

    if (S7_inherits(label, ob_label)) {
      if (all(label@center == ob_point(0,0))) {
        label@center <- center
      }
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_reuleaux")

    new_object(.parent = S7_object(),
               center =  center,
               n = d$n %||% n,
               radius = d$radius %||% radius,
               angle = turn(d$angle),
               label = label,
               vertex_radius = vertex_radius,
               alpha = d[["alpha"]] %||% alpha,
               color = d[["color"]] %||% color,
               fill = d[["fill"]] %||% fill,
               linewidth = d[["linewidth"]] %||% linewidth,
               linetype = d[["linetype"]] %||% linetype
    )


  }
    )


method(get_tibble, ob_reuleaux) <- function(x) {
  d <- x@tibble
  if ("radius" %in% colnames(d)) {
    d <- dplyr::rename(d, r = radius)
  }

  if ("vertex_radius" %in% colnames(d)) {
    d <- dplyr::rename(d, radius = vertex_radius)
  }

  d %>%
    dplyr::mutate(group = dplyr::row_number()) %>%
    dplyr::mutate(p = purrr::pmap(
      list(x0, y0, n, r, angle),
      \(x0,y0,n,r,angle) {
        ntheta <- 360 / n
        theta <- seq(0, 360, length.out = n + 1)[1:n] + angle
        s <- purrr::map_df(theta, \(th) {
          start <- degree(th)
          end <- start + degree(ntheta)
          opposite <- start + degree(ntheta / 2) + turn(.5)
          p_start <- ob_polar(start, r)
          p_end <- ob_polar(end, r)
          p_opposite <- ob_polar(opposite, r)
          start1 <- (p_start - p_opposite)@theta
          end1 <- (p_end - p_opposite)@theta
          if (start1 > end1) {end1 <- end1 + turn(1)}
          ob_arc(
            center = p_opposite + ob_point(x0,y0),
            start = degree(start1),
            end = degree(end1),
            radius = (p_start - p_opposite)@r,
          )@polygon %>%
            dplyr::select(-group)
        })
      })) %>%
    dplyr::select(-x0,-y0,-n, -r, -angle) %>%
    tidyr::unnest(p)
}


method(str, ob_reuleaux) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = omit_props(object,
                      include = c("center",
                                  "radius",
                                  "n",
                                  "angle"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev)
}

method(as.geom, ob_reuleaux) <- function(x, ...) {
  gp <- as.geom(super(x, has_style), ...)
  if (S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gp <- list(gp, gl)
  }
  gp
}

method(`[`, ob_reuleaux) <- function(x, y) {
  d <- x@tibble[y,]
  dl <- as.list(dplyr::select(d, -.data$x0, -.data$y0))
  z <- rlang::inject(ob_reuleaux(center = ob_point(d$x0, d$y0), !!!dl))
  z@label <- x@label[y]
  z
}
