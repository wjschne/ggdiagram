pt_styles <- c("alpha", "color", "fill", "shape", "size", "stroke")

pr_place <- new_property(class_function, getter = function(self) {
  \(from, where = "right", sep = 1, ...) place(x = self, from = from, where = where, sep = sep, ...)
})

pt_props <- list(
  # Primary ----
  primary = list(
    x = new_property(class = class_numeric, default = 0),
    y = new_property(class = class_numeric, default = 0)
  ),
  styles = ob_style@properties[pt_styles],
  # Derived ----
  derived = list(
    auto_label = new_property(getter = function(self) {
      label_object(self)
    }),
    bounding_box = new_property(getter = function(self) {
      ob_rectangle(southwest = ob_point(x = min(self@x), y = min(self@y)),
                northeast = ob_point(x = max(self@x), y = max(self@y)))
    }),
    length = new_property(
      getter = function(self) {
        length(self@x)
      }
    ),
    r = new_property(
      getter = function(self) {
        sqrt(self@x ^ 2 + self@y ^ 2)
      },
      setter = function(self, value) {
        set_props(self, x = cos(self@theta) * value, y = sin(self@theta) * value)
      }
    ),
    theta = new_property(
      getter = function(self) {
        radian(radian = atan2(self@y, self@x))
      },
      setter = function(self, value) {
        set_props(self, x = cos(value) * self@r, y = sin(value) * self@r)
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(pt_styles,
                         prop, object = self) |>
          `names<-`(pt_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        ob_point(self@x, self@y, style = self@style + value)
      }
    ),
    tibble = new_property(getter = function(self) {
      d <- list(x = self@x,
                     y = self@y,
                     alpha = self@alpha,
                     color = self@color,
                     fill = self@fill,
                     shape = self@shape,
                     size = self@size,
                     stroke = self@stroke
                     )
      get_non_empty_tibble(d)
    }),
    xy = new_property(
      getter = function(self) {
        `colnames<-`(cbind(self@x, self@y), c("x", "y"))
      }
    )
  ),
  # Functions ----
  funs = list(
    geom = new_property(class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    label = new_property(class_function, getter = function(self) {
      \(label = NULL,
        accuracy = .1,
        ...
      ) {
        if (is.null(label)) {
          label = label_object(self, accuracy)

        }
        if (is.numeric(label) & !S7_inherits(label)) {
          if (rlang::is_integerish(label)) {
            label <- signs::signs(label)
          } else {
            label = signs::signs(label, accuracy = accuracy, trim_leading_zeros = TRUE)
          }
        }
        ob_label(center = self, label = label, ...)
      }

    }),
    place = pr_place

  ),
  info = list(
    aesthetics = new_property(getter = function(self) {
    class_aesthetics_list(
      geom = ggplot2::geom_point,
      mappable_bare = character(0),
      mappable_identity = c("shape", "color", "size", "fill", "alpha", "stroke"),
      not_mappable = character(0),
      required_aes = c("x", "y"),
      omit_names = "group",
      inherit.aes = FALSE,
      style = pt_styles
    )
  }))
)



# ob_point----

#' ob_point
#'
#' Points are specified with x and y coordinates.
#' @export
#' @param x Vector of coordinates on the x-axis (also can take a tibble/data.frame or 2-column matrix as input.)
#' @param y Vector of coordinates on the y-axis
#' @param r Radius = Distance from the origin to the ob_point
#' @param theta Angle of the vector from the origin to the ob_point
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @slot auto_label Gets x and y coordinates and makes a label `"(x,y)"`
#' @slot length The number of points in the ob_point object
#' @param style Gets and sets the styles associated with points
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggplot2::geom_point`.
#' @slot xy Gets a 2-column matrix of the x and y coordinates of the ob_point object.
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to `ggplot2::geom_point`.
#' @inherit ob_style params
#' @export
ob_point <- new_class(
  name = "ob_point",
  parent = xy,
  properties = rlang::inject(list(
    !!!pt_props$primary,
    !!!pt_props$styles,
    !!!pt_props$derived,
    !!!pt_props$funs,
    !!!pt_props$info)),
  constructor = function(x = 0,
                         y = 0,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         shape = class_missing,
                         size = class_missing,
                         stroke = class_missing,
                         style = class_missing,
                         ...) {

    if ("data.frame" %in% class(x)) {
      return(rlang::inject(ob_point(!!!get_non_empty_list(x))))
    }

    if ("matrix" %in% class(x) && ncol(x) == 2) {
      return(ob_point(x[,1], x[,2]))
    }

    p_style <- style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        shape = shape,
        size = size,
        stroke = stroke
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(p_style)
    d <- tibble::tibble(x = x, y = y)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }



     new_object(S7_object(),
                 x = d$x,
                 y = d$y,
                 alpha = d[["alpha"]] %||% alpha,
                 color = d[["color"]] %||% color ,
                 fill = d[["fill"]]  %||% fill,
                 shape = d[["shape"]] %||% shape,
                 size = d[["size"]] %||% size,
                 stroke = d[["stroke"]] %||% stroke)
  }
)


# ob_polar point class ----
#' ob_polar
#'
#' Polar points are ordinary points but are specified with an angle (theta) and a radial distance (r)
#' @rdname ob_point
#' @export
ob_polar <- new_class(
  name = "ob_polar",
  parent = ob_point,
  constructor = function(theta = class_missing,
                         r = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         shape = class_missing,
                         size = class_missing,
                         stroke = class_missing,
                        style = class_missing) {
    if (length(r) == 0) r <- 1
    if (length(theta) == 0) theta <- degree(0)
    if (is.character(theta)) thata <- degree(theta)


    p <- ob_point(x = cos(theta) * r,
               y = sin(theta) * r,
               alpha = alpha,
               color = color,
               fill = fill,
               shape = shape,
               size = size,
               stroke = stroke,
               style = style
              )

    new_object(p)
  })


method(str, ob_point) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c("x","y"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                additional = additional)
}

method(str, ob_polar) <- function(
  object,
  nest.lev = 0,
  additional = TRUE,
  omit = omit_props(object, include = c("x","y", "r", 'theta'))) {
str_properties(object,
               omit = omit,
               nest.lev = nest.lev,
              additional = additional)
}

method(print, ob_point) <- function(x, ...) {
  str(x, ...)
    invisible(x)
  }


method(get_tibble, ob_point) <- function(x) {
  x@tibble
}


method(get_tibble_defaults, ob_point) <- function(x) {
  sp <- ob_style(
    alpha = replace_na(ggplot2::GeomPoint$default_aes$alpha, 1),
    color = replace_na(ggplot2::GeomPoint$default_aes$colour, "black"),
    fill = replace_na(ggplot2::GeomPoint$default_aes$fill, "black"),
    shape = replace_na(ggplot2::GeomPoint$default_aes$shape, 19),
    size = replace_na(ggplot2::GeomPoint$default_aes$size, 2),
    stroke = replace_na(ggplot2::GeomPoint$default_aes$stroke, 0.5)
  )
  get_tibble_defaults_helper(x, sp, required_aes = c("x", "y"))
}

#' polar2just
#'
#' Convert hjust and vjust parameters from polar coordinates
#' @param x angle
#' @param multiplier distance
#' @param axis vertical (v) or horizontal (h)
#' @export
polar2just <- new_generic(
  name = "polar2just",
  dispatch_args = "x",
  fun = function(x, multiplier = 1.2, axis = c("h", "v")) {
    S7_dispatch()
  }
)
method(polar2just, class_numeric) <- function(x, multiplier = 1.2, axis = c("h", "v")) {
  if (length(multiplier) == 0) multiplier <- 1.2
  axis <- match.arg(axis)
  if (axis == "h") {
    (((cos(x + pi) + 1)/2) - 0.5) * multiplier + 0.5
  } else {
    (((sin(x + pi) + 1)/2) - 0.5) * multiplier + 0.5
  }

}

method(polar2just, ob_angle) <- function(x, multiplier = 1.2, axis = c("h", "v")) {
  polar2just(x@radian, multiplier, axis)
}

method(`==`, list(ob_point, ob_point)) <- function(e1, e2) {
  (e1@x == e2@x) & (e1@y == e2@y)
}



# arithmetic ----
purrr::walk(list(`+`, `-`, `*`, `/`, `^`), \(.f) {
  method(.f, list(ob_point, ob_point)) <- function(e1, e2) {
    x <- .f(e1@x, e2@x)
    y <- .f(e1@y, e2@y)
    e2@x <- x
    e2@y <- y
    e2@style <- e1@style + e2@style
    e2
  }
  method(.f, list(ob_point, class_numeric)) <- function(e1, e2) {
    e1@x <- .f(e1@x, e2)
    e1@y <- .f(e1@y, e2)
    e1
  }
  method(.f, list(class_numeric, ob_point)) <- function(e1, e2) {
    e2@x <- .f(e1, e2@x)
    e2@y <- .f(e1, e2@y)
    e2
  }
})

method(midpoint, list(ob_point, ob_point)) <- function(x,y, position = .5, ...) {
  p <- x + ((y - x) * position)
  s <- rlang::list2(...)
  rlang::inject(set_props(p, !!!s))
}

method(`%*%`, list(ob_point, ob_point)) <- function(x, y) {
  x@xy[1, , drop = TRUE] %*% y@xy[1, , drop = TRUE]
}

# Perpendicular ----

#' Find point perpendicular to 2 points
#'
#' @name perpendicular_point
#' @param e1 first ob_point
#' @param e2 second ob_point
#' @examples
#' x <- ob_point(0,0)
#' y <- ob_point(1,1)
#' # Find point perpendicular to x and y going vertically first
#' x %|-% y
#' # Find point perpendicular to x and y going horizontally first
#' x %-|% y
NULL

#' @name perpendicular_vertical
#' @rdname perpendicular_point
#' @aliases %|-%
#' @export
`%|-%` <- new_generic("%|-%", c("e1", "e2"), fun = function(e1,e2) {S7_dispatch()})

method(`%|-%`, list(ob_point, ob_point)) <- function(e1,e2) {
  e2@x <- e1@x
  e2
  }



#' @name perpendicular_horizontal
#' @rdname perpendicular_point
#' @aliases %-|%
#' @export
`%-|%` <- new_generic("%-|%", c("e1", "e2"), fun = function(e1,e2) {S7_dispatch()})

method(`%-|%`, list(ob_point, ob_point)) <- function(e1,e2) {
  e2@y <- e1@y
  e2
  }

method(label_object, ob_point) <- function(object, accuracy = .1) {

      if (rlang::is_integerish(object@x)) {
        x <- signs::signs(object@x)
      } else {
        x = signs::signs(object@x, accuracy = accuracy, trim_leading_zeros = TRUE)
      }

      if (rlang::is_integerish(object@y)) {
        y <- signs::signs(object@y)
      } else {
        y = signs::signs(object@y, accuracy = accuracy, trim_leading_zeros = TRUE)
      }

      paste0("(", x, ",", y, ")")

}



method(`[`, ob_point) <- function(x, y) {
  d <- as.list(x@tibble[y,])
  rlang::inject(ob_point(!!!d))
}



method(`[<-`, ob_point) <- function(x, y, value) {
  d <- assign_data(x,y,value)
  rlang::inject(ob_point(!!!d))
}


method(connect, list(ob_point, ob_point)) <- function(x,y, arrow_head = arrowheadr::arrow_head_deltoid(d = 2.3, n = 100), length_head = 7, ...) {
  s <- ob_segment(x,y, arrow_head = arrow_head, length_head = length_head, ...)
  s

}

method(place, list(ob_point, ob_point)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p <- ob_polar(where, sep)
  x@x <- from@x + p@x
  x@y <- from@y + p@y
  x
}

point_or_list <- new_union(ob_point, class_list)


method(nudge, list(ob_point, class_numeric, class_numeric)) <- function(object, x, y) {
  object + ob_point(x, y)
}

method(nudge, list(ob_point, class_numeric, class_missing)) <- function(object, x, y) {
  object + ob_point(x, 0)
}

method(nudge, list(ob_point, class_missing, class_numeric)) <- function(object, x, y) {
  object + ob_point(0, y)
}

method(ob_array, ob_point) <- function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {
  s <- seq(0, sep * (k - 1), sep)
  px <- cos(degree(where)) * s
  py <- sin(degree(where)) * s
  p <- ob_point(px, py)
  bb <- p@bounding_box
  if (anchor == "center") {
    p_anchor <- bb@center
  } else {
    p_anchor <- bb@point_at(anchor)
  }
  ob_point((p - p_anchor + x)@xy, x@style, ...)
}


method(ob_covariance, list(ob_point, ob_point)) <- function(
    x,
    y,
    where = NULL,
    bend = 0,
    looseness = 1,
    arrow_head = arrowheadr::arrow_head_deltoid(d = 2.3, n = 100),
    resect = 2,
    ...) {
  if (!S7_inherits(where, ob_angle) && !is.null(where)) where <- degree(where)
  if (!S7_inherits(bend, ob_angle)) bend <- degree(bend)



  p <- purrr::pmap(list(xx = unbind(x), yy = unbind(y), bb = unbind(bend)), \(xx, yy, bb) {

    if (is.null(where)) {
      d_xy <- yy - xx
      x_angle <- d_xy@theta + degree(45)
      y_angle <- degree(135) + (d_xy@theta)
    } else {
      x_angle <- where
      y_angle <- degree(180) - where
    }
    s <- xx
    # m <- el@point_at(where)
    e <- yy
    # s_dist <- (s - m)@r * looseness * 2
    # e_dist <- (e - m)@r * looseness * 2
    m_dist <- looseness * (s - e)@r / 2

    bind(c(
      s,
      rotate(
        xx + ob_polar(theta = x_angle, r = m_dist),
        theta = bb,
        origin = s),
      rotate(
        yy + ob_polar(theta = y_angle, r = m_dist),
        theta = bb * -1,
        origin = e),
      e))
  })

  dots <- rlang::list2(...)

  l <- character(0)


  if (!is.null(dots$label)) {
    l <- dots$label
    if (!S7_inherits(l, ob_label)) l <- ob_label(l)
    dots$label <- NULL
  }

  rlang::inject(ob_bezier(p = p,
                        label = l,
                        label_sloped = FALSE,
                        arrow_head = arrow_head,
                        arrow_fins = arrow_head,
                        resect = resect,
                        !!!dots))
}
