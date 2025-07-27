pt_styles <- c("alpha", "color", "fill", "shape", "size", "stroke")

pr_place <- S7::new_property(S7::class_function, getter = function(self) {
  \(from, where = "right", sep = 1, ...) place(x = self, from = from, where = where, sep = sep, ...)
})

pt_props <- list(
  # Primary ----
  primary = list(
    x = S7::new_property(class = S7::class_numeric, default = 0),
    y = S7::new_property(class = S7::class_numeric, default = 0)
  ),
  styles = ob_style@properties[pt_styles],
  # Derived ----
  derived = list(
    auto_label = S7::new_property(getter = function(self) {
      label_object(self)
    }),
    bounding_box = S7::new_property(getter = function(self) {
      ob_rectangle(southwest = ob_point(x = min(self@x), y = min(self@y)),
                northeast = ob_point(x = max(self@x), y = max(self@y)))
    }),
    centroid = S7::new_property(getter = function(self) {
      ob_point(mean(self@x), mean(self@y), style = self[1]@style)
    }),
    length = S7::new_property(
      getter = function(self) {
        length(self@x)
      }
    ),
    r = S7::new_property(
      getter = function(self) {
        sqrt(self@x ^ 2 + self@y ^ 2)
      },
      setter = function(self, value) {
        set_props(self, x = cos(self@theta) * value, y = sin(self@theta) * value)
      }
    ),
    theta = S7::new_property(
      getter = function(self) {
        radian(radian = atan2(self@y, self@x))
      },
      setter = function(self, value) {
        set_props(self, x = cos(value) * self@r, y = sin(value) * self@r)
      }
    ),
    style = S7::new_property(
      getter = function(self) {
        pr <- purrr::map(pt_styles,
                         prop, object = self) |>
          `names<-`(pt_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% pt_styles]
        self <- rlang::inject(S7::set_props(self, !!!s_list))
        self
      }
    ),
    tibble = S7::new_property(getter = function(self) {
      d <- list(
        x = self@x,
        y = self@y,
        alpha = self@alpha,
        color = self@color,
        fill = self@fill,
        shape = self@shape,
        size = self@size,
        stroke = self@stroke,
        id = self@id
      )
      get_non_empty_tibble(d)
    }),
    xy = S7::new_property(
      getter = function(self) {
        cbind(x = self@x, y = self@y)
      }
    )
  ),
  # Functions ----
  funs = list(
    geom = S7::new_property(S7::class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    label = S7::new_property(S7::class_function, getter = function(self) {
      \(label = NULL,
        accuracy = .1,
        ...
      ) {
        if (is.null(label)) {
          label = label_object(self, accuracy)

        }
        if (is.numeric(label) & !S7::S7_inherits(label)) {
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
    aesthetics = S7::new_property(getter = function(self) {
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
#' @param theta Angle of the vector from the origin to the [`ob_point`]
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to `style`
#' @slot auto_label Gets x and y coordinates and makes a label `"(x,y)"`
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to `ggplot2::geom_point`.
#' @slot length The number of points in the ob_point object
#' @param style Gets and sets the styles associated with points
#' @slot tibble Gets a [`tibble::tibble`] containing parameters and styles used by [`ggplot2::geom_point`].
#' @slot xy Gets a 2-column matrix of the x and y coordinates of the ob_point object.
#' @inherit ob_style params
#' @export
#' @returns ob_point object
#' @examples
#' ggdiagram() +
#'   ob_point(1:5, 1:5) +
#'   ggplot2::theme_minimal()
#'
#' ggdiagram() +
#'   ob_polar(degree(seq(0, 330, 30)), r = 2) +
#'   ggplot2::theme_minimal()
ob_point <- S7::new_class(
  name = "ob_point",
  parent = xy,
  properties = rlang::list2(
    !!!pt_props$primary,
    !!!pt_props$styles,
    !!!pt_props$derived,
    !!!pt_props$funs,
    !!!pt_props$info),
  constructor = function(x = 0,
                         y = 0,
                         alpha = numeric(0),
                         color = character(0),
                         fill = character(0),
                         shape = numeric(0),
                         size = numeric(0),
                         stroke = numeric(0),
                         style = S7::class_missing,
                         id = character(0),
                         ...) {

    id <- as.character(id)

    if (inherits(x, "data.frame")) {
      return(rlang::inject(ob_point(!!!get_non_empty_list(x))))
    }

    if (inherits(x, "matrix")) {
      if (ncol(x) == 2) {
        y <- x[,2]
        x <- x[,1]
        # return(ob_point(x[,1], x[,2]))
      } else {
        stop(paste0("The input matrix must have 2 columns, not ", ncol(x), "."))
      }

    }

    p_style <- style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        shape = shape,
        size = size,
        stroke = stroke,
        id = id
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(p_style)
    d <- tibble::tibble(x = x, y = y)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }



     S7::new_object(S7::S7_object(),
                 x = d$x,
                 y = d$y,
                 alpha = d[["alpha"]] %||% alpha,
                 color = d[["color"]] %||% color ,
                 fill = d[["fill"]]  %||% fill,
                 shape = d[["shape"]] %||% shape,
                 size = d[["size"]] %||% size,
                 stroke = d[["stroke"]] %||% stroke,
                 id = d[["id"]] %||% id)
  }
)


# ob_polar point class ----
#' ob_polar
#'
#' Polar points are ordinary points but are specified with an angle (theta) and a radial distance (r)
#' @rdname ob_point
#' @export
ob_polar <- S7::new_class(
  name = "ob_polar",
  parent = ob_point,
  constructor = function(theta = S7::class_missing,
                         r = numeric(0),
                         alpha = numeric(0),
                         color = character(0),
                         fill = character(0),
                         shape = numeric(0),
                         size = numeric(0),
                         stroke = numeric(0),
                        style = S7::class_missing,
                        id = character(0)) {

    id <- as.character(id)

    if (length(r) == 0) r <- 1
    if (length(theta) == 0) theta <- degree(0)
    if (is.character(theta)) theta <- degree(theta)


    p <- ob_point(x = cos(theta) * r,
               y = sin(theta) * r,
               alpha = alpha,
               color = color,
               fill = fill,
               shape = shape,
               size = size,
               stroke = stroke,
               style = style,
               id = id
              )

    S7::new_object(p)
  })


S7::method(str, ob_point) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c("x","y"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                additional = additional)
}

S7::method(str, ob_polar) <- function(
  object,
  nest.lev = 0,
  additional = TRUE,
  omit = omit_props(object, include = c("x","y", "r", 'theta'))) {
str_properties(object,
               omit = omit,
               nest.lev = nest.lev,
              additional = additional)
}

S7::method(get_tibble, ob_point) <- function(x) {
  x@tibble
}

S7::method(get_tibble_defaults, ob_point) <- function(x) {
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

#' Convert hjust and vjust parameters from polar coordinates
#'
#' This function is how [`ob_label`]'s `vjust` and
#' `hjust` values are recalculated automatically when the `polar_just` parameter is specified.
#' @param x angle. Can be a named direction (e.g., "north"), number (in degrees), [`degree`], [`radian`], or [`turn`]
#' @param multiplier distance
#' @param axis vertical (v) or horizontal (h)
#' @export
#' @returns ob_angle object
#' @examples
#' a <- "northwest"
#' polar2just(a, axis = "h")
#' polar2just(a, axis = "v")
polar2just <- S7::new_generic(
  name = "polar2just",
  dispatch_args = "x",
  fun = function(x, multiplier = NULL, axis = c("h", "v")) {
    S7::S7_dispatch()
  }
)
S7::method(polar2just, S7::class_numeric) <- function(x, multiplier = NULL, axis = c("h", "v")) {
  if (length(multiplier) == 0 | is.null(multiplier)) multiplier <- 1.2
  axis <- match.arg(axis)
  if (axis == "h") {
    (((cos(x + pi) + 1)/2) - 0.5) * multiplier + 0.5
  } else {
    (((sin(x + pi) + 1)/2) - 0.5) * multiplier + 0.5
  }

}

S7::method(polar2just, S7::class_character) <- function(x, multiplier = NULL, axis = c("h", "v")) {
  x <- degree(x)
  polar2just(x@radian, multiplier, axis)
}

S7::method(polar2just, ob_angle) <- function(x, multiplier = NULL, axis = c("h", "v")) {
  polar2just(x@radian, multiplier, axis)
}

S7::method(polar2just, ob_point) <- function(x, multiplier = NULL, axis = c("h", "v")) {
  if (length(multiplier) == 0 | is.null(multiplier)) multiplier <- x@r
  polar2just(x@theta, multiplier, axis)
}

S7::method(polar2just, ob_polar) <- function(x, multiplier = NULL, axis = c("h", "v")) {
  if (length(multiplier) == 0 | is.null(multiplier)) multiplier <- x@r
  polar2just(x@theta, multiplier, axis)
}

S7::method(`==`, list(ob_point, ob_point)) <- function(e1, e2) {
  (e1@x == e2@x) & (e1@y == e2@y) # nocov
}



# arithmetic ----
purrr::walk(list(`+`, `-`, `*`, `/`, `^`), \(.f) { # nocov start
  S7::method(.f, list(ob_point, ob_point)) <- function(e1, e2) {
    x <- .f(e1@x, e2@x)
    y <- .f(e1@y, e2@y)
    e2@x <- x
    e2@y <- y
    e2@style <- e1@style + e2@style
    e2
  } # nocov end

  S7::method(.f, list(ob_point, S7::class_numeric)) <- function(e1, e2) { # nocov start
    e1@x <- .f(e1@x, e2)
    e1@y <- .f(e1@y, e2)
    e1
  } # nocov end
  S7::method(.f, list(S7::class_numeric, ob_point)) <- function(e1, e2) { # nocov start
    e2@x <- .f(e1, e2@x)
    e2@y <- .f(e1, e2@y)
    e2
  } # nocov end
})

S7::method(midpoint, list(ob_point, ob_point)) <- function(x,y, position = .5, ...) {
  p <- x + ((y - x) * position)
  s <- rlang::list2(...)
  rlang::inject(set_props(p, !!!s))
}

S7::method(`%*%`, list(ob_point, ob_point)) <- function(x, y) { # nocov start
  x@xy[1, , drop = TRUE] %*% y@xy[1, , drop = TRUE]
} # nocov end

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
#' @returns ob_point object
`%|-%` <- S7::new_generic("%|-%", c("e1", "e2"), fun = function(e1,e2) {S7::S7_dispatch()})

S7::method(`%|-%`, list(ob_point, ob_point)) <- function(e1,e2) {
  e2@x <- e1@x
  e2
  }



#' @name perpendicular_horizontal
#' @rdname perpendicular_point
#' @aliases %-|%
#' @export
#' @returns ob_point object
`%-|%` <- S7::new_generic("%-|%", c("e1", "e2"), fun = function(e1,e2) {S7::S7_dispatch()})

S7::method(`%-|%`, list(ob_point, ob_point)) <- function(e1,e2) {
  e2@y <- e1@y
  e2
  }

S7::method(label_object, ob_point) <- function(object, accuracy = .1) {

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



S7::method(`[`, ob_point) <- function(x, i) {
  i <- character_index(i, x@id)
  data2shape(x@tibble[i,], ob_point)
}

#' @export
`[<-.ggdiagram::ob_point` <- function(x, i, value) {
  i <- character_index(i, x@id)
  d <- assign_data(x, i, value)
  data2shape(d, ob_point)
}

# Connect ----

S7::method(connect, list(ob_point, ob_point)) <- function(
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
  if (is.null(from_offset) && is.null(to_offset) && (is.null(arc_bend) || all(arc_bend == 0))) {
    s <- ob_segment(from,
                    to,
                    label = label,
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
                    id = id, ...)
  } else if (!is.null(arc_bend)) {
    if(any(arc_bend == 0)) stop("An arc cannot have an arc_bend of 0.")
    m <- midpoint(from, to)
    chord_distance <- distance(from, to)
    theta_arc <- (m - from)@theta + sign(arc_bend) * degree(-90)
    m_arc <- ob_polar(theta_arc, r = 0.5 * chord_distance * abs(arc_bend)) + m
    sagitta_distance <- distance(m, m_arc)
    r_arc <- sagitta_distance / 2 + (chord_distance ^ 2) / (8 * sagitta_distance)
    center <- m_arc + ob_polar(theta_arc - turn(.5), r_arc)
    cc <- ob_circle(center = center,
                radius = distance(from, center))
    theta_start <- cc@angle_at(from)@positive
    theta_end <- cc@angle_at(to)@positive
    theta_end[arc_bend < 0 & theta_end > theta_start] <- theta_end[arc_bend < 0 & theta_end > theta_start] + turn(-1)


    theta_end[arc_bend > 0 & theta_end < theta_start] <- theta_end[arc_bend > 0 & theta_end < theta_start] + turn(1)
    s <- ob_arc(center = center,
                radius = cc@radius,
                label = label,
                start = theta_start,
                end = theta_end,
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

  } else {
    from1 <- NULL
    to1 <- NULL
    if (is.character(from_offset)) {
      from_offset <- ob_polar(theta = degree(from_offset), r = distance(from,to))
    }

    if (is.character(to_offset)) {
      to_offset <- ob_polar(theta = degree(to_offset), r = distance(from,to))
    }

    d <- tibble::tibble(from_x = from@x,
                        from_y = from@y,
                        to_x = to@x,
                        to_y = to@y)



    if (!is.null(from_offset)) {
      from1 <- from + from_offset
      d <- d |>
        dplyr::mutate(fromoffset_x = from1@x,
                      fromoffset_y = from1@y)
    }

    if (!is.null(to_offset)) {
      to1 <- to + to_offset
      d <- d |>
        dplyr::mutate(tooffset_x = to1@x,
                      tooffset_y = to1@y)
    }



    p_control <- d |>
      dplyr::mutate(rowid = dplyr::row_number()) |>
      tidyr::pivot_longer(-rowid) |>
      tidyr::separate(name, c("control", "name")) |>
      tidyr::pivot_wider() |>
      dplyr::mutate(control = factor(
        control,
        levels = c("from", "fromoffset", "tooffset", "to"))) |>
      dplyr::arrange(rowid, control) |>
      dplyr::select(-control) |>
      tidyr::nest(.by = rowid) |>
      dplyr::pull(data) |>
      purrr::map(ob_point)

    s <- ob_bezier(p = p_control,
                   label = label,
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
                   ...
                   )

  }

  s

}

S7::method(place, list(ob_point, ob_point)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p <- ob_polar(where, sep)
  x@x <- from@x + p@x
  x@y <- from@y + p@y
  x
}

point_or_list <- S7::new_union(ob_point, S7::class_list)

# Nudge ----

S7::method(nudge, list(ob_point, S7::class_numeric, S7::class_numeric)) <- function(object, x, y) {
  object + ob_point(x, y)
}

S7::method(nudge, list(ob_point, S7::class_numeric, S7::class_missing)) <- function(object, x, y) {
  object + ob_point(x, 0)
}

S7::method(nudge, list(ob_point, S7::class_missing, S7::class_numeric)) <- function(object, x, y) {
  object + ob_point(0, y)
}

S7::method(ob_array, ob_point) <- function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {
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


S7::method(ob_covariance, list(ob_point, ob_point)) <- function(
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
    e <- yy
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


