#' @keywords internal
#' @noRd
find_side <- function(theta, width = 1, height = 1) {
  my_pi <- turn(.5)
  if (!S7::S7_inherits(theta, ob_angle)) theta <- radian(theta)
  theta <- turn(theta@turn + 1 * (theta@turn < 0))
  ne <- radian(atan2(height, width))
  corners <- c(ne, my_pi - ne, ne + my_pi, 2 * my_pi - ne)
  side <- as.integer(theta > ne) + as.integer(theta > my_pi - ne) + as.integer(theta > my_pi + ne) + as.integer(theta > 2 * my_pi - ne) + 1L
  side[side == 5L] <- 1L
  side
}

#' @keywords internal
#' @noRd
rectangle_side <- S7::new_class(
  name = "retangle_side",
  package = "ggdiagram",
  properties = list(
    east = ob_segment,
    north = ob_segment,
    west = ob_segment,
    south = ob_segment))

rc_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype"
)

#' @keywords internal
#' @noRd
rc_aesthetics_list <- class_aesthetics_list(
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
    "angle",
    "label",
    "id"),
  inherit.aes = FALSE,
  style = rc_styles
)

rc_props <- list(
  # Primary ----
  primary = list(
      width = S7::new_property(class = S7::class_numeric, default = 1),
      height = S7::new_property(class = S7::class_numeric, default = 1),
      angle = S7::new_property(ob_angle_or_numeric, default = 0)
  ),
  # extra ----
  extra = list(
    vertex_radius = S7::new_property(class = class_numeric_or_unit, validator = function(value) {
      if (length(value) > 1) stop("The vertex_radius property must be of length 1.")
    })
  ),
  styles = ob_style@properties[rc_styles],
  # Derived ----
  derived = list(
    area = S7::new_property(getter = function(self) {
      self@width * self@height
    }),
    bounding_box = S7::new_property(getter = function(self) {

      d_rect <- tibble::tibble(
        x = c(
          self@northwest@x,
          self@northeast@x,
          self@southwest@x,
          self@southeast@x
        ),
        y = c(
          self@northwest@y,
          self@northeast@y,
          self@southwest@y,
          self@southeast@y
        )
      ) |>
        dplyr::summarise(xmin = min(x),
                         xmax = max(x),
                         ymin = min(y),
                         ymax = max(y))

      ob_rectangle(southwest = ob_point(d_rect$xmin, d_rect$ymin),
                northeast = ob_point(d_rect$xmax, d_rect$ymax))

    }),
    perimeter = S7::new_property(getter = function(self) {
      self@width * 2 + self@height * 2
    }),
    northeast = S7::new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / 2,
                     self@height / 2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    northwest = S7::new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / -2,
                     self@height / 2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    southwest = S7::new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / -2,
                     self@height / -2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    southeast = S7::new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / 2,
                     self@height / -2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    east = S7::new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / 2,
                     0,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    north = S7::new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(0,
                     self@height / 2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    west = S7::new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / -2,
                     0,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    south = S7::new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(0,
                     self@height / -2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    side = S7::new_property(rectangle_side, getter = function(self) {
      re = rectangle_side(
        east = ob_segment(p1 = self@northeast, p2 = self@southeast, style = self@style),
        north = ob_segment(p1 = self@northwest, p2 = self@northeast, style = self@style),
        west = ob_segment(p1 = self@northwest, p2 = self@southwest, style = self@style),
        south = ob_segment(p1 = self@southwest, p2 = self@southeast, style = self@style)
      )
    }),
    length = S7::new_property(
      getter = function(self) {
        length(self@width)
      }
    ),
    style = S7::new_property(
      getter = function(self) {
        pr <- purrr::map(rc_styles,
                         prop, object = self) |>
          `names<-`(rc_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% rc_styles]
        self <- rlang::inject(S7::set_props(self, !!!s_list))
        self
      }
    ),
    tibble = S7::new_property(getter = function(self) {



      d <- list(
        x = self@center@x,
        y = self@center@y,
        width = self@width,
        height = self@height,
        vertex_radius = self@vertex_radius,
        angle = self@angle@degree,
        alpha = self@alpha,
        color = self@color,
        fill = self@fill,
        linewidth = self@linewidth,
        linetype = self@linetype,
        id = self@id
        )
      get_non_empty_tibble(d)
    })
  ),
  # Functions ----
  funs = list(
    geom = S7::new_property(S7::class_function,  getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    normal_at = S7::new_property(S7::class_function, getter = function(self) {
      \(theta = degree(0), distance = 1) {
        if (!S7::S7_inherits(theta, ob_angle)) {
          theta <- degree(theta)
        }
        dl <- list(
          alpha = self@alpha,
          color = self@color,
          fill = self@fill,
          linewidth = self@linewidth,
          linetype = self@linetype,
          vertex_radius = self@vertex_radius
        ) |> get_non_empty_tibble()

        d <- tibble::tibble(
          x0 = self@center@x,
          y0 = self@center@y,
          width = self@width,
          height = self@height,
          angle = self@angle@radian,
          group = seq(self@length),
          theta = theta@radian
        ) |>
          dplyr::mutate(
            rtheta = theta - angle,
            side = find_side(rtheta, width, height),
            x = ifelse(side %in% c(1L, 3L),
                       sign(cos(rtheta)) * (width / 2 + distance),
                       sign(cos(rtheta)) * (height / 2) * abs(cos(rtheta) / sin(rtheta))),
            y = ifelse(side %in% c(2L, 4L),
                       sign(sin(rtheta)) * (height / 2 + distance),
                       sign(sin(rtheta)) * (width / 2) * abs(tan(rtheta)))) |>
          dplyr::select(group, x, y, angle, x0, y0) |>
          tidyr::nest(.by = c(group, angle, x0, y0)) |>
          dplyr::mutate(data = purrr::map2(data, angle, \(dd,aa) {
            as.matrix(dd) |>
              rotate2columnmatrix(aa) |>
              `colnames<-`(c("x", "y")) |>
              tibble::as_tibble()
          })) |>
          tidyr::unnest(data) |>
          dplyr::mutate(x = x  + x0,
                 y = y  + y0) |>
          dplyr::select(group, x,y) |>
          tidyr::nest(.by = group) |>
          dplyr::bind_cols(dl) |>
          tidyr::unnest(data)

        data2shape(d, ob_point)
      }
    }),
    place = pr_place,
    point_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (!S7::S7_inherits(theta, ob_angle)) {
            theta <- degree(theta)
          }

          dl <- list(
            alpha = self@alpha,
            color = self@color,
            fill = self@fill,
            linewidth = self@linewidth,
            linetype = self@linetype,
            vertex_radius = self@vertex_radius
          ) |> get_non_empty_tibble()

          d <- tibble::tibble(
            x0 = self@center@x,
            y0 = self@center@y,
            width = self@width,
            height = self@height,
            angle = self@angle@radian,
            group = seq(self@length),
            theta = theta@radian
          ) |>
            dplyr::mutate(
              rtheta = theta - angle,
              side = find_side(rtheta, width, height),
              x = ifelse(side %in% c(1L, 3L),
                         sign(cos(rtheta)) * width / 2,
                         sign(cos(rtheta)) * (height / 2) * abs(cos(rtheta) / sin(rtheta))),
              y = ifelse(side %in% c(2L, 4L),
                         sign(sin(rtheta)) * height / 2,
                         sign(sin(rtheta)) * (width / 2) * abs(sin(rtheta) / cos(rtheta)))) |>
            dplyr::select(group, x, y, angle, x0, y0) |>
            tidyr::nest(.by = c(group, angle, x0, y0)) |>
            dplyr::mutate(data = purrr::map2(data, angle, \(dd,aa) {
              if (is.na(aa)) aa <- 0
              as.matrix(dd) |>
                rotate2columnmatrix(aa) |>
                `colnames<-`(c("x", "y")) |>
                tibble::as_tibble()
            })) |>
            tidyr::unnest(data) |>
            dplyr::mutate(x = x  + x0,
                          y = y  + y0) |>
            dplyr::select(group, x,y) |>
            tidyr::nest(.by = group) |>
            dplyr::bind_cols(dl) |>
            tidyr::unnest(data)

          data2shape(d, ob_point)

        }
      }
    )),
  # Information ----
  info = list(
    aesthetics = S7::new_property(getter = function(self) {
      rc_aesthetics_list
    })))

# ob_rectangle ----

#' ob_rectangle class
#' @param center [`ob_point`] at center of the rectangle
#' @param width width
#' @param height height
#' @param east right middle point ([`ob_point`])
#' @param north top middle point ([`ob_point`])
#' @param west left middle point ([`ob_point`])
#' @param south top middle point ([`ob_point`])
#' @param northeast upper right point ([`ob_point`])
#' @param northwest upper left point ([`ob_point`])
#' @param southwest lower left point ([`ob_point`])
#' @param southeast lower right point ([`ob_point`])
#' @param label A character, angle, or [`ob_label`] object
#' @param x overrides x-coordinate in `center@x`
#' @param y overrides y-coordinate in `center@x`
#' @param vertex_radius A numeric or unit vector of length one, specifying the corner radius for rounded corners
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to `style`
#' @inherit ob_style params
#' @export
#' @returns [`ob_rectangle`] object
#'
#' @examples
#' ggdiagram() +
#'   ob_rectangle(center = ob_point(0,0), width = 3, height = 2)
ob_rectangle <- S7::new_class(
  name = "ob_rectangle",
  parent = centerpoint,
  package = "ggdiagram",
  properties = rlang::list2(
    !!!rc_props$primary,
    !!!rc_props$extra,
    !!!rc_props$styles,
    !!!rc_props$derived,
    !!!rc_props$funs,
    !!!rc_props$info),
  constructor = function(center = S7::class_missing,
                         width = numeric(0),
                         height = numeric(0),
                         east = S7::class_missing,
                         north = S7::class_missing,
                         west = S7::class_missing,
                         south = S7::class_missing,
                         northeast = S7::class_missing,
                         northwest = S7::class_missing,
                         southwest = S7::class_missing,
                         southeast = S7::class_missing,
                         angle = numeric(0),
                         vertex_radius = numeric(0),
                         label = character(0),
                         alpha = numeric(0),
                         color = character(0),
                         fill = character(0),
                         linewidth = numeric(0),
                         linetype = numeric(0),
                         style = S7::class_missing,
                         x = numeric(0),
                         y = numeric(0),
                         id = character(0),
                         ...) {

    id <- as.character(id)

    if (length(angle) == 0) angle <- degree(0)


    if (!S7::S7_inherits(angle, ob_angle)) {
      angle <- degree(angle)
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

    hasnorth <- FALSE
    hassouth <- FALSE
    haseast <- FALSE
    haswest <- FALSE

    if (length(north) > 0) {
      top <- north@y
      hasnorth <- TRUE
      if (length(width) > 0) {
        left <- north@x - width / 2
        right <- north@x + width / 2
        haswest <- TRUE
        haseast <- TRUE
      }
      if (length(height) > 0) {
        bottom <- north@y - height
        hassouth <- TRUE
      }
    }

    if (length(south) > 0) {
      bottom <- south@y
      hassouth <- TRUE

      if (length(width) > 0) {
        left <- south@x - width / 2
        right <- south@x + width / 2
        haswest <- TRUE
        haseast <- TRUE
      }
      if (length(height) > 0) {
        top <- south@y + height
        hasnorth <- TRUE
      }

    }

    if (length(west) > 0) {
      left <- west@x
      haswest <- TRUE

      if (length(width) > 0) {
        right <- west@x + width
        haseast <- TRUE
      }

      if (length(height) > 0) {
        top <- west@y - height / 2
        bottom <- west@y + height / 2
        hasnorth <- TRUE
        hassouth <- TRUE
      }
    }

    if (length(east) > 0) {
      right <- east@x
      haseast <- TRUE

      if (length(width) > 0) {
        left <- east@x + width
        haswest <- TRUE
      }

      if (length(height) > 0) {
        top <- east@y - height / 2
        bottom <- east@y + height / 2
        hasnorth <- TRUE
        hassouth <- TRUE
      }
    }

    if (length(northeast) > 0) {
      top <- northeast@y
      right <- northeast@x
      hasnorth <- TRUE
      haseast <- TRUE
    }
    if (length(northwest) > 0) {
      top <- northwest@y
      left <- northwest@x
      hasnorth <- TRUE
      haswest <- TRUE
    }

    if (length(southeast) > 0) {
      bottom <- southeast@y
      right <- southeast@x
      hassouth <- TRUE
      haseast <- TRUE
    }

    if (length(southwest) > 0) {
      bottom <- southwest@y
      left <- southwest@x
      hassouth <- TRUE
      haswest <- TRUE
    }

    if (hassouth && hasnorth) {
      height <- abs(top - bottom)
    }

    if (haswest && haseast) {
      width <- abs(left - right)
    }

    if (length(center) > 0 &&
        length(width) > 0 &&
        length(height) > 0) {

    } else if (length(center) > 0 &&
               (hasnorth ||
                hassouth) && (haswest || haseast)) {
      if (haseast) {
        width <- abs(center@x - right) * 2
      } else {
        width <- abs(center@x - left) * 2
      }
      if (hasnorth) {
        height <- abs(center@y - top) * 2
      } else {
        height <- abs(center@y - bottom) * 2
      }

    } else if (length(width) > 0 &&
               length(height) > 0 &&
               (hasnorth ||
                hassouth) && (haswest || haseast)) {
      if (haseast) {
        c.x <- right - width / 2
      } else {
        c.x <- left + width / 2
      }
      if (hasnorth) {
        c.y <- top - height / 2
      } else {
        c.y <- bottom + height / 2
      }
      center = ob_point(c.x, c.y)
    } else {
      if (length(width)  == 0)
        width <- 1
      if (length(height) == 0)
        height <- 1
      if (length(center) == 0)
        center <- ob_point(0, 0)
      # stop("There is not enough information to make a rectangle.")
    }


    rc_style <- ob_style(fill = NA_character_,
                         color = "black") +
      style +
      ob_style(
      alpha = alpha,
      color = color,
      fill = fill,
      linewidth = linewidth,
      linetype = linetype,
      id = id
    ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(rc_style)
    d <- tibble::tibble(
      x = center@x,
      y = center@y,
      width = width,
      height = height,
      angle = angle@radian
    )
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }

    center = set_props(center, x = d$x, y = d$y)
    center@style <- rc_style
    label <- centerpoint_label(
      label,
      center = center,
      d = d,
      shape_name = "ob_rectangle",
      angle = angle
    )

    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }

    S7::new_object(
      centerpoint(center = center),
      width = d$width,
      height = d$height,
      angle = radian(d$angle),
      label = label,
      vertex_radius = vertex_radius,
      alpha = d[["alpha"]] %||% alpha,
      color = d[["color"]] %||% color ,
      fill = d[["fill"]]  %||% fill,
      linewidth = d[["linewidth"]]  %||% linewidth,
      linetype = d[["linetype"]]  %||% linetype,
      id = d[["id"]] %||% id
    )
  }
)


S7::method(str, ob_rectangle) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = omit_props(object, include = c("center", "width", "height"))) {
  str_properties(object,
    omit = omit,
    nest.lev = nest.lev
  )
}

S7::method(get_tibble, ob_rectangle) <- function(x) {
  xx <- x
  d <- list(
    p = tibble::tibble(
      x = c(xx@northeast@x,
            xx@northwest@x,
            xx@southwest@x,
            xx@southeast@x),
      y = c(xx@northeast@y,
            xx@northwest@y,
            xx@southwest@y,
            xx@southeast@y),
      group = rep(seq(1,xx@length), 4)
    ) |>
      tidyr::nest(.by = group) |>
      dplyr::mutate(p = purrr::map(data, ob_point)) |>
      dplyr::pull(p),
    group = seq(1, x@length),
    alpha = xx@alpha,
    color = xx@color,
    fill = xx@fill,
    linewidth = xx@linewidth,
    linetype = xx@linetype,
    radius = xx@vertex_radius
  )
  get_non_empty_tibble(d) |>
    dplyr::mutate(
      p = purrr::map(p, \(xxx) {
        xxx@tibble |>
          dplyr::select(dplyr::all_of(c("x", "y")))})) |>
    tidyr::unnest(p)
}


S7::method(get_tibble_defaults, ob_rectangle) <- function(x) {
  sp <- ob_style(
    alpha = replace_na(as.double(ggforce::GeomShape$default_aes$alpha), 1),
    color = replace_na(ggforce::GeomShape$default_aes$colour, "black"),
    fill = replace_na(ggforce::GeomShape$default_aes$fill, "black"),
    lineend = "butt",
    linewidth = replace_na(ggforce::GeomShape$default_aes$linewidth, 0.5),
    linetype = replace_na(ggforce::GeomShape$default_aes$linetype, 1)
  )
  get_tibble_defaults_helper(
    x = x,
    default_style = sp,
    required_aes = c(rc_aesthetics_list@required_aes, "radius"))
}

S7::method(`==`, list(ob_rectangle, ob_rectangle)) <- function(e1, e2) { # nocov start
  (e1@center == e2@center) &&
    (e1@width == e2@width) &&
    (e1@height == e2@height) &&
    (e1@angle == e2@angle)
} # nocov end

S7::method(`[`, ob_rectangle) <- function(x, i) {
  i <- character_index(i, x@id)

  d <- list(
    x = x@center@x,
    y = x@center@y,
    width = x@width,
    height = x@height,
    alpha = x@alpha,
    color = x@color,
    fill = x@fill,
    linewidth = x@linewidth,
    linetype = x@linetype,
    angle = x@angle@radian,
    id = x@id) |>
    get_non_empty_tibble()

  d <- d[i,]
  z <- data2shape(d, ob_rectangle)

  z@label <- na2zero(x@label[i])
  if (!is.null(d$angle)) {
    z@angle <- x@angle[i]
  }
  z
}


S7::method(place, list(ob_point, ob_rectangle)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p <- from@point_at(where)
  p_sep <- ob_polar((p - from@center)@theta, sep)
  x@x <- p@x + p_sep@x
  x@y <- p@y + p_sep@y
  x

}

S7::method(place, list(ob_rectangle, ob_point)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p_sep <- ob_polar(where, sep)
  p <- x@center - x@point_at(where + degree(180))
  x@center@x <- from@x + p@x + p_sep@x
  x@center@y <- from@y + p@y + p_sep@y
  if (S7::S7_inherits(x@label)) x@label@center <- x@center
  x
}


S7::method(ob_array, ob_rectangle) <- function(
    x,
    k = 2,
    sep = 1,
    where = "east",
    anchor = "center",
    ...) {
  sa <- ob_array_helper(
    x = x,
    k = k,
    sep = sep,
    where = where,
    anchor = anchor,
    ...
  )

  rlang::inject(ob_rectangle(
    center = sa$p_center,
    width = x@width,
    height = x@height,
    angle = x@angle@degree,
    style = x@style,
    vertex_radius = x@vertex_radius,
    !!!sa$dots
  ))
}


