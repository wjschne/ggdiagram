find_side <- function(theta, width, height) {
  ne <- atan2(height, width)
  corners <- c(ne, pi - ne, ne + pi, 2 * pi - ne)
  side <- as.integer(theta > ne) + as.integer(theta > pi - ne) + as.integer(theta > pi + ne) + as.integer(theta > 2 * pi - ne) + 1L
  side[side == 5L] <- 1L
  side
}

rectangle_side <- new_class(
  name = "retangle_side",
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
  "linetype",
  "angle"
)

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
    "label"),
  inherit.aes = FALSE,
  style = rc_styles
)

rc_props <- list(
  # Primary ----
  primary = list(
      width = new_property(class = class_numeric, default = 1),
      height = new_property(class = class_numeric, default = 1)
  ),
  # extra ----
  extra = list(
    radius = new_property(class = class_numeric_or_unit, validator = function(value) {
      if (length(value) > 1) stop("The radius property must be of length 1.")
    })
  ),
  styles = ob_style@properties[rc_styles],
  # Derived ----
  derived = list(
    area = new_property(getter = function(self) {
      self@width * self@height
    }),
    bounding_box = new_property(getter = function(self) {

      d_rect <- self@tibble |>
        dplyr::summarise(xmin = min(x),
                         xmax = max(x),
                         ymin = min(y),
                         ymax = max(y))

      ob_rectangle(southwest = ob_point(d_rect$xmin, d_rect$ymin),
                northeast = ob_point(d_rect$xmax, d_rect$ymax))

    }),
    perimeter = new_property(getter = function(self) {
      self@width * 2 + self@height * 2
    }),
    northeast = new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / 2,
                     self@height / 2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    northwest = new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / -2,
                     self@height / 2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    southwest = new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / -2,
                     self@height / -2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    southeast = new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / 2,
                     self@height / -2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    east = new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / 2,
                     0,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    north = new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(0,
                     self@height / 2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    west = new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(self@width / -2,
                     0,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    south = new_property(
      ob_point,
      getter = function(self) {
        rotate(ob_point(0,
                     self@height / -2,
                     style = self@style),
               self@angle) + self@center
      }
    ),
    side = new_property(rectangle_side, getter = function(self) {
      re = rectangle_side(
        east = ob_segment(p1 = self@northeast, p2 = self@southeast, style = self@style),
        north = ob_segment(p1 = self@northwest, p2 = self@northeast, style = self@style),
        west = ob_segment(p1 = self@northwest, p2 = self@southwest, style = self@style),
        south = ob_segment(p1 = self@southwest, p2 = self@southeast, style = self@style)
      )
    }),
    length = new_property(
      getter = function(self) {
        length(self@width)
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(rc_styles,
                         prop, object = self) |>
          `names<-`(rc_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        ob_rectangle(
          center = self@center,
          width = self@width,
          height = self@height,
          style = self@style + value)
      }
    ),
    tibble = new_property(getter = function(self) {

      d <- list(
        p = tibble::tibble(
          x = c(self@northeast@x,
                self@northwest@x,
                self@southwest@x,
                self@southeast@x),
          y = c(self@northeast@y,
                self@northwest@y,
                self@southwest@y,
                self@southeast@y),
          group = rep(seq(1,self@length), 4)
        ) %>%
          tidyr::nest(.by = group) %>%
          dplyr::mutate(p = purrr::map(data, ob_point)) %>%
          dplyr::pull(p),
        group = seq(1, self@length),
        alpha = self@alpha,
        color = self@color,
        fill = self@fill,
        linewidth = self@linewidth,
        linetype = self@linetype,
        radius = self@radius
      )
      get_non_empty_tibble(d) |>
        dplyr::mutate(
          p = purrr::map(p, \(x) {
            x@tibble |>
              dplyr::select(x,y)})) |>
        tidyr::unnest(p)

      # d <- list(
      #   x = self@center@x,
      #   y = self@center@y,
      #   width = self@width,
      #   height = self@height,
      #   alpha = self@alpha,
      #   color = self@color,
      #   fill = self@fill,
      #   linewidth = self@linewidth,
      #   linetype = self@linetype)
      # get_non_empty_tibble(d)
    })
  ),
  # Functions ----
  funs = list(
    geom = new_property(class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    normal_at = new_property(class_function, getter = function(self) {
      \(theta = degree(0), distance = 1) {
        if (!S7_inherits(theta, ob_angle)) {
          theta <- degree(theta)
        }
        dl <- list(
          alpha = self@alpha,
          color = self@color,
          fill = self@fill,
          linewidth = self@linewidth,
          linetype = self@linetype,
          radius = self@radius
        ) |> get_non_empty_tibble()

        d <- tibble::tibble(
          x0 = self@center@x,
          y0 = self@center@y,
          width = self@width,
          height = self@height,
          angle = self@angle@radian,
          group = seq(self@length)
        ) |>
          tidyr::crossing(theta = theta@radian) %>%
          mutate(
            rtheta = theta - angle,
            side = find_side(rtheta, width, height),
            x = ifelse(side %in% c(1L, 3L),
                       sign(cos(rtheta)) * (width / 2 + distance),
                       sign(cos(rtheta)) * (height / 2) * abs(cos(rtheta) / sin(rtheta))),
            y = ifelse(side %in% c(2L, 4L),
                       sign(sin(rtheta)) * (height / 2 + distance),
                       sign(sin(rtheta)) * (width / 2) * abs(tan(rtheta)))) %>%
          dplyr::select(group, x, y, angle, x0, y0) %>%
          tidyr::nest(.by = c(group, angle, x0, y0)) %>%
          dplyr::mutate(data = purrr::map2(data, angle, \(dd,aa) {
            as.matrix(dd) |>
              rotate2columnmatrix(aa) |>
              `colnames<-`(c("x", "y")) |>
              tibble::as_tibble()
          })) %>%
          tidyr::unnest(data) %>%
          mutate(x = x  + x0,
                 y = y  + y0) %>%
          dplyr::select(group, x,y) %>%
          tidyr::nest(.by = group) %>%
          dplyr::bind_cols(dl) %>%
          tidyr::unnest(data)

        rlang::inject(ob_point(!!!d))

      }
    }),
    point_at = new_property(
      class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (!S7_inherits(theta, ob_angle)) {
            theta <- degree(theta)
          }

          # p <- purrr::map(theta@degree, \(th) {
          #   s <- ob_segment(self@center,
          #           self@center + ob_polar(theta = degree(th),
          #                               r = distance(self@center,
          #                                            self@northeast)))
          #   pp <- bind(intersection(self, s))
          #   st <- self@style + ob_style(...)
          #   pp@style <- st
          #   pp
          # }) |>
          #   bind()

          dl <- list(
            alpha = self@alpha,
            color = self@color,
            fill = self@fill,
            linewidth = self@linewidth,
            linetype = self@linetype,
            radius = self@radius
          ) |> get_non_empty_tibble()

          d <- tibble::tibble(
            x0 = self@center@x,
            y0 = self@center@y,
            width = self@width,
            height = self@height,
            angle = self@angle@radian,
            group = seq(self@length)
          ) |>
            tidyr::crossing(theta = theta@radian) %>%
            dplyr::mutate(
              rtheta = theta - angle,
              side = find_side(rtheta, width, height),
              x = ifelse(side %in% c(1L, 3L),
                         sign(cos(rtheta)) * width / 2,
                         sign(cos(rtheta)) * (height / 2) * abs(cos(rtheta) / sin(rtheta))),
              y = ifelse(side %in% c(2L, 4L),
                         sign(sin(rtheta)) * height / 2,
                         sign(sin(rtheta)) * (width / 2) * abs(tan(rtheta)))) %>%
            dplyr::select(group, x, y, angle, x0, y0) %>%
            tidyr::nest(.by = c(group, angle, x0, y0)) %>%
            dplyr::mutate(data = purrr::map2(data, angle, \(dd,aa) {
              as.matrix(dd) |>
                rotate2columnmatrix(aa) |>
                `colnames<-`(c("x", "y")) |>
                tibble::as_tibble()
            })) %>%
            tidyr::unnest(data) %>%
            dplyr::mutate(x = x  + x0,
                          y = y  + y0) %>%
            dplyr::select(group, x,y) %>%
            tidyr::nest(.by = group) %>%
            dplyr::bind_cols(dl) %>%
            tidyr::unnest(data)

            rlang::inject(ob_point(!!!d))


        }
      }
    )),
  # Information ----
  info = list(
    aesthetics = new_property(getter = function(self) {
      rc_aesthetics_list
    })))

# ob_rectangle ----

#' ob_rectangle class
#' @param center point at center of the circle
#' @param width width
#' @param height height
#' @param east right middle point
#' @param north top middle point
#' @param west left middle point
#' @param south top middle point
#' @param northeast upper right point
#' @param northwest upper left point
#' @param southwest lower left point
#' @param southeast lower right point
#' @param label A character, angle, or label object
#' @param x overrides x-coordinate in `center@x`
#' @param y overrides y-coordinate in `center@x`
#' @param radius A numeric or unit vector of length one, specifying the corner radius
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object
#' @inherit ob_style params
#' @examples
#' # specify center point and radius
#' p <- ob_point(0,0)
#' ob_rectangle(p, width = 2, height = 2)
#' @export
ob_rectangle <- new_class(
  name = "ob_rectangle",
  parent = centerpoint,
  properties = rlang::inject(list(
    !!!rc_props$primary,
    !!!rc_props$extra,
    !!!rc_props$styles,
    !!!rc_props$derived,
    !!!rc_props$funs,
    !!!rc_props$info)),
  constructor = function(center = class_missing,
                         width = class_missing,
                         height = class_missing,
                         east = class_missing,
                         north = class_missing,
                         west = class_missing,
                         south = class_missing,
                         northeast = class_missing,
                         northwest = class_missing,
                         southwest = class_missing,
                         southeast = class_missing,
                         angle = 0,
                         radius = class_missing,
                         label = class_missing,
                         alpha = class_missing,
                         color = "black",
                         fill = NA_character_,
                         linewidth = class_missing,
                         linetype = class_missing,
                         style = class_missing,
                         x = class_missing,
                         y = class_missing,
                         ...) {

    if (!S7_inherits(angle, ob_angle)) angle <- degree(angle)

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
                              length(width) > 0 && length(height) > 0) {

                          } else if (length(center) > 0 &&
                                     (hasnorth || hassouth) && (haswest || haseast)) {
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
                                     (hasnorth || hassouth) && (haswest || haseast)) {
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
                            if (length(width)  == 0) width <- 1
                            if (length(height) == 0) height <- 1
                            if (length(center) == 0) center <- ob_point(0,0)
                            # stop("There is not enough information to make a rectangle.")
                          }


    rc_style <- ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype
      ) +
      style +
      ob_style(...)



    non_empty_list <- get_non_empty_props(rc_style)
    d <- tibble::tibble(x = center@x,
                        y = center@y,
                        width = width,
                        height = height,
                        angle = angle@radian)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }

    center = set_props(center, x = d$x, y = d$y)
    center@style <- rc_style
    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_rectangle",
                               angle = angle)





     new_object(centerpoint(center = center),
                width = d$width,
                height = d$height,
                angle = radian(d$angle),
                 label = label,
                radius = radius,
                 alpha = d[["alpha"]] %||% alpha,
                 color = d[["color"]] %||% color ,
                 fill = d[["fill"]]  %||% fill,
                 linewidth = d[["linewidth"]]  %||% linewidth,
                 linetype = d[["linetype"]]  %||% linetype)
  }
)


method(str, ob_rectangle) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = omit_props(object, include = c("center", "width", "height"))) {
  str_properties(object,
    omit = omit,
    nest.lev = nest.lev
  )
}


method(print, ob_rectangle) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}






method(get_tibble, ob_rectangle) <- function(x) {
  x@tibble
}


method(get_tibble_defaults, ob_rectangle) <- function(x) {
  sp <- ob_style(
    alpha = replace_na(as.double(ggforce::GeomShape$default_aes$alpha), 1),
    color = replace_na(ggforce::GeomShape$default_aes$colour, "black"),
    fill = replace_na(ggforce::GeomShape$default_aes$fill, "black"),
    lineend = "butt",
    linewidth = replace_na(ggforce::GeomShape$default_aes$linewidth, 0.5),
    linetype = replace_na(ggforce::GeomShape$default_aes$default_aes$linetype, 1)
  )
  get_tibble_defaults_helper(
    x = x,
    default_style = sp,
    required_aes = c(rc_aesthetics_list@required_aes, "radius"))
}

method(`==`, list(ob_rectangle, ob_rectangle)) <- function(e1, e2) {
  (e1@center@x == e2@center@x) &&
    (e1@center@y == e2@center@y) &&
    (e1@width == e2@width) &&
    (e1@height == e2@height) %%
    (e1@angle == e2@angle)
}

method(`[`, ob_rectangle) <- function(x, y) {

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
    angle = x@angle@radian) |>
    get_non_empty_tibble()

  d <- d[y,]

  dl <- as.list(dplyr::select(d, -.data$x, -.data$y))
  z <- rlang::inject(ob_rectangle(center = ob_point(d$x, d$y), !!!dl))
  z@label <- x@label[y]
  if (!is.null(dl$angle)) {
    z@angle <- x@angle[y]
  }
  z
}


method(place, list(ob_point, ob_rectangle)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p <- from@point_at(where)
  p_sep <- ob_polar((p - from@center)@theta, sep)
  x@x <- p@x + p_sep@x
  x@y <- p@y + p_sep@y
  x

}

method(place, list(ob_rectangle, ob_point)) <- function(x, from, where = "right", sep = 1) {
  where <- degree(where)
  p_sep <- ob_polar(where, sep)
  p <- x@center - x@point_at(where + degree(180))
  x@center@x <- from@x + p@x + p_sep@x
  x@center@y <- from@y + p@y + p_sep@y
  if (S7_inherits(x@label)) x@label@p <- x@center
  x
}






