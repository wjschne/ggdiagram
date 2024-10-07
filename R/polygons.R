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
          paste0("Group ", idx, " needs at least 3 points. It has ", x@length, ".")
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
    radius = new_property(class = class_numeric_or_unit, validator = function(value) {
      if (length(value) > 1) stop("The radius property must be of length 1.")
    })
  ),
  styles = ob_style@properties[ob_polygon_styles],
  # derived ----
  derived = list(
    bounding_box = new_property(getter = function(self) {

      d_rect <- self@tibble |>
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
        ob_point(self@x, self@y, style = self@style + value)
      }
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
        radius = self@radius
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

#' The ob_polygon (polygon) class
#'
#' A polygon is specified with an obpoint that contains at least 3 points, the start and the end. Any number of intermediate points are possible.
#'
#' If you wish to specify multiple polygons, you must supply a list of ob_points. When plotted, the ob_polygon function uses the ggplot2::geom_polygon function to create the geom.
#' @export
#' @param p ob_point or list of ob_point objects
#' @param label A character, angle, or label object
#' @param radius A numeric or unit vector of length one, specifying the corner radius
#' @slot length The number of polygons in the ob_polygon object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @param style Gets and sets the styles associated with polygons
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggplot2::geom_polygon`.
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
                         label = class_missing,
                         radius = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         linewidth = class_missing,
                         linetype = class_missing,
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
        if (all(label@p@x == 0) && all(label@p@y == 0)) {
          label@p <- center
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
               radius = radius,
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
  x@tibble
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
  dl <- d %>%
    dplyr::select(-.data$x, -.data$y, -.data$group) %>%
    unique() %>%
    as.list()
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
#' @param radius A numeric or unit vector of length one, specifying the vertex radius
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
        purrr::map(as.list(self@center), \(cc) {
          cc + ob_polar(degree(c(90, 210, 330)),
                        r = self@width * .5 / cos(degree(30)))
        })
      }),
      polygon = new_property(getter = function(self) {
        ob_polygon(self@p, style = self@style, radius = self@radius)
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
                         label = class_missing,
                         top = class_missing,
                         left = class_missing,
                         right = class_missing,
                         radius = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         linewidth = class_missing,
                         linetype = class_missing,
                         x0 = class_missing,
                         y0 = class_missing,
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
      if (all(label@p == ob_point(0,0))) {
        label@p <- center
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
               radius = radius,
               alpha = d[["alpha"]] %||% alpha,
               color = d[["color"]] %||% color,
               fill = d[["fill"]] %||% fill,
               linewidth = d[["linewidth"]] %||% linewidth,
               linetype = d[["linetype"]] %||% linetype
    )
  })


method(get_tibble, ob_intercept) <- function(x) {
  x@tibble
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
