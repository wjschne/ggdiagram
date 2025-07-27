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
    "label",
    "id"),
  inherit.aes = FALSE,
  style = ob_polygon_styles
)


ob_polygon_props <- list(
  # primary ----
  primary = list(
    p = S7::new_property(class = point_or_list, validator = function(value) {
      if (inherits(value, "list")) {
        allsameclass(value, "ob_point")
      }

      if (S7::S7_inherits(value, ob_point)) value <- list(value)

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
    vertex_radius = S7::new_property(class = class_numeric_or_unit, validator = function(value) {
      if (length(value) > 1) stop("The vertex_radius property must be of length 1.")
    })
  ),
  styles = ob_style@properties[ob_polygon_styles],
  # derived ----
  derived = list(
    bounding_box = S7::new_property(getter = function(self) {

      d_rect <- get_tibble(self) |>
        dplyr::summarise(xmin = min(x),
                         xmax = max(x),
                         ymin = min(y),
                         ymax = max(y))

      ob_rectangle(southwest = ob_point(d_rect$xmin, d_rect$ymin),
                northeast = ob_point(d_rect$xmax, d_rect$ymax))

    }),
    center = S7::new_property(ob_point, getter = function(self) {
      d <- get_tibble(self)
      gr <- dplyr::intersect(colnames(d), c("group", pt_styles))
      d <- dplyr::summarise(d, .by = dplyr::any_of(gr),
                       x = mean(x, na.rm = TRUE),
                       y = mean(y, na.rm = TRUE)) |>
        dplyr::select(-.data$group)

      rlang::inject(ob_point(!!!d))
    }),
    length = S7::new_property(
      getter = function(self) {
        if (inherits(self@p, "list")) {
          l <- length(self@p)
        } else l <- 1
        l
      }
    ),
    segment = S7::new_property(getter = function(self) {
      self@p |>
        purrr::map(\(pp) {
          k <- pp@length
          ob_segment(pp[c(1:k, 1)])
        })
    }),
    style = S7::new_property(
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
    tibble = S7::new_property(getter = function(self) {
      p <- self@p
      if (S7::S7_inherits(self@p, ob_point)) p <- list(p)
      d <- list(
        p = p,
        group = seq(1, self@length),
        alpha = self@alpha,
        color = self@color,
        fill = self@fill,
        linewidth = self@linewidth,
        linetype = self@linetype,
        vertex_radius = self@vertex_radius,
        id = self@id
      )
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
    point_at = S7::new_property(S7::class_function, getter = function(self) {
      \(theta = degree(0), ...) {
        if (!S7::S7_inherits(theta, ob_angle)) theta <- degree(theta)

      }
    })
  ),
  # info ----
  info = list(aesthetics = S7::new_property(
    getter = function(self) {
      ob_polygon_aesthetics
    }
  ))
)


# ob_polygon ----

#' The ob_polygon class
#'
#' A polygon is specified with an [`ob_point`] that contains at least 3 points, the start and the end. Any number of intermediate points are possible.
#'
#' If you wish to specify multiple polygons, you must supply a list of [`ob_point`] objects. When plotted, the ob_polygon function uses the [`ggforce::geom_shape`] function to create the geom.
#' @export
#' @returns ob_polygon object
#' @param p [`ob_point`] or list of [`ob_point`] objects
#' @param label A character, angle, or [`ob_label`] object
#' @param vertex_radius A numeric or unit vector of length one, specifying the corner radius
#' @slot length The number of polygons in the ob_polygon object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @param style Gets and sets the styles associated with polygons
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by [`ggforce::geom_shape`].
#' @inherit ob_style params
ob_polygon <- S7::new_class(
  name = "ob_polygon",
  parent = has_style,
  properties = rlang::list2(
      !!!ob_polygon_props$primary,
      !!!ob_polygon_props$extra,
      !!!ob_polygon_props$styles,
      !!!ob_polygon_props$derived,
      !!!ob_polygon_props$funs,
      !!!ob_polygon_props$info
  ),
  constructor = function(p = S7::class_missing,
                         label = character(0),
                         vertex_radius = numeric(0),
                         alpha = numeric(0),
                         color = character(0),
                         fill = character(0),
                         linewidth = numeric(0),
                         linetype = numeric(0),
                         style = S7::class_missing,

                         id = character(0),
                         ...) {

    id <- as.character(id)

    ob_polygon_style <- style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype,
        id = id
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(ob_polygon_style)

    if (S7::S7_inherits(p, ob_point)) p <- list(p)

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
      if (S7::S7_inherits(label, ob_label)) {
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

    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }


    S7::new_object(.parent = S7::S7_object(),
               p =  d$p,
               label = label,
               vertex_radius = vertex_radius,
               alpha = d[["alpha"]] %||% alpha,
               color = d[["color"]] %||% color,
               fill = d[["fill"]] %||% fill,
               linewidth = d[["linewidth"]] %||% linewidth,
               linetype = d[["linetype"]] %||% linetype,
               id = d[["id"]] %||% id
    )

  })


S7::method(str, ob_polygon) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c(""))) {

  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
  cli::cli_h1("<points>")
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

S7::method(get_tibble, ob_polygon) <- function(x) {
  d <- x@tibble |>
    dplyr::mutate(p = purrr::map(p, \(x) {x@tibble |> dplyr::select(x,y)})) |>
    tidyr::unnest(p)
  if ("vertex_radius" %in% colnames(d)) {
    d <- dplyr::rename(d, radius = vertex_radius)
  }
  d
}


S7::method(as.geom, ob_polygon) <- function(x, ...) {
  gp <- as.geom(S7::super(x, has_style), ...)
  if (S7::S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gp <- list(gp, gl)
  }
  gp
}

S7::method(connect, list(ob_polygon, ob_polygon)) <- function(
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
  centroid_segment <- ob_segment(from@center, to@center)
  connect(intersection(from, centroid_segment), intersection(to, centroid_segment),
          label = label,
          from_offset = from_offset,
          arc_bend = arc_bend,
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

S7::method(connect, list(ob_polygon, ob_point)) <- function(
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
  centroid_segment <- ob_segment(from@center, to)
  connect(intersection(from, centroid_segment), to,
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

S7::method(connect, list(ob_point, ob_polygon)) <- function(
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
  centroid_segment <- ob_segment(from, to@center)
  connect(from, intersection(to, centroid_segment),
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


S7::method(connect, list(centerpoint, ob_polygon)) <- function(
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
  centroid_segment <- ob_segment(from@center, to@center)
  p <- intersection(to, centroid_segment)
  connect(from,
          p,
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


S7::method(connect, list(ob_polygon, centerpoint)) <- function(
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
  centroid_segment <- ob_segment(from@center, to@center)
  p <- intersection(from, centroid_segment)
  connect(p,
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

# ob_intercept ----
#' ob_intercept
#'
#' Triangle polygons used in path diagrams.
#' @param center [`ob_point`] at center
#' @param width length of side
#' @param label A character, angle, or [`ob_label`] object
#' @param vertex_radius A numeric or unit vector of length one, specifying the vertex radius
#' @param top Top vertex of triangle
#' @param left Left vertex of triangle
#' @param right Right vertex of triangle
#' @param x overrides x-coordinate in `center@x`
#' @param y overrides x-coordinate in `center@y`
#' @slot length The number of polygons in the ob_polygon object
#' @param style Gets and sets the styles associated with polygons
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggplot2::geom_polygon`.
#' @inherit ob_style params
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
#' @returns ob_polygon object
ob_intercept <- S7::new_class(
  name = "ob_intercept",
  parent = centerpoint,
  properties = rlang::list2(
      width = S7::class_numeric,
      p = S7::new_property(getter = function(self) {
        purrr::map(unbind(self@center), \(cc) {
          cc + ob_polar(degree(c(90, 210, 330)),
                        r = self@width * .5 / cos(degree(30)))
        })
      }),
      polygon = S7::new_property(getter = function(self) {
        ob_polygon(
          self@p,
          style = self@style,
          vertex_radius = self@vertex_radius)
      }),
      top = S7::new_property(getter = function(self) {
        self@center + ob_polar(degree(90),
                               r = self@width * .5 / cos(degree(30)))
      }),
      left = S7::new_property(getter = function(self) {
        self@center + ob_polar(degree(210),
                               r = self@width * .5 / cos(degree(30)))
      }),
      right = S7::new_property(getter = function(self) {
        self@center + ob_polar(degree(330),
                               r = self@width * .5 / cos(degree(30)))
      }),
      !!!ob_polygon_props$extra,
      !!!ob_polygon_props$styles,
      !!!ob_polygon_props$derived[!names(ob_polygon_props$derived) %in% c("center")],
      !!!ob_polygon_props$funs,
      !!!ob_polygon_props$info
  ),
  constructor = function(center = ob_point(0,0),
                         width = 1,
                         label = character(0),
                         top = S7::class_missing,
                         left = S7::class_missing,
                         right = S7::class_missing,
                         vertex_radius = numeric(0),
                         alpha = numeric(0),
                         color = character(0),
                         fill = character(0),
                         linewidth = numeric(0),
                         linetype = numeric(0),
                         x = numeric(0),
                         y = numeric(0),
                         style = S7::class_missing,
                         id = character(0),
                         ...) {

    id <- as.character(id)

    if ((length(x) > 0) || (length(y) > 0)) {
      if (length(x) == 0) {
        x <- 0
      }
      if (length(y) == 0) {
        y <- 0
      }
      center <- ob_point(x = x, y = y)
    }

    ob_polygon_style <- ob_style() + center@style + style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype,
        id = id
      ) +
      ob_style(...)


    non_empty_list <- get_non_empty_props(ob_polygon_style)

    d <- tibble::tibble(x = center@x, y = center@y, width = width)

    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }

    center = set_props(center, x = d$x, y = d$y)

    if (S7::S7_inherits(label, ob_label)) {
      if (all(label@center == ob_point(0,0))) {
        label@center <- center
      }
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_intercept")

    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }

    S7::new_object(.parent = S7::S7_object(),
               center =  center,
               width = d$width %||% width,
               label = label,
               vertex_radius = vertex_radius,
               alpha = d[["alpha"]] %||% alpha,
               color = d[["color"]] %||% color,
               fill = d[["fill"]] %||% fill,
               linewidth = d[["linewidth"]] %||% linewidth,
               linetype = d[["linetype"]] %||% linetype,
               id = d[["id"]] %||% id
    )
  })


S7::method(get_tibble, ob_intercept) <- function(x) {
  d <- x@tibble |>
    dplyr::mutate(p = purrr::map(p, \(x) {x@tibble |> dplyr::select(x,y)})) |>
    tidyr::unnest(p)
  if ("vertex_radius" %in% colnames(d)) {
    d <- dplyr::rename(d, radius = vertex_radius)
  }
  d
}

S7::method(connect, list(ob_intercept, ob_intercept)) <- function(
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
  connect(from@polygon,
          to@polygon,
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

S7::method(connect, list(ob_intercept, ob_point)) <- function(
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
  centroid_segment <- ob_segment(from@center, to)
  connect(from@polygon,
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

S7::method(connect, list(ob_point, ob_intercept)) <- function(
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
  connect(from,
          to@polygon,
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


S7::method(connect, list(centerpoint, ob_intercept)) <- function(
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
  connect(from,
          to@polygon,
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


S7::method(connect, list(ob_intercept, centerpoint)) <- function(
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
  connect(from@polygon,
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
          id = id,...)
}

S7::method(as.geom, ob_intercept) <- function(x, ...) {
  gp <- as.geom(S7::super(x@polygon, has_style), ...)
  if (S7::S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gp <- list(gp, gl)
  }
  gp
}

# ob_ngon ----

ob_ngon_props <- list(
  ## primary ----
  primary = list(
    n = S7::new_property(S7::class_numeric, validator = \(value) {
      if (!all(value >= 3))
        stop("There must be at least three sides to a regular polygon.")
    }),
    radius = S7::class_numeric,
    angle = ob_angle_or_character
  ),
  ## derived----
  derived = list(
    side_length = S7::new_property(getter = \(self) {
      self@radius * 2 * sin(pi / self@n)
    }),
    apothem = S7::new_property(getter = \(self) {
      self@radius * cos(pi / self@n)
    }),
    area = S7::new_property(getter = \(self) {
      self@n * self@side_length * self@apothem / 2
    }),
    bounding_box = S7::new_property(getter = \(self) {
      self@vertices@bounding_box
    }),
    circumscribed = S7::new_property(getter = \(self) {
      ob_circle(self@center, radius = self@radius, style = self@style)
    }),
    inscribed = S7::new_property(getter = \(self) {
      ob_circle(self@center, radius = self@apothem, style = self@style)
    }),
    length = S7::new_property(
      getter = function(self) {
        self@center@length
      }
    ),
    perimeter = S7::new_property(getter = \(self) {
      self@n * self@side_length
    }),
    segments = S7::new_property(getter = \(self) {
      map_ob(self, \(s) {
        theta <- degree(seq(0, 360, length.out = s@n + 1)) + s@angle

        p <- s@center + ob_polar(theta, r = s@radius)

        ob_segment(p[seq(1, s@n)], p[seq(2, s@n + 1)], style = s@style)
      })
    }),
    style = S7::new_property(
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
    tibble = S7::new_property(
      getter = function(self) {
        d <- list(
          x = self@center@x,
          y = self@center@y,
          radius = self@radius,
          n = self@n,
          angle = c(self@angle) * 360,
          vertex_radius = self@vertex_radius,
          alpha = self@alpha,
          color = self@color,
          fill = self@fill,
          linewidth = self@linewidth,
          linetype = self@linetype,
          id = self@id
        )
        get_non_empty_tibble(d)
      }
    ),
    vertices = S7::new_property(getter = \(self) {
      map_ob(self, \(s) {
        theta <- degree(seq(0, 360 - 360 / s@n, length.out = s@n)) + s@angle

        s@center + ob_polar(theta, r = s@radius, color = s@color)
      })
    })
  ),
  # functions ----
  funs = list(
    angle_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(point) {
          dp <- point - self@center
          dp@theta
        }
      }
    ),
    normal_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(theta = degree(0), distance = 1, ...) {
          if (S7::S7_inherits(theta, ob_point)) {
            theta <- projection(theta, self)@theta
          }
          if (!S7::S7_inherits(theta, ob_angle)) {
            theta <- degree(theta)
          }

          if (!S7::S7_inherits(theta, ob_angle))
            theta <- degree(theta)
          p <- purrr::map2(unbind(self), unbind(theta), \(s, th) {
            th_p <- th@turn - s@angle@turn
            th_n <- 1 / s@n
            th_r <- th_p / th_n
            th_floor <- floor(th_r)
            th_normal <- turn(th_floor * th_n + ifelse(th_floor == th_r, 0, th_n / 2)) + s@angle
            s@point_at(th) + ob_polar(th_normal, distance)

          }) |>
            bind()
          st <- rlang::list2(...)
          rlang::inject(set_props(p, !!!st))
        }
      }
    ),
    point_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          st <- rlang::list2(...)

          if (!S7::S7_inherits(theta, ob_angle)) {
            theta <- degree(theta)
          }

          p <- purrr::map(unbind(self), \(s) {
            s_radius <- ob_segment(s@center, s@center + ob_polar(theta = theta, r = s@radius + 1))
            intersection(s_radius, s@segments)[1]
          }) |>
            bind()
          rlang::inject(set_props(p, !!!st))
        }
      }
    ),
    tangent_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (S7::S7_inherits(theta, ob_point)) {
            theta <- projection(theta, self)@theta
          }
          if (!S7::S7_inherits(theta, ob_angle)) {
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
#' An ngon is a regular polygon, meaning that each side is of equal length. The [`ob_ngon`] object can be specified with a center, n (number of sides), radius, and angle. Instead of specifying a radius, one can specify either the `side_length` or the length of the `apothem` (i.e., the distance from the center to a side's midpoint.
#' @export
#' @returns [`ob_ngon`] object
#' @param center point at center of the ngon
#' @param n Number of sides
#' @param radius Distance from center to a vertex
#' @param side_length Distance of each side
#' @param apothem Distance from center to a side's midpoint
#' @param angle description
#' @param label A character, angle, or label object
#' @param vertex_radius A numeric or unit vector of length one, specifying the corner radius
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @param style Gets and sets the styles associated with [`ob_ngon`]
#' @slot area The area of the ngons in the [`ob_ngon`] object
#' @slot length The number of ngons in the [`ob_ngon`] object
#' @slot normal_at A function that finds a point that is perpendicular from the ngon and at a specified distance
#' @slot perimeter The length of all the side segments
#' @slot point_at A function that finds a point on the ngon at the specified angle.
#' @slot segments side segments of the regular polygon
#' @slot tangent_at A function that finds the tangent line at the specified angle.
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggforce::geom_shape`.
#' @slot vertices points on the regular polygon
#' @param x overrides x-coordinate in `center@x`
#' @param y overrides y-coordinate in `center@y`
#' @inherit ob_style params
ob_ngon <- S7::new_class(
  name = "ob_ngon",
  parent = centerpoint,
  properties = rlang::list2(
      !!!ob_ngon_props$primary,
      !!!ob_polygon_props$extra,
      !!!ob_polygon_props$styles,
      !!!ob_ngon_props$derived,
      !!!compass_props,
      !!!ob_polygon_props$funs["geom"],
      !!!ob_ngon_props$funs,
      !!!ob_polygon_props$info
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
                         style = S7::class_missing,
                         x = numeric(0),
                         y = numeric(0),
                         id = character(0),
                         ...) {

    id <- as.character(id)
    if (!S7::S7_inherits(angle, ob_angle)) angle <- degree(angle)

    if (length(side_length) > 0) {
      radius <- side_length / (2 * sin(turn(1 / (2 * n))))
    }

    if (length(apothem) > 0) {
      radius <- apothem / (cos(turn(1 / (2 * n))))
    }

    if (length(radius) == 0) {
      radius <- 1
    }

    if ((length(x) > 0) || (length(y) > 0)) {
      if (length(x) == 0) {
        x <- 0
      }
      if (length(y) == 0) {
        y <- 0
      }
      center <- ob_point(x = x, y = y)
    }

    ob_polygon_style <- ob_style() + center@style + style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype,
        id = id
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(ob_polygon_style)
    d <- tibble::tibble(x = center@x,
                        y = center@y,
                        n = n,
                        radius = radius,
                        angle = c(angle))
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }


    center = set_props(center, x = d$x, y = d$y)

    if (S7::S7_inherits(label, ob_label)) {
      if (all(label@center == ob_point(0,0))) {
        label@center <- center
      }
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_intercept")

    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }

    S7::new_object(.parent = S7::S7_object(),
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
               linetype = d[["linetype"]] %||% linetype,
               id = d[["id"]] %||% id
    )
  })

S7::method(get_tibble, ob_ngon) <- function(x) {
  d <- x@tibble |>
    dplyr::rename(x0 = x,
                  y0 = y)
  if ("radius" %in% colnames(d)) {
    d <- dplyr::rename(d, r = radius)
  }

  if ("vertex_radius" %in% colnames(d)) {
    d <- dplyr::rename(d, radius = vertex_radius)
  }

  d |>
    dplyr::mutate(group = dplyr::row_number(),
                  theta = purrr::map2(n, angle, \(nn, aa) {seq(0, 360, length.out = nn + 1) + aa })) |>
    tidyr::unnest(theta) |>
    dplyr::mutate(x = cospi(theta / 180) * r + x0,
                  y = sinpi(theta / 180) * r + y0) |>
    dplyr::select(-x0, -y0, -r, -theta)
}


S7::method(as.geom, ob_ngon) <- function(x, ...) {
  gp <- as.geom(S7::super(x, has_style), ...)
  if (S7::S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gp <- list(gp, gl)
  }
  gp
}

S7::method(str, ob_ngon) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = omit_props(object, include = c("center","radius", "n", "angle"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev)
}

S7::method(`[`, ob_ngon) <- function(x, i) {
  i <- character_index(i, x@id)
  z <- x@tibble[i,] |>
    data2shape(ob_ngon)
  z@label <- na2zero(x@label[i])
  z
}


S7::method(connect, list(ob_ngon, ob_ngon)) <- function(
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
  s <- ob_segment(from@center, y@center)
  connect(intersection(from, s), intersection(to, s),
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

S7::method(connect, list(ob_ngon, ob_point)) <- function(
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
  s <- ob_segment(from@center, to)
  connect(intersection(from, s), to,
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

S7::method(connect, list(ob_point, ob_ngon)) <- function(
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
  s <- ob_segment(from, to@center)
  connect(from, intersection(to, s),
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


S7::method(connect, list(centerpoint, ob_ngon)) <- function(
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
  s <- ob_segment(from@center, to@center)
  p <- intersection(to, s)
  connect(
    from,
    p,
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

# ob_reuleaux ----
#' Reuleaux polygon
#'
#' @param center [`ob_point`] at center of the rectangle
#' @param n Number of sides. True Reuleaux polygons have an odd number of sides, but Reauleaux-like shapes with an even number of sides are possible.
#' @param radius Distance from center to a vertex
#' @inherit ob_style params
#' @inherit ob_polygon params
#'
#' @export
#' @returns ob_reuleaux object
ob_reuleaux <- S7::new_class(
  name = "ob_reuleaux",
  parent = centerpoint,
  properties = list(
    n = S7::class_integer,
    radius = S7::class_double,
    angle = ob_angle_or_numeric,
    label = label_or_character_or_angle,
    vertex_radius = S7::new_property(class = class_numeric_or_unit, validator = function(value) {
      if (length(value) > 1) stop("The vertex_radius property must be of length 1.")
    }),
    ob_style@properties$alpha,
    ob_style@properties$color,
    ob_style@properties$fill,
    ob_style@properties$linewidth,
    ob_style@properties$linetype,
    ob_polygon@properties$style,
    arc_radius = S7::new_property(getter = \(self) {
      self@chord_length * sin((degree(180) - self@inscribed_angle) / 2) / sin(self@inscribed_angle)
    }),
    arcs = S7::new_property(getter = \(self) {
      map_ob(self, \(s) {
        ntheta <- 360 / s@n
        even <- 1 - s@n %% 2
        theta <- seq(0, 360, length.out = s@n + 1)[1:s@n] + s@angle@degree + even * 180 / s@n

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
        }) |>
          bind()

      })
    }),
    central_angle = S7::new_property(getter = \(self) {
      degree(360 / self@n)
    }),
    chord_length = S7::new_property(getter = \(self) {
      sqrt((2 * self@radius ^ 2) * (1 - cos(self@inscribed_angle)))
    }),
    circumscribed = S7::new_property(getter = \(self) {
      ob_circle(self@center, radius = self@radius, style = self@style)
    }),
    inscribed_angle = S7::new_property(getter = \(self) {
      degree(180 / self@n)
    }),
    length = ob_ngon@properties$length,
    tibble = S7::new_property(
      getter = function(self) {
        d <- list(
          x = self@center@x,
          y = self@center@y,
          radius = self@radius,
          n = self@n,
          angle = c(self@angle) * 360,
          vertex_radius = self@vertex_radius,
          alpha = self@alpha,
          color = self@color,
          fill = self@fill,
          linewidth = self@linewidth,
          linetype = self@linetype,
          id = self@id
        )
        get_non_empty_tibble(d)
      }
    ),
    vertices = ob_ngon@properties$vertices,
    aesthetics = ob_polygon_props$info$aesthetics,
    # functions ----
    angle_at = S7::new_property(S7::class_function, getter = function(self) {
      \(point) {
        dp <- point - self@center
        dp@theta
      }
    }),
    arc_at = S7::new_property(S7::class_function, getter = \(self) {
      \(theta, ...) {
        if (!S7::S7_inherits(theta, ob_angle)) {
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
    normal_at = S7::new_property(S7::class_function, getter = function(self) {
      \(theta = degree(0), distance = 1, ...) {
        if (S7::S7_inherits(theta, ob_point)) theta <- self@angle_at(theta)
        if (!S7::S7_inherits(theta, ob_angle)) theta <- degree(theta)
        p <- self@point_at(theta)
        a <- self@arc_at(theta)
        theta_a <- (p - a@center)@theta
        a@normal_at(theta_a, ...)

      }
    }),
    point_at = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          if (!S7::S7_inherits(theta, ob_angle)) theta <- degree
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
                            style = S7::class_missing,
                            id = character(0),
                            ...) {

    id <- as.character(id)

    if (!S7::S7_inherits(angle, ob_angle)) angle <- degree(angle)
    n <- as.integer(n)

    ob_polygon_style <- ob_style() + center@style + style +
      ob_style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype,
        id = id
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(ob_polygon_style)
    d <- tibble::tibble(x = center@x,
                        y = center@y,
                        n = n,
                        radius = radius,
                        angle = c(angle))
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }


    center = set_props(center, x = d$x, y = d$y)

    if (S7::S7_inherits(label, ob_label)) {
      if (all(label@center == ob_point(0,0))) {
        label@center <- center
      }
    }

    label <- centerpoint_label(label,
                               center = center,
                               d = d,
                               shape_name = "ob_reuleaux")

    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }

    S7::new_object(.parent = S7::S7_object(),
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
               linetype = d[["linetype"]] %||% linetype,
               id = d[["id"]] %||% id
    )


  }
    )


S7::method(get_tibble, ob_reuleaux) <- function(x) {
  d <- x@tibble
  if ("radius" %in% colnames(d)) {
    d <- dplyr::rename(d, r = radius)
  }

  if ("vertex_radius" %in% colnames(d)) {
    d <- dplyr::rename(d, radius = vertex_radius)
  }

  d |>
    dplyr::mutate(group = dplyr::row_number()) |>
    dplyr::mutate(p = purrr::pmap(
      list(x, y, n, r, angle),
      \(x,y,n,r,angle) {
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
            center = p_opposite + ob_point(x,y),
            start = degree(start1),
            end = degree(end1),
            radius = (p_start - p_opposite)@r,
          )@polygon |>
            dplyr::select(-group)
        })
      })) |>
    dplyr::select(-x,-y,-n, -r, -angle) |>
    tidyr::unnest(p)
}

S7::method(str, ob_reuleaux) <- function(
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

S7::method(as.geom, ob_reuleaux) <- function(x, ...) {
  gp <- as.geom(S7::super(x, has_style), ...)
  if (S7::S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gp <- list(gp, gl)
  }
  gp
}
