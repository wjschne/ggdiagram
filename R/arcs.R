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

# cat(paste0(arc_styles, ' = ', arc_styles, collapse = ",\n"))

arc_props <- list(
  primary = list(
    radius = new_property(class = class_numeric, default = 1),
    start = new_property(class = class_angle_or_numeric, default = 0),
    end = new_property(class = class_angle_or_numeric, default = 0)
  ),
  styles = style@properties[arc_styles],
  derived = list(
    length = new_property(
      getter = function(self) {
        length(self@radius)
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(arc_styles,
          prop,
          object = self
        ) %>%
          `names<-`(arc_styles)
        rlang::inject(style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        point(self@x, self@y, style = self@style + value)
      }
    ),
    theta = new_property(getter = function(self) {
      self@end - self@start
    }),
    tibble = new_property(getter = function(self) {
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
      get_non_empty_tibble(d)
    })
  ),
  funs = list(
    label = new_property(class_function, getter = function(self) {
      \(label = as.character(degree(self@theta)),
        position = .5,
        angle = (mp - self@center)@theta - degree(180),
        distance = 1.4,
        ...) {
        mp <- midpoint(self, position = position)
        label(p = mp, label = label, polar_just = polar(theta = angle, r = distance), ...)
      }
    }),
    midpoint = new_property(class_function, getter = function(self) {
      \(position = .5, ...) midpoint(self, position = position, ...)
    }),
    point_at_theta = new_property(
      class_function,
      getter = function(self) {
        \(theta = degree(0)) polar(theta = theta, r = self@radius, style = self@style)
      }
    ),
    tangent_at_theta = new_property(
      class = class_function,
      getter = function(self) {
        \(theta = degree(0)) {
          x0 <- self@center@x
          y0 <- self@center@y
          x1 <- cos(theta) * self@radius + self@center@x
          y1 <- cos(theta) * self@radius + self@center@y
          line(
            a = x1 - x0,
            b = y1 - y0,
            c = x0^2 - (x1 * x0) + y0^2 - (y1 * y0) - self@radius^2,
            style = self@style
          )
        }
      }
    )
  )
)


# arc----

#' arc class
#' @param center point at center of the arc (default = point(0,0))
#' @param radius distance between center and edge arc (default = 1)
#' @param start start angle (default = 0 degrees)
#' @param end end angle (default = 0 degrees)
#' @param theta interior angle (end - start)
#' @param n number of points in arc (default = 360)
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
#' @examples
#' # specify center point and radius
#' p <- point(0,0)
#' arc(p, radius = 6, start = degree(0), end = degree(30))
#' @export
arc <- new_class(
  name = "arc",
  parent = centerpoint,
  properties = rlang::inject(
    list(
      !!!arc_props$primary,
      !!!arc_props$styles,
      !!!arc_props$derived,
      !!!arc_props$funs
    )
  ),
  constructor = function(center = point(0, 0),
                         radius = 1,
                         start = 0,
                         end = 0,
                         n = 360,
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
                         ...) {
    arc_style <- center@style + style +
      style(
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
      style(...)

    if (!S7_inherits(start, class_angle)) {
      start <- radian(start)
    }

    if (!S7_inherits(end, class_angle)) {
      end <- radian(end)
    }



    non_empty_list <- get_non_empty_props(arc_style)
    d <- tibble::tibble(
      x0 = center@x,
      y0 = center@y,
      radius = radius,
      start = c(start) * 2 * pi,
      end = c(end) * 2 * pi
    )
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }

    center = set_props(center, x = d$x0, y = d$y0)
    center@style <- arc_style



    new_object(
      centerpoint(center = center),
      radius = d$radius,
      start = radian(d$start),
      end = radian(d$end),
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


method(str, arc) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","radius", "start", "end"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

method(as.geom, arc) <- function(x, ...) {

  d <- get_tibble_defaults(x)
  if ("arrowhead_length" %in% colnames(d)) {
    d <- dplyr::rename(d, length = arrowhead_length)
  }

 d <- d %>%
  dplyr::mutate(group = factor(dplyr::row_number())) %>%
  dplyr::mutate(xy = purrr::pmap(list(x0, y0, r, start, end, n), \(X0, Y0, R, START, END, N) {
    THETA <- seq(c(START), c(END), length.out = N)
    tibble::tibble(
      x = X0 + cos(THETA) * R,
      y = Y0 + sin(THETA) * R
    )
  })) %>%
  tidyr::unnest(xy) %>%
  dplyr::select(-c(x0, y0, r, start, end, n))

overrides <- get_non_empty_props(style(...))
  if (!("arrow_head" %in% c(colnames(d), names(overrides)))) {
    overrides$arrow_head <- ggarrow::arrow_head_minimal(90)
  }


  make_geom_helper(
    d = d,
    .geom_x = ggarrow::geom_arrow,
    user_overrides = overrides,
    mappable_bare = character(0),
    mappable_identity = c("color", "linewidth", "linetype", "alpha"),
    not_mappable = c("n", "lineend", "linejoin", "arrow_head", "arrow_fins",
                     "length","length_head", "length_fins", "length_mid",
                     "resect", "resect_fins", "resect_head", "linemitre"),
    required_aes = c("x", "y", "group"),
    omit_names = c("linejoin", "rule", "x0", "y0", "r", "start", "end"),
    inherit.aes = FALSE)
}

method(get_tibble, arc) <- function(x) {
  x@tibble
}


method(get_tibble_defaults, arc) <- function(x) {
  sp <- style(
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
  list(arc, class_missing)) <- function(x,y, position = .5, ...) {
  m <- x@start@turn + (x@theta@turn * position)
  x@center + polar(
    theta = turn(m),
    r = x@radius,
    style = x@style + style(...))
}
