pt_styles <- c("alpha", "color", "fill", "shape", "size", "stroke")
pt_props <- list(
  primary = list(
    x = new_property(class = class_numeric, default = 0),
    y = new_property(class = class_numeric, default = 0)
  ),
  styles = style@properties[pt_styles],
  derived = list(
    theta = new_property(
      getter = function(self) {
        radian(radian = atan2(self@y, self@x))
      },
      setter = function(self, value) {
        set_props(self, x = cos(value) * self@r, y = sin(value) * self@r)
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
    xy = new_property(
      getter = function(self) {
        `colnames<-`(cbind(self@x, self@y), c("x", "y"))
      }
    ),
    length = new_property(
      getter = function(self) {
        length(self@x)
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(pt_styles,
                         prop, object = self) %>%
          `names<-`(pt_styles)
        rlang::inject(style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        point(self@x, self@y, style = self@style + value)
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
    })
  ),
  funs = list(
    label = new_property(class_function, getter = function(self) {
      \(label = paste0(
        "(",
        signs::signs(round(self@x, 2)),
        ",",
        signs::signs(round(self@y, 2)),
        ")"),
        ...
      ) {
        label(p = self, label = label, ...)
      }

    })

  )
)



# Point----

#' point
#'
#' Points are specified with x and y coordinates.# Polar class ----
#' @export
#' @param x Coordinate on the x-axis
#' @param y Coordinate on the y-axis
#' @param style a style object
#' @param n the length of the point object
#' @param r Radius = Distance from the origin to the point
#' @param theta Angle of the vector from the origin to the point
#' @param ... properties passed to style
#' @export
point <- new_class(
  name = "point",
  parent = xy,
  properties = rlang::inject(list(
    !!!pt_props$primary,
    !!!pt_props$styles,
    !!!pt_props$derived,
    !!!pt_props$funs)),
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

    p_style <- style +
      style(
        alpha = alpha,
        color = color,
        fill = fill,
        shape = shape,
        size = size,
        stroke = stroke
      ) +
      style(...)

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


# Polar Point class ----
#' polar
#'
#' Polar points are ordinary points but are specified with an angle (theta) and a radial distance (r)
#' @rdname point
#' @export
polar <- new_class(
  name = "polar",
  parent = point,
  constructor = function(theta = class_missing,
                         r = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         shape = class_missing,
                         size = class_missing,
                         stroke = class_missing,
                        style = class_missing) {
    if (length(r) == 0) r <- 0
    if (length(theta) == 0) theta <- angle(0)


    p <- point(x = cos(theta) * r,
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


method(str, point) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c("x","y"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                additional = additional)
}

method(str, polar) <- function(
  object,
  nest.lev = 0,
  additional = TRUE,
  omit = omit_props(object, include = c("x","y", "r", 'theta'))) {
str_properties(object,
               omit = omit,
               nest.lev = nest.lev,
              additional = additional)
}



method(get_tibble, point) <- function(x) {
  x@tibble
}


method(get_tibble_defaults, point) <- function(x) {
  sp <- style(
    alpha = replace_na(ggplot2::GeomPoint$default_aes$alpha, 1),
    color = replace_na(ggplot2::GeomPoint$default_aes$colour, "black"),
    fill = replace_na(ggplot2::GeomPoint$default_aes$fill, "black"),
    shape = replace_na(ggplot2::GeomPoint$default_aes$shape, 19),
    size = replace_na(ggplot2::GeomPoint$default_aes$size, 2),
    stroke = replace_na(ggplot2::GeomPoint$default_aes$stroke, 0.5)
  )
  get_tibble_defaults_helper(x, sp, required_aes = c("x", "y"))
}

method(as.geom, point) <- function(x, ...) {
  d <- get_tibble_defaults(x)
  make_geom_helper(
    d = d,
    .geom_x = ggplot2::geom_point,
    user_overrides = get_non_empty_props(style(...)),
    mappable_bare = "",
    not_mappable = "",
    required_aes = c("x", "y"),
    omit_names = "group",
    inherit.aes = FALSE)

}

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

method(polar2just, class_angle) <- function(x, multiplier = 1.2, axis = c("h", "v")) {
  polar2just(x@radian, multiplier, axis)
}

# arithmetic ----
purrr::walk(list(`+`, `-`, `*`, `/`, `^`), \(.f) {
  method(.f, list(point, point)) <- function(e1, e2) {
    x <- .f(e1@x, e2@x)
    y <- .f(e1@y, e2@y)
    e2@x <- x
    e2@y <- y
    e2
  }
  method(.f, list(point, class_numeric)) <- function(e1, e2) {
    e1@x <- .f(e1@x, e2)
    e1@y <- .f(e1@y, e2)
    e1
  }
  method(.f, list(class_numeric, point)) <- function(e1, e2) {
    e2@x <- .f(e1, e2@x)
    e2@y <- .f(e1, e2@y)
    e2
  }
})

method(midpoint, list(point, point)) <- function(x,y, position = .5, ...) {
x + ((y - x) * position)
}



# Centerpoint----
centerpoint <- new_class(
  name = "centerpoint",
  properties = list(center = new_property(
    class = point,
    default = point(0, 0)
  )),
  parent = xy
)