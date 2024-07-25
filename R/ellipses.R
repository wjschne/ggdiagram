el_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype",
  "n"
)

el_props <- list(
  primary = list(
    a = new_property(class = class_numeric, default = 1),
    b = new_property(class = class_numeric, default = 1),
    angle = new_property(class_angle_or_numeric, default = 0),
    m1 = new_property(class = class_numeric, default = 2),
    m2 = new_property(class = class_numeric, default = 2)
  ),
  styles = style@properties[el_styles],
  derived = list(
    length = new_property(
      getter = function(self) {
        length(self@a)
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(el_styles,
                         prop, object = self) %>%
          `names<-`(el_styles)
        rlang::inject(style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        point(self@x, self@y, style = self@style + value)
      }
    ),
    tibble = new_property(getter = function(self) {
      d <- list(
        x0 = self@center@x,
        y0 = self@center@y,
        a = self@a,
        b = self@b,
        angle = self@angle,
        m1 = self@m1,
        m2 = self@m2,
        alpha = self@alpha,
        color = self@color,
        fill = self@fill,
        linewidth = self@linewidth,
        linetype = self@linetype,
        n = self@n)
      get_non_empty_tibble(d)
    })
  ),
  funs = list(
    point_at_theta = new_property(class_function, getter = function(self) {
      \(theta = degree(0)) {
        cost <- cos(theta)
        sint <- sin(theta)
        x <- sign(cost) * self@a * abs(cost) ^ (2 / self@m1)
        y <- sign(sint) * self@b * abs(sint) ^ (2 / self@m2)

        xy <- rotate2columnmatrix(x = cbind(x,y), theta = self@angle)
        point(xy[,1], xy[,2], style = self@style)
      }
    })

  )
)

# ellipse----

#' ellipse class
#' @param center point at center of ellipse
#' @param a distance of semi-major axis
#' @param b distance of semi-minor axis
#' @param m1 exponent of semi-major axis
#' @param m2 exponent of semi-minor axis
#' @param theta ellipse rotation
#' @param n number of points in ellipse (default = 360)
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
#' @examples
#' # specify center point and semi-major axes
#' p <- point(0,0)
#' ellipse(p, a = 2, b = 3)
#' @export
ellipse <- new_class(
  name = "ellipse",
  parent = centerpoint,
  properties = rlang::inject(list(
    !!!el_props$primary,
    !!!el_props$styles,
    !!!el_props$derived,
    !!!el_props$funs)),
  constructor = function(center = point(0,0),
                         a = 1,
                         b = 1,
                         angle = 0,
                         m1 = 2,
                         m2 = 2,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         linewidth = class_missing,
                         linetype = class_missing,
                         n = class_missing,
                         style = class_missing,
                         ...) {
    if (S7_inherits(angle, class_angle)) {
      angle <- angle@radian
    }

    c_style <- style +
      style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype,
        n = n
      ) +
      style(...)

    non_empty_list <- get_non_empty_props(c_style)
    d <- tibble::tibble(x0 = center@x, y0 = center@y, a = a, b = 2, angle = angle, m1 = m1, m2 = m2)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }

    center = set_props(center, x = d$x0, y = d$y0)



     new_object(centerpoint(center = center),
                 a = d$a,
                 b = d$b,
                 angle = d$angle,
                 m1 = d$m1,
                 m2 = d$m2,
                 alpha = d[["alpha"]] %||% alpha,
                 color = d[["color"]] %||% color ,
                 fill = d[["fill"]]  %||% fill,
                 linewidth = d[["linewidth"]]  %||% linewidth,
                 linetype = d[["linetype"]]  %||% linetype,
                 n = d[["n"]]  %||% n)
  }
)


method(str, ellipse) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","a", "b", "angle", "m1", "m2"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

method(as.geom, ellipse) <- function(x, ...) {
  d <- get_tibble_defaults(x)
  make_geom_helper(
    d = d,
    .geom_x = ggforce::geom_ellipse,
    user_overrides = get_non_empty_props(style(...)),
    mappable_bare = c("m1", "m2"),
    mappable_identity = c("linewidth", "linetype", "alpha", "color", "fill"),
    not_mappable = c("n"),
    required_aes = c("x0", "y0", "a", "b", "angle", "group"),
    omit_names = c("linejoin", "rule"),
    inherit.aes = FALSE
  )
}


 method(get_tibble, ellipse) <- function(x) {
  x@tibble
}

method(get_tibble_defaults, ellipse) <- function(x) {
  # ggforce::geom_ellipse uses GeomCircle
  sp <- style(
    alpha = replace_na(as.double(ggforce::GeomCircle$default_aes$alpha), 1),
    color = replace_na(ggforce::GeomCircle$default_aes$colour, "black"),
    fill = replace_na(ggforce::GeomCircle$default_aes$fill, "black"),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggforce::GeomCircle$default_aes$linewidth, 0.5),
    linetype = replace_na(ggforce::GeomCircle$default_aes$default_aes$linetype, 1),
    n = 360
  )
  get_tibble_defaults_helper(x, sp,required_aes = c("x0", "y0", "a", "b", "m1", "m2", "angle"))
}
