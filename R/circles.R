cr_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype",
  "n"
)

cr_props <- list(
  primary = list(
    # center = new_property(class = point, default = point(0,0)),
    radius = new_property(class = class_numeric, default = 1)
  ),
  styles = style@properties[cr_styles],
  derived = list(
    area = new_property(getter = function(self) {
      pi + self@radius ^ 2
    }),
    circumference = new_property(getter = function(self) {
      pi + self@radius * 2
    }),
    diameter = new_property(getter = function(self) {
      self@radius * 2
    }),
    length = new_property(
      getter = function(self) {
        length(self@radius)
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(cr_styles,
                         prop, object = self) %>%
          `names<-`(cr_styles)
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
        r = self@radius,
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
            c = x0 ^ 2 - (x1 * x0) + y0 ^ 2 - (y1 * y0) - self@radius ^ 2,
            style = self@style
          )
        }
      }
    ),
    point_at_theta = new_property(
      class_function,
      getter = function(self) {
        
        \(theta = degree(0)) polar(theta = theta, r = self@radius, style = self@style) + self@center
      }
    )))

# Circle----

#' circle class
#' @param center point at center of the circle
#' @param radius distance between center and edge circle
#' @param n number of points in circle (default = 360)
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
#' @examples
#' # specify center point and radius
#' p <- point(0,0)
#' circle(p, radius = 6)
#' @export
circle <- new_class(
  name = "circle",
  parent = centerpoint,
  properties = rlang::inject(list(
    !!!cr_props$primary,
    !!!cr_props$styles,
    !!!cr_props$derived,
    !!!cr_props$funs)),
  constructor = function(center = point(0,0),
                         radius = 1,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         linewidth = class_missing,
                         linetype = class_missing,
                         n = class_missing,
                         style = class_missing,
                         ...) {
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
    d <- tibble::tibble(x0 = center@x, y0 = center@y, radius = radius)
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }

    center = set_props(center, x = d$x0, y = d$y0)



     new_object(centerpoint(center = center),
                 radius = d$radius,
                 alpha = d[["alpha"]] %||% alpha,
                 color = d[["color"]] %||% color ,
                 fill = d[["fill"]]  %||% fill,
                 linewidth = d[["linewidth"]]  %||% linewidth,
                 linetype = d[["linetype"]]  %||% linetype,
                 n = d[["n"]]  %||% n)
  }
)


method(str, circle) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = omit_props(object, include = c("center","radius"))) {
str_properties(object,
                   omit = omit,
                   nest.lev = nest.lev)
}

method(as.geom, circle) <- function(x, ...) {
  d <- get_tibble_defaults(x)
  make_geom_helper(
    d = d,
    .geom_x = ggforce::geom_circle,
    user_overrides = get_non_empty_props(style(...)),
    mappable_bare = character(0),
    mappable_identity = c(
      "linewidth",
      "linetype",
      "alpha",
      "color",
      "fill"
    ),
    not_mappable = c("n"),
    required_aes = c("x0", "y0", "r", "group"),
    omit_names = c("linejoin", "rule"),
    inherit.aes = FALSE
  )
}


method(get_tibble, circle) <- function(x) {
  x@tibble
}


method(get_tibble_defaults, circle) <- function(x) {
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
  get_tibble_defaults_helper(x, sp,required_aes = c("x0", "y0", "r", "n"))
}
