rectangle_side <- new_class(
  name = "retangle_side",
  properties = list(
    east = segment,
    north = segment,
    west = segment,
    south = segment))

rc_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype"
)

rc_aesthetics_list <- class_aesthetics_list(
  geom = ggplot2::geom_tile,
  required_aes = c("x", "y", "width", "height", "group"),
  omit_names = c("linejoin", "rule"),
  mappable_bare = character(0),
  mappable_identity = c("color", "fill", "linewidth", "linetype", "alpha"),
  not_mappable = character(0),
  inherit.aes = FALSE,
  style = rc_styles
)

rc_props <- list(
  # Primary ----
  primary = list(
      width = new_property(class = class_numeric, default = 1),
      height = new_property(class = class_numeric, default = 1)
  ),
  styles = style@properties[rc_styles],
  # Derived ----
  derived = list(
    northeast = new_property(
      point,
      getter = function(self) {
        point(self@width / 2, self@height / 2) + self@center
      }
    ),
    northwest = new_property(
      point,
      getter = function(self) {
        point(self@width / -2, self@height / 2) + self@center
      }
    ),
    southwest = new_property(
      point,
      getter = function(self) {
        point(self@width / -2, self@height / -2) + self@center
      }
    ),
    southeast = new_property(
      point,
      getter = function(self) {
        point(self@width / 2, self@height / -2) + self@center
      }
    ),
    east = new_property(
      point,
      getter = function(self) {
        point(self@width / 2, 0) + self@center
      }
    ),
    north = new_property(
      point,
      getter = function(self) {
        point(0, self@height / 2) + self@center
      }
    ),
    west = new_property(
      point,
      getter = function(self) {
        point(self@width / -2, 0) + self@center
      }
    ),
    south = new_property(
      point,
      getter = function(self) {
        point(0, self@height / -2) + self@center
      }
    ),
    side = new_property(rectangle_side, getter = function(self) {
      re = rectangle_side(
        east = segment(p1 = self@northeast, p2 = self@southeast, style = self@style),
        north = segment(p1 = self@northwest, p2 = self@northeast, style = self@style),
        west = segment(p1 = self@northwest, p2 = self@southwest, style = self@style),
        south = segment(p1 = self@southwest, p2 = self@southeast, style = self@style)
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
        rlang::inject(style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        rectangle(
          center = self@center,
          width = self@width,
          height = self@height,
          style = self@style + value)
      }
    ),
    tibble = new_property(getter = function(self) {
      d <- list(
        x = self@center@x,
        y = self@center@y,
        width = self@width,
        height = self@height,
        alpha = self@alpha,
        color = self@color,
        fill = self@fill,
        linewidth = self@linewidth,
        linetype = self@linetype)
      get_non_empty_tibble(d)
    })
  ),
  # Functions ----
  funs = list(
    geom = new_property(class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    point_at = new_property(
      class_function,
      getter = function(self) {
        \(theta = degree(0), ...) {
          s <- segment(self@center, self@center + polar(theta = theta, r = distance(self@center, self@northeast)))
          st <- self@style + style(...)
          p <- intersection(self, s)[[1]]
          p@style <- st
          p
        }
      }
    )),
  # Information ----
  info = list(
    aesthetics = new_property(getter = function(self) {
      rc_aesthetics_list
    })))

# Rectangle ----

#' rectangle class
#' @param center point at center of the circle
#' @param width width
#' @param height height
#' @param xy 2-column matrix of points
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
#' @examples
#' # specify center point and radius
#' p <- point(0,0)
#' rectangle(p, width = 2, height = 2)
#' @export
rectangle <- new_class(
  name = "rectangle",
  parent = centerpoint,
  properties = rlang::inject(list(
    !!!rc_props$primary,
    !!!rc_props$styles,
    !!!rc_props$derived,
    !!!rc_props$funs,
    !!!rc_props$info)),
  constructor = function(center = class_missing,
                          width = class_missing,
                          height = class_missing,
                          northeast = class_missing,
                          northwest = class_missing,
                          southwest = class_missing,
                          southeast = class_missing,
                          label = class_missing,
                         alpha = class_missing,
                         color = "black",
                         fill = NA_character_,
                         linewidth = .5,
                         linetype = class_missing,
                         style = class_missing,
                         ...) {

                          hasnorth <- FALSE
                          hassouth <- FALSE
                          haseast <- FALSE
                          haswest <- FALSE
                          if (length(northeast) > 0) {
                            north <- northeast@y
                            east <- northeast@x
                            hasnorth <- TRUE
                            haseast <- TRUE
                          }
                          if (length(northwest) > 0) {
                            north <- northwest@y
                            west <- northwest@x
                            hasnorth <- TRUE
                            haswest <- TRUE
                          }

                          if (length(southeast) > 0) {
                            south <- southeast@y
                            east <- southeast@x
                            hassouth <- TRUE
                            haseast <- TRUE
                          }

                          if (length(southwest) > 0) {
                            south <- southwest@y
                            west <- southwest@x
                            hassouth <- TRUE
                            haswest <- TRUE
                          }

                          if (hassouth && hasnorth) {
                            height <- abs(north - south)
                          }

                          if (haswest && haseast) {
                            width <- abs(west - east)
                          }

                          if (length(center) > 0 &&
                              length(width) > 0 && length(height) > 0) {

                          } else if (length(center) > 0 &&
                                     (hasnorth || hassouth) && (haswest || haseast)) {
                            if (haseast) {
                              width <- abs(center@x - east) * 2
                            } else {
                              width <- abs(center@x - west) * 2
                            }
                            if (hasnorth) {
                              height <- abs(center@y - north) * 2
                            } else {
                              height <- abs(center@y - south) * 2
                            }

                          } else if (length(width) > 0 &&
                                     length(height) > 0 &&
                                     (hasnorth || hassouth) && (haswest || haseast)) {
                            if (haseast) {
                              c.x <- east - width / 2
                            } else {
                              c.x <- west + width / 2
                            }
                            if (hasnorth) {
                              c.y <- north - height / 2
                            } else {
                              c.y <- south + height / 2
                            }
                            center = point(c.x, c.y)
                          } else {
                            stop("There is not enough information to make a rectangle.")
                          }


    rc_style <- style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype
      ) +
      style +
      style(...)



    non_empty_list <- get_non_empty_props(rc_style)
    d <- tibble::tibble(x = center@x, y = center@y, width = width, height = height)
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
                               shape_name = "rectangle")





     new_object(centerpoint(center = center),
                 width = d$width,
                 height = d$height,
                 label = label,
                 alpha = d[["alpha"]] %||% alpha,
                 color = d[["color"]] %||% color ,
                 fill = d[["fill"]]  %||% fill,
                 linewidth = d[["linewidth"]]  %||% linewidth,
                 linetype = d[["linetype"]]  %||% linetype)
  }
)


method(str, rectangle) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = omit_props(object, include = c("center", "width", "height"))) {
  str_properties(object,
    omit = omit,
    nest.lev = nest.lev
  )
}


method(print, rectangle) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}






method(get_tibble, rectangle) <- function(x) {
  x@tibble
}


method(get_tibble_defaults, rectangle) <- function(x) {
  sp <- style(
    alpha = replace_na(as.double(ggplot2::GeomTile$default_aes$alpha), 1),
    color = replace_na(ggplot2::GeomTile$default_aes$colour, "black"),
    fill = replace_na(ggplot2::GeomTile$default_aes$fill, "black"),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggplot2::GeomTile$default_aes$linewidth, 0.5),
    linetype = replace_na(ggplot2::GeomTile$default_aes$default_aes$linetype, 1)
  )
  get_tibble_defaults_helper(x, sp,required_aes = c("x", "y", "width", "height"))
}

method(`==`, list(rectangle, rectangle)) <- function(e1, e2) {
  (e1@center@x == e2@center@x) &&
    (e1@center@y == e2@center@y) &&
    (e1@width == e2@width) &&
    (e1@height == e2@height)
}

method(`[`, rectangle) <- function(x, y) {
  d <- as.list(x@tibble[y,])
  rlang::inject(rectangle(!!!d))
}


