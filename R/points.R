# Point----

#' point
#'
#' Points are specified with x and y coordinates.# Polar class ----
#' @export
#' @param x Coordinate on the x-axis
#' @param y Coordinate on the y-axis
#' @param r Radius = Distance from the origin to the point
#' @param theta Angle of the vector from the origin to the point
#' @param style a style object
#' @param ... properties passed to style
#' @export
point <- new_class(
  name = "point",
  parent = xy,
  properties = list(
    x = new_property(class = class_numeric, default = 0),
    y = new_property(class = class_numeric, default = 0),
    theta = new_property(
      getter = function(self)
        radian(radian = atan2(self@y, self@x))
    ),
    r = new_property(
      getter = function(self)
        sqrt(self@x ^ 2 + self@y ^ 2)

    ),
    xy = new_property(
      getter = function(self)
        matrix(
          c(self@x, self@y),
          ncol = 2,
          dimnames = list(NULL, c("x", "y"))
        )
    ),
    style = new_property(class = style),
    label = new_property(class_function, getter = function(self) {
      \(label = paste0(
        "(",
        signs::signs(round(self@x, 2)),
        ",",
        signs::signs(round(self@y, 2)),
        ")"),
        ...
      ) {
        label(self, label, ...)
      }

    })
  ),
  constructor = function(x = class_missing,
                         y = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         shape = class_missing,
                         size = class_missing,
                         stroke = class_missing,
                         style = class_missing,
                         ...) {
    # Start with 1 empty style_point to ensure final result is style_point(s),
    # add previous style, add new styles, add lazy dots
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



    # slot x might be a 2-column matrix
    if ("matrix" %in% class(x)) {
      if (ncol(x) == 2) {
        y <- x[,2, drop = TRUE]
        x <- x[,1, drop = TRUE]
      } else {
        stop(paste("A point list can be created with a 2-column matrix, but this matrix has", ncol(x), "columns."))
      }
    }







    d <- get_non_empty_tibble(list(
      x = x,
      y = y,
      style = c(p_style)
    ))



    new_object(S7_object(), x = d$x, y = d$y, style = d@style)
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
                         style = class_missing,
                         ...) {
    if (length(r) == 0) r <- 0
    if (length(theta) == 0) theta <- angle(0)

    p <- point(theta = theta, r = r, style = style + style(...))
    new_object(p)
  })
