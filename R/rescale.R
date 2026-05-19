# Rescale ----

#' Rescale an object in 2 dimensions
#'
#' @param x object
#' @param scale numeric value by which the object is rescaled
#' @param origin length 2 vector or point from which scaling occurs
#' @export
#' @returns shape object
rescale <- S7::new_generic(
  name = "rescale",
  dispatch_args = "x",
  fun = function(x, scale = 1, origin = ob_point(0, 0)) {
    S7::S7_dispatch()
  }
)


## Rescale point ----
S7::method(rescale, ob_point) <- function(
    x,
    scale = 1,
    origin = ob_point(0, 0)
) {
  (x - origin) * scale
}

ob_radius <- S7::new_union(ob_circle, ob_ngon, ob_reuleaux)

## Rescale circle, ngon, reuleaux ----
S7::method(rescale, ob_radius) <- function(
    x,
    scale = 1,
    origin = ob_point(0, 0)
) {
  x@center <- (x@center - origin) * scale
  x@radius <- x@radius * scale
  x
}

## Rescale ellipse ----
S7::method(rescale, ob_ellipse) <- function(
    x,
    scale = 1,
    origin = ob_point(0, 0)
) {
  x@center <- (x@center - origin) * scale
  x@a <- x@a * scale
  x@b <- x@b * scale
  x
}


## Rescale rectangle ----
S7::method(rescale, ob_rectangle) <- function(
    x,
    scale = 1,
    origin = ob_point(0, 0)
) {
  x@center <- (x@center - origin) * scale
  x@width <- x@width * scale
  x@height <- x@height * scale
  x
}

## Rescale ob_polygon, ob_path, ob_bezier ----
S7::method(rescale, ob_point_list) <- function(
    x,
    scale = 1,
    origin = ob_point(0, 0)
) {
  x@p <- purrr::pmap(list(pp = x@p,
                          s = scale,
                          o = unbind(origin)), \(pp, s, o) {rescale(x = pp, scale = s, origin = o)})
  x
}

## Rescale ob_segment ----
S7::method(rescale, ob_segment) <- function(
    x,
    scale = 1,
    origin = ob_point(0, 0)
) {
  x@p1 <- rescale(x@p1, scale = scale, origin = origin)
  x@p2 <- rescale(x@p2, scale = scale, origin = origin)
  x
}

## Rescale ob_segment ----
S7::method(rescale, ob_shape_list) <- function(
    x,
    scale = 1,
    origin = ob_point(0, 0)
) {
  purrr::map(x, \(xx) rescale(xx, scale = scale, origin = origin)) |>
    ob_shape_list()
}
