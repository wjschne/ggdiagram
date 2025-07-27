# inside----
#' is an ob_point inside a shape ?
#'
#' @param x object
#' @param y object
#' @export
#' @returns numeric vector where 1 = inside, 0 = on, -1 = outside
inside <- S7::new_generic("inside", c("x", "y"), fun = function(x,y) S7::S7_dispatch())

S7::method(inside, list(ob_point, ob_rectangle)) <- function(x,y) {
  rx <- rotate(x, theta = -1*y@angle, origin = y@center)
  ry <- y
  ry@angle <- degree(0)
 insideTF <-  (rx@x <= ry@northeast@x &
    rx@x >= ry@northwest@x &
    rx@y <= ry@northeast@y &
    rx@y >= ry@southeast@y) * 2 - 1

 insideTF[insideTF == 1 & (rx@x == ry@east@x | rx@x == ry@west@x | rx@y == ry@north@y | rx@y == ry@south@y)] <- 0



 insideTF
}

S7::method(inside, list(ob_point, ob_circle)) <- function(x,y) {
  xc <- x - y@center
  -1 * sign(round(xc@r - y@radius, digits = 10))
}

S7::method(inside, list(ob_point, ob_ellipse)) <- function(x,y) {
  rx <- rotate(x, theta = -1*y@angle, origin = y@center)
  xc <- rx - y@center
  -1 * sign((xc@x / y@a) ^ y@m1 + (xc@y / y@b) ^ y@m2 - 1)
}

point_in_polygon <- function(x,y, vertices) {
  if (!is.matrix(vertices) && !is.data.frame(vertices)) stop("vertices must be a matrix or data.frame")
  if (!all(c("x", "y") %in% colnames(vertices))) stop("vertices must have two columns for x and y coordinates")

  n <- nrow(vertices)
  inside <- FALSE

  for (i in 1:n) {
    j <- if (i == 1) n else i - 1

    xi <- vertices[i, "x"]
    yi <- vertices[i, "y"]
    xj <- vertices[j, "x"]
    yj <- vertices[j, "y"]

    # Check if point is on the edge
    on_edge <- ((y - yi) * (xj - xi) == (x - xi) * (yj - yi)) &&
      (x >= min(xi, xj) && x <= max(xi, xj)) &&
      (y >= min(yi, yj) && y <= max(yi, yj))
    if (on_edge) return(0)

    # Ray casting algorithm https://en.wikipedia.org/wiki/Point_in_polygon
    intersect <- ((yi > y) != (yj > y)) &&
      (x < (xj - xi) * (y - yi) / (yj - yi + 1e-12) + xi)
    if (intersect) inside <- !inside
  }

  if (inside) {
    return(1)
  } else {
    return(-1)
  }
}


S7::method(inside, list(ob_point, ob_polygon)) <- function(x,y) {
  if (!(x@length == 1 || y@length == 1 || x@length == y@length)) stop(paste0("ob_point and ob_polygon with length > 1 need to be of the same size."))
  tibble::tibble(
    xx = x@x,
    xy = x@y,
    pg = y@p) |>
    dplyr::mutate(vertices = purrr::map(pg, \(x) x@tibble)) |>
    dplyr::select(x = xx, y = xy, vertices) |>
    purrr::pmap_dbl(point_in_polygon)

}


S7::method(inside, list(ob_point, ob_ngon)) <- function(x,y) {
  if (!(x@length == 1 || y@length == 1 || x@length == y@length)) stop(paste0("ob_point and ob_ngon with length > 1 need to be of the same size."))

  tibble::tibble(
    p = unbind(x),
    pg = unbind(y)) |>
    dplyr::mutate(x = purrr::map_dbl(p, \(x) x@x),
                  y = purrr::map_dbl(p, \(x) x@y),
                  vertices = purrr::map(pg, \(x) x@vertices@tibble)) |>
    dplyr::select(x,y, vertices) |>
    purrr::pmap_dbl(point_in_polygon)

}


S7::method(inside, list(ob_point, ob_reuleaux)) <- function(x,y) {
  if (!(x@length == 1 || y@length == 1 || x@length == y@length)) stop(paste0("ob_point and ob_ngon with length > 1 need to be of the same size."))

  tibble::tibble(p = unbind(x), pg = unbind(y)) |>
    purrr::pmap_int(\(p, pg) {
      min(inside(p, pg@arcs@circle))
    })
}
