do_nothing <- function(x) {
  # nocov start
  # Helps devtools:check find packages
  if (FALSE) {
    p1 <- ggforce::geom_circle()
    p2 <- arrowheadr::arrow_head_deltoid(d = 2.3, n = 100)
    p3 <- geomtextpath::geom_labelcurve()
    p4 <- bezier::bezier(t = 0.5, p = c(0, 0, 1, 1))
    p5 <- tinter::tinter("red")
  }
  # nocov end
}

#' @export
#' @importFrom S7 prop
S7::prop

#' @export
#' @importFrom S7 set_props
S7::set_props

#' @export
#' @importFrom S7 props
S7::props

# internal states ----
the <- new.env(parent = emptyenv())


#' @keywords internal
#' @noRd
character_index <- function(i, id) {
  if (is.character(i) || is.factor(i)) {
    i <- vctrs::vec_locate_matches(i, id)$haystack |>
      stats::na.omit() |>
      unclass()
    if (length(i) == 0) {
      stop("There are no objects with an id equal to the value specified.")
    }
  }
  i
}

# classes ----
## class_aesthetics_list ----
#' class_aesthetics_list
#'
#' list of aesthetics
#' @param geom Which geom function converts the shape
#' @param style vector of style names
#' @param mappable_bare aesthetics used without identity function
#' @param mappable_identity aesthetics used with identity function
#' @param not_mappable properties that cannot be mapped and thus are created with separate geom objects for each unique combination of values
#' @param required_aes required aesthetics
#' @param omit_names properties that are ignored
#' @param inherit.aes Defaults to `FALSE` so that ggdiagram objects do not interfere with other layers in the ggplot
#' @keywords internal
#' @returns a class_aesthetics_list object
class_aesthetics_list <- S7::new_class(
  name = "class_aesthetics_list",
  package = "ggdiagram",
  properties = list(
    geom = S7::class_function,
    style = S7::class_character,
    mappable_bare = S7::class_character,
    mappable_identity = S7::class_character,
    not_mappable = S7::class_character,
    required_aes = S7::class_character,
    omit_names = S7::class_character,
    inherit.aes = S7::class_logical
  )
)

## class_gg ----
class_gg <- S7::new_S3_class("gg")

## class_unit ----
class_unit <- S7::new_S3_class(
  "unit",
  constructor = function(.data = numeric(), units = "mm") {
    if (inherits(.data, "unit")) {
      return(.data)
    } else {
      grid::unit(.data, units)
    }
  },
  validator = function(self) {
    if (!is.numeric(self)) {
      stop("Underlying data for units must be numeric.")
    }
  }
)

## class_margin ----
#' Create a structure for label margins and label padding
#' @keywords internal
#' @param x A vector of numeric data, grid
#' @return class_margin
class_margin <- S7::new_class(
  name = "class_margin",
  package = "ggdiagram",
  parent = S7::class_list,
  constructor = function(x = numeric(0), units = "pt") {
    if (S7::S7_inherits(x, class_margin)) {
      return(x)
    }
    if (length(x) > 0) {
      if (is.numeric(x) && !grid::is.unit(x)) {
        x <- grid::unit(x, units = units)
      }
      if (is.list(x)) {
        if (
          all(purrr::map_lgl(x, \(o) {
            inherits(o, "margin")
          }))
        ) {
          return(purrr::map(x, class_margin)) # nocov
        }
        if (
          all(purrr::map_lgl(x, \(o) {
            inherits(o, "unit")
          }))
        ) {
          return(purrr::map(x, class_margin))
        }
        if (
          all(purrr::map_lgl(x, \(o) {
            S7::S7_inherits(o, class_margin)
          }))
        ) {
          return(purrr::map(x, class_margin))
        }
      }

      if (inherits(x, "margin")) {} else if (inherits(x, "unit")) {
        if (length(x) == 1) {
          x <- ggplot2::margin(x, x, x, x, unit = units)
        } else if (length(x) == 2) {
          x <- ggplot2::margin(x[1], x[2], x[1], x[2], unit = units)
        } else if (length(x) == 4) {
          x <- ggplot2::margin(x[1], x[2], x[3], x[4], unit = units)
        } else {
          stop(
            "Margins can have 1 (all sides), 2 (horizontal vs vertical), or 4 (top right bottom left) elements."
          )
        }
      } else {
        stop("Margins can be of class margin, unit, or numeric")
      }
    }
    S7::new_object(list(x))
  }
)


## class_arrowhead ----
class_arrowhead <- S7::new_class(
  "class_arrowhead",
  package = "ggdiagram",
  S7::class_list,
  constructor = function(x) {
    if (S7::S7_inherits(x, class_arrowhead)) {
      return(x)
    }
    if (is.list(x)) {
      return(purrr::map(x, class_arrowhead))
    }
    if (
      !(is.numeric(x) &&
        is.matrix(x) &&
        ncol(x) == 2)
    ) {
      print(x)
      stop("Arrowheads must be a 2-column matrix of numbers.")
    }

    S7::new_object(list(x))
  }
)


the$arrow_head <- class_arrowhead(arrowheadr::arrow_head_deltoid(
  d = 2.3,
  n = 100
))


#' Return default arrowhead
#'
#' The arrowhead function returns the default arrowhead. The set_default_arrowhead function will change the default arrowhead in the current R session. For details about making arrowheads, see the [ggarrow](https://teunbrand.github.io/ggarrow/articles/customisation.html) and [arrowheadr](https://wjschne.github.io/arrowheadr/) packages.
#' @param m A matrix used to make a ggarrow arrowhead
#' @export
#' @returns 2-column matrix
#' @examples
#' arrowhead()
#' # Set new default
#' set_default_arrowhead(ggarrow::arrow_head_wings(offset = 25))
#' arrowhead()
#' # restore default
#' set_default_arrowhead()
#' arrowhead()
arrowhead <- function() {
  the$arrow_head
}

#' @rdname arrowhead
#' @export
#' @returns previous default arrowhead
set_default_arrowhead <- function(m = NULL) {
  if (is.null(m)) {
    m <- arrowheadr::arrow_head_deltoid(d = 2.3, n = 100)
  }
  old <- the$arrow_head
  the$arrow_head <- class_arrowhead(m)
  invisible(old)
}

## has_style ----
has_style <- S7::new_class(
  name = "has_style",
  package = "ggdiagram",
  properties = list(id = class_character),
  abstract = TRUE
)
S7::S4_register(has_style)
S7::method(print, has_style) <- function(x, ...) {
  cli::cli_h3("{.cls {S7::S7_class(x)@name}}")
  print(x@tibble)
  invisible(x)
}


## ob_shape_list ----

#' ob_shape_list
#'
#' makes a heterogeneous list of different ggdiagram objects
#' @param .data a list of objects
#' @export
#' @returns An object of [`ob_shape_list`] class. List of objects that can be converted to geoms
ob_shape_list <- S7::new_class(
  "ob_shape_list",
  package = "ggdiagram",
  S7::class_list,
  validator = function(self) {
    if (!all(purrr::map_lgl(self, S7::S7_inherits, class = has_style))) {
      "All objects must be ggdiagram objects that can be converted to geoms."
    }
  }
)

## assign data ----

#' @keywords internal
#' @noRd
assign_data <- function(x, i, value) {
  dx <- x@tibble
  dx_unit <- purrr::map_lgl(dx, grid::is.unit)
  dx_unit <- names(dx_unit[dx_unit])

  dx <- dplyr::select(dx, !dplyr::all_of(dx_unit))

  dv <- value@tibble
  dv_unit <- purrr::map_lgl(dv, grid::is.unit)
  dv_unit <- names(dv_unit[dv_unit])
  dv <- dplyr::select(dv, !dplyr::all_of(dv_unit))

  # Filtering late necessary because unit variable cannot be
  # zero-length
  d_combined <- dplyr::bind_rows(
    dx,
    dv
  ) |>
    dplyr::filter(FALSE)

  dx <- d_combined |>
    dplyr::add_row(dx)

  dv <- d_combined |>
    dplyr::add_row(dv)

  dx[i, ] <- dv
  dx
}

# shape ----
shape <- S7::new_class(
  name = "shape",
  package = "ggdiagram",
  parent = has_style,
  abstract = TRUE
)

S7::method(`[`, shape) <- function(x, i) {
  i <- character_index(i, x@id)
  z <- data2shape(x@tibble[i, ], S7::S7_class(x))
  z@label <- na2zero(x@label[i])
  z
}

S7::method(unique, shape) <- function(x, incomparables = FALSE, ...) {
  d <- x@tibble
  if (S7::prop_exists(x, "label")) {
    if (length(x@label) > 0) {
      if (S7::S7_inherits(x@label, ob_label)) {
        l <- unbind(x@label)

        d <- d %>%
          dplyr::mutate(label = l)
      }
    }
  }
  ud <- d %>%
    unique()

  u <- ud
  u$label <- NULL

  u <- data2shape(u, S7::S7_class(x))

  if ("label" %in% colnames(ud)) {
    if (all(purrr::map_lgl(ud$label, S7::S7_inherits, class = ob_label))) {
      u@label <- bind(ud$label)
    }
  }

  u
}

# length shape
S7::method(length, shape) <- function(x) {
  x@length
}


S7::method(unique, ob_shape_list) <- function(x, incomparables = FALSE, ...) {
  purrr::map(x, unique) |>
    ob_shape_list()
}


xy <- S7::new_class(
  name = "xy",
  package = "ggdiagram",
  parent = shape,
  abstract = TRUE
)




#' ggdiagram function
#'
#' This is a convenient way to specify geom defaults
#'
#' @param font_family font family
#' @param font_size font size in points
#' @param point_size point size
#' @param linewidth line width
#' @param rect_linewidth line width of rectangles
#' @param theme_function A complete [ggplot2 theme][ggplot2::theme_minimal] function (e.g., [ggplot2::theme_minimal]). Defaults to [ggplot2::theme_void]
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments sent to [ggplot2::theme]
#' @returns ggplot function
#' @export
#'
#' @examples
#' ggdiagram(font_size = 20, font_family = "serif", linewidth = 3) +
#'    ob_circle(label = "Circle") +
#'    ob_rectangle(label = "Rectangle", x = 3, width = 3)
ggdiagram <- function(
  font_family = "sans",
  font_size = 11,
  linewidth = 0.5,
  point_size = 1.5,
  rect_linewidth = linewidth,
  theme_function = ggplot2::theme_void,
  ...
) {
  ggplot2::update_geom_defaults(
    geom = ggtext::GeomRichText,
    new = list(family = font_family, size = font_size / ggplot2::.pt)
  )

  ggplot2::update_geom_defaults(
    geom = "line",
    new = list(linewidth = linewidth)
  )

  ggplot2::update_geom_defaults(
    geom = "point",
    new = list(size = point_size)
  )

  ggplot2::update_geom_defaults(
    geom = ggarrow::GeomArrowSegment,
    new = list(linewidth = linewidth)
  )

  ggplot2::update_geom_defaults(
    geom = ggarrow::GeomArrow,
    new = list(linewidth = linewidth)
  )

  ggplot2::update_geom_defaults(
    geom = geomtextpath::GeomLabelpath,
    list(
      family = font_family,
      size = font_size / ggplot2::.pt
    )
  )

  ggplot2::update_geom_defaults(
    geom = ggplot2::GeomPolygon,
    list(linewidth = linewidth)
  )

  ggplot2::update_geom_defaults(
    geom = ggforce::GeomCircle,
    list(linewidth = linewidth)
  )

  ggplot2::update_geom_defaults(
    geom = ggforce::GeomShape,
    list(linewidth = linewidth)
  )

  ggplot2::ggplot() +
    theme_function(
      base_family = font_family,
      base_size = font_size,
      base_line_size = linewidth,
      base_rect_size = rect_linewidth
    ) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme(...)
}

