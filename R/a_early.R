do_nothing <- function(x) { # nocov start
  # Helps devtools:check find packages
  if (FALSE) {
    p1 <- ggforce::geom_circle()
    p2 <- arrowheadr::arrow_head_deltoid(d = 2.3, n = 100)
    p3 <- geomtextpath::geom_labelcurve()
    p4 <- bezier::bezier(t = .5, p = c(0, 0, 1, 1))
    p5 <- tinter::tinter("red")
  }
} # nocov end

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
    if (length(i) == 0) stop("There are no objects with an id equal to the value specified.")
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
  properties = list(
    geom = S7::class_function,
    style = S7::class_character,
    mappable_bare = S7::class_character,
    mappable_identity = S7::class_character,
    not_mappable = S7::class_character,
    required_aes = S7::class_character,
    omit_names = S7::class_character,
    inherit.aes = S7::class_logical
  ))

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
    if (!is.numeric(self))
      stop("Underlying data for units must be numeric.")
  }
)

## class_margin ----
class_margin <- S7::new_class(
  name = "class_margin",
  parent = S7::class_list,
  constructor = function(x = numeric(0), units = "pt") {

    if (S7::S7_inherits(x, class_margin))
      return(x)
    if (length(x) > 0) {
      if (is.numeric(x) && !grid::is.unit(x)) {
        x <- grid::unit(x, units = units)
      }
      if (is.list(x)) {
         if (all(purrr::map_lgl(x, \(o) {inherits(o, "margin")}))) {

          return(purrr::map(x, class_margin))
         }
        if (all(purrr::map_lgl(x, \(o) {inherits(o, "unit")}))) {
          return(purrr::map(x, class_margin))
        }
        if (all(purrr::map_lgl(x, \(o) {S7::S7_inherits(o, class_margin)}))) {
          return(purrr::map(x, class_margin))
        }
      }

      if (inherits(x, "margin")) {

      } else if (inherits(x, "unit")) {
        if (length(x) == 1) {
          x <- rep(x, 4)
          class(x) <- c("margin", class(x))
        } else if (length(x) == 2) {
          x <- rep(x, 2)
          class(x) <- c("margin", class(x))
        } else if (length(x) == 4) {
          class(x) <- c("margin", class(x))
        } else {
          stop(
            "Margins can have 1 (all sides), 2 (horiztonal vs vertical), or 4 (top right bottom left) elements."
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
  S7::class_list,
  constructor = function(x) {
    if (S7::S7_inherits(x, class_arrowhead))
      return(x)
    if (is.list(x))
      return(purrr::map(x, class_arrowhead))
    if (!(is.numeric(x) &&
          is.matrix(x) &&
          ncol(x) == 2))
      stop("Arrowheads must be a 2-column matrix of numbers.")

    S7::new_object(list(x))


  }
)


the$arrow_head <- class_arrowhead(arrowheadr::arrow_head_deltoid(d = 2.3, n = 100))


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
has_style <- S7::new_class(name = "has_style", properties = list(id = class_character), abstract = TRUE)
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
  S7::class_list,
  validator = function(self) {
    if (!all(purrr::map_lgl(self, S7::S7_inherits, class = has_style)))
      "All objects must be ggdiagram objects that can be converted to geoms"
  }
)

## assign data ----

#' @keywords internal
#' @noRd
assign_data <- function(x,i, value) {
  dx <- x@tibble
  dx_unit <- sapply(dx, grid::is.unit)
  dx_unit <- names(dx_unit[dx_unit])

  dx <- dplyr::select(dx, !dplyr::all_of(dx_unit))


  dv <- value@tibble
  dv_unit <- sapply(dv, grid::is.unit)
  dv_unit <- names(dv_unit[dv_unit])
  dv <- dplyr::select(dv, !dplyr::all_of(dv_unit))


  # Filtering late necessary because unit variable cannot be
  # zero-length
  d_combined <- dplyr::bind_rows(
    dx, dv) |>
    dplyr::filter(FALSE)

  dx <- d_combined |>
    dplyr::add_row(dx)



  dv <- d_combined |>
    dplyr::add_row(dv)

  dx[i, ] <- dv
  dx
}

# shape ----
shape <- S7::new_class(name = "shape",
                   parent = has_style,
                   abstract = TRUE)

S7::method(`[`, shape) <- function(x, i) {
  i <- character_index(i, x@id)
  z <- data2shape(x@tibble[i,], S7::S7_class(x))
  z@label <- na2zero(x@label[i])
  z
}

xy <- S7::new_class(name = "xy",
                parent = shape,
                abstract = TRUE)

# generics ----

## variance ----
#' create double-headed arrow paths indicating variance
#'
#' @param x object
#' @param where Location on object. Can be numeric (degrees), [degree], [radian], [turn], or named direction (e.g., "northwest", "east", "below", "left")
#' @param theta angle width
#' @param looseness distance of control points as a ratio of the distance to the object's center (e.g., in a circle of radius 1, looseness = 1.5 means that that the control points will be 1.5 units from the start and end points.)
#' @param bend Angle by which the control points are rotated. Can be numeric (degrees), [degree], [radian], [turn], or named direction (e.g., "northwest", "east", "below", "left"). Defaults to 0.
#' @inherit ob_style params
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @returns Returns an object of type [`ob_bezier`]
#' @export
ob_variance <- S7::new_generic("ob_variance", dispatch_args = "x", fun = function(
    x,
    where = "north",
    theta = 50,
    bend = 0,
    looseness = 1,
    arrow_head = the$arrow_head,
    resect = 2,
    ...) {
  S7::S7_dispatch()
})


## covariance ----
#' create double-headed arrow paths indicating variance
#'
#' @param x object
#' @param y object
#' @param where exit angle. Can be numeric (degrees), [degree], [radian], [turn], or named direction (e.g., "northwest", "east", "below", "left")
#' @param looseness distance of control points as a ratio of the distance to the object's center (e.g., in a circle of radius 1, looseness = 1.5 means that that the control points will be 1.5 units from the start and end points.)
#' @param bend Angle by which the control points are rotated. Can be numeric (degrees), [degree], [radian], [turn], or named direction (e.g., "northwest", "east", "below", "left"). Defaults to 0
#' @inherit ob_style params
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @returns An [`ob_bezier`] object
#' @export
ob_covariance <- S7::new_generic(
  "ob_covariance",
  dispatch_args = c("x", "y"),
  fun = function(x,
                 y,
                 where = NULL,
                 bend = 0,
                 looseness = 1,
                 arrow_head = the$arrow_head,
                 length_head = 7,
                 length_fins = 7,
                 resect = 2,
                 ...) {
    S7::S7_dispatch()
  }
)

## ob_array ----
#' make an array of shapes along a line
#'
#' @param x shape
#' @param k number of duplicate shapes to make
#' @param sep separation distance between shapes
#' @param where angle or named direction (e.g.,northwest, east, below, left)
#' @param anchor bounding box anchor
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to shape
#' @returns An array of shapes of the same class as object passed to x
#' @export
ob_array <- S7::new_generic(name = "ob_array", dispatch_args = "x", fun = function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {
  S7::S7_dispatch()
})

## bind ----

#' bind method
#' @param x list of objects to bind
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @examples
#' bind(c(ob_point(1,2), ob_point(3,4)))
#' bind(c(ob_circle(ob_point(0,0), radius = 1),
#'        ob_circle(ob_point(1,1), radius = 2)))
#' @export
#' @returns a bound object of same class as x (or list of objects if x contains objects of different types)
bind <- S7::new_generic(name = "bind", dispatch_args = "x")

S7::method(bind, S7::class_list) <- function(x, ...) {

  all_angles <- all(sapply(lapply(x, class), function(xx)
    "ob_angle" %in% xx))
  if (all_angles) {
    if (length(x) == 0) {
      return(degree())
      }
    trns <- unlist(x)
    if (S7::S7_inherits(x[[1]], turn)) {
      return(turn(trns))
    } else if (S7::S7_inherits(x[[1]], radian)) {
      return(radian(trns * pi * 2))
    } else {
      return(degree(trns * 360))
    }
  }

  x <- unlist(x)
  .f <- S7::S7_class(x[[1]])@name
  allsame <- allsameclass(x, .f)
  if (length(allsame) > 0) {
    return(bind(ob_shape_list(x)))
  }

  d <- get_non_empty_list(
    dplyr::bind_rows(
      purrr::map(
        x,
        \(o) {
          if (S7::S7_inherits(o, ob_style)) get_tibble(o) else o@tibble
          }
        )))

  .fn <- switch(.f,
                degree = degree,
                radian = radian,
                turn = turn,
                ob_arc = ob_arc,
                ob_bezier = ob_bezier,
                ob_circle = ob_circle,
                ob_ellipse = ob_ellipse,
                ob_label = ob_label,
                ob_line = ob_line,
                ob_ngon = ob_ngon,
                ob_path = ob_path,
                ob_point = ob_point,
                ob_rectangle = ob_rectangle,
                ob_reuleaux = ob_reuleaux,
                ob_segment = ob_segment,
                ob_style = ob_style,
                ob_polar = ob_point)
  o <- rlang::inject(.fn(!!!d))
  dots <- rlang::list2(...)
  o <- rlang::inject(S7::set_props(o, !!!dots))
  if (S7::prop_exists(x[[1]], "label") &&
      !S7::S7_inherits(x[[1]], ob_label)) {
    x_label <- purrr::map(x, \(xx) xx@label)
    is_ob_label <- purrr::map_lgl(x_label, S7::S7_inherits, class = ob_label)
    if (any(is_ob_label)) {
      ol <- ob_label(NA)
      x_label <- purrr::map(x_label, \(xx) {
        if (!S7::S7_inherits(xx, ob_label)) {
          xx <- ol
        }
        xx
      })
      o@label <- bind(x_label)

    }
  }
  o
}


S7::method(bind, ob_shape_list) <- function(x, ...) {
  .f <- unique(lapply(x, S7::S7_class))

  csl <- lapply(.f, \(.ff) {
    Filter(f = \(xx){
      S7::S7_inherits(xx, .ff)
    } , x = S7::S7_data(x)) |>
      bind()
  })

  if (length(csl) > 1) {
    csl_names <- purrr::map_chr(csl, \(xx) S7::S7_class(xx)@name)
    ob_shape_list(csl) |>
      `names<-`(csl_names)

  } else {
    csl[[1]]

  }
}

## unbind ----

#' unbind
#'
#' Converts an object with k elements into a list of k objects
#' @param x object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> additional arguments (not used at this time)
#' @export
#' @returns a list of objects, each of length 1
unbind <- S7::new_generic("unbind", dispatch_args = "x")

S7::method(unbind, has_style) <- function(x) {
  purrr::map(seq(1, x@length), \(i) x[i])
}

S7::method(unbind, ob_shape_list) <- function(x) {
  as.list(x)
}

#' map_ob
#'
#' A wrapper for [purrr::map]. It takes a ggdiagram object with multiple elements, applies a function to each element within the object, and returns a ggdiagram object
#' @param .x a ggdiagram object
#' @param .f a function that returns a ggdiagram object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to .f
#' @param .progress display progress if TRUE
#'
#' @returns a ggdiagram object
#' @export
map_ob <- function(.x, .f, ..., .progress = FALSE) {
  if (S7::S7_inherits(.x, has_style) | S7::S7_inherits(.x, ob_angle)) {
    .x <- unbind(.x)
     }
  purrr::map(.x, .f, ..., .progress = .progress) |>
    bind()
}


## str----
str <- S7::new_external_generic(package = "utils", name = "str", dispatch_args = "object")



## plus----
S7::method(`+`, list(S7::class_any, S7::class_any)) <- function(e1, e2) {
  .Primitive("+")(e1, e2)
}

S7::method(`+`, list(S7::class_character, S7::class_character)) <- function(e1, e2) {
  paste0(e1, e2)
}
S7::method(`+`, list(S7::class_numeric, S7::class_character)) <- function(e1, e2) {
  paste0(e1, e2)
}
S7::method(`+`, list(S7::class_character, S7::class_numeric)) <- function(e1, e2) {
  paste0(e1, e2)
}

## get_tibble----
#' Get object data with styles in a tibble
#'
#' @param x object
#' @export
#' @returns a [tibble::tibble]
get_tibble <- S7::new_generic("get_tibble", "x", fun = function(x) {S7::S7_dispatch()})
S7::method(get_tibble, S7::class_list) <- function(x) {
  purrr::map_df(S7::S7_data(x), get_tibble)
}

# Resect ----
#' resect
#'
#' Shorten segments
#' @param resect a numeric distance
#' @param x object
#' @param distance resect distance
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
#' @returns object of same class as x
resect <- S7::new_generic("resect", c("x", "distance"))


#' Get object data in a tibble, filling in any missing styles with defaults
#'
#' @param x object
#' @export
#' @returns a [tibble::tibble]
#' @rdname get_tibble
get_tibble_defaults <- S7::new_generic("get_tibble_defaults", "x", fun = function(x) S7::S7_dispatch())
S7::method(get_tibble_defaults, S7::class_any) <- function(x) {
  get_tibble(x)
}


#' Move an object
#'
#' @param object object
#' @param x nudge right and left
#' @param y nudge up and down
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @examples
#' ob_circle() |> nudge(x = 2)
#' # Alternative to nudge:
#' ob_circle() + ob_point(2, 0)
#' @export
#' @returns object of same class as `object`
nudge <- S7::new_generic("nudge", c("object", "x", "y"))

# unions ----
class_numeric_or_character <- S7::new_union(S7::class_numeric, S7::class_character)
class_numeric_or_unit <- S7::new_union(S7::class_numeric, class_unit)

#' Make a variant of a function with alternate defaults
#'
#' Makes a copy of a function with new defaults. Similar to [`purrr::partial`] except that arguments with new defaults still accept input.
#'
#' @param .f function
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> new defaults
#'
#' @returns function
#' @export
#'
#' @examples
#' squircle <- redefault(ob_ellipse, m1 = 4)
#' squircle(a = 3)
redefault <- function(.f, ...) {
  # adapted from diversitree::set.defaults
  new_defs <- rlang::list2(...)
  att <- attributes(.f)
  formals(.f)[names(new_defs)] <- new_defs
  attributes(.f) <- att[names(att) != "srcref"]
  .f
}


# helpers ----

.namedpositions <- c(
  east = 0,
  right = 0,
  `east-northeast` = 22.5,
  northeast = 45,
  `top right` = 45,
  `above right` = 45,
  `north-northeast` = 67.5,
  north = 90,
  top = 90,
  above = 90,
  `north-northwest` = 112.5,
  northwest = 135,
  `top left` = 135,
  `above left` = 135,
  `west-northwest` = 157.5,
  west = 180,
  left = 180,
  `west-southwest` = 202.5,
  southwest = 225,
  `bottom left` = 225,
  `below left` = 225,
  `south-southwest` = 247.5,
  south = 270,
  bottom = 270,
  below = 270,
  `south-southeast` = 292.5,
  southeast = 315,
  `bottom right` = 315,
  `below right` = 315,
  `east-southeast` = 337.5
)

#' @keywords internal
#' @noRd
cardinalpoint <- function(x) {
  .namedpositions
  if (!all(x %in% names(.namedpositions))) {
    stop(paste0("Position must be an angle, numeric, or one of these named positions:\n", stringr::str_wrap(paste0(names(.namedpositions), collapse = ", "))))
  }
  unname(.namedpositions[x])
}

#' @keywords internal
#' @noRd
allsameclass <- function(l, classname) {
  classname[classname == "ob_polar"] <- "ob_point"
  classname <- paste0("ggdiagram::", classname)
  allsame <- all(sapply(lapply(l, class), function(x)
    classname %in% x))
  if (!allsame) {
    paste0("All items must be ", classname, ".")
  }
}

#' @keywords internal
#' @noRd
aes_injection <- function(bare_mapping,
                          identity_mapping,
                          omit_mapping = NULL) {
  identity_mapping <- setdiff(identity_mapping,
                              c(bare_mapping, omit_mapping))
  bare_mapping <- setdiff(bare_mapping, omit_mapping)
  i_styles <- purrr::map(rlang::syms(identity_mapping), \(i) call("I", i))
  names(i_styles) <- identity_mapping
  b_styles <- rlang::syms(bare_mapping)
  names(b_styles) <- bare_mapping

  ggplot2::aes(!!!b_styles, !!!i_styles)

}



#' @keywords internal
#' @noRd
get_tibble_defaults_helper <- function(
    x,
    default_style,
    required_aes = c("x", "y")) {

  d <- get_tibble(x) |>
    dplyr::select(-dplyr::any_of("id"))

  if ("x0" %in% required_aes && "x" %in% colnames(d)) {
    d <- dplyr::rename(d, dplyr::all_of(c(x0 = "x")))
  }

  if ("y0" %in% required_aes && "y" %in% colnames(d)) {
    d <- dplyr::rename(d, dplyr::all_of(c(y0 = "y")))
  }

  for (n in setdiff(colnames(d), required_aes)) {
    d_prop <- S7::prop(default_style, n)
    if (!(is.null(d_prop) || identical(d_prop, list()))) {
      d_prop <- ifelse(is.vector(d_prop), d_prop, c(d_prop))
      missings <- is.na(`[[`(d, n))
      if (!all(missings) && any(missings)) {
        d[missings, n] <- d_prop
      }


    }
  }
  d
}



#' @keywords internal
#' @noRd
get_non_empty_props <- function(x) {
  Filter(function(s) {
    ifelse(length(s) > 0,
           ifelse(
             S7::S7_inherits(s),
             length(S7::S7_data(s)) > 0,
             !rlang::is_function(s)
           ),
           FALSE)

  }, props(x))
}

#' @keywords internal
#' @noRd
get_non_empty_list <- function(l) {
  Filter(\(x) length(x) > 0, l)
}

#' @keywords internal
#' @noRd
get_non_empty_tibble <- function(d) {
  d <- Filter(\(x) length(x) > 0, d)
  d <- Filter(\(x) !is.null(x), d)
  tibble::as_tibble(d)
}

#' @keywords internal
#' @noRd
replace_na <- function(x, y) {
  if (rlang::is_quosure(x)) return(y)
  ifelse(is.na(x), y, x)
}

#' @keywords internal
#' @noRd
na2zero <- function(x) {
  if (is.character(x)) {
    if (all(is.na(x))) {
      x <- character(0)
    }
  }
  x
}

#' @keywords internal
#' @noRd
ob_array_helper <- function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {
  if (x@length > 1) {stop("The shape must start with an object of length 1.")}

  dots <- rlang::list2(...)

  p <- as.list(seq(k))
  p[1] <- unbind(x)

  for (i in seq(2, k)) {
    p[i] <- unbind(place(x, p[[i - 1]], where = where, sep = sep))
  }
  p <- bind(p)

  bb <- p@bounding_box
  if (anchor == "center") {
    p_anchor <- bb@center
  } else {
    p_anchor <- bb@point_at(anchor)
  }

  p_center <- p@center - p_anchor + x@center

  l <- character(0)

  if (S7::S7_inherits(x@label, ob_label)) {
    if (x@label@length == 1) {
      l <- ob_label(subscript(x@label@label, seq(k)),
                 style = x@style,
                 center = p_center)
    }

    if (x@label@length == k) {
      l <- ob_label(x@label@label,
                 style = x@style,
                 center = p_center)
    }
  }

  if (is.null(dots$label)) {
    if (length(l) > 0) {
      dots$label <- l
    }
  } else {
    if (S7::S7_inherits(dots$label, ob_label)) {
      dots$label <- ob_label(center = p_center,
                          label = dots$label@label,
                          style = dots$label@style)
    }
  }

  list(
    x = x,
    k = k,
    sep = sep,
    anchor = anchor,
    p_center = p_center,
    dots = dots
  )

}



#' Create subscripts
#'
#' @param x text
#' @param subscript subscript
#' @param output Can be `markdown` (default) or `latex`
#'
#' @returns text
#' @export
#'
#' @examples
#' ggdiagram() +
#'   ob_circle(label = ob_label(subscript("X", 1), size = 16)) +
#'   ob_circle(x = 3, label = ob_label(superscript("A", 2), size = 16))
subscript <- function(x,
                      subscript = seq(length(x)),
                      output = c("markdown", "latex")) {
  output <- match.arg(output)
  if (output == "markdown") {
    paste0(x, "<sub>", subscript, "</sub>")
  } else {
    paste0(x, "_{", subscript, "}")
  }

}

#' Create superscript
#'
#' @param x string
#' @param superscript superscript
#' @param output Can be `markdown` (default) or `latex`
#'
#' @returns string
#' @rdname subscript
#' @export
superscript <- function(x,
                        superscript = seq(length(x)),
                      output = c("markdown", "latex")) {
  output <- match.arg(output)
  if (output == "markdown") {
    paste0(x, "<sup>", superscript, "</sup>")
  } else {
    paste0(x, "^{", superscript, "}")
  }

}


#' Centering signed numbers
#'
#'
#' A wrapper function for the signs::signs function. It adds a space to the right side of negative numbers so that it appear as if the minus sign does not affect the number's centering.
#' @param x a numeric vector
#' @param space a character to be added to negative numbers (defaults to a UTF-8 figure space)
#' @param encoding type of encoding (defaults to UTF-8)
#' @param ... parameters passed to signs:signs
#'
#' @returns a vector of numbers converted to characters
#' @export
signs_centered <- function(x, space = NULL, encoding = "UTF-8", ...) {
  if (is.null(space)) space <- "\u2007"
  x_new <- paste0(signs::signs(x, ...), ifelse(x < 0, space, ""))
  Encoding(x_new) <- encoding
  x_new
}

#' Probability rounding
#'
#' Rounds to significant digits, removing leading zeros.
#'
#' @param p probability
#' @param accuracy smallest increment
#' @param digits significant digits
#' @param max_digits maximum rounding digits
#' @param remove_leading_zero remove leading zero
#' @param round_zero_one round 0 and 1
#' @param phantom_text invisible text inserted on the right
#' @param phantom_color color of phantom text
#' @returns a character vector
#' @export
#' @examples
#' round_probability(c(0, .0012, .012, .12, .99, .992, .9997, 1), digits = 2)
round_probability <- function(p,
                              accuracy = 0.01,
                              digits = NULL,
                              max_digits = NULL,
                              remove_leading_zero = TRUE,
                              round_zero_one = TRUE,
                              phantom_text = NULL,
                              phantom_color = NULL) {
  if (is.null(digits)) {
    l <- scales::number(p, accuracy = accuracy)
  }
  else {
    abs_p <- abs(p)
    sig_digits <- abs(ceiling(log10(abs_p + abs_p / 1e+09)) - digits)
    pgt99 <- abs_p > 0.99
    sig_digits[pgt99] <- abs(ceiling(log10(1 - abs_p[pgt99])) - digits + 1)


    sig_digits[ceiling(log10(abs_p)) == log10(abs_p) &
                 (-log10(abs_p) >= digits)] <-
      sig_digits[ceiling(log10(abs_p)) == log10(abs_p) &
                   (-log10(abs_p) >= digits)] - 1


    sig_digits[is.infinite(sig_digits)] <- 0


    l <- purrr::map2_chr(p, sig_digits,
                         formatC,
                         format = "f",
                         flag = "#")
  }
  if (remove_leading_zero)
    l <- sub("^-0", "-", sub("^0", "", l))
  if (round_zero_one) {
    l[p == 0] <- "0"
    l[p == 1] <- "1"
    l[p == -1] <- "-1"
  }
  if (!is.null(max_digits)) {
    if (round_zero_one) {
      l[round(p, digits = max_digits) == 0] <- "0"
      l[round(p, digits = max_digits) == 1] <- "1"
      l[round(p, digits = max_digits) == -1] <- "-1"
    }
    else {
      l[round(p, digits = max_digits) == 0] <- paste0(
        ".",
        paste0(rep("0", max_digits),
               collapse = ""))


      l[round(p, digits = max_digits) == 1] <- paste0(
        "1.",
        paste0(rep("0", max_digits),
               collapse = ""))


      l[round(p, digits = max_digits) == -1] <- paste0(
        "-1.",
        paste0(rep("0", max_digits),
               collapse = ""))
    }
  }
  l <- sub(pattern = "-",
           replacement = "\u2212",
           x = l)
  if (!is.null(phantom_text)) {
    phantom_text <- paste0(
      ifelse(p < 0, "\u2212", ""),
      phantom_text)
    if (is.null(phantom_color)) phantom_color <- "white"
    l <- paste0(
      l,
      "<span style='color: ",
      phantom_color,
      "'>",
      phantom_text,
      "</span>")
  }


  Encoding(l) <- "UTF-8"
  dim(l) <- dim(p)
  l
}



# https://github.com/RConsortium/S7/issues/370
#' @keywords internal
#' @noRd
prop_integer_coerce <- function(name) {
  S7::new_property(
    name = name,
    class = S7::class_integer,
    setter = function(self, value) {
      if (rlang::is_integerish(value)) {
        value <- as.integer(value)
      }
      S7::prop(self, name) <- value
      self
    }
  )
}

#' @keywords internal
#' @noRd
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)),
        substring(s, 2),
        sep = "",
        collapse = " ")
}


#' @keywords internal
#' @noRd
.between <- function(x, lb, ub) {
  b <- cbind(lb = lb, ub = ub)
  ub <- apply(b, 1, max)
  lb <- apply(b, 1, min)
  (x >= lb) & (x <= ub)

}



# as.geom ----
#' as.geom function
#'
#' Converts a ggdiagram shape to a ggplot2 geom
#'
#' Usually the `as.geom` function is not necessary to call explicitly because it is called whenever a ggdiagram shape is added to a ggplot. However, in complex situations (e.g., making a function that assembles many objects), it is sometimes necessary to make the call explicitly.
#'
#' @param x a shape
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Pass arguments to ggplot2::geom_point
#' @rdname as.geom
#' @returns geom
#' @export
#' @examples
#' library(ggplot2)
#' c1 <- ob_circle(radius = 3)
#' ggplot() +
#'   as.geom(c1, fill = "black") +
#'   coord_equal()
#'
as.geom <- S7::new_generic("as.geom", "x")

S7::method(as.geom, ob_shape_list) <- function(x, ...) {
    unlist(lapply(c(x), \(g) as.geom(g, ...)))
}

S7::method(`+`, list(class_gg, has_style)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

S7::method(`+`, list(class_gg, ob_shape_list)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

if (packageVersion("ggplot2") >= "3.5.2.9000") {
  S7::method(update_ggplot, list(has_style, class_ggplot)) <-
    function(object, plot, ...) {
      plot + as.geom(object)
    }
}



#' @noRd
#' @keywords internal
make_geom_helper <- function(d = NULL,
                             aesthetics,
                             user_overrides,
                             ...) {

  omit_names <- unique(
    c(aesthetics@omit_names,
      setdiff(
        names(ob_style@properties),
        unique(c(
          aesthetics@mappable_bare,
          aesthetics@required_aes,
          aesthetics@not_mappable,
          aesthetics@mappable_identity)))))

  # add group so that I() function will not disturb drawing order
  if (!("group" %in% unique(c(omit_names, colnames(d))))) {
    d$group <- seq_len(nrow(d))
  }
  # 1 row per unique combination of not mappable arguments
  d_nested <- tidyr::nest(d, .by = dplyr::any_of(aesthetics@not_mappable))

  # all colnames  but data
  not_mapped_names <- colnames(d_nested)[colnames(d_nested) != "data"]
  d_isunit <- purrr::map_lgl(d_nested, grid::is.unit)
  d_unit_names <- names(d_isunit[d_isunit])
  d_unit_types <- purrr::map_chr(d_nested[, d_unit_names], grid::unitType)

  d_nested <- dplyr::mutate(d_nested,
                            dplyr::across(dplyr::all_of(d_unit_names),
                                          .fns = as.numeric))

  d_all <- tidyr::nest(d_nested, .by = dplyr::all_of("data"), .key = "unmappable")

  # make geom for each row in d_nested
  purrr::pmap(d_all, \(data, unmappable) {

    if ("p_unnest" %in% colnames(data)) {
      data <- tidyr::unnest(data = data, .data$p_unnest)
    }

    # make list of not mapped arguments
    not_mapped <- as.list(unmappable)

    not_mapped <- purrr::map2(not_mapped,
                              names(not_mapped), \(x, name) {
      if (name %in% c("label.margin", "label.padding")) {
        x <- x[[1]]
      }
      x
    })

    not_mapped <- purrr::map2(not_mapped,
                              names(not_mapped), \(x, name) {
      if (name %in% d_unit_names) {
        x <- grid::unit(x, d_unit_types[name])
      }
      x
    })

    # get mappable names
    data_names <- colnames(data)



    # get names for bare mapping
    bare_mapping <- intersect(unique(c(aesthetics@required_aes,
                                       aesthetics@mappable_bare)),
                              data_names)

    # omitted arguments
    omit_mapping <- unique(c(omit_names,
                             names(user_overrides),
                             not_mapped_names))

    # gename names for identity mapping
    identity_mapping <- setdiff(
      data_names,
      unique(c(bare_mapping, omit_mapping)))

    myaes <- aes_injection(
      bare_mapping = bare_mapping,
      identity_mapping = identity_mapping,
      omit_mapping = omit_mapping
    )

    rlang::inject(aesthetics@geom(
      mapping = myaes,
      data = data,
      !!!user_overrides,
      !!!not_mapped,
      ...
    ))
  })
}

#' @keywords internal
#' @noRd
trimmer <- function(x) {
  notabs <- gsub(x = x,
                 pattern = "\\t",
                 replacement = " ")
  trimws(gsub(
    x = notabs,
    pattern = "\\s+",
    replacement = " "
  ))
}

#' @keywords internal
#' @noRd
rounder <- function(x, digits = 2, add = FALSE, output = c("markdown", "latex")) {
  output <- match.arg(output)
  minus <- ifelse(output == "markdown", "\u2212", "-")
  if (add) {
    r <- paste0(ifelse(x > 0, " + ", paste0(" ", minus, " ")), trimws(formatC(
        abs(x), digits = digits, format = "fg"
      )))
  } else {
    r <- paste0(ifelse(x > 0, "", minus), trimws(formatC(
      abs(x), digits = digits, format = "fg"
    )))
  }
  r
}

#' @keywords internal
#' @noRd
emphasis <- function(x, output = "markdown") {
  if (output == "markdown") {
    x <- paste0("*", x, "*")
  }
  x
}

# Equation ----
#' equation
#'
#' Get equation for object
#'
#' @param x object
#' @param type equation type. Can be `y` (default), `general`, or `parametric`
#' @param output Can be `markdown` (default) or `latex`
#' @param digits rounding digits
#' @export
#' @returns string
#' @examples
#' l1 <- ob_line(slope = 2, intercept = 4)
#' c1 <- ob_circle(radius = 3)
#' ggdiagram() +
#'   l1 +
#'   c1 +
#'   ob_label(label = equation(c1),
#'            center = c1@center,
#'            size = 16) +
#'   ob_label(label = equation(l1),
#'            center = ob_segment(intersection(l1, c1))@midpoint(),
#'            angle = l1@angle,
#'            size = 16) +
#'  ggplot2::theme_minimal(base_size = 20)
equation <- S7::new_generic(
  "equation",
  dispatch_args = "x",
  fun = function(x,
                 type = c("y", "general", "parametric"),
                 output = c("markdown", "latex"),
                 digits = 2) {
    S7::S7_dispatch()
    })


# Projection ----
#' Find projection of a point on an object (e.g., line or segment)
#'
#' @param p ob_point
#' @param object object (e.g., line or segment)
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style object
#' @export
#' @returns ob_point
projection <- S7::new_generic("projection", c("p", "object"))


# midpoint----
#' Get one or more points at positions from 0 to 1
#'
#' It is possible to get more than one midpoint by specifying a position vector with a length greater than 1. Position values outside 0 and 1 will usually work, but will be outside the object.
#' @param x object
#' @param y object (can be omitted for segments and arcs)
#' @param position numeric vector. 0 is start, 1 is end. Defaults to .5
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
#' @returns ob_point
midpoint <- S7::new_generic(
  "midpoint",
  c("x", "y"),
  fun = function(x, y, position = .5, ...) {
    S7::S7_dispatch()
  }
)


#' @keywords internal
#' @noRd
rotate2columnmatrix <- function(x, theta) {
  if (all(theta == 0)) return(x)
  x_rotated <- x %*%  matrix(
    c(cos(theta),
      -sin(theta),
      sin(theta),
      cos(theta)),
    nrow = 2,
    ncol = 2)
  colnames(x_rotated) <- NULL
  x_rotated
}


#' Automatic label for objects
#'
#' @param object object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> additional arguments
#' @returns string
#' @export
label_object <- S7::new_generic("label_object", "object")

# connect ----

#' Arrow connect one shape to another
#'
#' By default, will create an [`ob_segment`] with an arrowhead on the end. If `arc_bend` is specified, an [`ob_arc`] with an arrowhead will be created instead. If `from_offset` or `to_offset` are specified, an [`ob_bezier`] with an arrowhead will be created.
#' @param from first shape object
#' @param to second shape object
#' @param arc_bend If specified, the arrow will be an arc with a sagitta sized in proportion to the distance between points. The sagitta is is the largest distance from the arc's chord to the arc itself. Negative values bend left. Positive values bend right. 1 and -1 create semi-circles. 0 is a straight segment. If specified, will override `from_offset` and `to_offset`.
#' @param from_offset If specified, arrow will be a bezier curve. The `from_offset` is a point (ob_point or ob_polar) that is added to `from` to act as a control point in the bezier curve.
#' @param to_offset If specified, arrow will be a bezier curve. The `to_offset` is a point (ob_point or ob_polar) that is added to `to` to act as a control point in the bezier curve.
#' @inheritParams ob_bezier
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments passed to [ob_style]
#' @export
#' @returns ob_segment
connect <- S7::new_generic(
  "connect",
  c("from", "to"),
  function(from,
           to,
           ...,
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
           id = character(0)) {
    S7::S7_dispatch()
  })



#' Place an object a specified distance from another object
#'
#' @param x shape object
#' @param from shape that x is placed in relation to
#' @param where named direction, angle, or number (degrees)
#' @param sep separation distance
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments passed to [ob_style]
#' @export
#' @returns object of same class as `x`
place <- S7::new_generic("place", c("x", "from"),
                     fun = function(x, from, where = "right", sep = 1, ...) {
                       S7::S7_dispatch()
                     })




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
    linewidth = .5,
    point_size = 1.5,
    rect_linewidth = linewidth,
    theme_function = ggplot2::theme_void,
    ...) {

  ggplot2::update_geom_defaults(
    geom = ggtext::GeomRichText,
    new = list(family = font_family,
               size = font_size / ggplot2::.pt))

  ggplot2::update_geom_defaults(
    geom = "line",
    new = list(linewidth = linewidth))

  ggplot2::update_geom_defaults(
    geom = "point",
    new = list(size = point_size))

  ggplot2::update_geom_defaults(
    geom = ggarrow::GeomArrowSegment,
    new = list(linewidth = linewidth))

  ggplot2::update_geom_defaults(
    geom = ggarrow::GeomArrow,
    new = list(linewidth = linewidth))

  ggplot2::update_geom_defaults(
    geom = geomtextpath::GeomLabelpath,
    list(
      family = font_family,
      size = font_size / ggplot2::.pt))

  ggplot2::update_geom_defaults(
    geom = ggplot2::GeomPolygon,
    list(linewidth = linewidth))

  ggplot2::update_geom_defaults(
    geom = ggforce::GeomCircle,
    list(linewidth = linewidth))

  ggplot2::update_geom_defaults(
    geom = ggforce::GeomShape,
    list(linewidth = linewidth))

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

# data2shape ----

#' Make shapes from data
#'
#' Allows a data.frame or tibble to be converted to shape objects.
#' @param data data.frame or tibble
#' @param shape shape function
#' @returns shape object
#' @export
#' @examples
#' d <- data.frame(
#'   x = 1:2,
#'   y = 1:2,
#'   fill = c("blue", "forestgreen"),
#'   color = NA,
#'   radius = c(.25,0.5))
#'
#' ggdiagram() +
#'   data2shape(d, ob_circle)
data2shape <- function(data, shape) {
  l <- as.list(data)
  rlang::inject(shape(!!!l))
}


#' Function to calculate hierarchy depth in lavaan models
#'
#' @param x character vector of variables in a lavaan model
#' @param model character, lavaan fit object, or lavaan parameter table
#' @param depth initial depth
#' @param max_depth max depth at which to stop (prevents infinite loops for non-recursive models)
#'
#' @returns integer
#' @export
#'
#' @examples
#' model <- "X =~ X1 + X2"
#' get_depth("X", model = model)
#' get_depth("X1", model = model)
get_depth <- function(x, model, depth = 0L, max_depth = 20) {
  if (inherits(model, "character")) {
    model <- lavaan::lavaanify(model)
  }

  if (inherits(model, "lavaan")) {
    model <- lavaan::parametertable(model)
  }

  if (!all(c("op", "lhs", "rhs") %in% colnames(model))) {
    stop("model must be a lavaan fit object, a lavaan parameter table, or a character vector specifying a lavaan model.")
  }

  purrr::map_int(
    x,
    \(xx) get_depth_helper(
      xx,
      model = model,
      depth = depth,
      max_depth = 20
    )
  )
}

#' Function to calculate hierarchy depth in lavaan models
#'
#' Helps with get_depth
#' @param x character vector of variables in a lavaan model
#' @param model character, lavaan fit object, or lavaan parameter table
#' @param depth initial depth
#' @param max_depth max depth at which to stop (prevents infinite loops for non-recursive models)
#' @keywords internal
#' @noRd
get_depth_helper <- function(x, model, depth = 0L, max_depth = 20) {

  # Does it have children?
  children <- model %>%
    dplyr::filter(.data$op == "=~", .data$lhs %in% x) %>%
    dplyr::pull(.data$rhs) %>%
    unique()

  # Detect infinite loops
  if (depth >= max_depth) stop("Maximum depth reached. May be nonrecursive. ")

  # Do children have children?
  if (length(children) > 0) {
    get_depth_helper(children, model, depth = depth + 1)
  } else {
    # No children, return depth
    return(depth + 1)
  }
}
