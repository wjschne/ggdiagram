library(S7)

do_nothing <- function(x) {
  # Helps devtools:check find packages
  if (FALSE) {
    p1 <- ggforce::geom_circle()
    p2 <- arrowheadr::arrow_head_deltoid(d = 2.3, n = 100)
    p3 <- geomtextpath::geom_labelcurve()
    p4 <- bezier::bezier(t = .5, p = c(0,0, 1,1))
    p5 <- tinter::tinter("red")
  }
}

utils::globalVariables("properties")

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
the$arrow_head <- arrowheadr::arrow_head_deltoid(d = 2.3, n = 100)

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
class_aesthetics_list <- new_class(
  name = "class_aesthetics_list",
  properties = list(
    geom = class_function,
    style = class_character,
    mappable_bare = class_character,
    mappable_identity = class_character,
    not_mappable = class_character,
    required_aes = class_character,
    omit_names = class_character,
    inherit.aes = class_logical
  )
                                     )
## class_ggplot ----
class_ggplot <- new_S3_class("ggplot")

## class_unit ----
class_unit <- new_S3_class(
  "unit",
  constructor = function(.data = numeric(), units = "mm") {
    if ("unit" %in% class(.data)) {
      return(.data)
    } else {
      ggplot2::unit(.data, units)
    }
  },
  validator = function(self) {
    if (!is.numeric(self))
      stop("Underlying data for units must be numeric.")
  }
)

## class_margin ----
class_margin <- new_class(
  name = "class_margin",
  parent = class_list,
  constructor = function(x = class_missing, units = "pt") {

    if (S7_inherits(x, class_margin))
      return(x)
    if (length(x) > 0) {
      if (is.numeric(x) && !grid::is.unit(x)) {
        x <- grid::unit(x, units = units)
      }
      if (is.list(x)) {
         if (all(purrr::map_lgl(x, \(o) {"margin" %in% class(o)}))) {

          return(purrr::map(x, class_margin))
         }
        if (all(purrr::map_lgl(x, \(o) {"unit" %in% class(o)}))) {
          return(purrr::map(x, class_margin))
        }
        if (all(purrr::map_lgl(x, \(o) {S7_inherits(o, class_margin)}))) {
          return(purrr::map(x, class_margin))

        }

      }

      if ("margin" %in% class(x)) {

      } else if ("unit" %in% class(x)) {
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
    new_object(list(x))
  }
)

## class_arrowhead ----
class_arrowhead <- new_class(
  "class_arrowhead",
  class_list,
  constructor = function(x) {
    if (S7_inherits(x, class_arrowhead))
      return(x)
    if (is.list(x))
      return(purrr::map(x, class_arrowhead))
    if (!(is.numeric(x) &&
          is.matrix(x) &&
          ncol(x) == 2))
      stop("Arrowheads must be a 2-column matrix of numbers.")

    new_object(list(x))


  }
)

#' ob_shape_list
#'
#' makes a heterogeneous list of different ggdiagram objects
#' @param .data a list of objects
#' @export
ob_shape_list <- new_class("ob_shape_list", class_list,validator = function(self) {
  if(!all(purrr::map_lgl(self, S7_inherits, class = has_style))) "All objects must be ggdiagram objects that can be converted to geoms"
})


## has_style ----
has_style <- new_class(name = "has_style", abstract = TRUE)
method(print, has_style) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

assign_data <- function(x,y, value) {
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

  dx[y, ] <- dv


  as.list(dx)
}

method(`[<-`, has_style) <- function(x, y, value) {
  .fn <- S7_class(x)
  d <- assign_data(x,y,value)
  l <- x@label
  if (length(l) > 0) {
    if (length(value@label) > 0) {
      l[y] <- value@label
    } else {
      l[y] <- ob_label(NA)
    }
    d$label <- l
  } else {
      if (length(value@label) > 0 && !is.na(value@label)) {
        l <- bind(rep(value@label, x@length))
        l@label <- rep(NA_character_, x@length)
        l@label[y] <- value@label
      }
    }
  new_x <- rlang::inject(.fn(!!!d))
  if (prop_exists(new_x, "vertex_radius")) {
    new_x@vertex_radius <- x@vertex_radius
  }
  new_x
}


shape <- new_class(name = "shape",
                   parent = has_style,
                   abstract = TRUE)
xy <- new_class(name = "xy",
                parent = shape,
                abstract = TRUE)





# generics ----

## variance ----
#' create double-headed arrow paths indicating variance
#'
#' @param x object
#' @param where angle or named direction (e.g.,northwest, east, below, left)
#' @param theta angle width
#' @param looseness distance of control points as a ratio of the distance to the object's center (e.g., in a circle of radius 1, looseness = 1.5 means that that the control points will be 1.5 units from the start and end points.)
#' @param bend Angle by which the control points are rotated
#' @inherit ob_style params
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
ob_variance <- new_generic("ob_variance", dispatch_args = "x", fun = function(
    x,
    where = "north",
    theta = 50,
    bend = 0,
    looseness = 1,
    arrow_head = arrowheadr::arrow_head_deltoid(d = 2.3, n = 100),
    resect = 2,
    ...) {
  S7_dispatch()
})


## covariance ----
#' create double-headed arrow paths indicating variance
#'
#' @param x object
#' @param y object
#' @param where exit angle
#' @param looseness distance of control points as a ratio of the distance to the object's center (e.g., in a circle of radius 1, looseness = 1.5 means that that the control points will be 1.5 units from the start and end points.)
#' @param bend Angle by which the control points are rotated
#' @inherit ob_style params
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
ob_covariance <- new_generic(
  "ob_covariance",
  dispatch_args = c("x", "y"),
  fun = function(x,
                 y,
                 where = NULL,
                 bend = 0,
                 looseness = 1,
                 arrow_head = arrowheadr::arrow_head_deltoid(d = 2.3, n = 100),
                 resect = 2,
                 ...) {
    S7_dispatch()
  }
)

## shape array ----
#' make an array of shapes along a line
#'
#' @param x shape
#' @param k number of duplicate shapes to make
#' @param sep separation distance between shapes
#' @param where angle or named direction (e.g.,northwest, east, below, left)
#' @param anchor bounding box anchor
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to shape
#' @export
ob_array <- new_generic(name = "ob_array", dispatch_args = "x", fun = function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {
  S7_dispatch()
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
bind <- new_generic(name = "bind", dispatch_args = "x")

method(bind, class_list) <- function(x, ...) {
  x <- unlist(x)
  .f <- S7_class(x[[1]])@name
  allsame <- allsameclass(x, .f)
  if (length(allsame) > 0) {
    return(bind(ob_shape_list(x)))
  }

  d <- get_non_empty_list(
    dplyr::bind_rows(
      purrr::map(
        x,
        \(o) if (S7_inherits(o, ob_style)) get_tibble(o) else o@tibble
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
                ob_point = ob_point,
                ob_rectangle = ob_rectangle,
                ob_segment = ob_segment,
                ob_style = ob_style,
                ob_polar = ob_point)
  o <- rlang::inject(.fn(!!!d))
  dots <- rlang::list2(...)
  o <- rlang::inject(S7::set_props(o, !!!dots))
  if (S7::prop_exists(x[[1]], "label") &&
      !S7_inherits(x[[1]], ob_label)) {
    x_label <- purrr::map(x, \(xx) xx@label)
    if (any(purrr::map_lgl(x_label, S7_inherits, class = ob_label))) {
      o@label <- bind(x_label)

    }
  }
  o
}


method(bind, ob_shape_list) <- function(x, ...) {
  .f <- unique(lapply(x, S7::S7_class))

  csl <- lapply(.f, \(.ff) {
    Filter(f = \(xx){
      S7_inherits(xx, .ff)
    } ,x = S7_data(x)) |>
      bind()
  })

  if (length(csl) > 1) {
    csl_names <- purrr::map_chr(csl, \(xx) S7_class(xx)@name)
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
#' @export
unbind <- S7::new_generic("unbind", dispatch_args = "x")

method(unbind, has_style) <- function(x, ...) {
  purrr::map(seq(1, x@length), \(i) x[i])
}

method(unbind, ob_shape_list) <- function(x, ...) {
  as.list(x)
}

#' map_ob
#'
#' A wrapper for purrr::map. It takes a ggdiagram object with multiple elements, applies a function to each element within the object, and returns a ggdiagram object
#' @param .x a ggdiagram object
#' @param .f a function that returns a ggdiagram object
#' @param ...arguments passed to .f
#' @param .progress display progress if TRUE
#'
#' @return a ggdiagram object
#' @export
map_ob <- function(.x, .f, ..., .progress = FALSE) {
  if (S7_inherits(.x, has_style) | S7_inherits(.x, ob_angle)) .x <- unbind(.x)
  purrr::map(.x, .f, ..., .progress = .progress) |>
    bind()
}




# class(ob_bezier(ob_point(1:3)))
## str----
str <- new_external_generic(package = "utils", name = "str", dispatch_args = "object")



## plus----
method(`+`, list(class_any, class_any)) <- function(e1, e2) {
  .Primitive("+")(e1, e2)
}

method(`+`, list(class_character, class_character)) <- function(e1, e2) {
  paste0(e1, e2)
}
method(`+`, list(class_numeric, class_character)) <- function(e1, e2) {
  paste0(e1, e2)
}
method(`+`, list(class_character, class_numeric)) <- function(e1, e2) {
  paste0(e1, e2)
}

## get_tibble----
#' Get object data with styles in a tibble
#'
#' @param x object
#' @export
get_tibble <- new_generic("get_tibble", "x", fun = function(x) {S7::S7_dispatch()})
method(get_tibble, class_list) <- function(x) {
  purrr::map_df(S7_data(x), get_tibble)
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
resect <- new_generic("resect", c("x", "distance"))


#' Get object data in a tibble, filling in any missing styles with defaults
#'
#' @param x object
#' @export
#' @rdname get_tibble
get_tibble_defaults <- new_generic("get_tibble_defaults", "x", fun = function(x) S7_dispatch())
method(get_tibble_defaults, class_any) <- function(x) {
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
nudge <- new_generic("nudge", c("object", "x", "y"))

as_arrow <- new_generic("as_arrow", c("object"))

# unions ----
class_numeric_or_character <- new_union(class_numeric, class_character)
class_numeric_or_unit <- new_union(class_numeric, class_unit)



#' Make a variant of a function with alternate defaults
#'
#' Makes a copy of a function with new defaults. Similar to `purrr::partial` except that arguments with new defaults still accept input.
#'
#' @param .f function
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> new defaults
#'
#' @return function
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
cardinalpoint <- function(x) {
  .namedpositions
  if (!all(x %in% names(.namedpositions))) {
    stop(paste0("Position must be an angle, numeric, or one of these named positions:\n", stringr::str_wrap(paste0(names(.namedpositions), collapse = ", "))))
  }
  unname(.namedpositions[x])
}

#' @keywords internal
allsameclass <- function(l, classname) {
  classname[classname == "ob_polar"] <- "ob_point"
  allsame <- all(sapply(lapply(l, class), function(x)
    classname %in% x))
  if (!allsame) {
    paste0("All items must be ", classname, ".")
  }
}

#' @keywords internal
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
get_tibble_defaults_helper <- function(
    x,
    default_style,
    required_aes = c("x", "y")) {

  d <- get_tibble(x)

  for (n in setdiff(colnames(d), required_aes)) {
    d_prop <- prop(default_style, n)
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
get_non_empty_props <- function(x) {
  Filter(function(s) {
    ifelse(length(s) > 0,
           ifelse(
             S7_inherits(s),
             length(S7_data(s)) > 0,
             !rlang::is_function(s)
           ),
           FALSE)

  }, props(x))
}

#' @keywords internal
get_non_empty_list <- function(l) {
  Filter(\(x) length(x) > 0, l)
}

#' @keywords internal
get_non_empty_tibble <- function(d) {
  d <- Filter(\(x) length(x) > 0, d)
  d <- Filter(\(x) ! is.null(x), d)
  d <- Filter(\(x) ! is.null(x), d)
  tibble::as_tibble(d)
}

#' @keywords internal
replace_na <- function(x, y) {
  ifelse(is.na(x), y, x)
}


#' @keywords internal
ob_array_helper <- function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {
  if (x@length > 1) stop("The shape must start with an object of length 1.")

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

  if (S7_inherits(x@label, ob_label)) {
    if (x@label@length == 1) {
      l <- ob_label(subscript(x@label@label, seq(k)),
                 style = x@style,
                 p = p_center)
    }

    if (x@label@length == k) {
      l <- ob_label(x@label@label,
                 style = x@style,
                 p = p_center)
    }
  }

  if (is.null(dots$label)) {
    if (length(l) > 0) {
      dots$label <- l
    }
  } else {
    if (S7_inherits(dots$label, ob_label)) {
      dots$label <- ob_label(p = p_center,
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
#'
#' @return text
#' @export
#'
#' @examples
#' subscript("X", 1:3)
#' superscript(c("A", "B"), 2)
subscript <- function(x, subscript = seq(length(x))) {
  paste0(x, "<sub>", subscript, "</sub>")
}

#' Create superscript
#'
#' @param x text
#' @param superscript superscript
#'
#' @return text
#' @rdname subscript
#' @export
superscript <- function(x, superscript = seq(length(x))) {
  paste0(x, "<sup>", superscript, "</sup>")
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
#' @return a vector of numbers converted to characters
#' @export
#'
#' @examples
#' library(ggplot2)
#' d <- data.frame(x = -4:0, y = -4:0)
#' # In these 2 plots, Compare the centering of the negative numbers on the x-axis
#' ggplot(d, aes(x,y))
#' ggplot(d, aes(x,y)) +
#'   scale_x_continuous(labels = signs_centered)
signs_centered <- function(x, space = "\u2007", encoding = "UTF-8", ...) {
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
#' @return a character vector
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
prop_integer_coerce <- function(name) {
  new_property(
    name = name,
    class = class_integer,
    setter = function(self, value) {
      if (rlang::is_integerish(value)) {
        value <- as.integer(value)
      }
      prop(self, name) <- value
      self
    }
  )
}

# prop_props <- list(point = list(
#   required = c("x", "y"),
#   style = c("alpha", "color", "fill", "shape", "size", "stroke")
# ),
# label = list(
#   required = c("p", "label"),
#   style = c(
#     "alpha",
#     "color",
#     "angle",
#     "family",
#     "fill",
#     "fontface",
#     "hjust",
#     "label.color",
#     "label.margin",
#     "label.padding",
#     "label.r",
#     "label.size",
#     "lineheight",
#     "nudge_x",
#     "nudge_y",
#     "polar_just",
#     "size",
#     "text.color",
#     "vjust"
#   )
# ))
#
# get_prop_length <- function(prop) {
#   if (S7_inherits(prop, has_style)) {
#     get_shape_length(prop)
#   } else {
#     length(prop)
#   }
# }
#
# reclass <- c(polar = "point",
#              point = "point",
#              label = "label")
#
# get_prop_n <- function(object) {
#   check_is_S7(object)
#   object_class <- S7_class(object)@name
#   object_class <- reclass[object_class]
#   required <- prop_props[[object_class]][["required"]]
#   required_ns <- `names<-`(
#     purrr::map_int(required,
#                    \(pr) get_prop_length(prop(object, pr))),
#     required)
#   styles <- prop_props[[object_class]][["style"]]
#   styles_ns <- `names<-`(
#     purrr::map_int(styles,
#                    \(pr) get_prop_length(prop(object, pr))),
#     styles)
#
#   c(required_ns, styles_ns)
#
# }
#
# get_shape_length <- function(object) {
#   max(get_prop_n(object))
# }

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)),
        substring(s, 2),
        sep = "",
        collapse = " ")
}

# check_inconsistent <- function(object) {
#   prop_n <- get_prop_n(object)
#   max_n <- max(prop_n)
#   prop_inconsistent <- prop_n[!(prop_n %in% unique(c(0, 1, max_n)))]
#   if (length(prop_inconsistent) > 0) {
#     msg <- tibble::enframe(prop_n[!(prop_n %in% unique(c(0, 1)))]) |>
#       dplyr::summarize(.by = .data$value,
#                        name = paste0(.data$name, collapse = ", ")) |>
#       dplyr::mutate(v = paste0(
#         "Size ",
#         .data$value,
#         ": Properties: ",
#         .data$name)) |>
#       dplyr::pull(.data$v) |>
#       paste0(collapse = "\n")
#     object_class <- .simpleCap(S7_class(object)@name)
#
#
#     stop(
#       paste0(
#         object_class,
#         " properties should have 0, 1, or consistently numbered elements.\nInconsistent elements:\n",
#         msg
#       )
#     )
#   }
# }

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
#' @export
#' @examples
#' library(ggplot2)
#' c1 <- ob_circle(radius = 3)
#' ggplot() +
#'   as.geom(c1, fill = "black") +
#'   coord_equal()
#'
as.geom <- new_generic("as.geom", "x")

method(as.geom, ob_shape_list) <- function(x, ...) {
  lapply(c(x), \(g) as.geom(g, ...)) |>
    unlist()
}

method(`+`, list(class_ggplot, has_style)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

method(`+`, list(class_ggplot, ob_shape_list)) <- function(e1, e2) {
  e1 + as.geom(e2)
}



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
    d$group <- seq(nrow(d))
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



  d_all <- tidyr::nest(d_nested, .by = .data$data, .key = "unmappable")

  # make geom for each row in d_nested
  purrr::pmap(d_all, \(data, unmappable) {
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
rounder <- function(x, digits = 2, add = FALSE) {
  if (add) {
    r <- paste0(ifelse(x > 0, " + ", "
      \u2212 "), trimws(formatC(
        abs(x), digits = digits, format = "fg"
      )))
  } else {
    r <- paste0(ifelse(x > 0, "", "\u2212"), trimws(formatC(
      abs(x), digits = digits, format = "fg"
    )))
  }
  r
}

# Equation ----
#' equation
#'
#' Get equation for object
#'
#' @param x object
#' @param type equation type. Can be `y`, `general`, or `parametric`
#' @param digits rounding digits
#' @export
equation <- new_generic(
  "equation",
  dispatch_args = "x",
  fun = function(x,
                 type = c("y", "general", "parametric"),
                 digits = 2) S7_dispatch())


# Projection ----
#' Find projection of a point on an object (e.g., line or segment)
#'
#' @param p ob_point
#' @param object object (e.g., line or segment)
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object
#' @export
projection <- new_generic("projection", c("p", "object"))


# midpoint----
#' Get one or more points at positions from 0 to 1
#'
#' It is possible to get more than one midpoint by specifying a position vector with a length greater than 1. Position values outside 0 and 1 will usually work, but will be outside the object.
#' @param x object
#' @param y object (can be omitted for segments and arcs)
#' @param position numeric vector. 0 is start, 1 is end. Defaults to .5
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
midpoint <- new_generic(
  "midpoint",
  c("x", "y"),
  fun = function(x, y, position = .5, ...) {
    S7::S7_dispatch()
  }
)


#' @keywords internal
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
label_object <- new_generic("label_object", "object")

#' Arrow connect one shape to another
#'
#' @param x first shape object
#' @param y second shape object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments passed to style
#' @export
connect <- new_generic("connect", c("x", "y"))

#' Place an object a specified distance from another object
#'
#' @param x shape object
#' @param from shape that x is placed in relation to
#' @param where named direction, angle, or number (degrees)
#' @param sep separation distance
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments passed to style
#' @export
place <- new_generic("place", c("x", "from"),
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
#' @param rect_linewidth line with of rectangles
#' @param theme_function ggplot2 theme
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments sent to ggplot2::theme
#'
#' @export
#'
#' @examples
#' ggdiagram() + ob_circle()
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

