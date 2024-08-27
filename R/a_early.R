library(S7)

do_nothing <- function(x) {
  if (FALSE) {
    p1 <- ggforce::geom_circle()
    p2 <- arrowheadr::arrow_head_deltoid()
    p3 <- geomtextpath::geom_labelcurve()
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


# classes ----
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

class_ggplot <- new_S3_class("ggplot")
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


has_style <- new_class(name = "has_style", abstract = TRUE)
method(print, has_style) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}




shape <- new_class(name = "shape",
                   parent = has_style,
                   abstract = TRUE)
xy <- new_class(name = "xy",
                parent = shape,
                abstract = TRUE)
class_shape_list <- new_class("class_shape_list", class_list)
c_gg <- function(...) {
  sl <- rlang::list2(...)
  sl <- Filter(f = \(s) S7_inherits(s), sl)
  class_shape_list(sl)
}



# generics ----

#' bind method
#' @param x list of objects to bind
#' @param ... Arguments passed to style
#' @examples
#' bind(c(point(1,2), point(3,4)))
#' bind(c(circle(point(0,0), radius = 1),
#'        circle(point(1,1), radius = 2)))
#' @export
bind <- new_generic(name = "bind", dispatch_args = "x")

method(bind, class_list) <- function(x, ...) {
  .f <- S7_class(x[[1]])@name
  allsame <- allsameclass(x, .f)
  if (length(allsame) > 0) stop(allsame)
  d <- get_non_empty_list(dplyr::bind_rows(purrr::map(x, \(o) o@tibble)))
  .fn <- switch(.f,
                arc = arc,
                bzcurve = bzcurve,
                circle = circle,
                ellipse = ellipse,
                label= label,
                line = line,
                point = point,
                rectangle = rectangle,
                segment = segment,
                style = style)
  o <- rlang::inject(.fn(!!!d))
  dots <- rlang::list2(...)
  rlang::inject(S7::set_props(o, !!!dots))
}



str <- new_external_generic(package = "utils", name = "str", dispatch_args = "object")


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


#' Get object data with styles in a tibble
#'
#' @param x object
#' @export
get_tibble <- new_generic("get_tibble", "x", fun = function(x) {S7::S7_dispatch()})
method(get_tibble, class_list) <- function(x) {
  purrr::map_df(S7_data(x), get_tibble)
}

#' Get points for making points
#'
#' @param x object
#' @keywords internal
get_points <- new_generic("get_points", "x")

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
#' circle() |> nudge(x = 2)
#' # Alternative to nudge:
#' circle() + point(2, 0)
#' @export
nudge <- new_generic("nudge", c("object", "x", "y"))

as_arrow <- new_generic("as_arrow", c("object"))

# unions ----
class_numeric_or_character <- new_union(class_numeric, class_character)
class_numeric_or_unit <- new_union(class_numeric, class_unit)



# helpers ----

.namedpositions <- c(
  east = 0,
  `east-northeast` = 22.5,
  northeast = 45,
  `north-northeast` = 67.5,
  north = 90,
  `north-northwest` = 112.5,
  northwest = 135,
  `west-northwest` = 157.5,
  west = 180,
  `west-southwest` = 202.5,
  southwest = 225,
  `south-southwest` = 247.5,
  south = 270,
  `south-southeast` = 292.5,
  southeast = 315,
  `east-southeast` = 337.5,
  right = 0,
  `top right` = 45,
  top = 90,
  `top left` = 135,
  left = 180,
  `bottom left` = 215,
  bottom = 270,
  `bottom right` = 315
)

#' @keywords internal
cardinalpoint <- function(x) {
  .namedpositions
  if (!all(x %in% names(.namedpositions))) {
    stop("Position must be an angle, numeric, or a one of cardinal points:\nnoorth, east, south, west, northeast, northwest, southwest, southeast, east-northest, north-northeast, north-northwest, west-northwest, west-southwest, south-southwest, south-southeast, east-southeast")
  }
  unname(.namedpositions[x])
}

#' @keywords internal
allsameclass <- function(l, classname) {
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
#'
#' @return a character vector
#' @export
#' @examples
#' round_probability(c(0, .0012, .012, .12, .99, .992, .9997, 1), digits = 2)
round_probability <- function(p,
                              accuracy = 0.01,
                              digits = NULL,
                              max_digits = NULL,
                              remove_leading_zero = TRUE,
                              round_zero_one = TRUE) {
  if (is.null(digits)) {
    l <- scales::number(p, accuracy = accuracy)
  }
  else {
    sig_digits <- abs(ceiling(log10(p + p / 1e+09)) - digits)
    pgt99 <- p > 0.99
    sig_digits[pgt99] <- abs(ceiling(log10(1 - p[pgt99])) - digits + 1)

    sig_digits[ceiling(log10(p)) == log10(p) &
                 (-log10(p) >= digits)] <-
      sig_digits[ceiling(log10(p)) == log10(p) &
                   (-log10(p) >= digits)] - 1

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

prop_props <- list(point = list(
  required = c("x", "y"),
  style = c("alpha", "color", "fill", "shape", "size", "stroke")
),
label = list(
  required = c("p", "label"),
  style = c(
    "alpha",
    "color",
    "angle",
    "family",
    "fill",
    "fontface",
    "hjust",
    "label.color",
    "label.margin",
    "label.padding",
    "label.r",
    "label.size",
    "lineheight",
    "nudge_x",
    "nudge_y",
    "polar_just",
    "size",
    "text.color",
    "vjust"
  )
))

get_prop_length <- function(prop) {
  if (S7_inherits(prop, has_style)) {
    get_shape_length(prop)
  } else {
    length(prop)
  }
}

reclass <- c(polar = "point",
             point = "point",
             label = "label")

get_prop_n <- function(object) {
  check_is_S7(object)
  object_class <- S7_class(object)@name
  object_class <- reclass[object_class]
  required <- prop_props[[object_class]][["required"]]
  required_ns <- `names<-`(
    purrr::map_int(required,
                   \(pr) get_prop_length(prop(object, pr))),
    required)
  styles <- prop_props[[object_class]][["style"]]
  styles_ns <- `names<-`(
    purrr::map_int(styles,
                   \(pr) get_prop_length(prop(object, pr))),
    styles)

  c(required_ns, styles_ns)

}

get_shape_length <- function(object) {
  max(get_prop_n(object))
}

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)),
        substring(s, 2),
        sep = "",
        collapse = " ")
}

check_inconsistent <- function(object) {
  prop_n <- get_prop_n(object)
  max_n <- max(prop_n)
  prop_inconsistent <- prop_n[!(prop_n %in% unique(c(0, 1, max_n)))]
  if (length(prop_inconsistent) > 0) {
    msg <- tibble::enframe(prop_n[!(prop_n %in% unique(c(0, 1)))]) |>
      dplyr::summarize(.by = .data$value,
                       name = paste0(.data$name, collapse = ", ")) |>
      dplyr::mutate(v = paste0(
        "Size ",
        .data$value,
        ": Properties: ",
        .data$name)) |>
      dplyr::pull(.data$v) |>
      paste0(collapse = "\n")
    object_class <- .simpleCap(S7_class(object)@name)


    stop(
      paste0(
        object_class,
        " properties should have 0, 1, or consistently numbered elements.\nInconsistent elements:\n",
        msg
      )
    )
  }
}

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
#' c1 <- circle(radius = 3)
#' ggplot() +
#'   as.geom(c1, fill = "black") +
#'   coord_equal()
#'
as.geom <- new_generic("as.geom", "x")

method(as.geom, class_shape_list) <- function(x, ...) {
  c(lapply(x, \(g) as.geom(g, ...)[[1]]))
}

method(`+`, list(class_ggplot, has_style)) <- function(e1, e2) {
  e1 + as.geom(e2)
}


method(`+`, list(class_ggplot, class_shape_list)) <- function(e1, e2) {
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
        names(style@properties),
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
#' @param p point
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
#' @param ... additional arguments
label_object <- new_generic("label_object", "object")

#' Arrow connect one shape to another
#'
#' @param x first shape (e.g., point, circle, ellipse, rectangle)
#' @param y second shape
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments passed to style
#' @export
connect <- new_generic("connect", c("x", "y"))

#' Place an object a specified distance from another object
#'
#' @param x shape (e.g., point, circle, ellipse, rectangle)
#' @param from shape that x is placed in relation to
#' @param where named direction, angle, or number (degrees)
#' @param sep separation distance
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments passed to style
#' @export
place <- new_generic("place", c("x", "from"),
                     fun = function(x, from, where = "right", sep = 1, ...) {
                       S7::S7_dispatch()
                     })

method(as.list, shape) <- function(x, ...) {
  purrr::map(seq(1, x@length), \(i) x[i])
}
