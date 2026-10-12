# generics ----

## variance ----
#' Variance object
#'
#' @description
#' Creates double-headed arrow paths indicating variance
#'
#' # Methods
#' `ob_variance` is an S7 generic with methods available for the following classes:
#'
#' `r doclisting::methods_list("ob_variance", "ggdiagram")`
#'
#' @param x object of type `r doclisting::methods_inline("ob_variance", "ggdiagram")`
#' @param where Location on object. Can be numeric (degrees), [degree], [radian], [turn], or named direction (e.g., "northwest", "east", "below", "left")
#' @param theta angle width
#' @param looseness distance of control points as a ratio of the distance to the object's center (e.g., in a circle of radius 1, looseness = 1.5 means that that the control points will be 1.5 units from the start and end points.)
#' @param bend Angle by which the control points are rotated. Can be numeric (degrees), [degree], [radian], [turn], or named direction (e.g., "northwest", "east", "below", "left"). Defaults to 0.
#' @inherit ob_style params
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @returns Returns an object of type [`ob_bezier`]
#' @export
ob_variance <- S7::new_generic(
  "ob_variance",
  dispatch_args = "x",
  fun = function(
    x,
    where = "north",
    theta = 50,
    bend = 0,
    looseness = 1,
    arrow_head = the$arrow_head,
    arrow_fins = the$arrow_head,
    resect = 2,
    ...
  ) {
    S7::S7_dispatch()
  }
)


## covariance ----
#' Covariance object
#'
#' @description Creates double-headed arrow paths indicating variance
#'
#' # Methods
#' `ob_covariance` is an S7 generic with methods available for the following classes:
#'
#' `r doclisting::methods_list("ob_covariance", "ggdiagram")`
#'
#' @param x object of type `r doclisting::methods_inline("ob_covariance", "ggdiagram")`
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
  fun = function(
    x,
    y,
    where = NULL,
    bend = 0,
    looseness = 1,
    arrow_head = the$arrow_head,
    length_head = 7,
    length_fins = 7,
    resect = 2,
    ...
  ) {
    S7::S7_dispatch()
  }
)

## ob_array ----
#' Object Arrays
#'
#' @description
#' Make an array of shapes along a line
#'
#' # Methods
#' `ob_array` is an S7 generic with methods available for the following classes:
#'
#' `r doclisting::methods_list("ob_array", "ggdiagram")`
#' @param x shape
#' @param k number of duplicate shapes to make
#' @param sep separation distance between shapes
#' @param where angle or named direction (e.g.,northwest, east, below, left)
#' @param anchor bounding box anchor
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to shape
#' @returns An array of shapes of the same class as object passed to x
#' @export
ob_array <- S7::new_generic(
  name = "ob_array",
  dispatch_args = "x",
  fun = function(x, k = 2, sep = 1, where = "east", anchor = "center", ...) {
    S7::S7_dispatch()
  }
)

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

S7::method(bind, shape) <- function(x, ...) {
  x
}

S7::method(bind, S7::class_list) <- function(x, ...) {
  all_angles <- all(purrr::map_lgl(x, S7::S7_inherits, class = ob_angle))
  if (all_angles) {
    if (length(x) == 0) {
      return(degree()) # nocov
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
      )
    )
  )

  .fn <- switch(
    .f,
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
    ob_polygon = ob_polygon,
    ob_rectangle = ob_rectangle,
    ob_reuleaux = ob_reuleaux,
    ob_segment = ob_segment,
    ob_style = ob_style,
    ob_polar = ob_point
  )
  o <- rlang::inject(.fn(!!!d))
  dots <- rlang::list2(...)
  o <- rlang::inject(S7::set_props(o, !!!dots))
  if (
    S7::prop_exists(x[[1]], "label") &&
    !S7::S7_inherits(x[[1]], ob_label)
  ) {
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
  if (nrow(d) == 0) o[0] else o
}


S7::method(bind, ob_shape_list) <- function(x, ...) {
  .f <- unique(lapply(x, S7::S7_class))

  csl <- lapply(.f, \(.ff) {
    Filter(
      f = \(xx) {
        S7::S7_inherits(xx, .ff)
      },
      x = S7::S7_data(x)
    ) |>
      bind()
  })

  # Remove length 0
  csl[names(Filter(function(o) length(o) == 0, csl))] <- NULL

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
  if (x@length == 0) {
    return(c(x))
  }
  purrr::map(seq_len(x@length), \(i) x[i])
}

S7::method(unbind, ob_shape_list) <- function(x) {
  as.list(x)
}


#' Map over a ggdiagram object
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
  if (S7::S7_inherits(.x, has_style) || S7::S7_inherits(.x, ob_angle)) {
    .x <- unbind(.x)
  }
  if (S7::S7_inherits(.x, ob_shape_list)) {
    .x <- c(.x)
  }
  purrr::map(.x, .f, ..., .progress = .progress) |>
    bind()
}

#' Map over two ggdiagram objects
#'
#' A wrapper for [purrr::map2]. It takes two ggdiagram objects with multiple elements, applies a function to each element within the objects, and returns a ggdiagram object
#' @param .x a ggdiagram object
#' @param .y a ggdiagram object
#' @param .f a function that returns a ggdiagram object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to .f
#' @param .progress display progress if TRUE
#'
#' @returns a ggdiagram object
#' @export
map2_ob <- function(.x, .y, .f, ..., .progress = FALSE) {
  if (S7::S7_inherits(.x, has_style) || S7::S7_inherits(.x, ob_angle)) {
    .x <- unbind(.x)
  }
  if (S7::S7_inherits(.y, has_style) || S7::S7_inherits(.y, ob_angle)) {
    .y <- unbind(.y)
  }

  if (S7::S7_inherits(.x, ob_shape_list)) {
    .x <- c(.x)
  }

  if (S7::S7_inherits(.y, ob_shape_list)) {
    .y <- c(.y)
  }
  purrr::map2(.x, .y, .f, ..., .progress = .progress) |>
    bind()
}


## str----
str <- S7::new_external_generic(
  package = "utils",
  name = "str",
  dispatch_args = "object"
)


## plus----
S7::method(`+`, list(S7::class_any, S7::class_any)) <- function(e1, e2) {
  .Primitive("+")(e1, e2) # nocov
}

S7::method(`+`, list(S7::class_character, S7::class_character)) <- function(
    e1,
    e2
) {
  paste0(e1, e2) # nocov
}
S7::method(`+`, list(S7::class_numeric, S7::class_character)) <- function(
    e1,
    e2
) {
  paste0(e1, e2) # nocov
}
S7::method(`+`, list(S7::class_character, S7::class_numeric)) <- function(
    e1,
    e2
) {
  paste0(e1, e2) # nocov
}

## get_tibble----
#' Get object data with styles in a tibble
#'
#' @param x object
#' @export
#' @returns a [tibble::tibble]
get_tibble <- S7::new_generic("get_tibble", "x", fun = function(x) {
  S7::S7_dispatch()
})
S7::method(get_tibble, S7::class_list) <- function(x) {
  purrr::map_df(x, get_tibble)
}

S7::method(get_tibble, ob_shape_list) <- function(x) {
  purrr::map(S7::S7_data(x), get_tibble)
}


## Resect ----
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
get_tibble_defaults <- S7::new_generic(
  "get_tibble_defaults",
  "x",
  fun = function(x) S7::S7_dispatch()
)
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
class_numeric_or_character <- S7::new_union(
  S7::class_numeric,
  S7::class_character
)
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




## as.geom ----
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
  e1 + as.geom(e2) # nocov
}

S7::method(`+`, list(class_gg, ob_shape_list)) <- function(e1, e2) {
  e1 + as.geom(e2) # nocov
}

S7::method(update_ggplot, list(has_style, class_ggplot)) <-
  function(object, plot, ...) {
    plot + as.geom(object) # nocov
  }

#' @noRd
#' @keywords internal
make_geom_helper <- function(d = NULL, aesthetics, user_overrides, ...) {
  omit_names <- unique(
    c(
      aesthetics@omit_names,
      setdiff(
        names(ob_style@properties),
        unique(c(
          aesthetics@mappable_bare,
          aesthetics@required_aes,
          aesthetics@not_mappable,
          aesthetics@mappable_identity
        ))
      )
    )
  )

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

  d_nested <- dplyr::mutate(
    d_nested,
    dplyr::across(dplyr::all_of(d_unit_names), .fns = as.numeric)
  )

  d_all <- tidyr::nest(
    d_nested,
    .by = dplyr::all_of("data"),
    .key = "unmappable"
  )

  # make geom for each row in d_nested
  purrr::pmap(d_all, \(data, unmappable) {
    if ("p_unnest" %in% colnames(data)) {
      data <- tidyr::unnest(data = data, "p_unnest")
    }

    # make list of not mapped arguments
    not_mapped <- as.list(unmappable)

    not_mapped <- purrr::map2(not_mapped, names(not_mapped), \(x, name) {
      if (name %in% c("label.margin", "label.padding", "gradient")) {
        x <- x[[1]]
      }
      x
    })

    not_mapped <- purrr::map2(not_mapped, names(not_mapped), \(x, name) {
      if (name %in% d_unit_names) {
        x <- grid::unit(x, d_unit_types[name]) # nocov
      }
      x
    })

    # get mappable names
    data_names <- colnames(data)

    # get names for bare mapping
    bare_mapping <- intersect(
      unique(c(aesthetics@required_aes, aesthetics@mappable_bare)),
      data_names
    )

    # omitted arguments
    omit_mapping <- unique(c(
      omit_names,
      names(user_overrides),
      not_mapped_names
    ))

    # gename names for identity mapping
    identity_mapping <- setdiff(
      data_names,
      unique(c(bare_mapping, omit_mapping))
    )

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
  notabs <- gsub(x = x, pattern = "\\t", replacement = " ")
  trimws(gsub(
    x = notabs,
    pattern = "\\s+",
    replacement = " "
  ))
}

#' @keywords internal
#' @noRd
rounder <- function(
    x,
    digits = 2,
    add = FALSE,
    output = c("markdown", "latex")
) {
  output <- match.arg(output)
  minus <- ifelse(output == "markdown", "\u2212", "-")
  if (add) {
    r <- paste0(
      ifelse(x > 0, " + ", paste0(" ", minus, " ")),
      trimws(formatC(
        abs(x),
        digits = digits,
        format = "fg"
      ))
    )
  } else {
    r <- paste0(
      ifelse(x > 0, "", minus),
      trimws(formatC(
        abs(x),
        digits = digits,
        format = "fg"
      ))
    )
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

## Equation ----
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
#' equation(c1)
#' equation(l1)
equation <- S7::new_generic(
  "equation",
  dispatch_args = "x",
  fun = function(
    x,
    type = c("y", "general", "parametric"),
    output = c("markdown", "latex"),
    digits = 2
  ) {
    S7::S7_dispatch()
  }
)


# Projection ----
#' Find projection of a point on an object (e.g., line or segment)
#'
#' @param p ob_point
#' @param object object (e.g., line or segment)
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style object
#' @export
#' @returns ob_point
projection <- S7::new_generic("projection", c("p", "object"))


## midpoint----
#' Get one or more points at positions from 0 to 1
#'
#' It is possible to get more than one midpoint by specifying a position vector with a length greater than 1. Position values outside 0 and 1 will usually work, but will be outside the object.
#' @param x object
#' @param y object (can be omitted for segments and arcs)
#' @param position numeric vector. 0 is start, 1 is end. Defaults to 0.5
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
#' @returns ob_point
midpoint <- S7::new_generic(
  "midpoint",
  c("x", "y"),
  fun = function(x, y, position = 0.5, ...) {
    S7::S7_dispatch()
  }
)


#' @keywords internal
#' @noRd
rotate2columnmatrix <- function(x, theta) {
  if (all(theta == 0)) {
    return(x)
  }
  x_rotated <- x %*%
    matrix(
      c(cos(theta), -sin(theta), sin(theta), cos(theta)),
      nrow = 2,
      ncol = 2
    )
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
  function(
    from,
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
    id = character(0)
  ) {
    S7::S7_dispatch()
  }
)


#' Place an object a specified distance from another object
#'
#' @param x shape object
#' @param from shape that x is placed in relation to
#' @param where named direction, angle, or number (degrees)
#' @param sep separation distance
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments passed to [ob_style]
#' @export
#' @returns object of same class as `x`
place <- S7::new_generic(
  name = "place",
  c("x", "from"),
  fun = function(x, from, where = "right", sep = 1, ...) {
    S7::S7_dispatch()
  }
)



