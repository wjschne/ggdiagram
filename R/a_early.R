library(S7)

# geomprops

props_geom <- list(
  point = list(
    required = c("x", "y"),
    mappable_bare = character(),
    mappable_identity = c("alpha", "color", "fill", "shape", "size", "stroke"),
    not_mappable = character(),
    omit_names = "group",
    inherit.aes = FALSE
  )
)


# classes ----
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
    if (!is.numeric(self)) stop("Underlying data for units must be numeric.")
  })

class_margin <- new_class(
  name = "class_margin",
  parent = class_list,
  constructor = function(x = class_missing, units = "pt") {
    if (S7_inherits(x, class_margin)) return(x)
    if(length(x) > 0) {
    if (is.numeric(x) && !grid::is.unit(x)) {
      x <- unit(x, units = units)
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
        stop("Margins can have 1 (all sides), 2 (horiztonal vs vertical), or 4 (top right bottom left) elements.")
      }
    } else {
      stop("Margins can be of class margin, unit, or numeric")

    }
  }
    new_object(list(x))
    }
)

class_arrowhead <- new_class("class_arrowhead", class_list, constructor = function(x) {

  if (S7_inherits(x, class_arrowhead)) return(x)
  if (is.list(x)) return(purrr::map(x, class_arrowhead))
  if (!(is.numeric(x) && is.matrix(x) && ncol(x) == 2)) stop("Arrowheads must be a 2-column matrix of numbers.")

  new_object(list(x))


})


has_style <- new_class("has_style", abstract = TRUE)
shape <- new_class("shape", parent = has_style,  abstract = TRUE)
xy <- new_class("xy", parent = shape, abstract = TRUE)
class_shape_list <- new_class("class_shape_list",class_list)
c_gg <- function(...) {
  sl <- rlang::list2(...)
  sl <- Filter(f = \(s) S7_inherits(s), sl)
  class_shape_list(sl)
}

# generics ----

# str ----
#' structure
#'
#' @param object object
#' @keywords internal
str <- new_generic(
  name = "str",
  dispatch_args = "object")

#' Addition
#'
#' @param e1 object
#' @param e2 object
#' @export
`+` <- new_generic("+", c("e1", "e2"))

method(`+`, list(class_ggplot, has_style)) <- function(e1,e2) {
  e1 + as.geom(e2)
}

method(`+`, list(class_any, class_any)) <- function(e1,e2) {
  .Primitive("+")(e1,e2)
}

method(`+`, list(class_character, class_character)) <- function(e1,e2) {
  paste0(e1,e2)
}
method(`+`, list(class_numeric, class_character)) <- function(e1,e2) {
  paste0(e1,e2)
}
method(`+`, list(class_character, class_numeric)) <- function(e1,e2) {
  paste0(e1,e2)
}




#' Get object data with styles in a tibble
#'
#' @param x object
#' @export
get_tibble <- new_generic("get_tibble", "x")
method(get_tibble, class_list) <- function(x) {
  purrr::map_df(S7_data(x), get_tibble)
}

#' Get object data with styles in a tibble
#'
#' @param x object
#' @export
bind_shape <- new_generic("bind_shape", "x")
method(bind_shape, class_list) <- function(x) {
  d <- purrr::map_df(x, get_tibble)
  d <- as.list(d)

  o <- x[[1]]
  o <- rlang::inject(set_props(o, !!!d))

  if (prop_exists(o, "arrow_head") && length(o@arrow_head) > 0) {
    o@arrow_head <- class_arrowhead(o@arrow_head)
  }
  o
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
#' @export
resect <- new_generic("resect", c("x", "distance"))


#' Get object data in a tibble, filling in any missing styles with defaults
#'
#' @param x object
#' @export
#' @rdname get_tibble
get_tibble_defaults <- new_generic("get_tibble_defaults", "x")
method(get_tibble_defaults, class_any) <- function(x) {
  get_tibble(x)
}


#' Make an automatic label
#'
#' @param object object
#' @param label character label
#' @export
#' @rdname label
labeler <- new_generic("labeler", c("object", "label"))

#' Make an automatic label
#'
#' @param object object
#' @param x nudge right and left
#' @param y nudge up and down
#' @export
nudge <- new_generic("nudge", c("object", "x", "y"))

as_arrow <- new_generic("as_arrow", c("object"))

# unions ----
class_numeric_or_character <- new_union(class_numeric, class_character)
class_numeric_or_unit <- new_union(class_numeric, class_unit)



# internal states ----
the <- new.env(parent = emptyenv())
# the$point <- c("alpha", "color", "fill", "shape", "size", "stroke")
# the$line <- c("alpha", "color", "stroke", "lineend", "linejoin", "linetype", "linewidth")
# the$polygon <- c("alpha", "color", "fill", "linetype", "linewidth")
# the$text <- c("alpha", "angle", "color", "family", "fontface", "hjust", "size", "size.unit", "vjust")
# the$richtext <- c("alpha", "angle", "color", "family", "fontface", "hjust", "size", "vjust", "nudge_x", "nudge_y", "label.color", "label.padding", "label.margin", "label.r", "label.size", "lineheight")
the$arrow_head <- arrowheadr::arrow_head_deltoid()


# helpers ----

#' @keywords internal
allsameclass <- function(l, classname) {
  allsame <- all(sapply(lapply(l, class),
                        function(x)  classname %in% x))
  if (!allsame) {
    paste0("All items must be ", classname, ".")
  }
}

#' @keywords internal
aes_injection <- function(bare_mapping, identity_mapping, omit_mapping = NULL) {
  identity_mapping <- setdiff(identity_mapping, c(bare_mapping, omit_mapping))
  bare_mapping <- setdiff(bare_mapping, omit_mapping)
  i_styles <- purrr::map(
    rlang::syms(identity_mapping),
    \(i) call("I", i))
  names(i_styles) <- identity_mapping
  b_styles <- rlang::syms(bare_mapping)
  names(b_styles) <- bare_mapping

  ggplot2::aes(!!!b_styles, !!!i_styles)

}



#' @keywords internal
get_tibble_defaults_helper <- function(x, default_style, required_aes = c("x", "y")) {
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
             !rlang::is_function(s)),
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
  d <- Filter(\(x) !is.null(x), d)
  d <- Filter(\(x) !is.null(x), d)
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
  if (is.null(digits)
  ) {
    l <- scales::number(p,
                        accuracy = accuracy)
  }
  else {
    sig_digits <- abs(ceiling(log10(p + p / 1e+09)) - digits)
    pgt99 <- p > 0.99
    sig_digits[pgt99] <- abs(ceiling(log10(1 - p[pgt99])) - digits + 1)

    sig_digits[ceiling(log10(p)) == log10(p) & (-log10(p) >= digits)] <-
      sig_digits[ceiling(log10(p)) == log10(p) & (-log10(p) >= digits)] - 1

    sig_digits[is.infinite(sig_digits)] <- 0

    l <- purrr::map2_chr(p, sig_digits, formatC, format = "f", flag = "#")
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
      l[round(p, digits = max_digits) == 0] <- paste0(".", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == 1] <- paste0("1.", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == -1] <- paste0("-1.", paste0(rep("0", max_digits), collapse = ""))
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

prop_props <- list(
  point = list(required = c("x", "y"),
               style = c("alpha",
                         "color",
                         "fill",
                         "shape",
                         "size",
                         "stroke")),
  label = list(required = c("p", "label"),
               style = c("alpha",
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
                         "vjust"))
)

get_prop_length <- function(prop) {
  if (S7_inherits(prop, has_style)) {
    get_shape_length(prop)
  } else {
    length(prop)
  }
}

reclass <- c(polar = "point", point = "point", label = "label")

get_prop_n <- function(object) {
  check_is_S7(object)
  object_class <- S7_class(object)@name
  object_class <- reclass[object_class]
  required <- prop_props[[object_class]][["required"]]
  required_ns <- `names<-`(purrr::map_int(required, \(pr) get_prop_length(prop(object, pr))), required)
  styles <- prop_props[[object_class]][["style"]]
  styles_ns <- `names<-`(purrr::map_int(styles, \(pr) get_prop_length(prop(object, pr))), styles)
  c(required_ns, styles_ns)

  # Filter(x = required_ns, f = \(i) !(i %in% c(1, max(required_ns))))
  # unique(required_ns)
}

get_shape_length <- function(object) {
  max(get_prop_n(object))
  }

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

check_inconsistent <- function(object) {
  prop_n <- get_prop_n(object)
  max_n <- max(prop_n)
  prop_inconsistent <- prop_n[!(prop_n %in% unique(c(0,1, max_n)))]
  if (length(prop_inconsistent) > 0) {
    msg <- tibble::enframe(prop_n[!(prop_n %in% unique(c(0,1)))]) %>%
      dplyr::summarize(.by = value, name = paste0(name, collapse = ", ")) %>%
      dplyr::mutate(v = paste0("Size ", value, ": Properties: ", name)) %>%
      dplyr::pull(v) %>%
      paste0(collapse = "\n")
    object_class <- .simpleCap(S7_class(object)@name)


    stop(paste0(object_class, " properties should have 0, 1, or consistently numbered elements.\nInconsistent elements:\n", msg))
    }
}

# as.geom ----
#' Convert shapes to ggplot2 geoms
#'
#' @param x a shape
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Pass arguments to ggplot2::geom_point
#' @rdname as.geom
#' @export
as.geom <- new_generic("as.geom", "x")

method(as.geom, class_shape_list) <- function(x, ...) {
  c(lapply(x, \(g) as.geom(g, ...)[[1]]))
  }

method(`+`, list(class_ggplot, class_shape_list)) <- function(e1,e2) {

  e1 + as.geom(e2)
}


#' @keywords internal
make_geom_helper <- function(
    d = NULL,
    .geom_x,
    user_overrides,
    not_mappable = character(),
    required_aes = character(),
    mappable_bare = character(),
    mappable_identity = character(),
    omit_names = character(),
    ...
) {
  not_mappable = c("lineend", "linejoin", "arrow_head",
                   'arrow_fins', "length","length_head",
                   "length_fins", "length_mid", "resect",
                   "resect_fins", "resect_head", "linemitre")
  required_aes = c("x", "y", "xend", "yend")
  omit_names = c("linejoin", "rule", "group")
  mappable_bare = ""
d <- get_tibble_defaults(resect(segment(point(), point(1,1)), unit(1,"pt")))



  # add group so that I() function will not disturb drawing order
  if (!("group" %in% unique(c(omit_names,colnames(d))))) {
    d$group <- seq(nrow(d))
  }
  # 1 row per unique combination of not mappable arguments
  d_nested <- tidyr::nest(d, .by = any_of(not_mappable))


  # all colnames  but data
  not_mapped_names <- colnames(d_nested)[colnames(d_nested) != "data"]
  d_isunit <- purrr::map_lgl(d_nested, grid::is.unit)
  d_unit_names <- names(d_isunit[d_isunit])
  d_unit_types <- purrr::map_chr(d_nested[, d_unit_names], grid::unitType)

  d_nested <- mutate(d_nested, across(all_of(d_unit_names), .fns = as.numeric))



  d_all <- tidyr::nest(d_nested, .by = data, .key = "unmappable")
  d_all %>%
    mutate(unmapable  = purrr::map(\(.d) {
      .d %>% mutate(across(all_of(d_unit_names), .fns = \(.sx) {
        print(names(.sx))
      }))}))

  d_all$unmappable[[1]]
  }))

for (r in nrow(d_all)) {
  d_all[r, "unmappable"]
}
d_all
d_all$unmappable[[1]]





  print(dplyr::pull(d_all, unmappable)[[1]] )



  # make geom for each row in d_nested
  purrr::pmap(d_all, \(data, unmappable) {


    # make list of not mapped arguments
    not_mapped <- as.list(unmappable)

    not_mapped <- purrr::map2(not_mapped, names(not_mapped), \(x, name) {
      if (name %in% c("label.margin", "label.padding")) {x <- x[[1]]}
      x})





    # get mappable names
    data_names <- colnames(data)



    # get names for bare mapping
    bare_mapping <- intersect(unique(c(required_aes, mappable_bare)), data_names)

    # omitted arguments
    omit_mapping <- unique(c(omit_names, names(user_overrides), not_mapped_names))

    # gename names for identity mapping
    identity_mapping <- setdiff(data_names, unique(c(bare_mapping, omit_mapping)))



    myaes <- aes_injection(
      bare_mapping = bare_mapping,
      identity_mapping = identity_mapping,
      omit_mapping = omit_mapping
    )



    rlang::inject(.geom_x(
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
  notabs <- gsub(x = x, pattern = "\\t", replacement = " ")
  trimws(gsub(x = notabs, pattern = "\\s+", replacement = " "))
}

#' @keywords internal
rounder <- function(x, digits = 2, add = FALSE) {
  if (add) {
    r <- paste0(ifelse(
      x > 0,
      " + ", "
      \u2212 "),
      trimws(formatC(abs(x),
             digits = digits,
             format = "fg")))
  } else {
    r <- paste0(
      ifelse(
        x > 0,
        "",
        "\u2212"),
      trimws(formatC(abs(x),
              digits = digits,
              format = "fg")))
  }
  r
}

# Equation ----
#' equation
#'
#' @param x object
#' @param type equation type. Can be `y`, `general`, or `parametric`
#' @export
equation <- new_generic("equation", dispatch_args = "x")


# Projection ----
#' Find projection of a point on an object (e.g., line or segment)
#'
#' @param point point
#' @param object object (e.g., line or segment)
#' @export
projection <- new_generic("projection", c("point", "object"))


# midpoint----
#' Get one or more points at positions from 0 to 1
#'
#' It is possible to get more than one midpoint by specifying a position vector with a length greater than 1. Position values outside 0 and 1 will usually work, but will be outside the object.
#' @param x object
#' @param y object (can be omitted for segments and arcs)
#' @param position numeric vector. 0 is start, 1 is end. Defaults to .5
#' @export
midpoint <- new_generic("midpoint", c("x", "y"), fun = function(x,y, position = .5, ...) {
  S7::S7_dispatch()
})


#' @keywords internal
rotate2columnmatrix <- function(x, theta) {
  x_rotated <- x %*%  matrix(c(cos(theta),
                               -sin(theta),
                               sin(theta),
                               cos(theta)),
                             nrow = 2, ncol = 2)
  colnames(x_rotated) <- colnames(x)
  x_rotated
}
