library(S7)


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
class_margin <- new_S3_class("margin")
shape <- new_class("shape", abstract = TRUE)
shape_list <- new_class("shape_list", parent = class_list)
xy <- new_class("xy", parent = shape, abstract = TRUE)

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

method(`+`, list(class_ggplot, new_union(shape, shape_list))) <- function(e1,e2) {
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

#' Get points for making points
#'
#' @param x object
#' @keywords internal
get_points <- new_generic("get_points", "x")




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
             TRUE),
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
