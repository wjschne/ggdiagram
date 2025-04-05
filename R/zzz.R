.onLoad <- function(...) {
  suppressMessages(S7::methods_register())
}


# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

utils::globalVariables(c("properties"))


#' @export
`[<-.has_style` <- function(x, i, value) {
  .fn <- S7::S7_class(x)
  d <- assign_data(x, i, value)
  l <- x@label
  if (length(l) > 0) {
    if (length(value@label) > 0) {
      l[i] <- value@label
    } else {
      l[i] <- ob_label(NA)
    }
    d$label <- l
  } else {
    if (length(value@label) > 0 && !is.na(value@label)) {
      l <- bind(rep(value@label, x@length))
      l@label <- rep(NA_character_, x@length)
      l@label[i] <- value@label
    }
  }
  new_x <- rlang::inject(.fn(!!!d))
  if (S7::prop_exists(new_x, "vertex_radius")) {
    new_x@vertex_radius <- x@vertex_radius
  }
  new_x
}

#' @export
`[<-.ggdiagram::has_style` <- function(x, i, value) {
  .fn <- S7::S7_class(x)
  d <- assign_data(x, i, value)
  l <- x@label
  if (length(l) > 0) {
    if (length(value@label) > 0) {
      l[i] <- value@label
    } else {
      l[i] <- ob_label(NA)
    }
    d$label <- l
  } else {
    if (length(value@label) > 0 && !is.na(value@label)) {
      l <- bind(rep(value@label, x@length))
      l@label <- rep(NA_character_, x@length)
      l@label[i] <- value@label
    }
  }
  new_x <- rlang::inject(.fn(!!!d))
  if (S7::prop_exists(new_x, "vertex_radius")) {
    new_x@vertex_radius <- x@vertex_radius
  }
  new_x
}

#' @export
`[<-.ggdiagram::ob_angle` <- function(x, i, value) {
  d <- c(x)
  d[i] <- c(value)
  S7::S7_data(x) <-  d
  x
}

#' @export
`[<-.ggdiagram::ob_label` <- function(x, i, value) {
  dx <- x@tibble
  dv <- value@tibble
  dx[i, ] <- value@tibble
  d <- as.list(dx)
  rlang::inject(ob_label(!!!d))
}

#' @export
`[<-.ggdiagram::ob_point` <- function(x, i, value) {
  d <- assign_data(x,i,value)
  rlang::inject(ob_point(!!!d))
}
