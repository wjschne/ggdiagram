

# from S7
obj_type <- function(x) {
  if (identical(x, quote(expr = ))) {
    "missing"
  }
  else if (inherits(x, "S7_object")) {
    "S7"
  }
  else if (isS4(x)) {
    "S4"
  }
  else if (is.object(x)) {
    "S3"
  }
  else {
    "base"
  }
}

obj_desc <- function(x) {
  switch(
    obj_type(x),
    missing = "MISSING",
    base = paste0("<", typeof(x), ">"),
    S3 = paste0("S3<", paste(class(x), collapse = "/"), ">"),
    S4 = paste0("S4<", class(x), ">"),
    S7 = paste0("<", class(x)[[1]], ">")
  )
}

# modified from S7 internals
str_nest <- function(object,
                     prefix,
                     nest.lev = 0,
                     omit,
                     indent.str = paste(
                       rep.int(" ",
                               max(0, nest.lev)),
                       collapse = ".."
                     ),
                     ...
) {
  pnames <- format(names(object))
  for (i in seq_along(object)) {
    cat(indent.str, prefix, " ", pnames[[i]], ":", sep = "")
    xi <- object[[i]]

    if (is.function(xi)) {
      S7:::str_function(xi, nest.lev = nest.lev + 1)
    } else if (S7::S7_inherits(xi)) {
      str(xi, ..., nest.lev = nest.lev + 1)
    } else {
      utils::str(xi, ...)
    }

  }
}

omit_props <- function(object, include = character(0), omit = character(0)) {
  if (length(include) > 0) {
    setdiff(
      names(S7_class(object)@properties),
      include)
  } else if (length(omit) > 0) {
    intersect(
      omit,
      names(S7_class(object)@properties)
    )
  } else {
    character(0)
  }

}

str_properties <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit) {
  p_names <- prop_names(object)
  cat(if (nest.lev > 0) " ")
  cat(S7:::obj_desc(object))

  cat("\n")

  str_nest(object = props(object)[!(p_names %in% omit)],
           prefix = "@",
           nest.lev = nest.lev)
  if (length(omit) > 0 && additional && nest.lev == 0) {
    additional_text <- paste0("Other props: ", paste(p_names[p_names %in% omit], collapse = ", "),"\n")
    cat(additional_text)
  }

}



method(print, has_style) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(print, class_arrowhead) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(str, class_arrowhead) <- function(
  object,
  nest.lev = 0,
  additional = TRUE,
  omit = omit_props(object, include = c("x","y"))) {
str_properties(object,
               omit = omit,
               nest.lev = nest.lev,
              additional = additional)
}




