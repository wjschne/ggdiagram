# str ----
#' structure
#'
#' @param object object
#' @keywords internal
str <- new_generic(
  name = "str",
  dispatch_args = "object")

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
  names <- format(names(object))
  for (i in seq_along(object)) {
    cat(indent.str, prefix, " ", names[[i]], ":", sep = "")
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
  if (length(omit) > 0 && additional && nest.lev && all((omit == ".data"))) {
    additional_text <- paste0(" Omitted props: ", paste(p_names[p_names %in% omit], collapse = ", "))
    cat(additional_text)
  }
  cat("\n")

  str_nest(object = props(object)[!(p_names %in% omit)],
           prefix = "@",
           nest.lev = nest.lev)


}





