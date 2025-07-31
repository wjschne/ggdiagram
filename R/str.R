

# from S7
obj_type <- function(x) {
  if (identical(x, quote(expr = ))) {
    "missing"
  }
  else if (S7::S7_inherits(x)) {
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

# from S7 internals
str_function <- function(object, ..., nest.lev = 0) {
  attr(object, "srcref") <- NULL
  if (identical(class(object), "function")) {
    cat(" ")
  }
  utils::str(object, ..., nest.lev = nest.lev)
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
      str_function(xi, nest.lev = nest.lev + 1)
      utils::str(xi, ..., nest.lev = nest.lev + 1)
    } else {
      utils::str(xi, ...)
    }

  }
}

omit_props <- function(object, include = character(0), omit = character(0)) {
  if (length(include) > 0) {
    setdiff(
      names(S7::S7_class(object)@properties),
      include)
  } else if (length(omit) > 0) {
    intersect(
      omit,
      names(S7::S7_class(object)@properties)
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
  p_names <- S7::prop_names(object)
  cat(if (nest.lev > 0) " ")
  cli::cli_h1(obj_desc(object))

  p_display <- p_names[!(p_names %in% omit)]
  props_display <- lapply(p_display, prop, object = object) |>
    `names<-`(p_display)
  # props(object)[!(p_names %in% omit)]
  str_nest(object = props_display,
           prefix = "@",
           nest.lev = nest.lev)
  if (length(omit) > 0 && additional && nest.lev == 0) {
    additional_text <- paste0("Other props: ", paste(p_names[p_names %in% omit], collapse = ", "),"\n")
    cat(stringr::str_wrap(string = additional_text, width = 67, exdent = 13))
  }
  if (nest.lev == 0) cat("\n")

}





# S7::method(print, class_arrowhead) <- function(x, ...) {
#   str(x, ...)
#   invisible(x)
# }

S7::method(str, class_arrowhead) <- function(
  object,
  nest.lev = 0,
  additional = TRUE,
  omit = omit_props(object, include = c("x","y"))) {

  cli::cli_h3("<class_arrowhead>")

  for (i in S7::S7_data(object)) {
    cli::cli_text(
      paste0("A matrix with 2 columns and ",
             nrow(i),
             " rows.",
             ifelse(nrow(i) > 6,
                    " First 6 rows:",
                    ""))
      )
    print(round(head(i), 2))
  }
  invisible(object)
}

S7::method(str, class_margin) <- function(
  object,
  nest.lev = 0,
  additional = FALSE,
  omit = "") {
  cli::cli_h3("<class_margin>")

  for (i in S7::S7_data(object)) {
    print(i)
  }
  invisible(object)
}


S7::method(str, class_aesthetics_list) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = omit_props(object, include = c("geom","style", "mappable_bare", "mappable_identity", "not_mappable", "required_aes"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev)
}

S7::method(str, ob_shape_list) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = "") {
  cli::cli_h1("<ob_shape_list>")
  lapply(S7::S7_data(object), print)
  invisible(object)
}
