#' color class
#'
#' @param color character (R color or hex code)
#' @param transparentize function to return the color with a new transparency (i.e., alpha)
#' @param lighten function to return a lighter color
#' @param darken function to return a darker color
#' @export
#' @examples
#' mycolor <- class_color("blue")
#' mycolor
#' c(mycolor)
#' mycolor@tranparentize(.5)
#' mycolor@lighten(.5)
#' mycolor@darken(.5)
class_color <- new_class(
  name = "class_color",
  parent = class_character,
  properties = list(
    color = new_property(class_character, getter = function(self) {
      as.character(self)
    }),
    transparentize = new_property(class_function, getter = function(self) {
      \(alpha = .5) class_color(scales::alpha(alpha = alpha, colour = self))
    }),
    lighten = new_property(class_function, getter = function(self) {
      \(amount = 0.2) {
        class_color(tinter::lighten(amount = amount, x = self))
      }
    }),
    darken = new_property(class_function, getter = function(self) {
      \(amount = 0.2) {
        class_color(tinter::darken(amount = amount, x = S7_data(self)))
      }
    })
  ), constructor = function(color = class_missing) {
    decoded <- farver::decode_colour(color, alpha = TRUE)
    new_object(farver::encode_colour(decoded, alpha = decoded[,"alpha"]))
  }
)


class_color_or_character <- new_union(class_color, class_character)


method(str, class_color) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = omit_props(object, include = c(".data", "color"))) {
  str_properties(
    object,
    omit = omit,
    nest.lev = nest.lev)
}

method(print, class_color) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(`[`, class_color) <- function(x, y) {
  S7::S7_data(x) <-  c(x)[y]
  x
}
