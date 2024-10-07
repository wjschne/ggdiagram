#' color class
#'
#' @param color character (R color or hex code)
#' @slot transparentize function to return the color with a new transparency (i.e., alpha)
#' @slot lighten function to return a lighter color
#' @slot darken function to return a darker color
#' @param hue get or set the hue of a color (i.e., the h in the hsv model)
#' @param saturation get or set the saturation of a color (i.e., the s in the hsv model)
#' @param brightness get or set the brightness of a color (i.e., the v in the hsv model)
#' @param alpha get or set the transparency of a color
#' @export
#' @examples
#' mycolor <- class_color("blue")
#' mycolor
#' # Display html hexcode
#' c(mycolor)
#' # Set transparency
#' mycolor@transparentize(.5)
#' # Lighten color
#' mycolor@lighten(.5)
#' # Darken color
#' mycolor@darken(.5)
class_color <- new_class(
  name = "class_color",
  parent = class_character,
  properties = list(
    color = new_property(
      class = class_character,
      getter = function(self) {
        as.character(self)
      }
    ),
    transparentize = new_property(
      class = class_function,
      getter = function(self) {
        \(alpha = .5) class_color(scales::alpha(alpha = alpha, colour = self))
      }
    ),
    lighten = new_property(
      class = class_function,
      getter = function(self) {
        \(amount = 0.2) {
          tibble::tibble(amount = amount, x = c(self)) %>%
            purrr::pmap_chr(tinter::lighten) %>%
            class_color()
        }
      }
    ),
    darken = new_property(
      class = class_function,
      getter = function(self) {
        \(amount = 0.2) {
          tibble::tibble(amount = amount, x = c(self)) %>%
            purrr::pmap_chr(tinter::darken) %>%
            class_color()
        }
      }
    ),
    saturation = new_property(
      class = class_integer,
      getter = function(self) {
        farver::get_channel(c(self), channel = "s", space = "hsv")

      },
      setter = function(self, value) {
        S7_data(self) <- farver::set_channel(
          colour = c(self),
          value = value,
          channel = "s",
          space = "hsv"
        )
        self
      }
    ),
    hue = new_property(
      class = class_integer,
      getter = function(self) {
        farver::get_channel(colour = c(self),
                            channel = "h",
                            space = "hsv")
      },
      setter = function(self, value) {
        S7_data(self) <- farver::set_channel(c(self), value, channel = "h", space = "hsv")
        self
      }
    ),
    brightness = new_property(
      class = class_integer,
      getter = function(self) {
        farver::get_channel(c(self), channel = "v", space = "hsv")
      },
      setter = function(self, value) {
        S7_data(self) <- farver::set_channel(
          colour = c(self),
          value = value,
          channel = "v",
          space = "hsv"
        )
        self
      }
    ),
    alpha = new_property(
      class = class_integer,
      getter = function(self) {
        farver::get_channel(c(self), channel = "alpha", space = "hsv")

      },
      setter = function(self, value) {
        S7_data(self) <- farver::set_channel(
          colour = c(self),
          value = value,
          channel = "alpha",
          space = "hsv"
        )
        self
      }
    )
  ), constructor = function(color = class_missing, hue = NULL, saturation = NULL, brightness = NULL, alpha = NULL) {
    decoded <- farver::decode_colour(color, alpha = TRUE)

    if (!is.null(hue)) decodec <- farver::get_channel(decoded, channel = "h", space = "hsv")
    if (!is.null(saturation)) decodec <- farver::get_channel(decoded, channel = "s", space = "hsv")
    if (!is.null(brightness)) decodec <- farver::get_channel(decoded, channel = "v", space = "hsv")
    if (!is.null(alpha)) decodec <- farver::get_channel(decoded, channel = "alpha", space = "hsv")

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

method(mean, class_color) <- function(x, ...) {
  y <- class_color("white")
  y@hue <- mean(x@hue)
  y@saturation <- mean(x@saturation)
  y@brightness <- mean(x@brightness)
  y@alpha <- mean(x@alpha)
  y
}

#' Average across colors
#'
#' @param x color
#'
#' @return character
#' @export
#'
#' @examples
#' mean_color(c("red", "violet"))
mean_color <- function(x) {
  grDevices::colorRampPalette(x, space = "Lab")(3)[2]
}
