#' color class
#'
#' Useful for manipulating colors in R.
#' @param color character (R color or hex code)
#' @slot transparentize function to return the color with a new transparency (i.e., alpha)
#' @slot lighten function to return a lighter color
#' @slot darken function to return a darker color
#' @param hue get or set the hue of a color (i.e., the h in the hsv model)
#' @param saturation get or set the saturation of a color (i.e., the s in the hsv model)
#' @param brightness get or set the brightness of a color (i.e., the v in the hsv model)
#' @param alpha get or set the transparency of a color
#' @param id character identifier
#' @export
#' @returns class_color object
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
class_color <- S7::new_class(
  name = "class_color",
  parent = S7::class_character,
  package = "ggdiagram",
  properties = list(
    color = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        as.character(self)
      }
    ),
    transparentize = S7::new_property(
      class = S7::class_function,
      getter = function(self) {
        \(alpha = .5) class_color(scales::alpha(alpha = alpha, colour = self))
      }
    ),
    lighten = S7::new_property(
      class = S7::class_function,
      getter = function(self) {
        \(amount = 0.2) {
          tibble::tibble(amount = amount, x = c(self)) |>
            purrr::pmap(\(amount, x) {
              if (amount == 0) {
                return("#FFFFFF")
                } else {
                tinter::lighten(x = x, amount = amount)
              }
            } ) |>
            unlist() |>
            class_color()
        }
      }
    ),
    darken = S7::new_property(
      class = S7::class_function,
      getter = function(self) {
        \(amount = 0.2) {
          tibble::tibble(amount = amount, x = c(self)) |>
            purrr::pmap(\(amount, x) {
              if (amount == 0) {
                return(x)
              } else {
                tinter::darken(x = x, amount = amount)
              }
            } ) |>
            unlist() |>
            class_color()
        }
      }
    ),
    saturation = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        farver::get_channel(c(self), channel = "s", space = "hsv")

      },
      setter = function(self, value) {
        S7::S7_data(self) <- farver::set_channel(
          colour = c(self),
          value = value,
          channel = "s",
          space = "hsv"
        )
        self
      }
    ),
    hue = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        farver::get_channel(colour = c(self),
                            channel = "h",
                            space = "hsv")
      },
      setter = function(self, value) {
        S7::S7_data(self) <- farver::set_channel(c(self), value, channel = "h", space = "hsv")
        self
      }
    ),
    brightness = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        farver::get_channel(c(self), channel = "v", space = "hsv")
      },
      setter = function(self, value) {
        S7::S7_data(self) <- farver::set_channel(
          colour = c(self),
          value = value,
          channel = "v",
          space = "hsv"
        )
        self
      }
    ),
    alpha = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        farver::get_channel(c(self), channel = "alpha", space = "hsv")

      },
      setter = function(self, value) {
        S7::S7_data(self) <- farver::set_channel(
          colour = c(self),
          value = value,
          channel = "alpha",
          space = "hsv"
        )
        self
      }
    ),
    red = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        farver::get_channel(colour = c(self),
                            channel = "r",
                            space = "rgb")
      },
      setter = function(self, value) {
        S7::S7_data(self) <- farver::set_channel(c(self), value, channel = "r", space = "rgb")
        self
      }
    ),
    green = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        farver::get_channel(colour = c(self),
                            channel = "g",
                            space = "rgb")
      },
      setter = function(self, value) {
        S7::S7_data(self) <- farver::set_channel(c(self), value, channel = "g", space = "rgb")
        self
      }
    ),
    blue = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        farver::get_channel(colour = c(self),
                            channel = "b",
                            space = "rgb")
      },
      setter = function(self, value) {
        S7::S7_data(self) <- farver::set_channel(c(self), value, channel = "b", space = "rgb")
        self
      }
    ),
    mean = S7::new_property(getter = \(self) {
      r <- mean(self@red)
      g <- mean(self@green)
      b <- mean(self@blue)
      a <- mean(self@alpha)
      x <- class_color("white")
      x@red <- r
      x@green <- g
      x@blue <- b
      x@alpha <- a
      x
    }),
    tex = S7::new_property(getter = function(self) {
      paste0("\\color[HTML]{", substring(self@color, 2, 7), "}")
    }),
    id = S7::class_character
  ),
  constructor = function(color = character(0),
                         hue = NULL,
                         saturation = NULL,
                         brightness = NULL,
                         alpha = NULL,
                         id = character(0)) {

    if (length(color) == 0) {
      decoded <- farver::decode_colour("red", alpha = TRUE, to = "hsv")
      } else {
        decoded <- farver::decode_colour(color, alpha = TRUE, to = "hsv")
      }

    d <- tibble::as_tibble(decoded) |> as.list()


    if (!is.null(hue)) {
      # Make sure hue is between 0 and 360
      hue <- degree(hue)@positive@degree
      # Make sure hue is of same length as color
      max_length <- purrr::map_int(d, length) |> max()
      if (max_length == 1 | max_length == length(hue) | length(hue) == 1) {
        d$h <- hue
      }  else {
        stop("Hue must be of same length as color.")
      }

    }

    if (!is.null(saturation)) {
      # Make sure saturation is between 0 and 1
      saturation <- ifelse(abs(saturation) > 1, 1, abs(saturation))
      # Make sure saturation is of same length as color
      max_length <- purrr::map_int(d, length) |> max()
      if (max_length == 1 | max_length == length(saturation) | length(saturation) == 1) {
        d$s <- saturation
      }  else {
        stop("Saturation must be of same length as color.")
      }
    }


    if (!is.null(brightness)) {
      # Make sure brightness is between 0 and 1
      brightness <- ifelse(abs(brightness) > 1, 1, abs(brightness))
      # Make sure brightness is of same length as color
      max_length <- purrr::map_int(d, length) |> max()
      if (max_length == 1 | max_length == length(brightness) | length(brightness) == 1) {
        d$v <- brightness
      }  else {
        stop("Brightness must be of same length as color.")
      }
    }

    if (!is.null(alpha)) {
      # Make sure alpha is between 0 and 1
      alpha <- ifelse(abs(alpha) > 1, 1, abs(alpha))
      # Make sure alpha is of same length as color
      max_length <- purrr::map_int(d, length) |> max()
      if (max_length == 1 | max_length == length(alpha) | length(alpha) == 1) {
        d$alpha <- alpha
      }  else {
        stop("Alpha must be of same length as color.")
      }
    }

    decoded <- tibble::as_tibble(d) |> as.matrix()


    S7::new_object(
      farver::encode_colour(
        decoded[,c("h", "s", "v"),
                drop = FALSE],
        alpha = decoded[,"alpha"],
        from = "hsv"),
      id = id)
  }
)

class_color_or_character <- S7::new_union(class_color, S7::class_character)

S7::method(str, class_color) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = omit_props(object, include = c(".data", "color"))) {
  str_properties(
    object,
    omit = omit,
    nest.lev = nest.lev)
}

# S7::method(print, class_color) <- function(x, ...) {
#   str(x, ...)
#   invisible(x)
# }

S7::method(`[`, class_color) <- function(x, i) {
  i <- character_index(i, x@id)
  S7::S7_data(x) <-  c(x)[i]
  x
}

S7::method(mean, class_color) <- function(x, ...) {
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
#' @returns string
#' @export
#'
#' @examples
#' color_A <- "dodgerblue"
#' color_B <- "violet"
#' color_AB <- mean_color(c(color_A, color_B))
#' fills <- c(color_A,
#'            color_AB,
#'            color_B)
#' ggdiagram() +
#'   ob_circle(x = c(0, 3, 6),
#'             color = NA,
#'             fill = fills)
mean_color <- function(x) {
  grDevices::colorRampPalette(x, space = "Lab")(3)[2]
}

# latex_color ----
#' Surround TeX expression with a color command
#'
#' @param x TeX expression
#' @param color color
#'
#' @returns string
#' @export
#'
#' @examples
#' latex_color("X^2", "red")
latex_color <- function(x, color) {
  if (!S7::S7_inherits(color, class_color)) {
    color <- class_color(color)
  }
  paste0("{",color@tex," ", x,"}")
}

