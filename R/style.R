prop_hjust <- S7::new_property(
  class_numeric_or_character
  # validator = function(value) {
  #   if (is.character(value)) {
  #     if (length(value) > 0 && !all(value %in% c("left", "center", "right")))
  #       stop('vjust must be "left", "center", "right", or a numeric value')
  #   }
  # }
)

prop_vjust = S7::new_property(
  class_numeric_or_character
  # validator = function(value) {
  #   if (is.character(value)) {
  #     if (length(value) > 0 && !all(value %in% c("top", "middle", "bottom")))
  #       stop('vjust must be "top", "middle", "bottom", or a numeric value')
  #   }
  # }
)

# ob_style----
#' ob_style class
#'
#' @param id character string to identify object
#' @param alpha numeric value for alpha transparency
#' @param angle angle of text
#' @param arrow_fins A 2-column matrix of polygon points
#' @param arrow_head A 2-column matrix of polygon points
#' @param arrow_mid A 2-column matrix of polygon points
#' @param arrowhead_length Determines the size of the arrow ornaments. This parameter becomes the `length` parameter in ggarrow functions. Numeric values set the ornament size relative to the linewidth. A [grid::unit] value sets the ornament size in an absolute manner.
#' @param color character string for color
#' @param family font family
#' @param fill character string for fill color
#' @param fontface Can be plain, bold, italic, or bold.italic
#' @param hjust horizontal justification. 0 means left justified, 1 means right justified, 0.5 means horizontally centered
#' @param justify A numeric(1) between 0 and 1 to control where the arrows should be drawn relative to the path's endpoints. A value of 0 sets the arrow's tips at the path's end, whereas a value of 1 sets the arrow's base at the path's end. From ggarrow.
#' @param label.color Color of label outline.
#' @param label.padding Amount of padding around label. A [grid::unit] vector of length four. Usually created with [`ggplot2::margin`].
#' @param label.margin Amount of distance around label. A [grid::unit] vector of length four. Usually created with [`ggplot2::margin`].
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Width of label outline.
#' @param length_head Determines the size of the arrow head. Numeric values set the ornament size relative to the linewidth. A [grid::unit] value sets the ornament size in an absolute manner. From ggarrow.
#' @param length_mid Determines the size of the middle arrows. Numeric values set the ornament size relative to the linewidth. A [grid::unit] value sets the ornament size in an absolute manner. From ggarrow.
#' @param length_fins Determines the size of the arrow fins. Numeric values set the ornament size relative to the linewidth. A [grid::unit] value sets the ornament size in an absolute manner. From ggarrow.
#' @param linewidth_fins Line width for arrow fins
#' @param linewidth_head Line width for arrow fins
#' @param lineend Line end style (round, butt, square).
#' @param lineheight Height of line of text
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linewidth Width of lines
#' @param linetype Type of line. Can be specified with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a mapping to a discrete variable, or a string of an even number (up to eight) of hexadecimal digits which give the lengths in consecutive positions in the string.
#' @param n Number of points in a polygon, circle, arc, or ellipse
#' @param nudge_x Horizontal adjustment to nudge labels by.
#' @param nudge_y Vertical adjustment to nudge labels by.
#' @param polar_just an angle, polar point, or point that alters hjust and vjust (polar polar_just not stored in style)
#' @param resect A numeric(1) denoting millimeters or [grid::unit] to shorten the arrow head and fins.
#' @param resect_fins A numeric(1) denoting millimeters or [grid::unit] to shorten the arrow fins
#' @param resect_head A numeric(1) denoting millimeters or [grid::unit] to shorten the arrow head.
#' @param shape Point shape type. Can be specified with an integer (between 0 and 25), a single character (which uses that character as the plotting symbol), a . to draw the smallest rectangle that is visible (i.e., about one pixel), an NA to draw nothing, or a mapping to a discrete variable.
#' @param size numeric size
#' @param size.unit How the size aesthetic is interpreted: as points (`"pt"`), millimeters (`"mm"`), centimeters (`"cm"`), inches (`"in"`), or picas (`"pc"`).
#' @param stroke Width of point border line
#' @param stroke_color Color of point border line
#' @param stroke_width Stroke width in arrows
#' @param straight logical. If TRUE, make bzpath label text straight instead of curved.
#' @param text.color Color of label text.
#' @param vjust vertical justification. 0 means bottom aligned, 1 means top aligned, 0.5 means vertically centered
#' @param linetype type of lines
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> unused
#' @export
#' @returns ob_style object
ob_style <- S7::new_class(
  name = "ob_style",
  properties = list(
    id = S7::class_character,
    alpha = S7::new_property(S7::class_numeric, default = NULL),
    angle = ob_angle_or_numeric,
    arrow_fins = S7::class_list,
    arrow_head = S7::class_list,
    arrow_mid = S7::class_list,
    color = S7::class_character,
    family = S7::class_character,
    fill = S7::class_character,
    fontface = S7::class_character,
    hjust = prop_hjust,
    justify = S7::class_numeric,
    label.color = S7::class_character,
    label.padding = S7::class_list,
    label.margin = S7::class_list,
    label.r = class_numeric_or_unit,
    label.size = class_numeric_or_unit,
    arrowhead_length = class_numeric_or_unit,
    length_head = class_numeric_or_unit,
    length_fins = class_numeric_or_unit,
    length_mid  = class_numeric_or_unit,
    lineend = class_numeric_or_character,
    lineheight = S7::class_numeric,
    linejoin = class_numeric_or_character,
    linewidth_fins = S7::class_numeric,
    linewidth_head = S7::class_numeric,
    linewidth = S7::class_numeric,
    linetype = class_numeric_or_character,
    n = S7::class_numeric,
    nudge_x = S7::class_numeric,
    nudge_y = S7::class_numeric,
    polar_just = S7::new_property(
      S7::class_numeric,
      setter = function(self, value) {
        if (length(value) > 0) {
          multiplier <- 1.2
          if (S7::S7_inherits(value, ob_point)) {
            theta = value@theta
            multiplier <- value@r
          } else if (S7::S7_inherits(value, ob_angle)) {
            theta <- value
          } else {
            theta <- degree(value)
          }

          self@vjust = polar2just(theta, multiplier, axis = "v")
          self@hjust = polar2just(theta, multiplier, axis = "h")

        }
        self@polar_just = numeric(0)
        self
      }
    ),
    resect = class_numeric_or_unit,
    resect_fins = class_numeric_or_unit,
    resect_head = class_numeric_or_unit,
    shape = class_numeric_or_character,
    size = class_numeric_or_unit,
    size.unit = class_numeric_or_unit,
    straight = S7::class_logical,
    stroke = S7::class_numeric,
    stroke_color = S7::class_character,
    stroke_width = class_numeric_or_unit,
    text.color = S7::class_character,
    vjust = prop_vjust
  ),
  constructor = function(id = character(0),
                         alpha = numeric(0),
                         angle = numeric(0),
                         arrow_head = list(),
                         arrow_fins = list(),
                         arrow_mid = list(),
                         color = character(0),
                         family = character(0),
                         fill = character(0),
                         fontface = character(0),
                         hjust = numeric(0),
                         justify = numeric(0),
                         label.color = character(0),
                         label.margin = list(),
                         label.padding = list(),
                         label.r = numeric(0),
                         label.size = numeric(0),
                         arrowhead_length = numeric(0),
                         length_head = numeric(0),
                         length_fins = numeric(0),
                         length_mid  = numeric(0),
                         lineend = numeric(0),
                         lineheight = numeric(0),
                         linejoin = numeric(0),
                         linewidth_fins = numeric(0),
                         linewidth_head = numeric(0),
                         linewidth = numeric(0),
                         linetype = numeric(0),
                         n = numeric(0),
                         nudge_x = numeric(0),
                         nudge_y = numeric(0),
                         polar_just = numeric(0),
                         resect = numeric(0),
                         resect_fins = numeric(0),
                         resect_head = numeric(0),
                         shape = numeric(0),
                         size = numeric(0),
                         size.unit = numeric(0),
                         straight = logical(0),
                         stroke = numeric(0),
                         stroke_color = character(0),
                         stroke_width = numeric(0),
                         text.color = character(0),
                         vjust = numeric(0),
                         ...) {
    id <- as.character(id)
    the_style <- rlang::list2(...)
    color <- as.character(the_style$colour %||% color)
    label.color <- as.character(the_style$label.colour %||% label.color)
    text.color <- as.character(the_style$text.colour %||% text.color)
    stroke_color <- as.character(the_style$stroke_colour %||% stroke_color)
    if (is.logical(linetype)) linetype <- as.character(linetype)
    if (is.logical(linewidth)) linewidth <- as.double(linetype)

    if (length(label.padding) > 0) {
      label.padding <- class_margin(label.padding)
    }

    if (length(label.margin) > 0) {
      label.margin <- class_margin(label.margin)
    }


    if (length(polar_just) > 0) {

      if (S7::S7_inherits(polar_just, ob_angle) || is.numeric(polar_just)) {
        polar_just <- ob_polar(theta = degree(polar_just), r = 1.2)
      }
      hjust <- polar2just(polar_just@theta, polar_just@r, axis = "h")
      vjust <- polar2just(polar_just@theta, polar_just@r, axis = "v")
      polar_just <- S7::class_missing
    }


    if (length(arrow_head) > 0) {
      arrow_head <- class_arrowhead(arrow_head)
    }

    if (length(arrow_fins) > 0) {
      arrow_fins <- class_arrowhead(arrow_fins)
    }

    if (length(arrow_mid) > 0) {
      arrow_mid <- class_arrowhead(arrow_mid)
    }

    if (is.character(angle)) angle <- degree(angle)

    if (S7::S7_inherits(angle, ob_angle)) {
      angle <- c(angle) * 360
    }

    initial_list <- list(
      id = id,
      alpha = alpha,
      color = as.character(color),
      angle = angle,
      arrow_head = arrow_head,
      arrow_fins = arrow_fins,
      arrow_mid = arrow_mid,
      family = family,
      fill = as.character(fill),
      fontface = fontface,
      hjust = hjust,
      justify = justify,
      label.color = as.character(label.color),
      label.padding = label.padding,
      label.margin = label.margin,
      label.r = label.r,
      label.size = label.size,
      arrowhead_length = arrowhead_length,
      length_head = length_head,
      length_fins = length_fins,
      length_mid = length_mid,
      lineend = lineend,
      lineheight = lineheight,
      linejoin = linejoin,
      linewidth_fins = linewidth_fins,
      linewidth_head = linewidth_head,
      linewidth = linewidth,
      linetype = linetype,
      n = n,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      polar_just = S7::class_missing,
      resect = resect,
      resect_fins = resect_fins,
      resect_head = resect_head,
      shape = shape,
      size = size,
      size.unit = size.unit,
      straight = straight,
      stroke = stroke,
      stroke_color = as.character(stroke_color),
      stroke_width = stroke_width,
      text.color = as.character(text.color),
      vjust = vjust
    )
    non_empty_list <- get_non_empty_list(initial_list)

    if (length(non_empty_list) > 0) {
      d <-  tibble::tibble(!!!non_empty_list)
    }

    S7::new_object(
      S7::S7_object(),
      id = id,
      alpha = alpha,
      color = as.character(color),
      angle = angle,
      arrow_head = arrow_head,
      arrow_fins = arrow_fins,
      arrow_mid = arrow_mid,
      family = family,
      fill = as.character(fill),
      fontface = fontface,
      hjust = hjust,
      justify = justify,
      label.color = as.character(label.color),
      label.padding = label.padding,
      label.margin = label.margin,
      label.r = label.r,
      label.size = label.size,
      arrowhead_length = arrowhead_length,
      length_head = length_head,
      length_fins = length_fins,
      length_mid = length_mid,
      lineend = lineend,
      lineheight = lineheight,
      linejoin = linejoin,
      linewidth_fins = linewidth_fins,
      linewidth_head = linewidth_head,
      linewidth = linewidth,
      linetype = linetype,
      n = n,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      polar_just = S7::class_missing,
      resect = resect,
      resect_fins = resect_fins,
      resect_head = resect_head,
      shape = shape,
      size = size,
      size.unit = size.unit,
      straight = straight,
      stroke = stroke,
      stroke_color = as.character(stroke_color),
      stroke_width = stroke_width,
      text.color = as.character(text.color),
      vjust = vjust
    )

  })

S7::method(str, ob_style) <- function(object,
                                      nest.lev = 0,
                                      additional = FALSE,
                                      omit = NULL) {
  omit_names <- names(props(object))
  omit <- omit %||% Filter(\(o_name) {
    length(S7::prop(object, name = o_name)) == 0
  }, omit_names)


  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = FALSE)

}

S7::method(print, ob_style) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

S7::method(`+`, list(ob_style, ob_style)) <- function(e1, e2) { # nocov start
  pn <- S7::prop_names(e1)
  pnames <- pn[pn != "tibble"]
  for (p in pnames) {
    if (length(S7::prop(e2, p)) > 0) {
      S7::prop(e1, p) <- S7::prop(e2, p)
    }
  }
  e1
} # nocov end



S7::method(`+`, list(S7::class_missing, ob_style)) <- function(e1, e2) { # nocov start
  e2
} # nocov end

S7::method(`+`, list(ob_style, S7::class_missing)) <- function(e1, e2) { # nocov start
  e1
} # nocov end

S7::method(`+`, list(S7::class_any, ob_style)) <- function(e1, e2) { # nocov start
  e2
} # nocov end

S7::method(`+`, list(ob_style, S7::class_any)) <- function(e1, e2) { # nocov start
  e1
} # nocov end

S7::method(get_tibble, ob_style) <- function(x) {
  d <- get_non_empty_props(x)

  tibble::tibble(!!!d)
}

S7::method(`[`, ob_style) <- function(x, i) {
  i <- character_index(i, x@id)
  get_tibble(x)[i, ] |>
    data2shape(ob_style)
}

S7::method(as.geom, has_style) <- function(x, ...) {
  d <- get_tibble_defaults(x)
  make_geom_helper(
    d = d,
    user_overrides = get_non_empty_props(ob_style(...)),
    aesthetics = x@aesthetics
  )

}
