prop_hjust <- new_property(
  class_numeric_or_character,
  validator = function(value) {
    if (is.character(value)) {
      if (length(value) > 0 && !all(value %in% c("left", "center", "right")))
        stop('vjust must be "left", "center", "right", or a numeric value')
    }
  }
)

prop_vjust = new_property(
  class_numeric_or_character,
  validator = function(value) {
    if (is.character(value)) {
      if (length(value) > 0 && !all(value %in% c("top", "middle", "bottom")))
        stop('vjust must be "top", "middle", "bottom", or a numeric value')
    }
  }
)

prop_polar_just <- new_property(class_numeric, setter = function(self, value) {
  if (length(value) > 0) {
  multiplier <- 1.2
  if (S7_inherits(value, point)) {
    theta = value@theta
    multiplier <- value@r
  } else if (S7_inherits(value, class_angle)) {
      theta <- value
  } else {
    theta <- degree(value)
  }

  self@vjust = polar2just(theta, multiplier, axis = "v")
  self@hjust = polar2just(theta, multiplier, axis = "h")

  }
  self@polar_just = numeric(0)
  self


  })





# style----
#' style class
#'
#' @param alpha numeric value for alpha transparency
#' @param angle angle of text
#' @param arrow_fins A 2-column matrix of polygon points
#' @param arrow_head A 2-column matrix of polygon points
#' @param arrow_mid A 2-column matrix of polygon points
#' @param arrowhead_length Determines the size of the arrow ornaments. This parameter becomes the `length` parameter in ggarrow functions. Numeric values set the ornament size relative to the linewidth. A unit value sets the ornament size in an absolute manner.
#' @param color character string for color
#' @param family font family
#' @param fill character string for fill color
#' @param fontface Can be plain, bold, italic, or bold.italic
#' @param hjust horizontal justification.
#' @param justify A numeric(1) between 0 and 1 to control where the arrows should be drawn relative to the path's endpoints. A value of 0 sets the arrow's tips at the path's end, whereas a value of 1 sets the arrow's base at the path's end. From ggarrow.
#' @param label.color Color of label outline.
#' @param label.padding Amount of padding around label. Unit vector of length four. Usually created with `ggplot2::margin`.
#' @param label.margin Amount of distance around label. Unit vector of length four. Usually created with `ggplot2::margin`.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Width of label outline.
#' @param length_head Determines the size of the arrow head. Numeric values set the ornament size relative to the linewidth. A unit value sets the ornament size in an absolute manner. From ggarrow.
#' @param length_mid Determines the size of the middle arrows. Numeric values set the ornament size relative to the linewidth. A unit value sets the ornament size in an absolute manner. From ggarrow.
#' @param length_fins Determines the size of the arrow fins. Numeric values set the ornament size relative to the linewidth. A unit value sets the ornament size in an absolute manner. From ggarrow.
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
#' @param resect A numeric(1) denoting millimeters or <unit> to shorten the arrow head and fins.
#' @param resect_fins A numeric(1) denoting millimeters or <unit> to shorten the arrow fins
#' @param resect_head A numeric(1) denoting millimeters or <unit> to shorten the arrow head.
#' @param shape Point shape type. Can be specified with an integer (between 0 and 25), a single character (which uses that character as the plotting symbol), a . to draw the smallest rectangle that is visible (i.e., about one pixel), an NA to draw nothing, or a mapping to a discrete variable.
#' @param size numeric size
#' @param size.unit How the size aesthetic is interpreted: as points ("pt"), millimeters ("mm"), centimeters ("cm"), inches ("in"), or picas ("pc").
#' @param stroke Width of point border line
#' @param stroke_color Color of point border line
#' @param stroke_width Stroke width in arrows
#' @param text.color Color of label text.
#' @param vjust vertical justification
#' @param linetype type of lines
#' @param ... unused
#' @export
style <- new_class(
  name = "style",
  properties = list(
    alpha = class_numeric,
    angle = class_angle_or_numeric,
    arrow_fins = class_list,
    arrow_head = class_list,
    arrow_mid = class_list,
    color = class_character,
    family = class_character,
    fill = class_character,
    fontface = class_character,
    hjust = prop_hjust,
    justify = class_numeric,
    label.color = class_character,
    label.padding = class_list,
    label.margin = class_list,
    label.r = class_numeric_or_unit,
    label.size = class_numeric_or_unit,
    arrowhead_length = class_numeric_or_unit,
    length_head = class_numeric_or_unit,
    length_fins = class_numeric_or_unit,
    length_mid  = class_numeric_or_unit,
    lineend = class_numeric_or_character,
    lineheight = class_numeric,
    linejoin = class_numeric_or_character,
    linewidth_fins = class_numeric,
    linewidth_head = class_numeric,
    linewidth = class_numeric,
    linetype = class_numeric_or_character,
    n = class_numeric,
    nudge_x = class_numeric,
    nudge_y = class_numeric,
    polar_just = prop_polar_just,
    resect = class_numeric_or_unit,
    resect_fins = class_numeric_or_unit,
    resect_head = class_numeric_or_unit,
    shape = class_numeric_or_character,
    size = class_numeric_or_unit,
    size.unit = class_numeric_or_unit,
    stroke = class_numeric,
    stroke_color = class_character,
    stroke_width = class_character,
    text.color = class_character,
    vjust = prop_vjust
  ),
  constructor = function(alpha = class_missing,
                         angle = class_missing,
                         arrow_head = class_missing,
                         arrow_fins = class_missing,
                         arrow_mid = class_missing,
                         color = class_missing,
                         family = class_missing,
                         fill = class_missing,
                         fontface = class_missing,
                         hjust = class_missing,
                         justify = class_missing,
                         label.color = class_missing,
                         label.margin = class_missing,
                         label.padding = class_missing,
                         label.r = class_missing,
                         label.size = class_missing,
                         arrowhead_length = class_missing,
                         length_head = class_missing,
                         length_fins = class_missing,
                         length_mid  = class_missing,
                         lineend = class_missing,
                         lineheight = class_missing,
                         linejoin = class_missing,
                         linewidth_fins = class_missing,
                         linewidth_head = class_missing,
                         linewidth = class_missing,
                         linetype = class_missing,
                         n = class_missing,
                         nudge_x = class_missing,
                         nudge_y = class_missing,
                         polar_just = class_missing,
                         resect = class_missing,
                         resect_fins = class_missing,
                         resect_head = class_missing,
                         shape = class_missing,
                         size = class_missing,
                         size.unit = class_missing,
                         stroke = class_missing,
                         stroke_color = class_missing,
                         stroke_width = class_missing,
                         text.color = class_missing,
                         vjust = class_missing,
                         ...) {
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

      if (S7_inherits(polar_just, class_angle) || is.numeric(polar_just)) {
        polar_just <- polar(theta = degree(polar_just), r = 1.2)
      }
      hjust <- polar2just(polar_just@theta, polar_just@r, axis = "h")
      vjust <- polar2just(polar_just@theta, polar_just@r, axis = "v")
      polar_just <- class_missing
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

    if (S7_inherits(angle, class_angle)) {
      angle <- c(angle) * 360
    }



    initial_list <- list(
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
      polar_just = class_missing,
      resect = resect,
      resect_fins = resect_fins,
      resect_head = resect_head,
      shape = shape,
      size = size,
      size.unit = size.unit,
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




    new_object(
      S7_object(),
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
      polar_just = class_missing,
      resect = resect,
      resect_fins = resect_fins,
      resect_head = resect_head,
      shape = shape,
      size = size,
      size.unit = size.unit,
      stroke = stroke,
      stroke_color = as.character(stroke_color),
      stroke_width = stroke_width,
      text.color = as.character(text.color),
      vjust = vjust
    )


  })

  method(str, style) <- function(object,
    nest.lev = 0,
    additional = FALSE,
    omit = NULL) {

    omit_names <- names(props(object))
    omit <- omit %||% Filter(\(o_name) {length(prop(object, name = o_name)) == 0}, omit_names)


  str_properties(object,
  omit = omit,
  nest.lev = nest.lev,
  additional = FALSE)

  }


  method(print, style) <- function(x, ...) {
    str(x, ...)
    invisible(x)
  }


method(`+`, list(style, style)) <- function(e1, e2) {
  pnames <- Filter(\(x) x != "tibble", prop_names(e1))
  for (p in prop_names(e1)) {
    if (prop_exists(e2,p) && length(prop(e2,p)) > 0) {
      prop(e1,p) <- prop(e2,p)
    }
  }
  e1
}

method(`+`, list(class_missing, style)) <- function(e1, e2) {
  e2
}

method(`+`, list(style, class_missing)) <- function(e1, e2) {
  e1
}

method(`+`, list(class_any, style)) <- function(e1, e2) {
  e2
}

method(`+`, list(style, class_any)) <- function(e1, e2) {
  e1
}

method(get_tibble, style) <- function(x) {
  d <- get_non_empty_props(x)

   tibble::tibble(!!!d)
}

method(`[`, style) <- function(x, y) {
  d <- as.list(x@tibble[y,])
  rlang::inject(style(!!!d))
}

method(as.geom, has_style) <- function(x, ...) {
  d <- get_tibble_defaults(x)
  make_geom_helper(
    d = d,
    user_overrides = get_non_empty_props(style(...)),
    aesthetics = x@aesthetics)

}
