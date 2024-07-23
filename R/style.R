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


# style----
#' style class
#'
#' @param alpha numeric value for alpha transparency
#' @param angle angle of text
#' @param arrow_fins A 2-column matrix of polygon points
#' @param arrow_head A 2-column matrix of polygon points
#' @param arrow_mid A 2-column matrix of polygon points
#' @param color character string for color
#' @param family font family
#' @param fill character string for fill color
#' @param fontface Can be plain, bold, italic, or bold.italic
#' @param hjust horizontal justification
#' @param lineheight height of line of text
#' @param linejoin height of line of text
#' @param linewidth width of lines
#' @param linetype type of lines
#' @param n number of points in a polygon
#' @param polar_just an angle, polar point, or point that alters hjust and vjust (polar polar_just not stored in style)
#' @param shape type of shape
#' @param size numeric size
#' @param size.unit How the size aesthetic is interpreted: as points ("pt"), millimeters ("mm"), centimeters ("cm"), inches ("in"), or picas ("pc").
#' @param vjust vertical justification
#' @param linetype type of lines
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
    polar_just = class_numeric,
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

      if (S7_inherits(polar_just, S7_class(degree(0))@parent) || is.numeric(polar_just)) {
        polar_just <- polar(theta = radian(polar_just), r = 1.2)
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


