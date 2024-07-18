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
    color = class_color_or_character,
    family = class_character,
    fill = class_character,
    fontface = class_character,
    hjust = prop_hjust,
    justify = class_numeric,
    label.color = class_color_or_character,
    label.padding = class_list,
    label.margin = class_list,
    label.r = class_numeric_or_unit,
    label.size = class_numeric_or_unit,
    length = class_numeric_or_unit,
    length_head = class_numeric_or_unit,
    length_fins = class_numeric_or_unit,
    length_mid  = class_numeric_or_unit,
    lineend = class_numeric_or_character,
    lineheight = class_numeric,
    linejoin = class_character,
    linewidth_fins = class_numeric,
    linewidth_head = class_numeric,
    linewidth = class_numeric,
    linetype = class_numeric_or_character,
    n = class_numeric,
    nudge_x = class_numeric,
    nudge_y = class_numeric,
    polar_just = class_angle_or_numeric,
    resect = class_numeric_or_unit,
    resect_fins = class_numeric_or_unit,
    resect_head = class_numeric_or_unit,
    shape = class_numeric_or_character,
    size = class_numeric_or_unit,
    size.unit = class_numeric_or_unit,
    stroke = class_numeric,
    stroke_color = class_color_or_character,
    stroke_width = class_character,
    text.color = class_color_or_character,
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
                         length = class_missing,
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
    color <- class_color(the_style$colour %||% color)
    label.color <- class_color(the_style$label.colour %||% label.color)
    text.color <- class_color(the_style$text.colour %||% text.color)
    stroke_color <- class_color(the_style$stroke_colour %||% stroke_color)
    if (is.logical(linetype)) linetype <- as.character(linetype)

    if (length(polar_just) > 0) {
      if (S7_inherits(polar_just, S7_class(degree(0))@parent) || is.numeric(polar_just)) {
        polar_just <- polar(theta = radian(polar_just), r = 1.2)
      }
      hjust <- polar2just(polar_just@theta, polar_just@r, axis = "h")
      vjust <- polar2just(polar_just@theta, polar_just@r, axis = "v")
      polar_just <- class_missing
    }

    d <- get_non_empty_tibble(
      list(
        alpha = alpha,
        angle = c(angle),
        polar_just = c(polar_just),
        arrow_fins = ifelse(
          length(arrow_fins) > 0 &&
            !is.list(arrow_fins) && is.null(arrow_fins),
          list(arrow_fins),
          list()
        ),
        arrow_head = ifelse(
          length(arrow_head) > 0 &&
            !is.list(arrow_head) && is.null(arrow_head),
          list(arrow_head),
          list()
        ),
        arrow_mid = ifelse(
          length(arrow_mid) > 0 && !is.list(arrow_mid) && is.null(arrow_mid),
          list(arrow_mid),
          list()
        ),
        color = color,
        family = family,
        fill = fill,
        fontface = fontface,
        hjust = hjust,
        justify = justify,
        label.color = label.color,
        label.padding = ifelse(length(label.padding) > 0,
                               list(label.padding),
                               list()),
        label.margin = ifelse(length(label.margin) > 0,
                              list(label.margin),
                              list()),
        label.r = label.r,
        label.size = label.size,
        length = length,
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
        resect = resect,
        resect_fins = resect_fins,
        resect_head = resect_head,
        shape = shape,
        size = size,
        size.unit = size.unit,
        stroke = stroke,
        stroke_color = stroke_color,
        stroke_width = stroke_width,
        text.color = text.color,
        vjust = alpha
      )
    )
    d <- as.list(d)
    new_object(S7_object(), !!!d)

  })
