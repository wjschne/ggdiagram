lb_styles <- c("alpha", "angle", "color", "family","fill","fontface",
               "hjust","label.color","label.margin","label.padding",
               "label.r","label.size","lineheight","nudge_x","nudge_y",
               "polar_just","size","text.color","vjust")

lb_props <- list(
  primary = list(
    label = new_property(class = class_character),
    p = new_property(class = point)
  ),
  styles = style@properties[lb_styles],
  extras = list(
    plot_point = new_property(class_logical, validator = function(value) {
      if(length(value) !=1 ) {
        stop("The plot_point property must be a TRUE/FALSE value of length 1.")
      }
    })
  ),
  derived = list(
   length = new_property(
      getter = function(self) {
        length(self@label)
      }
    ),
   style = new_property(
     getter = function(self) {
       pr <- `names<-`(purrr::map(lb_styles,
                                  prop, object = self),
                       lb_styles)
       rlang::inject(style(!!!get_non_empty_list(pr)))},
     setter = function(self, value) {
       point(self@x, self@y, style = self@style + value)
     }),
    tibble = new_property(getter = function(self) {
      if (length(self@angle) > 0) {
        if (S7_inherits(self@angle, class_angle)) {
          self@angle <- self@angle@degree
        }
      }
      d <- list(x = self@p@x,
                y = self@p@y,
                label = self@label,
                alpha = self@alpha,
                color = self@color,
                angle = self@angle,
                family = self@family,
                fill = self@fill,
                fontface = self@fontface,
                hjust = self@hjust,
                label.color = self@label.color,
                label.margin = self@label.margin,
                label.padding = self@label.padding,
                label.r = self@label.r,
                label.size = self@label.size,
                lineheight = self@lineheight,
                nudge_x = self@nudge_x,
                nudge_y = self@nudge_y,
                polar_just = self@polar_just,
                size = self@size,
                text.color = self@text.color,
                vjust = self@vjust)
      get_non_empty_tibble(d)

    })
  ),
  funs = list()
)


# label----

#' label class
#'
#' @param label text label
#' @param p point
#' @param style a style list
#' @param ... properties passed to style
#' @export
label <- new_class(
  name = "label",
  parent = has_style,
  properties = rlang::inject(list(
    !!!lb_props$primary,
    !!!lb_props$styles,
    !!!lb_props$extras,
    !!!lb_props$derived,
    !!!lb_props$funs)),
  constructor = function(label = class_missing,
                         p = class_missing,
                         angle = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         family = class_missing,
                         fill = class_missing,
                         fontface = class_missing,
                         hjust = class_missing,
                         label.color = class_missing,
                         label.margin = ggplot2::margin(1,1,1,1,"pt"),
                         label.padding = ggplot2::margin(2,2,2,2,"pt"),
                         label.r = class_missing,
                         label.size = class_missing,
                         lineheight = class_missing,
                         polar_just = class_missing,
                         nudge_x = class_missing,
                         nudge_y = class_missing,
                         size = class_missing,
                         text.color = class_missing,
                         vjust = class_missing,
                         style = class_missing,
                         plot_point = FALSE,
                         ...) {

    if (missing(p)) {
      p <- point()
    }

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

    d <- tibble::tibble(x = p@x, y = p@y, label = as.character(label))
    if (length(angle) > 0) {
      if (nrow(d) > 1 && length(angle) == 1) {
        S7_data(angle) <- rep(S7_data(angle), nrow(d))

      }
    }


    l_style <- p@style + style(size = ggtext::GeomRichText$default_aes$size * ggplot2::.pt, label.color = NA, fill = "white") + style +
      style(
        alpha = alpha,
        color = as.character(color),
        angle = angle,
        family = family,
        fill = as.character(fill),
        fontface = fontface,
        hjust = hjust,
        label.color = as.character(label.color),
        label.padding = label.padding,
        label.margin = label.margin,
        label.r = label.r,
        label.size = label.size,
        lineheight = lineheight,
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        polar_just = polar_just,
        size = size,
        text.color = as.character(text.color),
        vjust = vjust
      )






    if (S7::S7_inherits(p, segment)) {
      l_style@angle <- p@line@angle@degree
      l_style@vjust = ifelse(length(l_style@vjust) == 0 || is.na(l_style@vjust), 0,l_style@vjust)
      p <- midpoint(p)
    }

    if (S7::S7_inherits(p, arc)) {
      if (missing(label)) {
        label = as.character(degree(p@theta@degree))
      }
      a <- p
      p <- midpoint(p)
      l_style <- polar_just(l_style,
                            (p - a@center)@theta + angle(turn = .5),
                            multiplier = 1.15) + style(...)
    }

    if (missing(label)) {
      label = paste0("(", signs::signs(round(p@x, 2)), ",",
                     signs::signs(round(p@y, 2)), ")")
    } else if (is.numeric(label)) {
      label <- trimws(formatC(label, format = "fg", digits = 2))
    }

    non_empty_list <- get_non_empty_props(l_style)

    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }

    p@x <- d$x
    p@y <- d$y
    alpha = d[["alpha"]] %||% alpha
    new_object(
      S7_object(),
      label = d[["label"]],
      p = p,
      alpha = d[["alpha"]] %||% alpha,
      color = d[["color"]] %||% color,
      angle = d[["angle"]] %||% angle,
      family = d[["family"]] %||% family,
      fill = d[["fill"]] %||% fill,
      fontface = d[["fontface"]] %||% fontface,
      hjust = d[["hjust"]] %||% hjust,
      label.color = d[["label.color"]] %||% label.color,
      label.margin = d[["label.margin"]] %||% label.margin,
      label.padding = d[["label.padding"]] %||% label.padding,
      label.r = d[["label.r"]] %||% label.r,
      label.size = d[["label.size"]] %||% label.size,
      lineheight = d[["lineheight"]] %||% lineheight,
      nudge_x = d[["nudge_x"]] %||% nudge_x,
      nudge_y = d[["nudge_y"]] %||% nudge_y,
      polar_just = d[["polar_just"]] %||% polar_just,
      size = d[["size"]] %||% size,
      text.color = d[["text.color"]] %||% text.color,
      vjust = d[["vjust"]] %||% vjust,
      plot_point = plot_point
    )
  }
)


method(str, label) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c("label","p"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev)
}



method(get_tibble, label) <- function(x) {
  x@tibble
}

method(get_tibble_defaults, label) <- function(x) {
  sp <- style(
    alpha = 1,
    color = "black",
    angle = degree(0),
    family = "sans",
    fill = "fill",
    fontface = "plain",
    hjust = .5,
    label.color = "black",
    label.margin = class_margin(ggplot2::margin(2,2,2,2, "pt")),
    label.padding = class_margin(ggplot2::margin(1,1,1,1, "pt")),
    label.size = .25,
    lineheight = 1.2,
    nudge_x = 0,
    nudge_y = 0,
    size =  11,
    text.color = "black",
    vjust = .5
  )
  get_tibble_defaults_helper(x, sp, required_aes = c("x", "y", "label"))
}



method(as.geom, label) <- function(x, ...) {
  overides <- get_non_empty_props(style(...))
  if ("size" %in% names(overides)) {
    overides$size <- overides$size / ggplot2::.pt
  }

  d <- get_tibble_defaults(x)

  if ("size" %in% colnames(d)) {
    d <- dplyr::mutate(d, size = size / ggplot2::.pt)
  }


  gl <- make_geom_helper(
    d = d,
    .geom_x = ggtext::geom_richtext,
    user_overrides = overides,
    mappable_bare = c("angle", "family", "fontface", "hjust", "vjust", "label.size", "lineheight"),
    not_mappable = c("label.margin", "label.padding", "label.r", "nudge_x", "nudge_y"),
    required_aes = c("x", "y", "label"),
    omit_names = "group",
    inherit.aes = FALSE)

    if (x@plot_point) {

      gp <- as.geom(x@p)
      gl <- list(gl, gp)
    }
  gl

}
