lb_styles <- c("alpha", "angle", "color", "family","fill","fontface",
               "hjust","label.color","label.margin","label.padding",
               "label.r","label.size","lineheight","nudge_x","nudge_y",
               "polar_just","size","straight","text.color","vjust", "id")

gtextcurve_aes <- class_aesthetics_list(
  geom = purrr::partial(geomtextpath::geom_labelpath,rich = TRUE, arrow = NULL, text_only = TRUE),
  mappable_bare = c(
    "family",
    "fontface",
    "group",
    "hjust",
    "vjust",
    "lineheight",
    "spacing"),
  mappable_identity = c(
    "color",
    "fill",
    "size",
    "alpha",
    "textcolour",
    "boxcolour",
    "boxlinewidth"
  ),
  not_mappable = c(
    "halign",
    "label.padding",
    "label.r",
    "straight"
  ),
  required_aes = c("x", "y", "label"),
  omit_names = c("position", "id", "label.margin"),
  inherit.aes = FALSE,
  style = lb_styles
)

lb_props <- list(
  # primary ----
  primary = list(
    label = S7::new_property(class = S7::class_character),
    center = S7::new_property(class = ob_point)
  ),
  styles = ob_style@properties[lb_styles],
  # derived ----
  derived = list(
    auto_label = S7::new_property(getter = function(self) {
      label_object(self@center)
    }),
   length = S7::new_property(
      getter = function(self) {
        length(self@label)
      }
    ),
   style = S7::new_property(
     getter = function(self) {
       pr <- `names<-`(purrr::map(lb_styles,
                                  prop, object = self),
                       lb_styles)
       rlang::inject(ob_style(!!!get_non_empty_list(pr)))},
     setter = function(self, value) {
       s <- self@style + value
       s_list <- get_non_empty_props(s)
       s_list <- s_list[names(s_list) %in% lb_styles]
       self <- rlang::inject(set_props(self, !!!s_list))
       self
     }),
    tibble = S7::new_property(getter = function(self) {
      if (length(self@angle) > 0) {
        if (S7::S7_inherits(self@angle, ob_angle)) {
          self@angle <- self@angle@degree
        }
      }
      d <- list(x = self@center@x,
                y = self@center@y,
                label = self@label,
                spacing = self@spacing,
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
                straight = self@straight,
                size = self@size,
                text.color = self@text.color,
                vjust = self@vjust,
                id = self@id
                )
      d <- get_non_empty_tibble(d)
      if (!is.null(d$label.margin)) {
        d$label.margin <- purrr::map(d$label.margin, \(m) {
          if (S7::S7_inherits(m, class_margin)) m <- c(m)[[1]]
          m
        })
      }
      if (!is.null(d$label.padding)) {
        d$label.padding <- purrr::map(d$label.padding, \(m) {
          if (S7::S7_inherits(m, class_margin)) m <- c(m)[[1]]
          m
        })
      }
      d

    })
  ),
  # functions ----
  funs = list(
    geom = S7::new_property(S7::class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    place = pr_place
  ),
  extras = list(
    plot_point = S7::new_property(S7::class_logical, validator = function(value) {
      if (length(value) != 1) {
        stop("The plot_point property must be a TRUE/FALSE value of length 1.")
      }
    }),
    position = S7::new_property(S7::class_numeric),
    spacing = S7::new_property(S7::class_numeric)
  ),
  info = list(aesthetics = S7::new_property(
    getter = function(self) {
      class_aesthetics_list(
        geom = ggtext::geom_richtext,
        mappable_bare = c(
          "angle",
          "family",
          "fontface",
          "hjust",
          "vjust",
          "lineheight"),
        mappable_identity = c(
          "color",
          "fill",
          "size",
          "alpha",
          "text.color",
          "label.color",
          "label.size"
        ),
        not_mappable = c(
          "label.margin",
          "label.padding",
          "label.r",
          "nudge_x",
          "nudge_y"
        ),
        required_aes = c("x", "y", "label"),
        omit_names = c("group", "position", "straight", "id"),
        inherit.aes = FALSE,
        style = lb_styles
      )
    }
  ))
)


# ob_label----

#' ob_label class
#'
#' @param label text label
#' @param center [ob_point] indicating the center of the label
#' @param style a style list
#' @param plot_point plot center [ob_point] (default = FALSE)
#' @param spacing letter spacing for labels used with ob_path and ob_bezier
#' @param position position (0 to 1). Used to position a label on an [ob_segment], [ob_arc], [ob_path], or [ob_bezier]
#' @param x x-coordinate of center point. If specified, overrides x-coordinate of `@center`.
#' @param y x-coordinate of center point. If specified, overrides y-coordinate of `@center`.
#' @inherit ob_style params
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @export
#' @returns ob_label object
ob_label <- S7::new_class(
  name = "ob_label",
  parent = has_style,
  properties = rlang::list2(
    !!!lb_props$primary,
    !!!lb_props$styles,
    !!!lb_props$extras,
    !!!lb_props$derived,
    !!!lb_props$funs,
    !!!lb_props$info),
  constructor = function(label = character(0),
                         center = S7::class_missing,
                         angle = numeric(0),
                         alpha = numeric(0),
                         color = character(0),
                         family = character(0),
                         fill = character(0),
                         fontface = character(0),
                         hjust = numeric(0),
                         label.color = character(0),
                         label.margin = class_margin(ggplot2::margin(1,1,1,1,"pt")),
                         label.padding = class_margin(ggplot2::margin(2,2,2,2,"pt")),
                         label.r = numeric(0),
                         label.size = numeric(0),
                         lineheight = numeric(0),
                         polar_just = numeric(0),
                         nudge_x = numeric(0),
                         nudge_y = numeric(0),
                         size = numeric(0),
                         straight = logical(0),
                         text.color = character(0),
                         vjust = numeric(0),
                         style = S7::class_missing,
                         plot_point = FALSE,
                         position = .5,
                         spacing = numeric(0),
                         x = S7::class_missing,
                         y = S7::class_missing,
                         id = character(0),
                         ...) {

    id <- as.character(id)

    # If center is missing, assign x and y
    if (missing(center)) {
      if (missing(x)) {
        x <- 0
      }
      if (missing(y)) {
        y <- 0
      }
      center <- ob_point(x, y)
    } else {
      if (!missing(x)) {
        center@x <- x
      }
      if (!missing(y)) {
        center@y <- y
      }
    }

    is_center_segment <- FALSE
    if (S7::S7_inherits(center, ob_segment)) {
      alt_center = center
      center <- midpoint(alt_center)
      is_center_segment <- TRUE
    }

    is_center_arc <- FALSE
    if (S7::S7_inherits(center, ob_arc)) {
      alt_center <- center
      center <- midpoint(alt_center)
      is_center_arc <- TRUE
    }

    if (length(label.padding) > 0) {
      label.padding <- class_margin(label.padding)
    }

    if (length(label.margin) > 0) {
      label.margin <- class_margin(label.margin)
    }

    if (length(polar_just) > 0) {
      if (S7::S7_inherits(polar_just, ob_angle) ||
          is.numeric(polar_just)) {
        polar_just <- ob_polar(theta = radian(polar_just), r = 1.2)
      }
      hjust <- polar2just(polar_just@theta, polar_just@r, axis = "h")
      vjust <- polar2just(polar_just@theta, polar_just@r, axis = "v")
      polar_just <- S7::class_missing
    }

    # Default label is point location or arc degree
    if (missing(label)) {
      if (is_center_arc) {
        label = as.character(degree(alt_center@theta@degree))
      } else {
        label = paste0(
          "(",
          ifelse(
            rlang::is_integerish(center@x),
            center@x,
            signs::signs(center@x, accuracy = .1)
          ),
          ",",
          ifelse(
            rlang::is_integerish(center@y),
            center@y,
            signs::signs(center@y, accuracy = .1)
          ),
          ")"
        )

      }
    }

    d <- tibble::tibble(x = center@x, y = center@y, label = as.character(label))

    if (length(angle) > 0) {
      if (nrow(d) > 1 && length(angle) == 1) {
        if (S7::S7_inherits(angle, ob_angle)) {
          S7::S7_data(angle) <- rep(S7::S7_data(angle), nrow(d))
        } else {
          angle <- degree(rep(angle, nrow(d)))
        }
      }
    }

    p_style <- center@style
    p_style@size <- 12


    l_style <- p_style + ob_style(size = ggtext::GeomRichText$default_aes$size * ggplot2::.pt, label.color = NA, fill = "white") + style +
      ob_style(
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
        straight = straight,
        text.color = as.character(text.color),
        vjust = vjust,
        id = id
      )

    if (is_center_segment) {
      l_style@angle <- alt_center@line@angle@degree
      l_style@vjust = ifelse(length(l_style@vjust) == 0 ||
                               is.na(l_style@vjust),
                             0,
                             l_style@vjust)
    }

    if (is_center_arc) {
      l_style <- l_style +
        ob_style(polar2just = ob_polar(
          theta = (center - alt_center@center)@theta + turn(.5),
          r = 1.15))
    }

    non_empty_list <- get_non_empty_props(l_style)

    if (!is.null(non_empty_list$label.margin)) {
      non_empty_list$label.margin <- purrr::map(non_empty_list$label.margin, class_margin)
    }

    if (!is.null(non_empty_list$label.padding)) {
      non_empty_list$label.padding <- purrr::map(non_empty_list$label.padding, class_margin)
    }

    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(
        d,
        tibble::tibble(!!!non_empty_list))
    }

    center@x <- d$x
    center@y <- d$y

    S7::new_object(
      S7::S7_object(),
      label = d[["label"]],
      center = center,
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
      straight = d[["straight"]] %||% straight,
      text.color = d[["text.color"]] %||% text.color,
      vjust = d[["vjust"]] %||% vjust,
      plot_point = plot_point,
      position = position,
      spacing = spacing,
      id = d[["id"]] %||% id
    )
  }
)

label_or_character_or_angle <- S7::new_union(ob_label, S7::class_character, ob_angle)

# Centerpoint----
centerpoint <- S7::new_class(
  name = "centerpoint",
  parent = xy,
  properties = list(center = S7::new_property(
    class = ob_point,
    default = ob_point(0, 0)
  ),
  label = S7::new_property(label_or_character_or_angle),
  xy = S7::new_property(getter = function(self) {
    self@center@xy
  }))
)

S7::method(as.geom, centerpoint) <- function(x, ...) {
  gc <- as.geom(S7::super(x, has_style), ...)

  if (S7::S7_inherits(x@label, ob_label)) {
    gl <- as.geom(x@label)
    gc <- list(gc, gl)
  }
  gc
}

S7::method(`+`, list(centerpoint, ob_point)) <- function(e1, e2) { # nocov start
  e1@center <- e1@center + e2
  if (S7::S7_inherits(e1@label, ob_label)) e1@label@center <- e1@label@center + e2
  e1
} # nocov end

S7::method(`-`, list(centerpoint, ob_point)) <- function(e1, e2) { # nocov start
  e1@center <- e1@center - e2
  if (S7::S7_inherits(e1@label, ob_label)) e1@label@center <- e1@label@center - e2
  e1
} # nocov end

S7::method(`+`, list(ob_point, centerpoint)) <- function(e1, e2) { # nocov start
  e2@center <- e1 + e2@center
  if (S7::S7_inherits(e2@label, ob_label)) e2@label@center <- e2@label@center + e1
  e2
} # nocov end

S7::method(`-`, list(ob_point, centerpoint)) <- function(e1, e2) { # nocov start
  e2@center <- e1 - e2@center
  if (S7::S7_inherits(e2@label, ob_label)) e2@label@center <- e2@label@center - e1
  e2
} # nocov end

S7::method(`%|-%`, list(centerpoint, ob_point)) <- function(e1,e2) {
  `%|-%`(e1@center, e2)
  }

S7::method(`%|-%`, list(ob_point, centerpoint)) <- function(e1,e2) {
  `%|-%`(e1, e2@center)
  }

S7::method(`%|-%`, list(centerpoint, centerpoint)) <- function(e1,e2) {
  `%|-%`(e1@center, e2@center)
  }

S7::method(`%-|%`, list(centerpoint, ob_point)) <- function(e1,e2) {
  `%-|%`(e1@center, e2)
}

S7::method(`%-|%`, list(ob_point, centerpoint)) <- function(e1,e2) {
  `%-|%`(e1, e2@center)
  }

S7::method(`%-|%`, list(centerpoint, centerpoint)) <- function(e1,e2) {
  `%-|%`(e1@center, e2@center)
  }

S7::method(str, ob_label) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c("label","center"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev)
}



S7::method(get_tibble, ob_label) <- function(x) {
  d <- x@tibble
  if (!is.null(d$label.margin)) {
    d$label.margin <- purrr::map(d$label.margin, \(m) {
      if (S7::S7_inherits(m, class_margin)) m <- S7::S7_data(m)
      m
    })
  }

  if (!is.null(d$label.padding)) {
    d$label.padding <- purrr::map(d$label.padding, \(m) {
      if (S7::S7_inherits(m, class_margin)) m <- S7::S7_data(m)
      m
    })
  }

  d
}

S7::method(get_tibble_defaults, ob_label) <- function(x) {
  sp <- ob_style(
    alpha = replace_na(ggtext::GeomRichText$default_aes$alpha, 1),
    color = replace_na(ggtext::GeomRichText$default_aes$colour, "black"),
    angle = replace_na(ggtext::GeomRichText$default_aes$angle, degree(0)),
    family = replace_na(ggtext::GeomRichText$default_aes$family, "sans"),
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



S7::method(as.geom, ob_label) <- function(x, ...) {
  overides <- get_non_empty_props(ob_style(...))
  if ("size" %in% names(overides)) {
    overides$size <- overides$size / ggplot2::.pt
  }

  d <- get_tibble_defaults(x)

  if ("size" %in% colnames(d)) {
    d <- dplyr::mutate(d, size = size / ggplot2::.pt)
  }


  gl <- make_geom_helper(
    d = d,
    aesthetics = x@aesthetics,
    user_overrides = overides)

    if (x@plot_point) {

      gp <- as.geom(x@center)
      gl <- list(gl, gp)
    }
  gl

}

S7::method(label_object, ob_label) <- function(object, accuracy = .1) {
  label_object(object@center, accuracy = accuracy)
}



S7::method(`[`, ob_label) <- function(x, i) {
  i <- character_index(i, x@id)
  d <- x@tibble[i, ]

  if (all(is.na(d$label))) {
    return(character(0))
  }

  as.list(d) |>
    get_non_empty_tibble() |>
    data2shape(ob_label)
}

#' @export
`[<-.ggdiagram::ob_label` <- function(x, i, value) {
    if (!S7::S7_inherits(value, ob_label)) stop("value must be of class ob_label.")
   i <- character_index(i, x@id)
   d <- x@tibble |>
     dplyr::bind_rows(dplyr::filter(value@tibble, FALSE))
   d[i,] <- value@tibble

  if (all(is.na(d$label))) {
    return(character(0))
  }

   as.list(d) |>
     get_non_empty_tibble() |>
     data2shape(ob_label)
}


S7::method(unbind, ob_label) <- function(x) {
  purrr::map(seq(1, x@length), \(i) x[i])
}

centerpoint_label <- function(label, center, d, shape_name = "shape", ...) {

  if (S7::S7_inherits(label, ob_label)) {
    label@center <- center
  }

  if ((is.character(label) && length(label) > 0) || S7::S7_inherits(label, ob_angle) || is.numeric(label)) {
    label <- ob_label(label = label, center = center, fill = NA, ...)
  }



  if (length(label) > 0) {
    if (nrow(d) > 1) {
      if (!(label@length == 1 || label@length == nrow(d))) {
        stop(
          paste0(
            "Label length is ",
            label@length,
            ". It must be either of length 1 or compatible with the length of the ",
            shape_name,
            " (length = ",
            nrow(d),
            ")."
          )
        )
      }
    }

  } else {
    label <- character(0)
  }
  label
}

S7::method(nudge, list(ob_label, S7::class_numeric, S7::class_numeric)) <- function(object, x, y) {
  object@center <- object@center + ob_point(x, y)
  object
}

S7::method(nudge, list(ob_label, S7::class_numeric, S7::class_missing)) <- function(object, x, y) {
  object@center <- object@center + ob_point(x, 0)
  object
}

S7::method(nudge, list(ob_label, S7::class_missing, S7::class_numeric)) <- function(object, x, y) {
  object@center <- object@center + ob_point(0, y)
  object
}


S7::method(nudge, list(centerpoint, S7::class_numeric, S7::class_numeric)) <- function(object, x, y) {
  object@center <- object@center + ob_point(x, y)
  object
}

S7::method(nudge, list(centerpoint, S7::class_numeric, S7::class_missing)) <- function(object, x, y) {
  object@center <- object@center + ob_point(x, 0)
  object
}

S7::method(nudge, list(centerpoint, S7::class_missing, S7::class_numeric)) <- function(object, x, y) {
  object@center <- object@center + ob_point(0, y)
  object
}


S7::method(place, list(ob_label, ob_point)) <- function(
    x,
    from,
    where = "right",
    sep = 1) {
  where <- degree(where)
  p <- ob_polar(where, sep)
  x@center@x <- from@x + p@x
  x@center@y <- from@y + p@y
  x
}

S7::method(place, list(ob_label, ob_label)) <- function(
    x,
    from,
    where = "right",
    sep = 1) {
  where <- degree(where)
  p <- ob_polar(where, sep)
  x@center@x <- from@center@x + p@x
  x@center@y <- from@center@y + p@y
  x
}



