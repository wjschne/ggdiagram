sg_styles <- c(
  "alpha",
  "arrow_head",
  "arrow_fins",
  "arrowhead_length",
  "color",
  "length_head",
  "length_fins",
  "lineend",
  "linejoin",
  "linewidth",
  "linewidth_fins",
  "linewidth_head",
  "linetype",
  "resect",
  "resect_fins",
  "resect_head",
  "stroke_color",
  "stroke_width"
)

sg_props <- list(
  # primary ----
  primary = list(p1 = S7::new_property(class = ob_point),
                 p2 = S7::new_property(class = ob_point)),
  # extra ----
  extra = list(
    label = S7::new_property(label_or_character_or_angle),
    label_sloped = S7::new_property(S7::class_logical)
  ),
  styles = ob_style@properties[sg_styles],
  # derived ----
  derived = list(
    bounding_box = S7::new_property(getter = function(self) {

      d_rect <- self@tibble |>
        dplyr::summarise(xmin = min(c(x,xend)),
                         xmax = max(c(x,xend)),
                         ymin = min(c(y,yend)),
                         ymax = max(c(y,yend)))

      ob_rectangle(southwest = ob_point(d_rect$xmin, d_rect$ymin),
                northeast = ob_point(d_rect$xmax, d_rect$ymax))

    }),
    distance = S7::new_property(
      getter = function(self) {
        distance(self)
      }
    ),
    length = S7::new_property(
      getter = function(self) {
        length(self@p1@x)
      }
    ),
    line = S7::new_property(
      getter = function(self) {
        ob_line(
          a = self@p1@y - self@p2@y,
          b = self@p2@x - self@p1@x,
          c = self@p1@x * self@p2@y - self@p2@x * self@p1@y,
          style = self@style
        )
      }
    ),
    style = S7::new_property(
      getter = function(self) {
        pr <- purrr::map(sg_styles, prop, object = self) |>
          `names<-`(sg_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% sg_styles]
        self <- rlang::inject(set_props(self, !!!s_list))
        self
      }
    ),
    tibble = S7::new_property(
      getter = function(self) {
        d <- list(
          x = self@p1@x,
          y = self@p1@y,
          xend = self@p2@x,
          yend = self@p2@y,
          alpha = self@alpha,
          arrow_head = self@arrow_head,
          arrow_fins = self@arrow_fins,
          arrowhead_length = self@arrowhead_length,
          length_head = self@length_head,
          length_fins = self@length_fins,
          color = self@color,
          lineend = self@lineend,
          linejoin = self@linejoin,
          linewidth = self@linewidth,
          linewidth_fins = self@linewidth_fins,
          linewidth_head = self@linewidth_head,
          linetype = self@linetype,
          resect = self@resect,
          resect_fins = self@resect_fins,
          resect_head = self@resect_head,
          stroke_color = self@stroke_color,
          stroke_width = self@stroke_width,
          id = self@id
        )
        get_non_empty_tibble(d)
      }
    )
  ),
  # functions ----
  funs = list(
    geom = S7::new_property(S7::class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    hatch = S7::new_property(S7::class_function, getter = function(self) {
      \(k = 1, sep = .05, height = .05, position = .5, ...) {
        h <- map_ob(self, \(s) {
          m <- s@midpoint(position = position)
          p_center <- ob_array(m, k = k, sep = sep, where = s@line@angle)
          p_top <- p_center + ob_polar(s@line@angle + degree(90), height)
          p_bottom <- p_center + ob_polar(s@line@angle - degree(90), height)
          ob_segment(p_top, p_bottom, style = s@style)
        })

        h@style <- h@style + ob_style(...)
        h

    }}),
    midpoint = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(position = .5, ...) midpoint(self, position = position, ...)
      }
    ),
    nudge = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(x = 0, y = 0) nudge(self, x, y)
      }
    ),
    set_label_x = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(x = NULL) {
          if (!S7::S7_inherits(self@label, ob_label)) stop("The ob_segment does not have a label.")
          if (is.null(x)) {
            x <- self[1]@label@center@x
          }
          self@label@center <- self@line@point_at_x(x)
          self
        }
      }
    ),
    set_label_y = S7::new_property(
      S7::class_function,
      getter = function(self) {
        \(y = NULL) {
          if (!S7::S7_inherits(self@label, ob_label)) stop("The ob_segment does not have a label.")
          if (is.null(y)) {
            y <- self[1]@label@center@y
          }
          self@label@center <- self@line@point_at_y(y)
          self
        }
      }
    )
  ),
  # info ----
  info = list(
    aesthetics = S7::new_property(S7::class_function, getter = function(self) {
      class_aesthetics_list(
        geom = ggarrow::geom_arrow_segment,
        mappable_bare = character(0),
        mappable_identity = c(
          "color",
          "linewidth",
          "linewidth_head",
          "linewidth_fins",
          "linetype",
          "arrow_head",
          "arrow_fins",
          "arrow_mid",
          "resect_head",
          "resect_fins",
          "alpha",
          "stroke_colour",
          "stroke_width"),
        not_mappable = c(
          "lineend",
          "linejoin",
          "arrow_head",
          'arrow_fins',
          "length",
          "length_head",
          "length_fins",
          "length_mid",
          "resect",
          "resect_fins",
          "resect_head",
          "linemitre"
        ),
        required_aes = c("x", "y", "xend", "yend"),
        omit_names = c("linejoin", "rule", "group", "label", "label_sloped"),
        inherit.aes = FALSE,
        style = sg_styles
      )
    })
  )
)

# ob_segment----

#' ob_segment class
#'
#' @param p1 starting point ([`ob_point`])
#' @param p2 end point ([`ob_point`])
#' @param label A character, angle, or [`ob_label`] object
#' @param label_sloped A logical value indicating whether the label should be sloped with the segment
#' @param x overrides the x-coordinate of p1
#' @param xend overrides the y-coordinate of p1
#' @param y overrides the x-coordinate of p2
#' @param yend overrides the y-coordinate of p2
#' @param style a style list
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to `ggarrow::geom_arrow_segment`.
#' @slot hatch A function that puts hatch (tally) marks on segments. Often used to indicate which segments have the same length. The `k` parameter controls how many hatch marks to display. The `height` parameter controls how long the hatch mark segment is. The `sep` parameter controls the separation between hatch marks when `k > 2`. Additional parameters sent to `ob_segment`.
#' @slot midpoint A function that selects 1 or more midpoints of the ob_segment. The `position` argument can be between 0 and 1. Additional arguments are passed to `ob_point`.
#' @slot nudge A function to move the segment by x and y units.
#' @inherit ob_style params
#' @export
#' @returns ob_segment object
ob_segment <- S7::new_class(
  name = "ob_segment",
  parent = shape,
  package = "ggdiagram",
  properties =  rlang::inject(
    list(
      !!!sg_props$primary,
      !!!sg_props$extra,
      !!!sg_props$styles,
      !!!sg_props$derived,
      !!!sg_props$funs,
      !!!sg_props$info
    )
  ),
  constructor = function(p1 = S7::class_missing,
                         p2 = S7::class_missing,
                         label = character(0),
                         label_sloped = TRUE,
                         alpha = numeric(0),
                         arrow_head = ggarrow::arrow_head_minimal(90),
                         arrow_fins = list(),
                         arrowhead_length = 7,
                         length_head = numeric(0),
                         length_fins = numeric(0),
                         color = character(0),
                         lineend = numeric(0),
                         linejoin = numeric(0),
                         linewidth = numeric(0),
                         linewidth_fins = numeric(0),
                         linewidth_head = numeric(0),
                         linetype = numeric(0),
                         resect = numeric(0),
                         resect_fins = numeric(0),
                         resect_head = numeric(0),
                         stroke_color = character(0),
                         stroke_width = numeric(0),
                         style = S7::class_missing,
                         x = S7::class_missing,
                         xend = S7::class_missing,
                         y = S7::class_missing,
                         yend = S7::class_missing,
                         id = character(0),
                         ...) {
    id <- as.character(id)

    if ((length(x) > 0) || (length(y) > 0)) {
      if (length(x) == 0) {
        x <- 0
      }
      if (length(y) == 0) {
        y <- 0
      }
      p1 <- ob_point(tibble::tibble(x = x, y = y))
    }

    if ((length(xend) > 0) || (length(yend) > 0)) {
      if (length(xend) == 0) {
        xend <- 0
      }
      if (length(yend) == 0) {
        yend <- 0
      }
      p2 <- ob_point(tibble::tibble(x = xend, y = yend))
    }

    if (length(p1) == 0) {
      stop("p1 must be a ob_point object with one or more points.")
    } else {
      if (length(p2) == 0) {
        if (p1@length < 2) {
          stop("If p2 is missing, p1 must be a ob_point object with multiple points.")
        } else {
          p2 <- p1
          p2 <- set_props(
            p2,
            x = p2@x[-1],
            y = p2@y[-1],
            alpha = p2@alpha[-1],
            color = p2@color[-1],
            shape = p2@shape[-1],
            size = p2@size[-1],
            stroke = p2@stroke[-1]
          )
          n <- p1@length * -1
          p1 <- set_props(
            p1,
            x = p1@x[n],
            y = p1@y[n],
            alpha = p1@alpha[n],
            color = p1@color[n],
            shape = p1@shape[n],
            size = p1@size[n],
            stroke = p1@stroke[n]
          )

        }
      }
    }

    user_overrides <- rlang::list2(...)

    s_style <- p1@style + p2@style + ob_style(
      id = id,
      alpha = alpha,
      arrow_head = arrow_head,
      arrow_fins = arrow_fins,
      arrowhead_length = arrowhead_length,
      length_head = length_head,
      length_fins = length_fins,
      color = color,
      lineend = lineend,
      linejoin = linejoin,
      linewidth = linewidth,
      linewidth_fins = linewidth_fins,
      linewidth_head = linewidth_head,
      linetype = linetype,
      resect = resect,
      resect_fins = resect_fins,
      resect_head = resect_head,
      stroke_color = stroke_color,
      stroke_width = stroke_width
    ) + style + rlang::inject(ob_style(!!!user_overrides))

    d <- get_non_empty_tibble(list(
      p1x = p1@x,
      p1y = p1@y,
      p2x = p2@x,
      p2y = p2@y
    ))


    d_names <- colnames(d)

    non_empty_list <- get_non_empty_props(s_style)

    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }
    pos <- .5

    if (S7::S7_inherits(label, ob_label)) {
      pos <- label@position
      if (all(label@center == ob_point(0,0))) {
        label@center <- midpoint(p1,p2, position = pos)
      }
      if (all(length(label@angle) == 0) && label_sloped) {
        label@angle = (p2 - p1)@theta
      }
      label@style <- s_style + label@style

    } else {
      if (length(label) > 0) {
        th <- degree(0)
        if (label_sloped) {
          th <- degree((p2 - p1)@theta)
        }
        label <- ob_label(
          label = label,
          center = midpoint(p1,p2, position = pos),
          vjust = ifelse(label_sloped, 0, 0.5),
          angle = th,
          fill = ifelse(label_sloped, NA, "white"))

        label@style <- s_style + label@style
      } else {
        label = character(0)
      }

    }






    # label <- centerpoint_label(
    #   label = label,
    #   center = midpoint(p1,p2, position = pos),
    #   d = d,
    #   shape_name = "ob_segment")

    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }

      p1 <- set_props(p1, x = d$p1x, y = d$p1y)
      p2 <- set_props(p2, x = d$p2x, y = d$p2y)




    S7::new_object(
      S7::S7_object(),
      p1 = p1,
      p2 = p2,
      label = label,
      label_sloped = label_sloped,
      alpha = d[["alpha"]] %||% alpha,
      arrow_head = d[["arrow_head"]] %||% arrow_head,
      arrow_fins = d[["arrow_fins"]] %||% arrow_fins,
      arrowhead_length = d[["arrowhead_length"]] %||% arrowhead_length,
      length_head = d[["length_head"]] %||% length_head,
      length_fins = d[["length_fins"]] %||% length_fins,
      color = d[["color"]] %||% color,
      lineend = d[["lineend"]] %||% lineend,
      linejoin = d[["linejoin"]] %||% linejoin,
      linewidth = d[["linewidth"]] %||% linewidth,
      linewidth_fins = d[["linewidth_fins"]] %||% linewidth_fins,
      linewidth_head = d[["linewidth_head"]] %||% linewidth_head,
      linetype = d[["linetype"]] %||% linetype,
      resect = d[["resect"]] %||% resect,
      resect_fins = d[["resect_fins"]] %||% resect_fins,
      resect_head = d[["resect_head"]] %||% resect_head,
      stroke_color = d[["stroke_color"]] %||% stroke_color,
      stroke_width = d[["stroke_width"]] %||% stroke_width,
      id = d[["id"]] %||% id
    )
  }

)

# arithmetic ----
purrr::walk(list(`+`, `-`, `*`, `/`, `^`), \(.f) {
  S7::method(.f, list(ob_segment, ob_segment)) <- function(e1, e2) { # nocov start
    e2@p1 <- .f(e1@p1, e2@p1)
    e2@p2 <- .f(e1@p2, e2@p2)
    e2@style <- e1@style + e2@style
    e2
  } # nocov end
})

S7::method(`+`, list(ob_segment, ob_point)) <- function(e1, e2) { # nocov start
  e1p1 <- e1@p1 + e2
  e1p2 <- e1@p2 + e2
  ob_segment(e1p1, e1p2, style = e1@style)
} # nocov end

S7::method(`-`, list(ob_segment, ob_point)) <- function(e1, e2) { # nocov start
  e1p1 <- e1@p1 - e2
  e1p2 <- e1@p2 - e2
  ob_segment(e1p1, e1p2, style = e1@style)
} # nocov end

S7::method(`+`, list(ob_point, ob_segment)) <- function(e1, e2) { # nocov start
  e2 + e1
} # nocov end

S7::method(`-`, list(ob_point, ob_segment)) <- function(e1, e2) { # nocov start
  e2 - e1
} # nocov end

S7::method(str, ob_segment) <- function(object,
                                 nest.lev = 0,
                                 additional = FALSE,
                                 omit = omit_props(object, include = c("p1", "p2"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = FALSE)
}

S7::method(midpoint, list(ob_segment, S7::class_missing)) <- function(x, y, position = .5, ...) {
  x@p1@style <- x@p1@style + x@style
  x@p2@style <- x@p2@style + x@style
  midpoint(x@p1, x@p2, position = position, ...)
}

S7::method(get_tibble, ob_segment) <- function(x) {
  x@tibble
}

S7::method(get_tibble_defaults, ob_segment) <- function(x) {
  sp <- ob_style(
    alpha = replace_na(as.double(
      ggarrow::GeomArrowSegment$default_aes$alpha
    ), 1),
    arrow_head = ggarrow::arrow_head_minimal(90),
    arrow_fins = ggarrow::arrow_fins_minimal(90),
    color = replace_na(ggarrow::GeomArrowSegment$default_aes$colour, "black"),
    stroke_color = replace_na(ggarrow::GeomArrowSegment$default_aes$colour, "black"),
    stroke_width = replace_na(ggarrow::GeomArrowSegment$default_aes$stroke_width, 0.25),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggarrow::GeomArrowSegment$default_aes$linewidth, .5),
    linewidth_head = replace_na(ggarrow::GeomArrowSegment$default_aes$linewidth, 1),
    linewidth_fins = replace_na(ggarrow::GeomArrowSegment$default_aes$linewidth, 1),
    linetype = replace_na(ggarrow::GeomArrowSegment$default_aes$linetype, 1),
  )
  get_tibble_defaults_helper(x, sp, required_aes = c("x", "y", "xend", "yend"))
}


S7::method(as.geom, ob_segment) <- function(x, ...) {
  d <- get_tibble_defaults(x)
  if ("arrowhead_length" %in% colnames(d)) {
    d <- dplyr::rename(d, length = arrowhead_length)
  }

   gs <- make_geom_helper(
    d = d,
    user_overrides = get_non_empty_props(ob_style(...)),
    aesthetics = x@aesthetics
  )

   if (S7::S7_inherits(x@label, ob_label)) {
     gl <- as.geom(x@label)
     gs <- list(gs, gl)
   }
   gs

}


S7::method(resect, list(ob_segment, S7::class_numeric)) <- function(x, distance, distance_end = distance) {
  d <- x@p2 - x@p1
  x@p1 <- x@p1 + ob_polar(theta = d@theta, r = distance)
  x@p2 <- x@p2 + ob_polar(theta = d@theta + turn(.5), r = distance_end)
  x
}



S7::method(nudge, list(ob_segment, S7::class_numeric, S7::class_numeric)) <- function(object, x, y) {
  object + ob_point(x, y)
}

S7::method(nudge, list(ob_segment, S7::class_missing, S7::class_numeric)) <- function(object, x, y) {
  object + ob_point(0, y)
}

S7::method(nudge, list(ob_segment, S7::class_missing, S7::class_missing)) <- function(object, x, y) {
  object
}


S7::method(nudge, list(ob_segment, S7::class_numeric, S7::class_missing)) <- function(object, x, y) {
  object + ob_point(x, 0)
}

S7::method(nudge, list(ob_segment, ob_point, S7::class_missing)) <- function(object, x, y) {
  object + x
}

S7::method(nudge, list(ob_segment, ob_point, S7::class_numeric)) <- function(object, x, y) {
  object + (x + y)
}

S7::method(nudge, list(ob_segment, ob_segment, S7::class_missing)) <- function(object, x, y) {
  object + x
}


S7::method(`[`, ob_segment) <- function(x, i) {
  i <- character_index(i, x@id)
  d <- as.list(x@tibble[i,])
  l <- na2zero(x@label[i])

  rlang::inject(ob_segment(label = l, !!!d))
}


S7::method(`==`, list(ob_segment, ob_segment)) <- function(e1, e2) {
  (e1@p1 == e2@p1) & (e1@p2 == e2@p2) # nocov
}


S7::method(equation, ob_segment) <- function(
    x,
    type = c("y", "general", "parametric"),
    output = c("markdown", "latex"),
    digits = 2) {
  equation(x@line,
           type = match.arg(type),
           output = match.arg(output),
           digits = digits)
}
