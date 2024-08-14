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
  primary = list(p1 = new_property(class = point),
                 p2 = new_property(class = point)),
  extra = list(
    label = new_property(label_or_character_or_angle)
  ),
  styles = style@properties[sg_styles],
  # derived ----
  derived = list(
    distance = new_property(
      getter = function(self) {
        distance(self)
      }
    ),
    length = new_property(
      getter = function(self) {
        length(self@p1@x)
      }
    ),
    line = new_property(
      getter = function(self) {
        line(
          a = self@p1@y - self@p2@y,
          b = self@p2@x - self@p1@x,
          c = self@p1@x * self@p2@y - self@p2@x * self@p1@y,
          style = self@style
        )
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(sg_styles, prop, object = self) %>%
          `names<-`(sg_styles)
        rlang::inject(style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        segment(self@p1, self@p2, style = self@style + value)
      }
    ),
    tibble = new_property(
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
          stroke_width = self@stroke_width
        )
        get_non_empty_tibble(d)
      }
    )
  ),
  # functions ----
  funs = list(
    geom = new_property(class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    auto_label = new_property(
      class_function,
      getter = function(self) {
        \(
          label,
          position = .5,
          # offset_angle  = self@angle + degree(90),
          # polar_multiplier = 1.2,
          # polar_just = polar(self@angle, r = multiplier),
          # hjust = NULL,
          vjust = class_missing,
          angle = self@line@angle,
          ...
        ) {


          label(
            p = midpoint(self, position = position),
            label = purrr::map_chr(label, \(l) ifelse(is.numeric(l),signs::signs(l, accuracy = .01),l)),
            vjust = vjust,
            angle = angle,
            ...
          )
        }

      }
    ),
    midpoint = new_property(
      class_function,
      getter = function(self) {
        \(position = .5, ...) midpoint(self, position = position, ...)
      }
    ),
    nudge = new_property(
      class_function,
      getter = function(self) {
        \(x = 0, y = 0) nudge(self, x, y)
      }
    )
    # arrow_segment = new_property(
    #   class_function,
    #   getter = function(self) {
    #     purrr::partial(arrow_segment, p1 = self, p2 = class_missing)
    #   }
    # )
  ),
  # info ----
  info = list(
    aesthetics = new_property(class_function, getter = function(self) {
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
        omit_names = c("linejoin", "rule", "group", "label"),
        inherit.aes = FALSE,
        style = sg_styles
      )
    })
  )
)

# Segment----

#' segment class
#'
#' @param p1 starting point
#' @param p2 end point
#' @param style a style list
#' @param ... properties passed to style
#' @export
segment <- new_class(
  name = "segment",
  parent = shape,
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
  constructor = function(p1 = class_missing,
                         p2 = class_missing,
                         label = class_missing,
                         alpha = class_missing,
                         arrow_head = ggarrow::arrow_head_minimal(90),
                         arrow_fins = class_missing,
                         arrowhead_length = 4,
                         length_head = class_missing,
                         length_fins = class_missing,
                         color = class_missing,
                         lineend = class_missing,
                         linejoin = class_missing,
                         linewidth = .5,
                         linewidth_fins = class_missing,
                         linewidth_head = class_missing,
                         linetype = class_missing,
                         resect = class_missing,
                         resect_fins = class_missing,
                         resect_head = class_missing,
                         stroke_color = class_missing,
                         stroke_width = class_missing,
                         style = class_missing,
                         ...) {
    if (length(p1) == 0) {
      stop("p1 must be a point object with one or more points.")
    } else {
      if (length(p2) == 0) {
        if (p1@length == 1) {
          stop("If p2 is missing, p1 must be a point object with multiple points.")
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

    s_style <- p1@style + p2@style + style(
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
    ) + style + rlang::inject(style(!!!user_overrides))

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

    if (S7_inherits(label, ggdiagram::label)) {
      pos <- label@position
      if (all(label@p == point(0,0))) {
        label@p <- midpoint(p1,p2, position = pos)
      }
      if (all(length(label@angle) == 0)) {
        label@angle = (p2 - p1)@theta
      }
      label@style <- s_style + label@style
    }

    label <- centerpoint_label(
      label,
      midpoint(p1,p2, position = pos),
      d,
      "segment",
      style = s_style,
      vjust = 0,
      angle = (p2 - p1)@theta)



    p1 <- set_props(p1, x = d$p1x, y = d$p1y)
    p2 <- set_props(p2, x = d$p2x, y = d$p2y)

    new_object(
      S7_object(),
      p1 = p1,
      p2 = p2,
      label = label,
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
      stroke_width = d[["stroke_width"]] %||% stroke_width
    )
  }

)

method(`+`, list(segment, point)) <- function(e1, e2) {
  e1p1 <- e1@p1 + e2
  e1p2 <- e1@p2 + e2
  segment(e1p1, e1p2, style = e1@style)
}

method(`-`, list(segment, point)) <- function(e1, e2) {
  e1p1 <- e1@p1 - e2
  e1p2 <- e1@p2 - e2
  segment(e1p1, e1p2, style = e1@style)
}

method(`+`, list(point, segment)) <- function(e1, e2) {
  e2 + e1
}

method(`-`, list(point, segment)) <- function(e1, e2) {
  e2 - e1
}

method(str, segment) <- function(object,
                                 nest.lev = 0,
                                 additional = FALSE,
                                 omit = omit_props(object, include = c("p1", "p2"))) {
  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = FALSE)
}

method(midpoint, list(segment, class_missing)) <- function(x, y, position = .5, ...) {
  x@p1@style <- x@p1@style + x@style
  x@p2@style <- x@p2@style + x@style
  midpoint(x@p1, x@p2, position = position, ...)
}

method(get_tibble, segment) <- function(x) {
  x@tibble
}

method(get_tibble_defaults, segment) <- function(x) {
  sp <- style(
    alpha = replace_na(as.double(
      ggarrow::GeomArrowSegment$default_aes$alpha
    ), 1),
    arrow_head = ggarrow::arrow_head_minimal(90),
    arrow_fins = ggarrow::arrow_fins_minimal(90),
    color = replace_na(ggarrow::GeomArrowSegment$default_aes$colour, "black"),
    stroke_color = replace_na(ggarrow::GeomArrowSegment$default_aes$colour, "black"),
    stroke_width = replace_na(ggarrow::GeomArrowSegment$default_aes$colour, 0.25),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggarrow::GeomArrowSegment$default_aes$linewidth, .5),
    linewidth_head = replace_na(ggarrow::GeomArrowSegment$default_aes$linewidth, 1),
    linewidth_fins = replace_na(ggarrow::GeomArrowSegment$default_aes$linewidth, 1),
    linetype = replace_na(ggarrow::GeomArrowSegment$default_aes$linetype, 1),
  )
  get_tibble_defaults_helper(x, sp, required_aes = c("x", "y", "xend", "yend"))
}


method(as.geom, segment) <- function(x, ...) {
  d <- get_tibble_defaults(x)
  if ("arrowhead_length" %in% colnames(d)) {
    d <- dplyr::rename(d, length = arrowhead_length)
  }

   gs <- make_geom_helper(
    d = d,
    user_overrides = get_non_empty_props(style(...)),
    aesthetics = x@aesthetics
  )

   if (S7_inherits(x@label, label)) {
     gl <- as.geom(x@label)
     gs <- list(gs, gl)
   }
   gs

}


method(resect, list(segment, class_numeric)) <- function(x, distance, distance_end = distance) {
  d <- x@p2 - x@p1
  x@p1 <- x@p1 + polar(theta = d@theta, r = distance)
  x@p2 <- x@p2 + polar(theta = d@theta + turn(.5), r = distance_end)
  x
}



method(nudge, list(segment, class_numeric, class_numeric)) <- function(object, x, y) {
  object + point(x, y)
}

method(nudge, list(segment, class_missing, class_numeric)) <- function(object, x, y) {
  object + point(0, y)
}

method(nudge, list(segment, class_missing, class_missing)) <- function(object, x, y) {
  object
}


method(nudge, list(segment, class_numeric, class_missing)) <- function(object, x, y) {
  object + point(x, 0)
}

method(nudge, list(segment, point, class_missing)) <- function(object, x, y) {
  object + x
}

method(nudge, list(segment, point, class_numeric)) <- function(object, x, y) {
  object + (x + y)
}

method(nudge, list(segment, segment, class_missing)) <- function(object, x, y) {
  object + x
}



