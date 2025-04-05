path_styles <- c(
  "alpha",
  "arrow_head",
  "arrow_fins",
  "arrowhead_length",
  "length_head",
  "length_fins",
  "color",
  "fill",
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



path_props <- list(
  # primary ----
  primary = list(
    p = S7::new_property(class = point_or_list, validator = function(value) {
      if ("list" %in% class(value)) {
        allsameclass(value, "ob_point")
      }
    })
  ),
  extra = list(
    label = label_or_character_or_angle
  ),
  styles = ob_style@properties[path_styles],
  # derived ----
  derived = list(
    bounding_box = S7::new_property(getter = function(self) {

      d_rect <- self@tibble |>
        dplyr::summarise(xmin = min(x),
                         xmax = max(x),
                         ymin = min(y),
                         ymax = max(y))

      ob_rectangle(southwest = ob_point(d_rect$xmin, d_rect$ymin),
                northeast = ob_point(d_rect$xmax, d_rect$ymax))

    }),
    length = S7::new_property(
      getter = function(self) {
        if ("list" %in% class(self@p)) {
          l <- length(self@p)
        } else l <- 1
        l
      }
    ),
    segments = S7::new_property(getter = function(self) {
      purrr::map(self@p, ob_segment, style = self@style) %>%
        bind()
    }),
    style = S7::new_property(
      getter = function(self) {
        pr <- purrr::map(path_styles,
                         prop,
                         object = self
        ) |>
          `names<-`(path_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        ob_path(p = self@p,
                label = self@label,
                style = self@style + value,
                label_sloped = self@label_sloped)
      }
    ),
    tibble = S7::new_property(getter = function(self) {
      p <- self@p
      if (S7::S7_inherits(self@p, ob_point)) p <- list(p)
      d <- list(
        p = p,
        group = seq(1, self@length),
        alpha = self@alpha,
        arrow_head = self@arrow_head,
        arrow_fins = self@arrow_fins,
        arrowhead_length = self@arrowhead_length,
        length_head = self@length_head,
        length_fins = self@length_fins,
        color = self@color,
        fill = self@fill,
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
      get_non_empty_tibble(d) |>
        dplyr::mutate(p = purrr::map(p, \(x) {x@tibble |> dplyr::select(x,y)})) |>
        tidyr::unnest(p)

    }),
    vertex_angle = S7::new_property(getter = function(self) {
      a <- purrr::map(self@p, \(pp) {
        if (pp@length > 3) {
          aa <- purrr::map(seq(2,pp@length - 1), \(i) {
            a1 <- (pp[i - 1] - pp[i])@theta
            a2 <- (pp[i + 1] - pp[i])@theta
            a21 <- a1 - a2
            if (a21 < 0) a21 <- a21 + degree(360)
            a21
          }) %>%
            bind()

          if (pp[1] == pp[pp@length]) {
            al1 <- (pp[pp@length - 1] - pp[1])@theta -
              (pp[2] - pp[1])@theta
            if (al1 < 0) al1 <- al1 + degree(360)
           aa <- bind(list(aa, al1))

          }
          aa
        }
      })

      if (length(a) == 1) {
        a <- a[[1]]
      }
      a
    })
  ),
  # functions ----
  funs = list(
    geom = S7::new_property(S7::class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    midpoint = S7::new_property(S7::class_function, getter = function(self) {
      \(position = .5, ...) midpoint(self, position = position, ...)
    })
  ),
  # info ----
  info = list(aesthetics = S7::new_property(
    getter = function(self) {
      class_aesthetics_list(
        geom = ggarrow::geom_arrow,
        mappable_bare = character(0),
        mappable_identity = c(
          "color",
          "linewidth",
          "linetype",
          "alpha"),
        not_mappable = c(
          "lineend",
          "linejoin",
          "arrow_head",
          "arrow_fins",
          "length",
          "length_head",
          "length_fins",
          "length_mid",
          "resect",
          "resect_fins",
          "resect_head",
          "linemitre"
        ),
        required_aes = c(
          "x",
          "y",
          "group"),
        omit_names = c(
          "linejoin",
          "rule",
          "label"),
        inherit.aes = FALSE,
        style = path_styles
      )
    }
  ))
)

# ob_path----

#' The ob_path class
#'
#' A `ob_path` is specified with an `ob_point` object that contains at least 2 points, the start and the end. Any number of intermediate points are possible.
#'
#' If you wish to specify multiple paths, you must supply a list of `ob_point` objects. When plotted, the `ob_path` function uses the ggarrow::geom_arrow function to create the geom.
#' @export
#' @return ob_path object
#' @param p `ob_point` or list of `ob_point`s
#' @param label A character, angle, or label object
#' @param style Gets and sets the styles associated with paths
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @slot length The number of paths in the ob_path object
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggarrow::geom_arrow`.
#' @inherit ob_style params
ob_path <- S7::new_class(
  name = "ob_path",
  parent = has_style,
  properties = rlang::inject(
    list(
      !!!path_props$primary,
      !!!path_props$extra,
      !!!path_props$styles,
      !!!path_props$derived,
      !!!path_props$funs,
      !!!path_props$info
    )
  ),
  constructor = function(p = S7::class_missing,
                         label = character(0),
                         alpha = numeric(0),
                         arrow_head = S7::class_missing,
                         arrow_fins = S7::class_missing,
                         arrowhead_length = numeric(0),
                         length_head = numeric(0),
                         length_fins = numeric(0),
                         color = character(0),
                         fill = character(0),
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
                         ...) {

    if (S7::S7_inherits(p, ob_point)) p <- list(p)
    p_style <- purrr::map(p, \(x) {
      purrr::map(unbind(x), \(xx) xx@style) |>
        purrr::reduce(`+`)
    })

    p_style <- bind(p_style)

    path_style <- p_style + style +
      ob_style(
        alpha = alpha,
        arrow_head = arrow_head,
        arrow_fins = arrow_fins,
        arrowhead_length = arrowhead_length,
        length_head = length_head,
        length_fins = length_fins,
        color = color,
        fill = fill,
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
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(path_style)



    d <- tibble::tibble(
      p = p
    )

    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }



    if (length(label) == 0) {
      label = character(0)
      } else {
        d_l <- get_tibble(path_style)
        cnames <- dplyr::intersect(colnames(d_l), lb_styles)
        if (S7::S7_inherits(label, ob_label)) {
          if ("color" %in% cnames && all(length(label@color) == 0)) {
            label@color <- d_l$color
          }
        } else {
          label <- rlang::inject(ob_label(label = label, !!!d_l[, cnames]))
        }
      }

    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) %>%
          tidyr::uncount(.data$k)
      }
    }

    S7::new_object(.parent = S7::S7_object(),
               p =  d$p,
               label = label,
               alpha = d[["alpha"]] %||% alpha,
               arrow_head = d[["arrow_head"]] %||% arrow_head,
               arrow_fins = d[["arrow_fins"]] %||% arrow_fins,
               arrowhead_length = d[["arrowhead_length"]] %||% arrowhead_length,
               length_head = d[["length_head"]] %||% length_head,
               length_fins = d[["length_fins"]] %||% length_fins,
               color = d[["color"]] %||% color,
               fill = d[["fill"]] %||% fill,
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
  })

S7::method(str, ob_path) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c(""))) {

  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
  cat(" <points>\n")
  purrr::walk(object@p,
              \(o) {
                str_properties(
                  o,
                  omit = omit_props(
                    o,
                    include = c("x", "y")),
                  nest.lev = 2,
                  additional = TRUE)
              })

}



S7::method(get_tibble, ob_path) <- function(x) {
  x@tibble
}

S7::method(as.geom, ob_path) <- function(x, ...) {
  d <- get_tibble_defaults(x)
  if ("arrowhead_length" %in% colnames(d)) {
    d <- dplyr::rename(d, length = arrowhead_length)
  }

  overrides <- get_non_empty_props(ob_style(...))
  if (!("arrow_head" %in% c(colnames(d), names(overrides)))) {
    overrides$arrow_head <- ggarrow::arrow_head_minimal(90)
  }

  gc <- make_geom_helper(
    d = d,
    user_overrides = overrides,
    aesthetics = x@aesthetics)

  if (S7::S7_inherits(x@label, ob_label)) {
    d <- tidyr::nest(d |> dplyr::select(x,y,group), .by = group) |>
      dplyr::bind_cols(x@label@tibble |> select(-c(x,y)))



    if (!("hjust" %in% colnames(d))) {
      d <- dplyr::mutate(d, hjust = x@label@position)
    }

    d <- tidyr::unnest(d, data)

    if ("size" %in% colnames(d)) {
      d <- dplyr::mutate(d, size = size / ggplot2::.pt)
    }

    if (!("boxcolour" %in% colnames(d))) {
      d <- dplyr::mutate(d, boxcolour = NA)
    }

    if (!("label.padding" %in% colnames(d))) {
      d <- dplyr::mutate(d, label.padding = unit(2, "pt"))
    }

    d <- dplyr::mutate(d, label.padding = purrr::map_dbl(label.padding, \(lp) c(lp[1] / 96)))



    gl <- make_geom_helper(d, aesthetics = gtextcurve_aes, user_overrides = NULL)
    gc <- list(gc, gl)
  }
  gc
}


S7::method(`[`, ob_path) <- function(x, y) {
  d <- x@tibble[y,]
  dl <- d |>
    dplyr::select(-.data$x, -.data$y, -.data$group) |>
    unique() |>
    unbind()
  z <- rlang::inject(ob_path(p = x@p[y], !!!dl))
  z@label <- x@label[y]
  z
}
