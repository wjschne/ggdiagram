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
      if (inherits(value, "list")) {
        allsameclass(value, "ob_point")
      }
    }, setter = function(self, value) {
      if (S7::S7_inherits(value, ob_point)) {
        self@p <- list(value)
      } else if (inherits(value, "list")) {
        self@p <- value
      } else {
        stop("Control points must be an ob_point object or a list of ob_point objects.")
      }
      return(self)
    } )
  ),
  extra = list(
    label = label_or_character_or_angle,
    label_sloped = S7::class_logical
  ),
  styles = ob_style@properties[path_styles],
  # derived ----
  derived = list(
    bounding_box = S7::new_property(getter = function(self) {
      d_rect <- get_tibble(self) |>
        dplyr::summarise(
          xmin = min(x),
          xmax = max(x),
          ymin = min(y),
          ymax = max(y)
        )

      ob_rectangle(
        southwest = ob_point(d_rect$xmin, d_rect$ymin),
        northeast = ob_point(d_rect$xmax, d_rect$ymax)
      )
    }),
    length = pt_props$derived$length,
    segment = S7::new_property(getter = function(self) {

      purrr::map(self@p, \(s) ob_segment(s, style = self@style)) |>
        bind()

    }),
    style = S7::new_property(
      getter = function(self) {
        pr <- purrr::map(path_styles, prop, object = self) |>
          `names<-`(path_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% path_styles]
        self <- rlang::inject(S7::set_props(self, !!!s_list))
        self
      }
    ),
    tibble = S7::new_property(getter = function(self) {
      p <- self@p
      d <- list(
        p = p,
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
        stroke_width = self@stroke_width,
        id = self@id
      )
      get_non_empty_tibble(d) |>
        dplyr::mutate(group = dplyr::row_number())
    }),
    vertex_angle = S7::new_property(getter = function(self) {
      a <- purrr::map(self@p, \(pp) {
        if (pp@length > 2) {

          aa <- purrr::map(seq(2, pp@length - 1), \(i) {

            a1 <- (pp[i - 1] - pp[i])@theta
            a2 <- (pp[i + 1] - pp[i])@theta
            a21 <- a1 - a2
            a21@positive

          }) %>% bind()



          if (pp[1] == pp[pp@length]) {
            a11 <- (pp[pp@length - 1] - pp[1])@theta -
              (pp[2] - pp[1])@theta
            a11@positive
            aa <- bind(list(aa, a11))
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
  info = list(
    aesthetics = S7::new_property(
      getter = function(self) {
        class_aesthetics_list(
          geom = ggarrow::geom_arrow,
          mappable_bare = character(0),
          mappable_identity = c(
            "color",
            "linewidth",
            "linetype",
            "alpha"
          ),
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
            "group"
          ),
          omit_names = c(
            "linejoin",
            "rule",
            "label"
          ),
          inherit.aes = FALSE,
          style = path_styles
        )
      }
    )
  )
)

# ob_path----

#' The ob_path class
#'
#' An [`ob_path`] is specified with an [`ob_point`] object that contains at least 2 points, the start and the end. Any number of intermediate points are possible.
#'
#' If you wish to specify multiple paths, you must supply a list of [`ob_point`] objects. When plotted, the [`ob_path`] function uses the ggarrow::geom_arrow function to create the geom.
#' @export
#' @returns ob_path object
#' @param p [`ob_point`] or list of [`ob_point`] objects
#' @param label A character, angle, or [`ob_label`] object
#' @param style Gets and sets the styles associated with paths
#' @inherit ob_style params
#' @inherit ob_bezier params
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @prop aesthetics A list of information about the path's aesthetic properties
#' @prop bounding_box A rectangle that contains all the paths
#' @prop geom A function that converts the object to a geom. Any additional parameters are passed to `ggarrow::geom_arrow`.
#' @prop length The number of paths in the [`ob_path`] object
#' @prop midpoint A function that selects 1 or more midpoints of the ob_segment. The `position` argument can be between 0 and 1. Additional arguments are passed to `ob_point`.
#' @prop tibble Gets a [`tibble::tibble`] containing parameters and styles used by [`ggarrow::geom_arrow`].
#' @prop segments Gets the segments from the path
#' @prop vertex_angle Gets angles at each vertex
#' @examples
#' ggdiagram() +
#'  ob_path(list(ob_point(c(0, 0, 4), c(0, 1, 4)),
#'               ob_point(c(1, 2, 5, 6), c(0, 1, 2, 0))), color = c("red", "blue"))
ob_path <- S7::new_class(
  name = "ob_path",
  parent = has_style,
  package = "ggdiagram",
  properties = rlang::list2(
    !!!path_props$primary,
    !!!path_props$extra,
    !!!path_props$styles,
    !!!path_props$derived,
    !!!path_props$funs,
    !!!path_props$info
  ),
  constructor = function(
    p = S7::class_missing,
    label = character(0),
    label_sloped = TRUE,
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
    id = character(0),
    ...
  ) {
    id <- as.character(id)

    if (length(p) == 0) {
      stop("A path cannot have 0 points.")
    }

    if (S7::S7_inherits(p, ob_point)) {
      p <- list(p)
    }

    p_style <- purrr::imap(p, \(x, idx) {
      if (x@length < 2) {
        stop(
          paste0(
            "A path object must have at least 2 points. Path group ",
            idx,
            " has ",
            length(x@length),
            " point",
            ifelse(x@length == 1, "", "s"),
            "."
          )
        )
      }

      purrr::map(unbind(x), \(xx) xx@style) |>
        purrr::reduce(`+`)
    })

    p_style <- bind(p_style)

    path_style <- p_style +
      style +
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
        stroke_width = stroke_width,
        id = id
      ) +
      ob_style(...)

    non_empty_list <- get_non_empty_props(path_style)

    d <- tibble::tibble(
      p = p
    )

    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }

    if (
      is.character(label) ||
        is.numeric(label) ||
        S7::S7_inherits(label, ob_angle)
    ) {
      if (length(label) > 0) {
        label = ob_label(label)
        label@style <- path_style + label@style
      }
    }

    if (length(label) == 0) {
      label = character(0)
    }
    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }

    S7::new_object(
      .parent = S7::S7_object(),
      p = d$p,
      label = label,
      label_sloped = label_sloped,
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
      stroke_width = d[["stroke_width"]] %||% stroke_width,
      id = d[["id"]] %||% id
    )
  }
)

S7::method(get_tibble, ob_path) <- function(x) {
  x@tibble |>
    dplyr::mutate(
      p = purrr::map(p, \(x) {
        x@tibble |> dplyr::select(x, y)
      })
    ) |>
    tidyr::unnest(p)
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
    aesthetics = x@aesthetics
  )

  if (S7::S7_inherits(x@label, ob_label)) {
    d <- tidyr::nest(d |> dplyr::select(x, y, group), .by = group) |>
      dplyr::bind_cols(x@label@tibble |> dplyr::select(-c(x, y)))

    if (!("hjust" %in% colnames(d))) {
      pos <- x@label@position
      pos[is.na(pos)] <- .5
      d <- dplyr::mutate(d, hjust = pos)
    }

    d <- tidyr::unnest(d, data)

    if ("size" %in% colnames(d)) {
      d <- dplyr::mutate(d, size = size / ggplot2::.pt)
    }

    if (!("boxcolour" %in% colnames(d))) {
      d <- dplyr::mutate(d, boxcolour = NA)
    }

    if ("label.padding" %in% colnames(d)) {
      d <- dplyr::mutate(
        d,
        label.padding = purrr::map_dbl(label.padding, \(lp) c(lp[1] / 96))
      )

    }



    gl <- make_geom_helper(
      d,
      aesthetics = gtextcurve_aes,
      user_overrides = NULL
    )
    gc <- list(gc, gl)
  }
  gc
}


S7::method(`[`, ob_path) <- function(x, i) {
  i <- character_index(i, x@id)
  z <- data2shape(x@tibble[i, ], ob_path)
  z@label <- na2zero(x@label[i])
  z
}

S7::method(str, ob_path) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c(""))
) {
  str_properties(
    object,
    omit = omit,
    nest.lev = nest.lev,
    additional = additional
  )
  purrr::walk(object@p, \(o) {
    str_properties(
      o,
      omit = omit_props(
        o,
        include = c("x", "y")
      ),
      nest.lev = 2,
      additional = TRUE
    )
  })
}

S7::method(midpoint, list(ob_path, S7::class_missing)) <- function(
    x,
    y,
    position = .5,
    ...
) {
  purrr::map2(unbind(x), position, \(xx, pos) {
    if (pos < 0 || pos > 1) {
      ob_point(NA_real_, NA_real_, ...)
    } else {
      total_distance <- sum(xx@segment@distance)
      mid_distance <- total_distance * pos
      tibble::tibble(s = unbind(xx@segment)) |>
        dplyr::mutate(d = purrr::map_dbl(s, distance),
                      end_cd = cumsum(d),
                      end_cp = end_cd / max(end_cd),
                      start_cd = dplyr::lag(end_cd, default = 0),
                      start_cp = dplyr::lag(end_cp, default = 0)) |>
        dplyr::select(s, d, start_cd, end_cd, start_cp, end_cp) |>
        dplyr::filter(pos >= start_cp & pos <= end_cp) |>
        dplyr::slice(1) |>
        dplyr::mutate(md = mid_distance - start_cd,
                      mp = md / d,
                      m = purrr::map(s, midpoint, position = mp)) |>
        dplyr::pull(m)
    }

  }) |>
    bind()
}
