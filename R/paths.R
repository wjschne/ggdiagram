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
    })
  ),
  extra = list(
    label = label_or_character_or_angle,
    label_sloped = S7::class_logical
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
        if (inherits(self@p, "list")) {
          l <- length(self@p)
        } else l <- 1
        l
      }
    ),
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
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% path_styles]
        self <- rlang::inject(S7::set_props(self, !!!s_list))
        self
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
        stroke_width = self@stroke_width,
        id = self@id
      )
      get_non_empty_tibble(d)

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
          }) |>
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
    }),
    segments = S7::new_property(getter = function(self) {
      \(...) {
        purrr::map(self@p, \(s) ob_segment(s, style = self@style, ...)) |>
                     bind()
      }

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
#' An [`ob_path`] is specified with an [`ob_point`] object that contains at least 2 points, the start and the end. Any number of intermediate points are possible.
#'
#' If you wish to specify multiple paths, you must supply a list of [`ob_point`] objects. When plotted, the [`ob_path`] function uses the ggarrow::geom_arrow function to create the geom.
#' @export
#' @returns ob_path object
#' @param p [`ob_point`] or list of [`ob_point`]s
#' @param label A character, angle, or [`ob_label`] object
#' @param style Gets and sets the styles associated with paths
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> properties passed to style
#' @slot length The number of paths in the [`ob_path`] object
#' @slot tibble Gets a [`tibble::tibble`] containing parameters and styles used by [`ggarrow::geom_arrow`].
#' @inherit ob_style params
#' @inherit ob_bezier params
ob_path <- S7::new_class(
  name = "ob_path",
  parent = has_style,
  properties = rlang::list2(
      !!!path_props$primary,
      !!!path_props$extra,
      !!!path_props$styles,
      !!!path_props$derived,
      !!!path_props$funs,
      !!!path_props$info
  ),
  constructor = function(p = S7::class_missing,
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
                         ...) {

    id <- as.character(id)

    if (length(p) == 0) {
      stop("A path cannot have 0 points.")

    }

    if (S7::S7_inherits(p, ob_point)) p <- list(p)

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

    if (is.character(label) || is.numeric(label) || S7::S7_inherits(label, ob_angle)) {
      if (length(label)  > 0) {
        label = ob_label(label)
        label@style <- path_style + label@style
      }
    }

    if (length(label) == 0) label = character(0)
    # If there is one object but many labels, make multiple objects
    if (S7::S7_inherits(label, ob_label)) {
      if (label@length > 1 & nrow(d) == 1) {
        d <- dplyr::mutate(d, k = label@length) |>
          tidyr::uncount(.data$k)
      }
    }

    S7::new_object(.parent = S7::S7_object(),
               p =  d$p,
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
  })

S7::method(get_tibble, ob_path) <- function(x) {
  x@tibble |>
    dplyr::mutate(p = purrr::map(p, \(x) {
      x@tibble |> dplyr::select(x,y)
    })) |>
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
    aesthetics = x@aesthetics)

  if (S7::S7_inherits(x@label, ob_label)) {
    d <- tidyr::nest(d |> dplyr::select(x,y,group), .by = group) |>
      dplyr::bind_cols(x@label@tibble |> dplyr::select(-c(x,y)))



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


S7::method(`[`, ob_path) <- function(x, i) {
  i <- character_index(i, x@id)
  z <- data2shape(x@tibble[i,], ob_path)
  z@label <- na2zero(x@label[i])
  z
}

