

bz_styles <- c(
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
  "n",
  "resect",
  "resect_fins",
  "resect_head",
  "stroke_color",
  "stroke_width"
)



bz_props <- list(
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
    label_sloped = S7::new_property(S7::class_logical)
  ),
  styles = ob_style@properties[bz_styles],
  # derived ----
  derived = list(
    bounding_box = S7::new_property(getter = function(self) {

      d_rect <- get_tibble(self) |>
        tidyr::unnest(p_unnest) |>
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
    path = S7::new_property(getter = function(self) {
      get_tibble_defaults(self) |>
        dplyr::rename(p = p_unnest) |>
        dplyr::mutate(p = purrr::map(p, ob_point)) |>
        data2shape(ob_path)

    }),
    style = S7::new_property(
      getter = function(self) {
        pr <- purrr::map(bz_styles,
                         prop,
                         object = self
        ) |>
          `names<-`(bz_styles)
        rlang::inject(ob_style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        s <- self@style + value
        s_list <- get_non_empty_props(s)
        s_list <- s_list[names(s_list) %in% bz_styles]
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
        n = self@n,
        resect = self@resect,
        resect_fins = self@resect_fins,
        resect_head = self@resect_head,
        stroke_color = self@stroke_color,
        stroke_width = self@stroke_width,
        id = self@id
      )
      get_non_empty_tibble(d)

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
      \(position = .5, ...) {
        midpoint(self, position = position, ...)
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
          "n",
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
          "label",
          "label_sloped",
          "id"),
        inherit.aes = FALSE,
        style = bz_styles
      )
    }
  ))
)

# ob_bezier----

#' The ob_bezier (i.e., bezier curve) class
#'
#' The ob_bezier is specified with an ob_point object that contains at least 2 points, the start and the end. Such a "curve" would actually be a straight line segment. If three points are specified, the middle point is a control point, and a quadratic bezier curve will result. Higher-order bezier curves can be created by having more control points in the middle.
#'
#' If you wish to specify multiple bezier curves, you must supply a list of ob_point objects. When plotted, the ob_bezier function uses the bezier::bezier function to create the point coordinates of the curve and the ggarrow::geom_arrow function to create the geom.
#' @export
#' @returns ob_bezier object
#' @param p ob_point or list of ob_points
#' @param label A character, angle, or label object
#' @param label_sloped A logical value indicating whether the label should be sloped with the curve
#' @slot length The number of curves in the ob_bezier object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>  properties passed to style
#' @param style Gets and sets the styles associated with ob_beziers
#' @slot tibble Gets a tibble (data.frame) containing parameters and styles used by `ggarrow::geom_arrow`.
#' @inherit ob_style params
#' @slot geom A function that converts the object to a geom. Any additional parameters are passed to `ggarrow::geom_arrow`.
#' @slot midpoint A function that selects 1 or more midpoints of the ob_bezier. The `position` argument can be between 0 and 1. Additional arguments are passed to `ob_point`.
#' @slot aesthetics A list of information about the ob_bezier's aesthetic properties
#' @examples
#' control_points <- ob_point(c(0,1,2,4), c(0,4,0,0))
#' ggdiagram() +
#'   ob_bezier(control_points, color = "blue")
ob_bezier <- S7::new_class(
  name = "ob_bezier",
  parent = has_style,
  properties = rlang::list2(
      !!!bz_props$primary,
      !!!bz_props$extra,
      !!!bz_props$styles,
      !!!bz_props$derived,
      !!!bz_props$funs,
      !!!bz_props$info
  ),
  constructor = function(p = S7::class_missing,
                         label = character(0),
                         label_sloped = TRUE,
                         n = 100,
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

    if (S7::S7_inherits(p, ob_point)) p <- list(p)
    if (missing(p)) stop("Must specify 2 or more control points.")
    purrr::walk(p, \(pp) {
      if (!S7::S7_inherits(pp, ob_point)) {
        stop("Each item in list p must be an ob_point object of length 2 or more.")
      }
      if (pp@length < 1) {
        stop("Each item in list p must be an ob_point object of length 2 or more.")
      }
    })

    p_style <- purrr::map(p, \(x) {
      purrr::map(unbind(x), \(xx) xx@style) |>
        purrr::reduce(`+`)
    }) |>
      bind()

bz_style <- p_style + style +
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
        n = n,
        resect = resect,
        resect_fins = resect_fins,
        resect_head = resect_head,
        stroke_color = stroke_color,
        stroke_width = stroke_width
      ) +
  ob_style(...)

    non_empty_list <- get_non_empty_props(bz_style)

    if (S7::S7_inherits(p, ob_point)) p <- list(p)
    d <- tibble::tibble(
      p = p
    )
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }

    if (is.character(label) || is.numeric(label) || S7::S7_inherits(label, ob_angle)) {
      if (length(label)  > 0) {
        label = ob_label(label)
        label@style <- bz_style + label@style
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
      n = d[["n"]] %||% n,
      resect = d[["resect"]] %||% resect,
      resect_fins = d[["resect_fins"]] %||% resect_fins,
      resect_head = d[["resect_head"]] %||% resect_head,
      stroke_color = d[["stroke_color"]] %||% stroke_color,
      stroke_width = d[["stroke_width"]] %||% stroke_width,
      id = d[["id"]] %||% id
    )
  })



S7::method(str, ob_bezier) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c(""))) {

  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
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

S7::method(get_tibble, ob_bezier) <- function(x) {
  x@tibble |>
    dplyr::mutate(p = purrr::map(p, \(x) {
      x@tibble |> dplyr::select(x,y) |> as.matrix()
    })) |>
    dplyr::mutate(p_unnest = purrr::map2(p,n, \(pp,nn) {
      bezier::bezier(t = seq(0,1, length.out = nn),
                     p = pp) |>
        `colnames<-`(c("x", "y")) |>
        tibble::as_tibble()

    })) |>
    # tidyr::unnest(p) |>
    dplyr::select(-n, -p)
}


S7::method(get_tibble_defaults, ob_bezier) <- function(x) {
  sp <- ob_style(
    alpha = replace_na(as.double(ggarrow::GeomArrow$default_aes$alpha), 1),
    arrow_head = ggarrow::arrow_head_minimal(90),
    arrow_fins = ggarrow::arrow_fins_minimal(90),
    color = replace_na(ggarrow::GeomArrow$default_aes$colour, "black"),
    stroke_color = replace_na(ggarrow::GeomArrow$default_aes$colour, "black"),
    stroke_width = replace_na(ggarrow::GeomArrow$default_aes$stroke_width, 0.25),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggarrow::GeomArrow$default_aes$linewidth, .5),
    linewidth_head = replace_na(ggarrow::GeomArrow$default_aes$linewidth, 1),
    linewidth_fins = replace_na(ggarrow::GeomArrow$default_aes$linewidth, 1),
    linetype = replace_na(ggarrow::GeomArrow$default_aes$linetype, 1),
    n = 360
  )
  get_tibble_defaults_helper(x, sp,required_aes = c("group", "p_unnest"))
}


S7::method(as.geom, ob_bezier) <- function(x, ...) {

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



    if (all(x@label_sloped)) {

      d_label <- tidyr::nest(dplyr::select(d, p_unnest, group),
                             .by = group) |>
        dplyr::bind_cols(
          dplyr::select(x@label@tibble, -c(x, y))) |>
        tidyr::unnest(data)

      if ("size" %in% colnames(d_label)) {
        d_label <- dplyr::mutate(d_label,
                                 size = size / ggplot2::.pt)
      }


      if (!("boxcolour" %in% colnames(d_label))) {
        d_label <- dplyr::mutate(d_label, boxcolour = NA)
      }

      if (!("label.padding" %in% colnames(d_label))) {
        d_label <- dplyr::mutate(d_label, label.padding = unit(2, "pt"))
      } else {
        d_label <- dplyr::mutate(
          d_label,
          label.padding = purrr::map(label.padding, 1),
          .by = group)
      }

      gl <- make_geom_helper(
        d_label,
        aesthetics = gtextcurve_aes,
        user_overrides = NULL)

    } else {
      dpos <- tibble::tibble(group = unique(d$group),
                     pos = x@label@position)

      d_l <- dplyr::select(x@label@tibble, -c(x, y))


      d_label <- tidyr::unnest(d, p_unnest) |>
        dplyr::select(x,y,group) |>
        dplyr::left_join(dpos, by = "group") |>
        dplyr::mutate(x0 = dplyr::lag(x),
                      y0 = dplyr::lag(y),
                      .by = group) |>
        dplyr::filter(!is.na(x0)) |>
        dplyr::mutate(dist_xy = sqrt((x - x0) ^ 2 + (y - y0) ^ 2),
                      p = cumsum(dist_xy) / sum(dist_xy),
                      p0 = dplyr::lag(p, default = 0),
                      .by = group) |>
        dplyr::filter(p0 <= pos, pos <= p) |>
        dplyr::filter(p0 == min(p0), .by = group) |>
        dplyr::mutate(ppos =  (pos - p0) / (p - p0),
                      xpos = x0 + (x - x0) * ppos,
                      ypos = y0 + (y - y0) * ppos) |>
        dplyr::select(group, x = xpos, y = ypos) |>
        dplyr::bind_cols(d_l)

      if ("size" %in% colnames(d_label)) {
        d_label <- dplyr::mutate(d_label, size = size / ggplot2::.pt)
      }


      gl <- make_geom_helper(
        d = d_label,
        aesthetics = x@label@aesthetics,
        user_overrides = NULL)

    }



    gc <- list(gc, gl)

  }
  gc
}

S7::method(`[`, ob_bezier) <- function(x, i) {
  i <- character_index(i, x@id)
  z <- data2shape(x@tibble[i,], ob_bezier)
  z@label <- na2zero(x@label[i])
  z
}

S7::method(midpoint, list(ob_bezier, S7::class_missing)) <- function(x,y, position = .5, ...) {
  purrr::map(x@p, \(xx) {
    ob_point(bezier::bezier(t = position, p = xx@xy), ...)
    }) |>
    bind()
}

