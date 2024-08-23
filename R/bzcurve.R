

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
    p = new_property(class = point_or_list, validator = function(value) {
      if ("list" %in% class(value)) {
        allsameclass(value, "point")
      }
    })
  ),
  extra = list(
    label = label_or_character_or_angle
  ),
  styles = style@properties[bz_styles],
  # derived ----
  derived = list(
    length = new_property(
      getter = function(self) {
        if ("list" %in% class(self@p)) {
          l <- length(self@p)
        } else l <- 1
        l
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(bz_styles,
                         prop,
                         object = self
        ) %>%
          `names<-`(bz_styles)
        rlang::inject(style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        point(self@x, self@y, style = self@style + value)
      }
    ),
    tibble = new_property(getter = function(self) {
      p <- self@p
      if (S7_inherits(self@p, point)) p <- list(p)
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
        stroke_width = self@stroke_width
      )
      get_non_empty_tibble(d) |>
        dplyr::mutate(p = purrr::map(p, \(x) {x@tibble |> dplyr::select(x,y) |> as.matrix()})) |>
        dplyr::mutate(p = purrr::map2(p,n, \(pp,nn) {
          bezier::bezier(t = seq(0,1, length.out = nn), p = pp) |>
            `colnames<-`(c("x", "y")) |>
            tibble::as_tibble()
        })) |>
        tidyr::unnest(p) |>
        dplyr::select(-n)

    })
  ),
  # functions ----
  funs = list(
    midpoint = new_property(class_function, getter = function(self) {
      \(position = .5, ...) midpoint(self, position = position, ...)
    })
  ),
  # info ----
  info = list(aesthetics = new_property(
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
          "label"),
        inherit.aes = FALSE,
        style = bz_styles
      )
    }
  ))
)

# bzcurve----

#' The bzcurve (i.e., bezier curve) class
#'
#' The bzcurve is specified with a point object that contains at least 2 points, the start and the end. Such a "curve" would actually be a straight line segment. If three points are specified, the middle point is a control point, and a quadratic bezier curve will result. Higher-order bezier curves can be created by having more control points in the middle.
#'
#' If you wish to specify multiple bezier curves, you must supply a list of point objects. When plotted, the bzcurve function uses the bezier::bezier function to create the point coordinates of the curve and the ggarrow::geom_arrow function to create the geom.
#' @export
#' @param p point object or list of point objects
#' @param length The number of curves in the bzcurve object
#' @param ... properties passed to style
#' @param style Gets and sets the styles associated with bzcurves
#' @param tibble Gets a tibble (data.frame) containing parameters and styles used by `ggarrow::geom_arrow`.
bzcurve <- new_class(
  name = "bzcurve",
  parent = has_style,
  properties = rlang::inject(
    list(
      !!!bz_props$primary,
      !!!bz_props$extra,
      !!!bz_props$styles,
      !!!bz_props$derived,
      !!!bz_props$funs,
      !!!bz_props$info
    )
  ),
  constructor = function(p = class_missing,
                         label = class_missing,
                         n = 360,
                         alpha = class_missing,
                         arrow_head = class_missing,
                         arrow_fins = class_missing,
                         arrowhead_length = class_missing,
                         length_head = class_missing,
                         length_fins = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         lineend = class_missing,
                         linejoin = class_missing,
                         linewidth = .75,
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



bz_style <- style +
      style(
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
      style(...)





    non_empty_list <- get_non_empty_props(bz_style)

    if (S7_inherits(p, point)) p <- list(p)
    d <- tibble::tibble(
      p = p
    )
    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }

    if (length(label) == 0) label = character(0)



    new_object(.parent = S7_object(),
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
      n = d[["n"]] %||% n,
      resect = d[["resect"]] %||% resect,
      resect_fins = d[["resect_fins"]] %||% resect_fins,
      resect_head = d[["resect_head"]] %||% resect_head,
      stroke_color = d[["stroke_color"]] %||% stroke_color,
      stroke_width = d[["stroke_width"]] %||% stroke_width
    )
  })



method(str, bzcurve) <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit = omit_props(object, include = c(""))) {

  str_properties(object,
                 omit = omit,
                 nest.lev = nest.lev,
                 additional = additional)
  cat(" <control points>\n")
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

method(get_tibble, bzcurve) <- function(x) {
  x@tibble
}


method(as.geom, bzcurve) <- function(x, ...) {

  d <- get_tibble_defaults(x)
  if ("arrowhead_length" %in% colnames(d)) {
    d <- dplyr::rename(d, length = arrowhead_length)
  }

  overrides <- get_non_empty_props(style(...))
  if (!("arrow_head" %in% c(colnames(d), names(overrides)))) {
    overrides$arrow_head <- ggarrow::arrow_head_minimal(90)
  }


  gc <- make_geom_helper(
    d = d,
    user_overrides = overrides,
    aesthetics = x@aesthetics)

  if (S7_inherits(x@label, label)) {
    d <- tidyr::nest(d |> dplyr::select(x,y,group), .by = group) |>
      dplyr::bind_cols(x@label@tibble |> select(-c(x,y))) |>
      tidyr::unnest(data)

    if ("size" %in% colnames(d)) {
      d <- dplyr::mutate(d, size = size / ggplot2::.pt)
    }

    if (!("boxcolour" %in% colnames(d))) {
      d <- dplyr::mutate(d, boxcolour = NA)
    }

    if (!("label.padding" %in% colnames(d))) {
      d <- dplyr::mutate(d, label.padding = unit(2, "pt"))
    }

    gl <- make_geom_helper(d, aesthetics = gtextcurve_aes, user_overrides = NULL)
    gc <- list(gc, gl)
  }
  gc
}
