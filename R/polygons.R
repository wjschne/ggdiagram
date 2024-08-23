pgon_styles <- c(
  "alpha",
  "color",
  "fill",
  "linewidth",
  "linetype"
)

pgon_aesthetics <- class_aesthetics_list(
  geom = ggplot2::geom_polygon,
  mappable_bare = character(0),
  mappable_identity = c(
    "color",
    "fill",
    "linewidth",
    "linetype",
    "alpha"),
  not_mappable = c(
    character(0)
  ),
  required_aes = c(
    "x",
    "y",
    "group"),
  omit_names = c(
    "rule",
    "label"),
  inherit.aes = FALSE,
  style = pgon_styles
)


pgon_props <- list(
  # primary ----
  primary = list(
    p = new_property(class = point_or_list, validator = function(value) {
      if ("list" %in% class(value)) {
        allsameclass(value, "point")
      }

      if (S7_inherits(value, point)) value <- list(value)

      chk_points <- purrr::imap_chr(value, \(x, idx) {
        if (x@length < 3) {
          paste0("Group ", idx, " needs at least 3 points. It has ", x@length, ".")
        } else {
          ""
        }
      }) |>
        paste0(collapse = "")
      if (nchar(chk_points) > 0) chk_points

    })
  ),
  extra = list(
    label = label_or_character_or_angle
  ),
  styles = style@properties[pgon_styles],
  # derived ----
  derived = list(
    centroid = new_property(point, getter = function(self) {
      d <- self@tibble
      gr <- dplyr::intersect(colnames(d), c("group", pt_styles))
      d <- dplyr::summarise(d, .by = dplyr::any_of(gr),
                       x = mean(x, na.rm = TRUE),
                       y = mean(y, na.rm = TRUE)) |>
        dplyr::select(-group)

      rlang::inject(point(!!!d))
    }),
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
        pr <- purrr::map(pgon_styles,
                         prop,
                         object = self
        ) %>%
          `names<-`(pgon_styles)
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
        color = self@color,
        fill = self@fill,
        linewidth = self@linewidth,
        linetype = self@linetype
      )
      get_non_empty_tibble(d) |>
        dplyr::mutate(p = purrr::map(p, \(x) {x@tibble |> dplyr::select(x,y)})) |>
        tidyr::unnest(p)

    })
  ),
  # functions ----
  funs = list(
  ),
  # info ----
  info = list(aesthetics = new_property(
    getter = function(self) {
      pgon_aesthetics
    }
  ))
)


# pgon ----

#' The pgon (polygon) class
#'
#' A polygon is specified with a point object that contains at least 3 points, the start and the end. Any number of intermediate points are possible. The pgon class could not be named 'polygon' because that function name is used in base R graphics.
#'
#' If you wish to specify multiple polygons, you must supply a list of point objects. When plotted, the pgon function uses the ggplot2::geom_polygon function to create the geom.
#' @export
#' @param p point object or list of point objects
#' @param length The number of polygons in the pgon object
#' @param ... properties passed to style
#' @param style Gets and sets the styles associated with polygons
#' @param tibble Gets a tibble (data.frame) containing parameters and styles used by `ggplot2::geom_polygon`.
pgon <- new_class(
  name = "pgon",
  parent = has_style,
  properties = rlang::inject(
    list(
      !!!pgon_props$primary,
      !!!pgon_props$extra,
      !!!pgon_props$styles,
      !!!pgon_props$derived,
      !!!pgon_props$funs,
      !!!pgon_props$info
    )
  ),
  constructor = function(p = class_missing,
                         label = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         linewidth = .75,
                         linetype = class_missing,
                         ...) {



    pgon_style <- style +
      style(
        alpha = alpha,
        color = color,
        fill = fill,
        linewidth = linewidth,
        linetype = linetype
      ) +
      style(...)

    non_empty_list <- get_non_empty_props(pgon_style)

    if (S7_inherits(p, point)) p <- list(p)

    d <- tibble::tibble(
      p = p
    )


    if (length(non_empty_list) > 0) {
      d <- dplyr::bind_cols(d, tibble::tibble(!!!non_empty_list))
    }





    if (length(label) == 0) {
      label = character(0)
    } else {
      centroid <- dplyr::bind_rows(
        purrr::imap(p, \(pp, idx) {
          tibble::as_tibble(pp@xy) |>
            dplyr::mutate(group = idx)})) |>
        dplyr::bind_rows() |>
        dplyr::summarise(.by = group,
                         x = mean(x),
                         y = mean(y)) |>
        select(-group) |>
        point()

      centroid@style <- pgon_style
      if (S7_inherits(label, ggdiagram::label)) {
        if (all(label@p@x == 0) && all(label@p@y == 0)) {
          label@p <- centroid
        }
        if (length(label@fill) == 0 || all(label@fill == "white")) {
          label@fill <- d[["fill"]] %||% fill
        }
      } else {
        label <- label(label, centroid, fill = d[["fill"]] %||% fill)
      }

      }

    new_object(.parent = S7_object(),
               p =  d$p,
               label = label,
               alpha = d[["alpha"]] %||% alpha,
               color = d[["color"]] %||% color,
               fill = d[["fill"]] %||% fill,
               linewidth = d[["linewidth"]] %||% linewidth,
               linetype = d[["linetype"]] %||% linetype
    )
  })


method(str, pgon) <- function(
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

method(print, pgon) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(get_tibble, pgon) <- function(x) {
  x@tibble
}


method(as.geom, pgon) <- function(x, ...) {
  gp <- as.geom(super(x, has_style), ...)
  if (S7_inherits(x@label, label)) {
    gl <- as.geom(x@label)
    gp <- list(gp, gl)
  }
  gp
}


method(connect, list(pgon, pgon)) <- function(x,y, ...) {
  centroid_segment <- segment(x@centroid, y@centroid)
  connect(intersection(x, centroid_segment), intersection(y, centroid_segment), ...)
}

method(connect, list(pgon, point)) <- function(x,y, ...) {
  centroid_segment <- segment(x@centroid, y)
  connect(intersection(x, centroid_segment), y, ...)
}

method(connect, list(point, pgon)) <- function(x,y, ...) {
  centroid_segment <- segment(x, y@centroid)
  connect(x, intersection(y, centroid_segment), ...)
}
