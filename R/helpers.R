# helpers ----
.namedpositions <- c(
  east = 0,
  right = 0,
  `east-northeast` = 22.5,
  northeast = 45,
  `top right` = 45,
  `above right` = 45,
  `north-northeast` = 67.5,
  north = 90,
  top = 90,
  above = 90,
  `north-northwest` = 112.5,
  northwest = 135,
  `top left` = 135,
  `above left` = 135,
  `west-northwest` = 157.5,
  west = 180,
  left = 180,
  `west-southwest` = 202.5,
  southwest = 225,
  `bottom left` = 225,
  `below left` = 225,
  `south-southwest` = 247.5,
  south = 270,
  bottom = 270,
  below = 270,
  `south-southeast` = 292.5,
  southeast = 315,
  `bottom right` = 315,
  `below right` = 315,
  `east-southeast` = 337.5
)

#' @keywords internal
#' @noRd
cardinalpoint <- function(x) {
  if (!all(x %in% names(.namedpositions))) {
    stop(paste0(
      "Position must be an angle, numeric, or one of these named positions:\n",
      stringr::str_wrap(paste0(names(.namedpositions), collapse = ", "))
    ))
  }
  unname(.namedpositions[x])
}

#' @keywords internal
#' @noRd
allsameclass <- function(l, classname) {
  classname[classname == "ob_polar"] <- "ob_point"
  classname <- paste0("ggdiagram::", classname)
  allsame <- all(sapply(lapply(l, class), function(x) {
    classname %in% x
  }))
  if (!allsame) {
    paste0("All items must be ", classname, ".")
  }
}

#' @keywords internal
#' @noRd
aes_injection <- function(bare_mapping, identity_mapping, omit_mapping = NULL) {
  identity_mapping <- setdiff(identity_mapping, c(bare_mapping, omit_mapping))
  bare_mapping <- setdiff(bare_mapping, omit_mapping)
  i_styles <- purrr::map(rlang::syms(identity_mapping), \(i) call("I", i))
  names(i_styles) <- identity_mapping
  b_styles <- rlang::syms(bare_mapping)
  names(b_styles) <- bare_mapping

  ggplot2::aes(!!!b_styles, !!!i_styles)
}


#' @keywords internal
#' @noRd
get_tibble_defaults_helper <- function(
    x,
    default_style,
    required_aes = c("x", "y")
) {
  d <- get_tibble(x) |>
    dplyr::select(-dplyr::any_of("id"))

  if ("x0" %in% required_aes && "x" %in% colnames(d)) {
    d <- dplyr::rename(d, dplyr::all_of(c(x0 = "x")))
  }

  if ("y0" %in% required_aes && "y" %in% colnames(d)) {
    d <- dplyr::rename(d, dplyr::all_of(c(y0 = "y")))
  }

  for (n in setdiff(colnames(d), required_aes)) {
    if (S7::prop_exists(default_style, n)) {
      d_prop <- S7::prop(default_style, n)
      if (!(is.null(d_prop) || identical(d_prop, list()))) {
        d_prop <- ifelse(is.vector(d_prop), d_prop, c(d_prop))
        missings <- is.na(`[[`(d, n))
        if (!all(missings) && any(missings)) {
          d[missings, n] <- d_prop
        }
      }
    }
  }
  d
}


#' @keywords internal
#' @noRd
get_non_empty_props <- function(x) {
  Filter(
    function(s) {
      ifelse(
        length(s) > 0,
        ifelse(
          S7::S7_inherits(s),
          length(S7::S7_data(s)) > 0,
          !rlang::is_function(s)
        ),
        FALSE
      )
    },
    props(x)
  )
}

#' @keywords internal
#' @noRd
get_non_empty_list <- function(l) {
  Filter(\(x) length(x) > 0, l)
}

#' @keywords internal
#' @noRd
get_non_empty_tibble <- function(d) {
  d <- Filter(\(x) length(x) > 0, d)
  d <- Filter(\(x) !is.null(x), d)
  tibble::as_tibble(d)
}

#' @keywords internal
#' @noRd
replace_na <- function(x, y) {
  if (rlang::is_quosure(x)) {
    return(y)
  }
  ifelse(is.na(x), y, x)
}

#' @keywords internal
#' @noRd
na2zero <- function(x) {
  if (is.character(x)) {
    if (all(is.na(x))) {
      x <- character(0)
    }
  }
  x
}

#' @keywords internal
#' @noRd
ob_array_helper <- function(
    x,
    k = 2,
    sep = 1,
    where = "east",
    anchor = "center",
    ...
) {
  if (x@length > 1) {
    stop("The shape must start with an object of length 1.")
  }

  dots <- rlang::list2(...)

  p <- as.list(seq(k))
  p[1] <- unbind(x)

  for (i in seq(2, k)) {
    p[i] <- unbind(place(x, p[[i - 1]], where = where, sep = sep))
  }
  p <- bind(p)

  bb <- p@bounding_box
  if (anchor == "center") {
    p_anchor <- bb@center
  } else {
    p_anchor <- bb@point_at(anchor)
  }

  p_center <- p@center - p_anchor + x@center

  l <- character(0)

  if (S7::S7_inherits(x@label, ob_label)) {
    if (x@label@length == 1) {
      l <- ob_label(
        subscript(x@label@label, seq(k)),
        style = x@style,
        center = p_center
      )
    }
  }

  if (is.null(dots$label)) {
    if (length(l) > 0) {
      dots$label <- l
    }
  } else {
    if (S7::S7_inherits(dots$label, ob_label)) {
      dots$label <- ob_label(
        center = p_center,
        label = dots$label@label,
        style = dots$label@style
      )
    }
  }

  list(
    x = x,
    k = k,
    sep = sep,
    anchor = anchor,
    p_center = p_center,
    dots = dots
  )
}


#' Create subscripts
#'
#' @param x text
#' @param subscript subscript
#' @param output Can be `markdown` (default) or `latex`
#'
#' @returns text
#' @export
#'
#' @examples
#' subscript("X", 1)
#' superscript("A", 2)
subscript <- function(
    x,
    subscript = seq_along(x),
    output = c("markdown", "latex")
) {
  output <- match.arg(output)
  if (output == "markdown") {
    paste0(x, "<sub>", subscript, "</sub>")
  } else {
    paste0(x, "_{", subscript, "}")
  }
}

#' Create superscript
#'
#' @param x string
#' @param superscript superscript
#' @param output Can be `markdown` (default) or `latex`
#'
#' @returns string
#' @rdname subscript
#' @export
superscript <- function(
    x,
    superscript = seq_along(x),
    output = c("markdown", "latex")
) {
  output <- match.arg(output)
  if (output == "markdown") {
    paste0(x, "<sup>", superscript, "</sup>")
  } else {
    paste0(x, "^{", superscript, "}")
  }
}


#' Centering signed numbers
#'
#'
#' A wrapper function for the signs::signs function. It adds a space to the right side of negative numbers so that it appear as if the minus sign does not affect the number's centering.
#' @param x a numeric vector
#' @param space a character to be added to negative numbers (defaults to a UTF-8 figure space)
#' @param encoding type of encoding (defaults to UTF-8)
#' @param ... parameters passed to signs:signs
#'
#' @returns a vector of numbers converted to characters
#' @export
signs_centered <- function(x, space = NULL, encoding = "UTF-8", ...) {
  if (is.null(space)) {
    space <- "\u2007"
  }
  x_new <- paste0(signs::signs(x, ...), ifelse(x < 0, space, ""))
  Encoding(x_new) <- encoding
  x_new
}

#' Probability rounding
#'
#' Rounds to significant digits, removing leading zeros.
#'
#' @param p probability
#' @param accuracy smallest increment
#' @param digits significant digits
#' @param max_digits maximum rounding digits
#' @param remove_leading_zero remove leading zero
#' @param round_zero_one round 0 and 1
#' @param phantom_text invisible text inserted on the right
#' @param phantom_color color of phantom text
#' @returns a character vector
#' @export
#' @examples
#' round_probability(c(0, 0.0012, 0.012, 0.12, 0.99, 0.992, 0.9997, 1), digits = 2)
round_probability <- function(
    p,
    accuracy = 0.01,
    digits = NULL,
    max_digits = NULL,
    remove_leading_zero = TRUE,
    round_zero_one = TRUE,
    phantom_text = NULL,
    phantom_color = NULL
) {
  if (is.null(digits)) {
    l <- scales::number(p, accuracy = accuracy)
  } else {
    abs_p <- abs(p)
    sig_digits <- abs(ceiling(log10(abs_p + abs_p / 1e+09)) - digits)
    pgt99 <- (abs_p > 0.99) & !is.na(p)
    sig_digits[pgt99] <- abs(ceiling(log10(1 - abs_p[pgt99])) - digits + 1)

    sig_digits[
      ceiling(log10(abs_p)) == log10(abs_p) &
        (-log10(abs_p) >= digits)
    ] <-
      sig_digits[
        ceiling(log10(abs_p)) == log10(abs_p) &
          (-log10(abs_p) >= digits)
      ] -
      1

    sig_digits[is.infinite(sig_digits)] <- 0

    l <- purrr::map2_chr(p, sig_digits, \(pp, ss) {
      if (is.na(pp)) {
        ""
      } else {
        formatC(pp, ss, format = "f", flag = "#")
      }
    })
  }
  if (remove_leading_zero) {
    l <- sub("^-0", "-", sub("^0", "", l))
  }
  if (round_zero_one) {
    l[p == 0] <- "0"
    l[p == 1] <- "1"
    l[p == -1] <- "-1"
  }
  if (!is.null(max_digits)) {
    if (round_zero_one) {
      l[round(p, digits = max_digits) == 0] <- "0"
      l[round(p, digits = max_digits) == 1] <- "1"
      l[round(p, digits = max_digits) == -1] <- "-1"
    } else {
      l[round(p, digits = max_digits) == 0] <- paste0(
        ".",
        paste0(rep("0", max_digits), collapse = "")
      )

      l[round(p, digits = max_digits) == 1] <- paste0(
        "1.",
        paste0(rep("0", max_digits), collapse = "")
      )

      l[round(p, digits = max_digits) == -1] <- paste0(
        "-1.",
        paste0(rep("0", max_digits), collapse = "")
      )
    }
  }
  l <- sub(pattern = "-", replacement = "\u2212", x = l)
  if (!is.null(phantom_text)) {
    phantom_text <- paste0(
      ifelse(p < 0, "\u2212", ""),
      phantom_text
    )
    if (is.null(phantom_color)) {
      phantom_color <- "white"
    }
    l <- paste0(
      l,
      "<span style='color: ",
      phantom_color,
      "'>",
      phantom_text,
      "</span>"
    )
  }

  Encoding(l) <- "UTF-8"
  dim(l) <- dim(p)
  l
}


#' @keywords internal
#' @noRd
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
}


#' @keywords internal
#' @noRd
.between <- function(x, lb, ub) {
  b <- cbind(lb = lb, ub = ub)
  ub <- apply(b, 1, max)
  lb <- apply(b, 1, min)
  (x >= lb) & (x <= ub)
}


#' Finds the "previous" (lag) or "next" (lead) values in a vector or object with values at the end of the vector recycled to the beginning.
#'
#' @param x A vector or ggdiagram object
#' @param n Positive integer of length 1, giving the number of positions to lag or lead by
#'
#' @returns A vector with the same type and size as x but with elements shifted and cycled by n.
#' @export
#'
#' @examples
#' lead_cycle(1:5)
#' lead_cycle(1:5, 2)
#' lag_cycle(1:5)
#' lag_cycle(1:5, 2)
lead_cycle <- function(x, n = 1L) {
  k <- length(x)
  if (k < 2L) {
    return(x)
  }

  ni <- as.integer(n)

  if (!((ni == n) && ni > 0L && ni < k)) {
    stop("n must be a positive integer less than length of x.")
  }

  x[c(seq(ni + 1, k), seq(1, ni))]
}

#' @rdname lead_cycle
#' @export
lag_cycle <- function(x, n = 1L) {
  k <- length(x)
  if (k < 2) {
    return(x)
  }

  ni <- as.integer(n)

  if (!((ni == n) && ni > 0 && ni < k)) {
    stop("n must be a positive integer less than length of x.")
  }
  x[c(seq(k - ni + 1, k), seq(1, k - ni))]
}


# data2shape ----

#' Make shapes from data
#'
#' Allows a data.frame or tibble to be converted to shape objects.
#' @param data data.frame or tibble
#' @param shape shape function
#' @returns shape object
#' @export
#' @examples
#' d <- data.frame(
#'   x = 1:2,
#'   y = 1:2,
#'   fill = c("blue", "forestgreen"),
#'   color = NA,
#'   radius = c(0.25, 0.5))
#'
#' data2shape(d, ob_circle)
data2shape <- function(data, shape) {
  l <- as.list(data)
  rlang::inject(shape(!!!l))
}


#' Function to calculate hierarchy depth in lavaan models
#'
#' @param x character vector of variables in a lavaan model
#' @param model character, lavaan fit object, or lavaan parameter table
#' @param depth initial depth
#' @param max_depth max depth at which to stop (prevents infinite loops for non-recursive models)
#'
#' @returns integer
#' @export
#'
#' @examples
#' model <- "X =~ X1 + X2"
#' get_depth("X", model = model)
#' get_depth("X1", model = model)
get_depth <- function(x, model, depth = 0L, max_depth = 20) {
  if (inherits(model, "character")) {
    model <- lavaan::lavaanify(model)
  }

  if (inherits(model, "lavaan")) {
    model <- lavaan::parametertable(model)
  }

  if (!all(c("op", "lhs", "rhs") %in% colnames(model))) {
    stop(
      "model must be a lavaan fit object, a lavaan parameter table, or a character vector specifying a lavaan model."
    )
  }

  purrr::map_int(
    x,
    \(xx) {
      get_depth_helper(
        xx,
        model = model,
        depth = depth,
        max_depth = 20
      )
    }
  )
}

#' Function to calculate hierarchy depth in lavaan models
#'
#' Helps with get_depth
#' @param x character vector of variables in a lavaan model
#' @param model character, lavaan fit object, or lavaan parameter table
#' @param depth initial depth
#' @param max_depth max depth at which to stop (prevents infinite loops for non-recursive models)
#' @keywords internal
#' @noRd
get_depth_helper <- function(x, model, depth = 0L, max_depth = 20) {
  # Does it have children?
  children <- model %>%
    dplyr::filter(.data$op == "=~", .data$lhs %in% x) %>%
    dplyr::pull(.data$rhs) %>%
    unique()

  # Detect infinite loops
  if (depth >= max_depth) {
    stop("Maximum depth reached. May be nonrecursive. ")
  }

  # Do children have children?
  if (length(children) > 0) {
    get_depth_helper(children, model, depth = depth + 1)
  } else {
    # No children, return depth
    return(depth + 1)
  }
}


# arrow gradients ----

#' Arrow segments with a color gradient
#'
#' ggarrow does not accept [grid::linearGradient] patterns as colors. This geom lets ggarrow draw each arrow and then replaces the arrow's fill with a gradient running from its first point (fin) to its last point (head).
#'
#' Gradients require a graphics device that supports them (e.g., ragg, svg, pdf, or cairo png). The `gradient` colors apply to all arrows in the layer. The gradient replaces the `colour` of the arrow body; the stroke is unchanged (set `stroke_colour = NA` to hide it).
#'
#' @inheritParams ggarrow::geom_arrow_segment
#' @param gradient Character vector of 2 or more colors, from fin to head. If `NULL`, the arrow is drawn as usual.
#' @param stops Numeric vector of gradient stops, the same length as `gradient`. Defaults to equal spacing.
#' @return A ggplot2 layer
#' @export
#' @examples
#' library(ggplot2)
#' d <- data.frame(x = c(0, 0), y = c(0, 1), xend = c(1, 1), yend = c(0.3, 0.9))
#' ggplot(d) +
#'   geom_arrow_segment_gradient(
#'     aes(x, y, xend = xend, yend = yend),
#'     gradient = c("#DEEFEC", "#AD7422", "#a41e35"),
#'     linewidth = 6,
#'     stroke_colour = NA
#'   )
geom_arrow_segment_gradient <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  gradient = c("white", "black"),
  stops = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  l <- ggarrow::geom_arrow_segment(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    ...,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
  l$geom <- GeomArrowSegmentGradient
  l$geom_params$gradient <- gradient
  l$geom_params$stops <- stops
  l
}

#' @keywords internal
#' @noRd
GeomArrowSegmentGradient <- ggplot2::ggproto(
  "GeomArrowSegmentGradient",
  ggarrow::GeomArrowSegment,
  # ggplot2 only passes parameters named in draw_panel's formals, so they are
  # spelled out here (same as ggarrow::GeomArrowSegment) rather than using `...`
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    linejoin = "round",
    linemitre = 10,
    lineend = "butt",
    na.rm = FALSE,
    arrow = list(head = ggarrow::arrow_head_wings(), fins = NULL, mid = NULL),
    length = list(head = 4, fins = 4, mid = 4),
    justify = 0,
    force_arrow = FALSE,
    mid_place = 0.5,
    resect = list(head = 0, fins = 0),
    sep = 0,
    distort = NULL,
    gradient = NULL,
    stops = NULL
  ) {
    grob <- ggplot2::ggproto_parent(ggarrow::GeomArrowSegment, self)$draw_panel(
      data,
      panel_params,
      coord,
      linejoin = linejoin,
      linemitre = linemitre,
      lineend = lineend,
      na.rm = na.rm,
      arrow = arrow,
      length = length,
      justify = justify,
      force_arrow = force_arrow,
      mid_place = mid_place,
      resect = resect,
      sep = sep,
      distort = distort
    )
    if (length(gradient) < 2L) {
      return(grob)
    }
    gradient_arrows(
      grob,
      gradient,
      stops %||% seq(0, 1, length.out = length(gradient))
    )
  }
)

#' Use the gradient geom only when a gradient is specified
#' @keywords internal
#' @noRd
geom_arrow_segment_maybe_gradient <- function(..., gradient = NULL) {
  if (length(gradient) > 1L) {
    geom_arrow_segment_gradient(..., gradient = gradient)
  } else {
    ggarrow::geom_arrow_segment(...)
  }
}

#' Replace the fill of each arrow grob with a fin-to-head gradient
#' @keywords internal
#' @noRd
gradient_arrows <- function(x, colors, stops) {
  if (inherits(x, "arrow_path")) {
    len <- vctrs::field(x$id_rle, "length")
    id <- rep(seq_along(len), len)
    px <- as.numeric(x$x)
    py <- as.numeric(x$y)
    first <- match(unique(id), id)
    last <- length(id) + 1L - match(unique(id), rev(id))
    x$gp$fill <- lapply(seq_along(first), function(i) {
      grid::linearGradient(
        colours = colors,
        stops = stops,
        x1 = grid::unit(px[first[i]], "native"),
        y1 = grid::unit(py[first[i]], "native"),
        x2 = grid::unit(px[last[i]], "native"),
        y2 = grid::unit(py[last[i]], "native"),
        default.units = "native",
        extend = "pad",
        group = TRUE
      )
    })
    return(x)
  }
  if (inherits(x, "gTree")) {
    x$children[] <- lapply(x$children, gradient_arrows, colors = colors, stops = stops)
  }
  x
}

#' Arrows and paths with a color gradient
#'
#' Like [geom_arrow_segment_gradient()], but for [ggarrow::geom_arrow()], which draws paths and curves. The gradient runs in a straight line from each path's first point (fin) to its last point (head), so it follows the curve only approximately for strongly curved paths.
#'
#' @inheritParams ggarrow::geom_arrow
#' @inheritParams geom_arrow_segment_gradient
#' @return A ggplot2 layer
#' @export
#' @examples
#' library(ggplot2)
#' d <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
#' ggplot(d) +
#'   geom_arrow_gradient(
#'     aes(x, y),
#'     gradient = c("#DEEFEC", "#a41e35"),
#'     linewidth = 6,
#'     stroke_colour = NA
#'   )
geom_arrow_gradient <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  gradient = c("white", "black"),
  stops = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  l <- ggarrow::geom_arrow(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    ...,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
  l$geom <- GeomArrowGradient
  l$geom_params$gradient <- gradient
  l$geom_params$stops <- stops
  l
}

#' @keywords internal
#' @noRd
GeomArrowGradient <- ggplot2::ggproto(
  "GeomArrowGradient",
  ggarrow::GeomArrow,
  # ggplot2 only passes parameters named in draw_panel's formals, so they are
  # spelled out here (same as ggarrow::GeomArrow) rather than using `...`
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    na.rm = FALSE,
    arrow = list(head = ggarrow::arrow_head_wings(), fins = NULL, mid = NULL),
    length = list(head = 4, fins = 4, mid = 4),
    justify = 0,
    force_arrow = FALSE,
    mid_place = 0.5,
    sep = 0,
    resect = list(head = 0, fins = 0),
    distort = NULL,
    gradient = NULL,
    stops = NULL
  ) {
    grob <- ggplot2::ggproto_parent(ggarrow::GeomArrow, self)$draw_panel(
      data,
      panel_params,
      coord,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      arrow = arrow,
      length = length,
      justify = justify,
      force_arrow = force_arrow,
      mid_place = mid_place,
      sep = sep,
      resect = resect,
      distort = distort
    )
    if (length(gradient) < 2L) {
      return(grob)
    }
    gradient_arrows(
      grob,
      gradient,
      stops %||% seq(0, 1, length.out = length(gradient))
    )
  }
)

#' Use the gradient geom only when a gradient is specified
#' @keywords internal
#' @noRd
geom_arrow_maybe_gradient <- function(..., gradient = NULL) {
  if (length(gradient) > 1L) {
    geom_arrow_gradient(..., gradient = gradient)
  } else {
    ggarrow::geom_arrow(...)
  }
}
