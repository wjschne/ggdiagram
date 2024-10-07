#' ob_latex class
#'
#' make a latex equation
#' @param tex LaTeX equation
#' @param center ob_point
#' @param width width (specify width or height but not both)
#' @param height height (specify width or height but not both)
#' @slot rectangle gets or sets rectangle that contains the image
#' @param aspect_ratio alters the aspect ratio of the image
#' @param color set color of equation text
#' @param density image quality (dots per inch)
#' @param latex_packages load latex packages
#' @param preamble additional latex commands to load in preamble
#' @slot image raster bitmap
#' @param border border space (in points) around image
#' @param family font family (installed on system) of plain text
#' @param math_mode include dollar signs automatically. Set to `FALSE` when the latex command is not in math mode
#' @param filename bare file name without extension (e.g., `myequation`)
#' @param force_recompile Will re-run xelatex even if .pdf file exists already
#' @export
ob_latex <- new_class(
  name = "ob_latex",
  properties = list(
    tex = class_character,
    center = ob_point,
    width = class_numeric,
    height = class_numeric,
    rectangle = new_property(
      getter = function(self) {
        ob_rectangle(self@center, width = self@width, height = self@height)
    }, setter = function(self, value) {
      if (!S7_inherits(value, ob_rectangle)) {
        stop("The `rectangle` must be the output of the function `ob_rectangle`.")
        }
      self@center <- value@center
      self@width <- value@width
      self@height <- value@height
      self
    }),
    aspect_ratio = new_property(class_numeric, default = 1),
    border = class_numeric,
    family = class_character,
    math_mode = new_property(class_logical, default = TRUE),
    filename = class_character,
    color = class_color_or_character,
    density = new_property(class_numeric, default = 300),
    latex_packages = class_character,
    preamble = class_character,
    force_recompile = new_property(class = class_logical, default = TRUE),
    image = class_list),
  constructor = function(
    tex = class_missing,
    center = ob_point(0,0),
    width = class_missing,
    height = class_missing,
    aspect_ratio = 1,
    border = class_missing,
    family = class_missing,
    math_mode = TRUE,
    filename = class_missing,
    color = class_missing,
    density = 300,
    latex_packages = class_missing,
    preamble = class_missing,
    force_recompile = TRUE) {

    latex_color <- ""
    if (length(color) > 0) {
      if (S7_inherits(color, class_color)) {
        latex_color <- c(color)
      } else {
        latex_color <- c(class_color(color))
      }

      latex_color <- substr(latex_color, start = 2, stop = 7)
      latex_color <- paste0("\\color[HTML]{", latex_color, "} ")
    }

    lp <- ""
    if (length(latex_packages)) {
      lp <- paste0("\\usepackage{",
                   latex_packages,
                   "}\n",
                   collapse = "")
    }

    txt_border <- "border=1pt"
    if (length(border) > 1) {
      txt_border <- paste0("border={",paste0(border, "pt", collapse = " "),"}")
    }

    txt_family <- "Arial"
    if (length(family) > 0) {
      txt_family <- family
    }

    txt <- paste0(
      "\\documentclass[",
      txt_border,
      "]{standalone}\n\\usepackage{amsmath}\n\\usepackage{xcolor}\n",
      "\\usepackage{fontspec}\n\\setmainfont{", txt_family, "}\n",
      lp,
      "\n",
      preamble,
      "\n\\begin{document}\n",
      latex_color,
      ifelse(math_mode, "$", ""),
      tex,
      ifelse(math_mode, "$", ""),
      "\n\\end{document}"
    )

    if (length(filename) == 0) {
      filename <- janitor::make_clean_names(tex)
    }

    filename <- basename(filename)

    by_wh <- ifelse(length(width) > 0 || length(height) == 0, "width", "height")


    img_size <- ifelse(by_wh == "width",
                       ifelse(length(width) > 0, width, 1),
                       height)

    d <- tibble::tibble(tx = txt, fn = filename, imgsz = img_size, center = as.list(center))
    n <- nrow(d)

    image <- purrr::pmap_df(d, \(tx, fn, imgsz, center) {
      if (force_recompile || !file.exists(f_pdf)) {
        f_pdf <- paste0(fn, ".pdf")
        f_tex <- paste0(fn, ".tex")
        cat(tx, file = f_tex)
        if (tinytex::is_tinytex()) {
          try(tinytex::xelatex(f_tex))
        } else {
          try(shell(paste0("xelatex ", f_tex)))
        }
      }

      ps <- pdftools::pdf_pagesize(f_pdf)
      if (by_wh == "width") {
        img_width <- imgsz
        img_height <- img_width * ps$height / (ps$width * aspect_ratio)
      } else {
        img_height <- imgsz
        img_width <- aspect_ratio * img_height * ps$width / (ps$height)
      }

      i <- magick::image_read_pdf(f_pdf, density = density) %>%
        magick::image_raster(tidy = FALSE)

      file.remove(f_pdf)
      file.remove(f_tex)

      tibble::tibble(image = list(i), width = img_width, height = img_height)

    })


    new_object(
      S7_object(),
      tex = d$tx,
      center = bind(d$center),
      width = image$width,
      height = image$height,
      aspect_ratio = aspect_ratio,
      border = border,
      family = family,
      filename = filename,
      color = color,
      density = density,
      latex_packages = latex_packages,
      preamble = preamble,
      force_recompile = force_recompile,
      image = image$image,
      math_mode = math_mode
    )

  })



method(as.geom, ob_latex) <- function(x, ...) {

  purrr::pmap(list(x@image, x@width, x@height, as.list(x@center)), \(i,width, height, center) {

    ggplot2::annotation_raster(
      i,
      interpolate = TRUE,
      xmin = center@x - width / 2,
      xmax = center@x + width / 2,
      ymin = center@y - height / 2,
      ymax = center@y + height / 2
    )

    })

}

method(`+`, list(class_ggplot, ob_latex)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

method(place, list(ob_latex, centerpoint)) <- function(x, from, where = "right", sep = 1) {
  r1 <- x@rectangle
  x@rectangle <- place(r1, from, where, sep)
  x
}

method(place, list(ob_latex, ob_point)) <- function(x, from, where = "right", sep = 1) {
  r1 <- x@rectangle
  x@rectangle <- place(r1, from, where, sep)
  x
}
