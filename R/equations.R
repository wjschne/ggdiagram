#' ob_latex class
#'
#' make a latex equation
#' @param tex LaTeX equation
#' @param center An [ob_point]
#' @param width width (specify width or height but not both)
#' @param height height (specify width or height but not both)
#' @param hjust horizontal adjustment. 0 means left justified, 1 means right justified, 0.5 means centered
#' @slot rectangle gets or sets rectangle that contains the image
#' @param aspect_ratio alters the aspect ratio of the image
#' @param color set color of equation text
#' @param fill set color of background rectangle
#' @param density image quality (dots per inch)
#' @param latex_packages load latex packages
#' @param preamble additional latex commands to load in preamble
#' @slot image raster bitmap
#' @param border border space (in points) around image
#' @param family font family (installed on system) of plain text
#' @param math_mode include dollar signs automatically. Set to `FALSE` when the latex command is not in math mode
#' @param filename bare file name without extension (e.g., `myequation`)
#' @param force_recompile Will re-run xelatex even if .pdf file exists already
#' @param delete_files Delete .tex and .pdf files after image is generated.
#' @inherit ob_style params
#' @export
#' @returns ob_latex object
ob_latex <- S7::new_class(
  name = "ob_latex",
  properties = list(
    tex = S7::class_character,
    center = ob_point,
    width = S7::class_numeric,
    height = S7::class_numeric,
    hjust = S7::class_numeric,
    vjust = S7::class_numeric,
    angle = ob_angle_or_character,
    rectangle = S7::new_property(
      getter = function(self) {
        cc <- rotate(ob_point((self@hjust - .5) * -1 * self@width,
                              (self@vjust - .5) * -1 * self@height),
                     self@angle) + self@center
        ob_rectangle(cc,
                     width = self@width,
                     height = self@height,
                     angle = self@angle)
    }, setter = function(self, value) {
      if (!S7::S7_inherits(value, ob_rectangle)) {
        stop("The `rectangle` must be the output of the function `ob_rectangle`.")
        }
      self@center <- value@center
      self@width <- value@width
      self@height <- value@height
      self@angle <- value@angle
      self
    }),
    aspect_ratio = S7::new_property(S7::class_numeric, default = 1),
    border = S7::class_numeric,
    family = S7::class_character,
    math_mode = S7::new_property(S7::class_logical, default = TRUE),
    filename = S7::class_character,
    color = class_color_or_character,
    fill = class_color_or_character,
    density = S7::new_property(S7::class_numeric, default = 300),
    latex_packages = S7::class_character,
    preamble = S7::class_character,
    force_recompile = S7::new_property(class = S7::class_logical, default = TRUE),
    delete_files = S7::new_property(S7::class_logical, default = TRUE),
    image = S7::class_list,
    place = pr_place),
  constructor = function(
    tex = character(0),
    center = ob_point(0,0),
    width = numeric(0),
    height = numeric(0),
    hjust = .5,
    vjust = .5,
    angle = 0,
    aspect_ratio = 1,
    border = numeric(0),
    family = character(0),
    math_mode = TRUE,
    filename = character(0),
    color = character(0),
    fill = "white",
    density = 300,
    latex_packages = character(0),
    preamble = character(0),
    force_recompile = TRUE,
    delete_files = TRUE,
    id = character(0)) {

    if (!S7::S7_inherits(angle, ob_angle)) angle <- degree(angle)

    # Make color in latex
    latex_color <- ""
    if (length(color) > 0) {
      latex_color <- class_color(color)@tex
    }

    # Add latex packages
    lp <- ""
    if (length(latex_packages)) {
      lp <- paste0("\\usepackage{",
                   latex_packages,
                   "}\n",
                   collapse = "")
    }

    fill_tex <- ""
    if (length(fill) > 0) {
      if (S7::S7_inherits(fill, class_color)) {
        latex_fill <- c(fill)
      } else {
        latex_fill <- c(class_color(fill))
      }

      mybackground <- paste0("\n\\definecolor{mybackground}{HTML}{",
                             substr(latex_fill, start = 2, stop = 7),
                             "}\n")

      fill_tex <- paste0(
        mybackground,
        r"(\newsavebox\pagecolorbox
\savebox\pagecolorbox{%
  \makebox[0pt]{\raisebox{-\paperheight}[0pt][0pt]{%
    \textcolor{mybackground}{\rule{2\paperwidth}{\paperheight}}}}}
\AtBeginDvi{\box\pagecolorbox})")
    }

    fill_tex[is.na(fill)] <- ""

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
      "\n",
      fill_tex,
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

    d <- tibble::tibble(tx = txt, fn = filename, imgsz = img_size, center = unbind(center), theta = angle@degree, hj = hjust, vj = vjust)
    n <- nrow(d)

    image <- purrr::pmap_df(d, \(tx, fn, imgsz, center, theta, hj, vj) {
      f_pdf <- paste0(fn, ".pdf")
      f_tex <- paste0(fn, ".tex")
      if (force_recompile || !file.exists(f_pdf)) {
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

      i <- magick::image_read_pdf(f_pdf, density = density)
      i <- magick::image_raster(i, tidy = FALSE)

      if (delete_files) {
        file.remove(f_pdf)
        file.remove(f_tex)
      }


      tibble::tibble(
        image = list(i),
        width = img_width,
        height = img_height,
        angle = theta,
        hjust = hj,
        vjust = vj
      )

    })


    S7::new_object(
      S7::S7_object(),
      tex = d$tx,
      center = bind(d$center),
      width = image$width,
      height = image$height,
      hjust = image$hjust,
      vjust = image$vjust,
      angle = degree(image$angle),
      aspect_ratio = aspect_ratio,
      border = border,
      family = family,
      filename = filename,
      color = as.character(color),
      fill = as.character(fill),
      density = density,
      latex_packages = latex_packages,
      preamble = preamble,
      force_recompile = force_recompile,
      delete_files = delete_files,
      image = image$image,
      math_mode = math_mode,
      id = id
    )

  })



S7::method(as.geom, ob_latex) <- function(x, ...) {

  purrr::pmap(list(x@image, x@width, x@height, unbind(x@center), unbind(x@angle), x@hjust, x@vjust, x@fill), \(i,width, height, center, angle, hjust, vjust, fill) {

    cc <- rotate(ob_point((hjust - .5) * -1 * width,
                          (vjust - .5) * -1 * height),
                 angle) + center


      bb <- ob_rectangle(cc,
                         width = width,
                         height = height,
                         angle = angle)@bounding_box

      if (round(angle@degree,10) != 0) {
      i <- magick::image_read(i) |>
        magick::image_background("#FFFFFF00") |>
        magick::image_rotate(-angle@degree) |>
        magick::image_raster(tidy = FALSE)
    }

    ggplot2::annotation_raster(
      i,
      interpolate = TRUE,
      xmin = bb@west@x,
      xmax = bb@east@x,
      ymin = bb@south@y,
      ymax = bb@north@y
    )

    })

}

S7::method(`+`, list(class_gg, ob_latex)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

if (packageVersion("ggplot2") >= "3.5.2.9000") {
S7::method(update_ggplot, list(ob_latex, class_ggplot)) <-
  function(object, plot, ...) {
    plot + as.geom(object)
  }
}

S7::method(place, list(ob_latex, centerpoint)) <- function(x, from, where = "right", sep = 1) {
  r1 <- x@rectangle
  x@rectangle <- place(r1, from, where, sep)
  x
}

 S7::method(place, list(ob_latex, ob_point)) <- function(x, from, where = "right", sep = 1) {
  r1 <- x@rectangle
  x@rectangle <- place(r1, from, where, sep)
  x
}
