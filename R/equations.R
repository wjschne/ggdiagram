#' ob_latex class
#'
#' make a latex equation
#' @param tex LaTeX equation
#' @param center ob_point
#' @param width width (specify width or height but not both)
#' @param height height (specify width or height but not both)
#' @param border border space (in points) around image
#' @param family font family (installed on system) of plain text
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
    border = class_numeric,
    family = class_character,
    filename = class_character,
    color = class_color_or_character,
    force_recompile = new_property(class = class_logical, default = FALSE))
  )

method(as.geom, ob_latex) <- function(x, ...) {
  latex_color <- ""
  if (length(x@color) > 0) {
    if (S7_inherits(x@color, class_color)) {
      latex_color <- c(x@color)
    } else {
      latex_color <- c(class_color(x@color))
    }

    latex_color <- substr(latex_color, start = 2, stop = 7)
    latex_color <- paste0("\\color[HTML]{", latex_color, "} ")
  }



  txt <- paste0(
    "\\documentclass[border=",
    ifelse(length(x@border) > 0, x@border, 1),
    "pt]{standalone}\n\\usepackage{amsmath}\n\\usepackage{xcolor}\n",
    paste0("\\usepackage{fontspec}\n\\setmainfont{",ifelse(length(x@family) > 0, x@family, "Arial"), "}\n"),
    "\\begin{document}\n$",
    latex_color,
    x@tex,
    "$\n\\end{document}"
  )

  if (length(x@filename) == 0) {
    x@filename <- janitor::make_clean_names(x@tex)
  }

  file_pdf <- purrr::map2_chr(txt, x@filename, \(tx,fs) {
    tex_file <- paste0(fs, ".tex")
    cat(tx, file = tex_file)
    f_pdf <- paste0(fs, ".pdf")
    if (x@force_recompile || !file.exists(f_pdf)) {
      tinytex::xelatex(tex_file)
      }
    f_pdf
    })



  by_wh <- ifelse(length(x@width) > 0 || length(length(x@height) == 0), "width", "height")

  d <- tibble::tibble(x0 = x@center@x, y0 = x@center@y, image = file_pdf, size = ifelse(by_wh == "width",  replace_na(rlang::`%||%`(x@width, 1), 1), x@height))

  gg <- ggimage::geom_image(
    mapping = ggplot2::aes(
      x = x0,
      y = y0,
      image = image,
      size = I(size)),
    data = d,
    inherit.aes = FALSE,
    by = by_wh,
    ...
    )


  gg
}

method(`+`, list(class_ggplot, ob_latex)) <- function(e1, e2) {
  e1 + as.geom(e2)
}
