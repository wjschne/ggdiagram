% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{ob_label}
\alias{ob_label}
\title{ob_label class}
\usage{
ob_label(
  label = character(0),
  center = S7::class_missing,
  angle = numeric(0),
  alpha = numeric(0),
  color = character(0),
  family = character(0),
  fill = character(0),
  fontface = character(0),
  hjust = numeric(0),
  label.color = character(0),
  label.margin = class_margin(ggplot2::margin(1, 1, 1, 1, "pt")),
  label.padding = class_margin(ggplot2::margin(2, 2, 2, 2, "pt")),
  label.r = numeric(0),
  label.size = numeric(0),
  lineheight = numeric(0),
  polar_just = numeric(0),
  nudge_x = numeric(0),
  nudge_y = numeric(0),
  size = numeric(0),
  straight = logical(0),
  text.color = character(0),
  vjust = numeric(0),
  style = S7::class_missing,
  plot_point = FALSE,
  position = 0.5,
  spacing = numeric(0),
  x = S7::class_missing,
  y = S7::class_missing,
  id = character(0),
  ...
)
}
\arguments{
\item{label}{text label}

\item{center}{ob_point indicating the center of the label}

\item{angle}{angle of text}

\item{alpha}{numeric value for alpha transparency}

\item{color}{character string for color}

\item{family}{font family}

\item{fill}{character string for fill color}

\item{fontface}{Can be plain, bold, italic, or bold.italic}

\item{hjust}{horizontal justification. 0 means left justified, 1 means right justified, 0.5 means horizontally centered}

\item{label.color}{Color of label outline.}

\item{label.margin}{Amount of distance around label. Unit vector of length four. Usually created with \code{ggplot2::margin}.}

\item{label.padding}{Amount of padding around label. Unit vector of length four. Usually created with \code{ggplot2::margin}.}

\item{label.r}{Radius of rounded corners. Defaults to 0.15 lines.}

\item{label.size}{Width of label outline.}

\item{lineheight}{Height of line of text}

\item{polar_just}{an angle, polar point, or point that alters hjust and vjust (polar polar_just not stored in style)}

\item{nudge_x}{Horizontal adjustment to nudge labels by.}

\item{nudge_y}{Vertical adjustment to nudge labels by.}

\item{size}{numeric size}

\item{straight}{logical. If TRUE, make bzpath label text straight instead of curved.}

\item{text.color}{Color of label text.}

\item{vjust}{vertical justification. 0 means bottom aligned, 1 means top aligned, 0.5 means vertically centered}

\item{style}{a style list}

\item{plot_point}{plot ob_point (default = FALSE)}

\item{position}{position (0 to 1)}

\item{spacing}{letter spacing for labels used with ob_path and ob_bezier}

\item{x}{x-coordinate of center point. If specified, overrides x-coordinate of \verb{@center}.}

\item{y}{x-coordinate of center point. If specified, overrides y-coordinate of \verb{@center}.}

\item{id}{character string to identify object}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> properties passed to style}
}
\value{
ob_label object
}
\description{
ob_label class
}
