% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paths.R
\name{ob_path}
\alias{ob_path}
\title{The ob_path class}
\usage{
ob_path(
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
)
}
\arguments{
\item{p}{\code{ob_point} or list of \code{ob_point}s}

\item{label}{A character, angle, or label object}

\item{alpha}{numeric value for alpha transparency}

\item{arrow_head}{A 2-column matrix of polygon points}

\item{arrow_fins}{A 2-column matrix of polygon points}

\item{arrowhead_length}{Determines the size of the arrow ornaments. This parameter becomes the \code{length} parameter in ggarrow functions. Numeric values set the ornament size relative to the linewidth. A unit value sets the ornament size in an absolute manner.}

\item{length_head}{Determines the size of the arrow head. Numeric values set the ornament size relative to the linewidth. A unit value sets the ornament size in an absolute manner. From ggarrow.}

\item{length_fins}{Determines the size of the arrow fins. Numeric values set the ornament size relative to the linewidth. A unit value sets the ornament size in an absolute manner. From ggarrow.}

\item{color}{character string for color}

\item{fill}{character string for fill color}

\item{lineend}{Line end style (round, butt, square).}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{linewidth}{Width of lines}

\item{linewidth_fins}{Line width for arrow fins}

\item{linewidth_head}{Line width for arrow fins}

\item{linetype}{type of lines}

\item{resect}{A numeric(1) denoting millimeters or \if{html}{\out{<unit>}} to shorten the arrow head and fins.}

\item{resect_fins}{A numeric(1) denoting millimeters or \if{html}{\out{<unit>}} to shorten the arrow fins}

\item{resect_head}{A numeric(1) denoting millimeters or \if{html}{\out{<unit>}} to shorten the arrow head.}

\item{stroke_color}{Color of point border line}

\item{stroke_width}{Stroke width in arrows}

\item{style}{Gets and sets the styles associated with paths}

\item{id}{character string to identify object}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> properties passed to style}
}
\value{
ob_path object
}
\description{
A \code{ob_path} is specified with an \code{ob_point} object that contains at least 2 points, the start and the end. Any number of intermediate points are possible.
}
\details{
If you wish to specify multiple paths, you must supply a list of \code{ob_point} objects. When plotted, the \code{ob_path} function uses the ggarrow::geom_arrow function to create the geom.
}
\section{Slots}{

\describe{
\item{\code{length}}{The number of paths in the ob_path object}

\item{\code{tibble}}{Gets a tibble (data.frame) containing parameters and styles used by \code{ggarrow::geom_arrow}.}
}}

