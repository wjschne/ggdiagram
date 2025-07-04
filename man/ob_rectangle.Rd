% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rectangles.R
\name{ob_rectangle}
\alias{ob_rectangle}
\title{ob_rectangle class}
\usage{
ob_rectangle(
  center = S7::class_missing,
  width = numeric(0),
  height = numeric(0),
  east = S7::class_missing,
  north = S7::class_missing,
  west = S7::class_missing,
  south = S7::class_missing,
  northeast = S7::class_missing,
  northwest = S7::class_missing,
  southwest = S7::class_missing,
  southeast = S7::class_missing,
  angle = numeric(0),
  vertex_radius = numeric(0),
  label = character(0),
  alpha = numeric(0),
  color = character(0),
  fill = character(0),
  linewidth = numeric(0),
  linetype = numeric(0),
  style = S7::class_missing,
  x = numeric(0),
  y = numeric(0),
  id = character(0),
  ...
)
}
\arguments{
\item{center}{point at center of the rectangle}

\item{width}{width}

\item{height}{height}

\item{east}{right middle point}

\item{north}{top middle point}

\item{west}{left middle point}

\item{south}{top middle point}

\item{northeast}{upper right point}

\item{northwest}{upper left point}

\item{southwest}{lower left point}

\item{southeast}{lower right point}

\item{angle}{angle of text}

\item{vertex_radius}{A numeric or unit vector of length one, specifying the corner radius for rounded corners}

\item{label}{A character, angle, or label object}

\item{alpha}{numeric value for alpha transparency}

\item{color}{character string for color}

\item{fill}{character string for fill color}

\item{linewidth}{Width of lines}

\item{linetype}{type of lines}

\item{style}{a style object}

\item{x}{overrides x-coordinate in \code{center@x}}

\item{y}{overrides y-coordinate in \code{center@x}}

\item{id}{character string to identify object}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> arguments passed to style object}
}
\value{
ob_rectangle object
}
\description{
ob_rectangle class
}
\examples{
ob_rectangle(center = ob_point(0,0), width = 2, height = 2)
}
