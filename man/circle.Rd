% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circles.R
\name{circle}
\alias{circle}
\title{circle class}
\usage{
circle(
  center = point(0, 0),
  radius = 1,
  alpha = class_missing,
  color = class_missing,
  fill = class_missing,
  linewidth = class_missing,
  linetype = class_missing,
  n = class_missing,
  style = class_missing,
  ...
)
}
\arguments{
\item{center}{point at center of the circle}

\item{radius}{distance between center and edge circle}

\item{n}{number of points in circle (default = 360)}

\item{style}{a style object}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> arguments passed to style object if style is empty}
}
\description{
circle class
}
\examples{
# specify center point and radius
p <- point(0,0)
circle(p, radius = 6)
}
