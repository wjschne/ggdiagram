% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_early.R
\name{ob_variance}
\alias{ob_variance}
\title{create double-headed arrow paths indicating variance}
\usage{
ob_variance(
  x,
  where = "north",
  theta = 50,
  bend = 0,
  looseness = 1,
  arrow_head = arrowheadr::arrow_head_deltoid(d = 2.3, n = 100),
  resect = 2,
  ...
)
}
\arguments{
\item{x}{object}

\item{where}{angle or named direction (e.g.,northwest, east, below, left)}

\item{theta}{angle width}

\item{bend}{Angle by which the control points are rotated}

\item{looseness}{distance of control points as a ratio of the distance to the object's center (e.g., in a circle of radius 1, looseness = 1.5 means that that the control points will be 1.5 units from the start and end points.)}

\item{arrow_head}{A 2-column matrix of polygon points}

\item{resect}{A numeric(1) denoting millimeters or \if{html}{\out{<unit>}} to shorten the arrow head and fins.}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> properties passed to style}
}
\value{
Returns an object of type \code{ob_bezier}
}
\description{
create double-headed arrow paths indicating variance
}
