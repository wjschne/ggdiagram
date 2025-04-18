% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/points.R
\name{perpendicular_point}
\alias{perpendicular_point}
\alias{perpendicular_vertical}
\alias{\%|-\%}
\alias{perpendicular_horizontal}
\alias{\%-|\%}
\title{Find point perpendicular to 2 points}
\usage{
e1 \%|-\% e2

e1 \%-|\% e2
}
\arguments{
\item{e1}{first ob_point}

\item{e2}{second ob_point}
}
\value{
ob_point object

ob_point object
}
\description{
Find point perpendicular to 2 points
}
\examples{
x <- ob_point(0,0)
y <- ob_point(1,1)
# Find point perpendicular to x and y going vertically first
x \%|-\% y
# Find point perpendicular to x and y going horizontally first
x \%-|\% y
}
