% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lines.R
\name{ob_line}
\alias{ob_line}
\title{ob_line class}
\usage{
ob_line(
  slope = class_missing,
  intercept = class_missing,
  xintercept = class_missing,
  a = class_missing,
  b = class_missing,
  c = class_missing,
  alpha = class_missing,
  color = class_missing,
  lineend = class_missing,
  linejoin = class_missing,
  linewidth = class_missing,
  linetype = class_missing,
  style = class_missing,
  ...
)
}
\arguments{
\item{slope}{coefficient in y = slope * x + intercept}

\item{intercept}{value of y when x is 0}

\item{xintercept}{value of x when y is 0}

\item{a}{coefficient in general form: a * x + b * y + c = 0}

\item{b}{coefficient in general form: a * x + b * y + c = 0}

\item{c}{constant in general form: a * x + b * y + c = 0}

\item{alpha}{numeric value for alpha transparency}

\item{color}{character string for color}

\item{lineend}{Line end style (round, butt, square).}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{linewidth}{Width of lines}

\item{linetype}{type of lines}

\item{style}{a style list}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> properties passed to style}
}
\description{
Creates a line
}
