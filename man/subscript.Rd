% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_early.R
\name{subscript}
\alias{subscript}
\alias{superscript}
\title{Create subscripts}
\usage{
subscript(x, subscript = seq(length(x)), output = c("markdown", "latex"))

superscript(x, superscript = seq(length(x)), output = c("markdown", "latex"))
}
\arguments{
\item{x}{string}

\item{subscript}{subscript}

\item{output}{Can be \code{markdown} (default) or \code{latex}}

\item{superscript}{superscript}
}
\value{
text

string
}
\description{
Create subscripts

Create superscript
}
\examples{
subscript("X", 1:3)
superscript(c("A", "B"), 2)
}
