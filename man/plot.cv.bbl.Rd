% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{plot.cv.bbl}
\alias{plot.cv.bbl}
\title{Plot Cross-validation Outcome}
\usage{
\method{plot}{cv.bbl}(
  x,
  type = "b",
  log = "x",
  pch = 21,
  bg = "white",
  xlab = NULL,
  ylab = NULL,
  las = 1,
  ...
)
}
\arguments{
\item{x}{Object of class \code{cv.bbl} from a call to 
\code{\link{crossVal}}}

\item{type}{Symbol type in \code{\link{plot}}, present here to set default.}

\item{log}{Log scale argument to \code{\link{plot}}.}

\item{pch}{Symbol type code in \code{\link{par}}.}

\item{bg}{Symbol background color in \code{\link{par}}.}

\item{xlab}{X axis label}

\item{ylab}{Y axis label}

\item{las}{Orientation of axis labels in \code{\link{par}}.}

\item{...}{Other arguments to \code{\link{plot}}.}
}
\description{
Plot cross-validation score as a function of regularization parameter
}
\details{
This function will plot accuracy score as a function of regularization parameter
from a call to \code{\link{crossVal}}.
}
