% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{logLik.bbl}
\alias{logLik.bbl}
\title{Log likelihood for bbl object}
\usage{
\method{logLik}{bbl}(object, ...)
}
\arguments{
\item{object}{Object of class \code{bbl}}

\item{...}{Other arguments to methods}
}
\value{
An object of class \code{logLik}, the Log likelihood value
        and the attribute "df" (degrees of freedom), the number of
        parameters.
}
\description{
Compute log likelihood from a fitted \code{bbl} object
}
\details{
This method uses inferred parameters from calls to \code{bbl} 
and data to compute the log likelihood.
}
