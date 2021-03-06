% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{predict.cv.bbl}
\alias{predict.cv.bbl}
\title{Predict using Cross-validation Object}
\usage{
\method{predict}{cv.bbl}(object, ...)
}
\arguments{
\item{object}{Object of class \code{cv.bbl}.}

\item{...}{Other parameters to \code{\link{predict.bbl}}.}
}
\value{
Data frame of prediction; see \code{\link{predict.bbl}}.
}
\description{
Use the optimal fitted model from cross-validation run to make prediction
}
\details{
This method will use the fitted model with maximum accuracy score returned
by a call to \code{\link{crossVal}} to make prediction on new data
}
