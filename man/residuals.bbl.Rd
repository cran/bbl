% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{residuals.bbl}
\alias{residuals.bbl}
\title{Residuals of BBL fit}
\usage{
\method{residuals}{bbl}(object, ...)
}
\arguments{
\item{object}{Object of class \code{bbl}}

\item{...}{Other arguments}
}
\value{
Vector binary values for each data point
}
\description{
Binary-valued vector of fitted vs. true response group
}
\details{
Discrete response group identity for each data point is compared with 
the fitted group and 0 (discordant) or 1 (concordant) is returned
}
\examples{
titanic <- as.data.frame(Titanic)
dat <- freq2raw(titanic[,1:4], freq=titanic$Freq)
fit <- bbl(Survived ~ .^2, data=dat)
x <- residuals(fit)
table(x)
}
