% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gibbs.R
\name{mcSample}
\alias{mcSample}
\title{Sample Predictor Distributions}
\usage{
mcSample(object, nsteps = 1000, verbose = 1, progress.bar = TRUE)
}
\arguments{
\item{object}{Object of class \code{bbl}}

\item{nsteps}{Total number of MC steps}

\item{verbose}{Verbosity level of output}

\item{progress.bar}{Display progress bar}
}
\description{
Uses fitted BBL model to explore predictor distributions
}
\details{
After \code{bbl} fit, the resulting model is used by this function to sample 
predictor distributions in each response group and find the most likely
preditor set using MCMC.
}
\examples{
titanic <- as.data.frame(Titanic)
b <- bbl(Survived~., data=titanic[,1:4], weights=titanic$Freq)
pxy <- mcSample(b)
pxy
}
