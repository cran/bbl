% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample.R
\name{sample_xi}
\alias{sample_xi}
\title{Generate Random Samples from Boltzmann Distribution}
\usage{
sample_xi(nsample = 1, predictors = NULL, h, J, code_out = FALSE)
}
\arguments{
\item{nsample}{Sample size}

\item{predictors}{List of predictor factor levels.}

\item{h}{Bias parameter; see \code{\link{bbl}}.}

\item{J}{Interaction parameters; see \code{\link{bbl}}.}

\item{code_out}{Ouput in integer codes; \eqn{a_i = 0, \cdots, L_i-1}.
If \code{FALSE}, output in factors in \code{predictors}.}
}
\value{
Data frame of samples in rows and predictors in columns.
}
\description{
Random samples are drawn from Boltzmann distribution
}
\details{
All possible factor states are enumerated exhaustively using
input argument \code{predictors}. If the number of predictors \eqn{m}
or the number of factor levels \eqn{L_i} for each predictor \eqn{i}
are even moderately large (\eqn{m\ge 10} or \eqn{L_i\ge 5}), 
this function will likely hang because the number of all possible 
states grows exponentially.
}
\examples{
set.seed(512)
m <- 5
n <- 1000
predictors <- list()
for(i in 1:m) predictors[[i]] <- c('a','c','g','t')
par <- randompar(predictors)
xi <- sample_xi(nsample=n, predictors=predictors, h=par$h, J=par$J)
head(xi)
}
