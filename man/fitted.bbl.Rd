% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{fitted.bbl}
\alias{fitted.bbl}
\alias{fitted.values}
\title{Fitted Response Group Probabilities}
\usage{
\method{fitted}{bbl}(object, ...)
}
\arguments{
\item{object}{Object of class \code{bbl}.}

\item{...}{Other arguments}
}
\value{
Matrix of response group probabities with data points in rows and
        response groups in columns
}
\description{
Response group probabilities from BBL fit
}
\details{
This method returns predicted response group probabilities of trainig data
}
\examples{
titanic <- as.data.frame(Titanic)
fit <- bbl(Survived ~ Class + Sex + Age, data=titanic, weights=titanic$Freq)

}
