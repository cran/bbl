% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{predict.bbl}
\alias{predict.bbl}
\title{Predict Response Group Using \code{bbl} Model}
\usage{
\method{predict}{bbl}(object, newdata, type = "link", verbose = 1, progress.bar = FALSE, ...)
}
\arguments{
\item{object}{Object of class \code{bbl} containing trained model}

\item{newdata}{Data frame of new data for which prediction is to
be made. Columns must contain all of those in \code{model@data}.
If column names are present, the columns will be matched 
based on them. Extra columns will be ignored. If column names
are not provided, the columns should exactly match 
\code{model@data} predictor parts. If \code{NULL}, replaced
by \code{model@data} (self-prediction).}

\item{type}{Return value type. If \code{'link'}, 
the logit scale probabilities. If \code{'prob'} the probability itself.}

\item{verbose}{Verbosity level}

\item{progress.bar}{Display progress of response group probability. Useful
for large samples.}

\item{...}{Other arguments to methods}
}
\value{
Data frame of predicted posterior probabilities with samples in rows
        and response groups in columns. The last column is the predicted
        response group with maximum probability.
}
\description{
Make prediction of response group identity based on trained model
}
\details{
This method uses a new data set for predictors and trained \code{bbl} model
parameters to compute posterior probabilities of response group 
identity.
}
\examples{
set.seed(154)

m <- 5
L <- 3
n <- 1000

predictors <- list()
for(i in 1:m) predictors[[i]] <- seq(0,L-1)
names(predictors) <- paste0('v',1:m)
par <- list(randompar(predictors=predictors, dJ=0.5),
            randompar(predictors=predictors, h0=0.1, J0=0.1, dJ=0.5))
dat <- randomsamp(predictors=predictors, response=c('ctrl','case'), par=par, 
                 nsample=n)
dat <- dat[sample(n),]
dtrain <- dat[seq(n/2),]
dtest <- dat[seq(n/2+1,n),]

model <- bbl(y ~ .^2, data=dtrain)
pred <- predict(model, newdata=dtest)
score <- mean(dtest$y==pred$yhat)
score

auc <- pROC::roc(response=dtest$y, predictor=pred$case, direction='<')$auc
auc
}
