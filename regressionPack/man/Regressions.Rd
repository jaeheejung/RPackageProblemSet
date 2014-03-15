\docType{class}
\name{Regressions-class}
\alias{Regressions-class}
\alias{getCoefs}
\alias{getCoefs,Regressions-method}
\alias{getR2s}
\alias{getR2s,Regressions-method}
\alias{getRegressions}
\alias{getRegressions,Regressions-method}
\alias{initialize,Regressions-method}
\title{Regression analyses}
\usage{
getRegressions(object = "Regressions")

getCoefs(x, y, ...)

getR2s(x, y, ...)
}
\description{
Object of class \code{regressionPack} are created by the
\code{allRegressions} function
}
\details{
An object of the class `regressionPack' has the following
slots: \itemize{ \item \code{x} A matrix of possible
covariates \item \code{y} A dependent variable \item
\code{coefficients} A matrix of coefficients \item
\code{Rsquared} A vector of Rsquared values }
}
\author{
Jae Hee Jung: \email{jaeheejung@wustl.edu}
}

