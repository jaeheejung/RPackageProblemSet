\docType{class}
\name{Regressions-class}
\alias{Regressions-class}
\alias{getCoefs}
\alias{getCoefs,Regressions-method}
\alias{getPvalues}
\alias{getPvalues,Regressions-method}
\alias{getR2s}
\alias{getR2s,Regressions-method}
\alias{getRegressions}
\alias{getRegressions,Regressions-method}
\alias{getRegressions,subRegressions-method}
\alias{initialize,Regressions-method}
\alias{initialize,subRegressions-method}
\alias{subRegressions-class}
\title{Regression analyses}
\usage{
getRegressions(object = "Regressions")

getCoefs(x, y, ...)

getR2s(x, y, ...)

getPvalues(x, y, ...)
}
\description{
Object of class \code{Regressions} is created by the
\code{allRegressions} function
}
\details{
An object of the class `Regressions' has the following
slots: \itemize{ \item \code{x} A matrix of possible
covariates \item \code{y} A dependent variable in matrix
form \item \code{coefficients} A matrix of coefficients
\item \code{Rsquared} A vector of Rsquared values }
}
\author{
Jae Hee Jung: \email{jaeheejung@wustl.edu}

Jae Hee Jung: \email{jaeheejung@wustl.edu}
}

