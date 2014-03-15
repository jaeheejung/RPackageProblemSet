#' Regression analyses 
#' 
#' Object of class \code{regressionPack} are created by the \code{allRegressions} function
#'
#' 
#' An object of the class `regressionPack' has the following slots:
#' \itemize{
#' \item \code{x} A matrix of possible covariates
#' \item \code{y} A dependent variable
#' \item \code{coefficients} A matrix of coefficients
#' \item \code{Rsquared} A vector of Rsquared values
#' }
#'
#' @author Jae Hee Jung: \email{jaeheejung@@wustl.edu}
#' @aliases Regressions-class initialize,Regressions-method getRegressions,Regressions-method 
#' @rdname Regressions
#' @export
setClass(Class="Regressions",
	representation=representation(
		x="matrix",
		y="matrix",
		coefficients="numeric",
		Rsquared="numeric"
		),
	prototype=prototype(
		x=matrix(),
		y=matrix(),
		coefficients=numeric(),
		Rsquared=numeric()
		)
	)

#' @export
setMethod("initialize", "Regressions", 
     function(.Object, ...){
           value=callNextMethod()
           return(value)
         }
         ) 

#' @rdname Regressions
#' @export 
setGeneric("getRegressions",
     function(object="Regressions")  {
         standardGeneric("getRegressions")
       }
       )

#' @export
setMethod("getRegressions", "Regressions",
     function(object){ 
          return(list(coefficients=object@coefficients,Rsquared=object@Rsquared))
        }
        )