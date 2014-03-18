#' @include Regressions.R

#' Regression analyses
#' 
#' Object of class \code{subRegressions} as created by the \code{evalRegressions} functions
#'
#' 
#' An object of the class `subRegressions' has the following slots:
#' \itemize{
#' \item \code{x} A matrix of possible covariates
#' \item \code{y} A dependent variable
#' \item \code{coefficients} A matrix of coefficients
#' \item \code{Rsquared} A vector of Rsquared values
#' \item \code{pvalues} A matrix of pvalues
#' }
#'
#' @author Jae Hee Jung: \email{jaeheejung@@wustl.edu}
#' @aliases subRegressions-class initialize,subRegressions-method getRegressions,subRegressions-method
#' @rdname Regressions
#' @export
setClass(Class="subRegressions",
         contains="Regressions",
         representation = representation(
           x="matrix",
		   y="matrix",
		   coefficients="matrix",
		   Rsquared="numeric",
		   pvalues="matrix"
           ),
         prototype = prototype(
           x=matrix(),
		   y=matrix(),
		   coefficients=matrix(),
		   Rsquared=numeric(),
		   pvalues=matrix()
           )
         )

#' @export
setMethod("initialize", "subRegressions", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
          ) 

#' @export
setMethod("getRegressions", "subRegressions",
          function(object){
            out <- object@pvalues
            return(out)
                 }
            )

##This function allows an object of class Regressions to be coerced to an object of subRegressions. Since the last slot called pvalues is missing in objects of class Regressions, the slot pvalues will be NA once the object is coerced into the subclass subRegressions.          
setAs(from="Regressions", to="subRegressions", 
      def=function(from){
        new("subRegressions",
            x=from@x,
            y=from@y,
           	coefficients=from@coefficients,
           	Rsquared=from@Rsquared
            )
      }
      )
