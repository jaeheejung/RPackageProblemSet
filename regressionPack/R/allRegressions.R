#' Calculating regressions
#'
#' Finds coefficients and Rsquared values
#'
#' @param x A matrix of possible covariates
#' @param y A dependent variable in matrix form
#'
#' @return An object of class Regressions containing
#'  \item{x}{A matrix of possible covariates}
#'  \item{y}{A dependent variable in matrix form}
#'  \item{coefficients}{A matrix of coefficients}
#'  \item{Rsquared}{A vector of Rsquared values} 
#' @author Jae Hee Jung
#' @examples
#' 
#' myX <- matrix(data=c(0.31,0.33,-2.81,1.35,-0.48,0.14,3.84,-0.7,-0.67,-0.74,-0.37,-3.99,1.46,-0.89,0.27,-0.96,-0.92,-2.42,0.63,-1.44,-1.22,-2.36,2.7,3.79,-2.12,-3.46,2.77,-0.76,0.77,-0.9),nrow=10,ncol=3,dimnames=list(NULL,c("A","B","C")))
#' myY <- matrix(c(-1.82,2.49,0.08,1.04,1.61,0.48,0.78,-0.79,1.79,-0.29),dimnames=list(NULL,"D"))
#' allRegressions(myX,myY)
#' @rdname allRegressions
#' @aliases allRegressions,Regressions-method
#' @export
setGeneric(name="allRegressions",
        def=function(x,y,...)
        {standardGeneric("allRegressions")}
           )

#' @export ##This is the main function. It takes as input two arguments in matrix form (which occupy the slots x and y in the object of class Regressions). The function returns an object of class Regressions with the slots x, y, coefficients, and Rsquared. The last two slots use the functions that I created in the file Regressions.R.
setMethod(f="allRegressions",c("matrix","matrix"),
          definition=function(x,y,...){         
          	return(new("Regressions",x=x,y=y,coefficients=getCoefs(x,y),Rsquared=getR2s(x,y)))
          }
          )