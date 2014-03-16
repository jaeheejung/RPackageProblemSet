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
#' @aliases Regressions-class initialize,Regressions-method getRegressions,Regressions-method getCoefs,Regressions-method getR2s,Regressions-method getPvalues,Regressions-method
#' @rdname Regressions
#' @export
setClass(Class="Regressions",
	representation=representation(
		x="matrix",
		y="matrix",
		coefficients="matrix",
		Rsquared="numeric"
		),
	prototype=prototype(
		x=matrix(),
		y=matrix(),
		coefficients=matrix(),
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

#' @rdname Regressions
#' @export 
setGeneric("getCoefs",
     function(x,y,...)  {
         standardGeneric("getCoefs")
       }
       )

#' @export
setMethod("getCoefs",c("matrix","matrix"),
     function(x,y,...){
    x <- data.frame(x)
    
    y <- data.frame(y)
	
	covariate.names <- colnames(x)
	
	outcome.name <- colnames(y)
	
	combinations <- unlist(lapply(1:ncol(x),function(X){combn(ncol(x),X,simplify=F)}),recursive=F)
	
	formulas <- sapply(combinations,function(X){paste(paste(outcome.name,"~"),paste(covariate.names[X],collapse="+"))})
	
	all.lm <- lapply(formulas,function(X){lm(as.formula(X),data=cbind(y,x))})
	
	coef.matrix <- matrix("NA",nrow=length(all.lm),ncol=ncol(x)+1)
	
	for(i in 1:nrow(coef.matrix)){
coef.matrix[i,seq(length(unlist(sapply(all.lm,coef)[i],use.names=FALSE)))]<- unlist(sapply(all.lm,coef)[i],use.names=FALSE)
	}
	
	colnames(coef.matrix) <- c("Intercept",covariate.names)
	
	return(coef.matrix)
	
	} 
    )
    
#' @rdname Regressions
#' @export 
setGeneric("getR2s",
     function(x,y,...)  {
         standardGeneric("getR2s")
       }
       )

#' @export
setMethod("getR2s",c("matrix","matrix"),
     function(x,y,...){
    x <- data.frame(x)
    
    y <- data.frame(y)
	
	covariate.names <- colnames(x)
	
	outcome.name <- colnames(y)
	
	combinations <- unlist(lapply(1:ncol(x),function(X){combn(ncol(x),X,simplify=F)}),recursive=F)
	
	formulas <- sapply(combinations,function(X){paste(paste(outcome.name,"~"),paste(covariate.names[X],collapse="+"))})
	
	all.lm <- lapply(formulas,function(X){lm(as.formula(X),data=cbind(y,x))})
	
	R2.vector <- sapply(all.lm,function(X){summary(X)$r.squared})
	
	return(R2.vector)
	
	}
	)

#' @rdname Regressions
#' @export 
setGeneric("getPvalues",
     function(x,y,...)  {
         standardGeneric("getPvalues")
       }
       )

#' @export
setMethod("getPvalues",c("matrix","matrix"),
     function(x,y,...){
    x <- data.frame(x)
    
    y <- data.frame(y)
	
	covariate.names <- colnames(x)
	
	outcome.name <- colnames(y)
	
	combinations <- unlist(lapply(1:ncol(x),function(X){combn(ncol(x),X,simplify=F)}),recursive=F)
	
	formulas <- sapply(combinations,function(X){paste(paste(outcome.name,"~"),paste(covariate.names[X],collapse="+"))})
	
	all.lm <- lapply(formulas,function(X){lm(as.formula(X),data=cbind(y,x))})
	
	pvalues.matrix <- matrix("NA",nrow=length(all.lm),ncol=ncol(x)+1)
	
	for(i in 1:nrow(pvalues.matrix)){
pvalues.matrix[i,seq(length(unlist(sapply(all.lm,function(X){summary(X)$coefficients[,4]})[i],use.names=FALSE)))]<- unlist(sapply(all.lm,function(X){summary(X)$coefficients[,4]})[i],use.names=FALSE)
	}
	
	colnames(pvalues.matrix) <- c("Intercept",covariate.names)
	
	return(pvalues.matrix)
	
	}
	)


