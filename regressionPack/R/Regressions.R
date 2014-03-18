#' Regression analyses 
#' 
#' Object of class \code{Regressions} is created by the \code{allRegressions} function
#'
#' 
#' An object of the class `Regressions' has the following slots:
#' \itemize{
#' \item \code{x} A matrix of possible covariates
#' \item \code{y} A dependent variable in matrix form
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

#' @export ##This function extracts the slots coefficients and Rsquared from the object of class Regressions.
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

#' @export ##This function takes as input two arguments in matrix form (which occupy the slots x and y in the object of class Regressions). The function calculates a matrix of coefficients for all possible combinations of covariates. I made this function in order to use it in my main function allRegressions().
setMethod("getCoefs",c("matrix","matrix"),
     function(x,y,...){
     	
    ##Convert the matrices to dataframes
    x <- data.frame(x)
    y <- data.frame(y)
	
	##Extract the names of the covariates
	covariate.names <- colnames(x)
	
	##Extract the name of the outcome variable
	outcome.name <- colnames(y)
	
	##Use the combn() and lapply() functions to calculate  all possible combinations of the covariates.
	combinations <- unlist(lapply(1:ncol(x),function(X){combn(ncol(x),X,simplify=F)}),recursive=F)
	
	##Create the linear model formulas
	formulas <- sapply(combinations,function(X){paste(paste(outcome.name,"~"),paste(covariate.names[X],collapse="+"))})
	
	##Use the lapply() function to convert the formulas in the form of pasted characters to actual formulas that can be used in the lm() function
	all.lm <- lapply(formulas,function(X){lm(as.formula(X),data=cbind(y,x))})
	
	##Create an empty matrix to be filled
	coef.matrix <- matrix("NA",nrow=length(all.lm),ncol=ncol(x)+1)
	
	##Fill in the matrix using a for loop
	for(i in 1:nrow(coef.matrix)){
coef.matrix[i,seq(length(unlist(sapply(all.lm,coef)[i],use.names=FALSE)))]<- unlist(sapply(all.lm,coef)[i],use.names=FALSE)
	}
	
	##Assign column names to the matrix to make the matrix easy to understand
	colnames(coef.matrix) <- c("Intercept",covariate.names)
	
	##Return the matrix of coefficients
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

#' @export ##This function takes as input two arguments in matrix form (which occupy the slots x and y in the object of class Regressions). The function calculates a vector of Rsquared values for all the linear models I computed. I made this function in order to use it in my main function allRegressions(). Most of the code for this function is the same as the code for the function getCoefs() above, so I'll only document the parts of the code that are different.
setMethod("getR2s",c("matrix","matrix"),
     function(x,y,...){
    x <- data.frame(x)
    
    y <- data.frame(y)
	
	covariate.names <- colnames(x)
	
	outcome.name <- colnames(y)
	
	combinations <- unlist(lapply(1:ncol(x),function(X){combn(ncol(x),X,simplify=F)}),recursive=F)
	
	formulas <- sapply(combinations,function(X){paste(paste(outcome.name,"~"),paste(covariate.names[X],collapse="+"))})
	
	all.lm <- lapply(formulas,function(X){lm(as.formula(X),data=cbind(y,x))})
	
	##Use sapply() to calculate the r.squared value of each linear model
	R2.vector <- sapply(all.lm,function(X){summary(X)$r.squared})
	
	##Return the vector of rsquared values
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

#' @export ##This function takes as input two arguments in matrix form (which occupy the slots x and y in the object of class Regressions). The function calculates a matrix of pvalues for all possible combinations of covariates. I made this function in order to use it in my function evalRegressions() that is of the subclass called subRegressions. Most of the code for this function is the same as the codes for the functions getCoefs() and getR2s() above, so I'll only document the parts of the code that are different.
setMethod("getPvalues",c("matrix","matrix"),
     function(x,y,...){
    x <- data.frame(x)
    
    y <- data.frame(y)
	
	covariate.names <- colnames(x)
	
	outcome.name <- colnames(y)
	
	combinations <- unlist(lapply(1:ncol(x),function(X){combn(ncol(x),X,simplify=F)}),recursive=F)
	
	formulas <- sapply(combinations,function(X){paste(paste(outcome.name,"~"),paste(covariate.names[X],collapse="+"))})
	
	all.lm <- lapply(formulas,function(X){lm(as.formula(X),data=cbind(y,x))})
	
	##Create an empty matrix to fill in
	pvalues.matrix <- matrix("NA",nrow=length(all.lm),ncol=ncol(x)+1)
	
	##Use a for loop to fill the empty matrix with pvalues
	for(i in 1:nrow(pvalues.matrix)){
pvalues.matrix[i,seq(length(unlist(sapply(all.lm,function(X){summary(X)$coefficients[,4]})[i],use.names=FALSE)))]<- unlist(sapply(all.lm,function(X){summary(X)$coefficients[,4]})[i],use.names=FALSE)
	}
	
	##Assign column names to make the matrix easy to understand
	colnames(pvalues.matrix) <- c("Intercept",covariate.names)
	
	##Return the matrix of pvalues
	return(pvalues.matrix)
	
	}
	)


