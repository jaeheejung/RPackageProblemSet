###Problem Set 6###
###Jae Hee Jung###


##Load libraries.
library(devtools)
library(roxygen2)

##Set the working directory.
getwd()
setwd("/Users/jaeheejung/Desktop/Spring 2014/Applied Statistical Programming/PS6")

##Create the package structure. (This code should be run once.)
create(path="./regressionPack",check=FALSE)

##Build the package. (This code should be run whenever there is an update.)
current.code <- as.package("regressionPack")
load_all(current.code)
document(current.code)

##If the package is going to be released to CRAN, check the code for compliance.
check(current.code)

##Install the package.
install(pkg=current.code, local=TRUE)