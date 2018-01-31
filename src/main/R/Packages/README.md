# This area is for code that is created as R packages

## How to create a R package

Watch this [video](https://www.youtube.com/watch?v=rmiCnQEnB3g "youtube.com") for the simplest way possible to create a basic empty package.

## Basic Steps on how to create a package (from the Video)

### Operations in R:
  
    R> install.packages("devtools")
    R> library(devtools)
    R> setwd("~/IB/securities/src/main/R/Packages")
    R> create("TTREx")
	
### Operations in a bash shell:
  	 
    bash> cd ~/IB/securities/src/main/R/Packages/TTRex
    bash> rename R/TTRex-package.R  PMO.R
 
### Operations in an editor:
   
    vim> edit the DESCRIPTION file as show inthe video
    vim> edit PMO.R to have your code
    vim> edit PMO.R to put in all the meta data required to be a package (see video for details, also existing examples in the TTR package)

### Install Package:
##### (This step you will repeat continuously as you develop and test your package.)

    bash> cd ~/IB/securities/src/main/R/Packages 
    bash> R CMD INSTALL --no-multiarch TTREx
 
### (Re)Build documentation in R:
##### (This step you will repeat continuously as you develop and test your package.)
  
    R> library(devtools)
  	R> setwd("~/IB/securities/src/main/R/Packages/TTREx")
    R> document()

#### Continue to develop PMO.R file
As you modify the code and improve the documentation, repeat the __INSTALL__ and **DOCUMENTATION** steps as necessary.
