#vim:ts=2:sts=2:sw=2:ht=2:expandtab:number:ruler:showmode:smartindent:ff=unix:foldmethod=marker 
############################################################

# Stergios Marinopoulos
# Created  10/21/2013
#
# Load two different data sets from csv files, convert each into an xts object, merge
# the two xts object into one by doing an "outer" join, and finally save the merged object
# into a new csv file.
#
# Usage: edit the variables fileNameES_csv fileNameYM_csv and then run the
# program directly in the R system.
#
# Or run this script from the unix command line:
#
#   R CMD BATCH --no-save --no-restore '--args fileNameES_csv="vwap-ES-20110916-1min-20110613-20110909.csv" fileNameYM_csv="vwap-YM-20110916-1min-2011-06-13-2011-09-09.csv" ' ~/IB/securities/src/main/R/mergeTwoSecurities.R output-0.log
#
# The input and output locations are configured in the subroutine machineInfo()
# according to the value of R's Sys.info()[["nodename"]] . An example output
# file name is vwap-ESYM-1min-20110611-20110916.csv
#
#
# We really want to do a "left" join, or maybe an "inner" join.  The challenge
# is how do we handle the missing data columns?  Answer this question, and then
# we will know if we should do a "left" or and "inner" join.  


# Clear the work space.
rm(list=ls(all=TRUE))

library(xts)
library(lubridate)

############################################################
# The set of global variables that must be initialized:
#
#    fileNameES_csv    <- "vwap-ES-20121221-1min-20120917-20121214.csv"
#    fileNameYM_csv    <- "vwap-YM-20121221-1min-20120917-20121214.csv"
#
############################################################

############################################################
# Read the arguments listed on the command line & Initialize global variables here
############################################################

args <- commandArgs(TRUE)


if ( length(args)==0 ) {
  # No command line arguments supplied.  Initialize to defaults here.

  ##    fileNameES_csv    <- "vwap-ES-20130315-1min-20121217-20130308.csv"
  ##    fileNameYM_csv    <- "vwap-YM-20130315-1min-20121217-20130308.csv"
  
  ##    fileNameES_csv    <- "vwap-ES-20130621-1min-20130311-20130614.csv"
  ##    fileNameYM_csv    <- "vwap-YM-20130621-1min-20130311-20130614.csv"
  
  ##    fileNameES_csv    <- "vwap-ES-20130920-1min-20130617-20130913.csv"
  ##    fileNameYM_csv    <- "vwap-YM-20130920-1min-20130617-20130913.csv"
  
  ##    fileNameES_csv    <- "vwap-ES-20120921-1min-20120611-20120914.csv"
  ##    fileNameYM_csv    <- "vwap-YM-20120921-1min-20120611-20120914.csv"
      
        fileNameES_csv    <- "vwap-ES-20121221-1min-20120917-20121214.csv"
        fileNameYM_csv    <- "vwap-YM-20121221-1min-20120917-20121214.csv"
 
} else {

  # Command line arguments supplied.  Set defaults based on command line args here.

  for(i in 1:length(args)){
    # Cycle through each element of the list and evaluate the expressions.
    ## DEBUG    cat("arg ", i, text=args[[i]])
    # TODO: Check that only valid arguments are passed in.
    eval(parse(text=args[[i]]))
  }


}


############################################################


# The full directory path to the CSV files containing 1-minute data.  This value
# will be overwritten in the machineInfo() function.
inputDirName  <- "~/runTime/longTermArchive"

# The full directory path to where the merged CSV file will be written.  This
# value will be overwritten in the machineInfo() function.
outputDirName <- "~/runTime/longTermArchive"

machineInfo <- function() {
  ############################################################
  # Set machine specific values, usually these are file system locations.
  # For a new computer, run the code below in your R interpreter, capture the
  # output, and create a new section in the if-then-else block below specific
  # to your computer.
  #
  #               Sys.info()[["nodename"]]
  #

  actualMachine  <- Sys.info()[["nodename"]]

  if ( grepl("coupe", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's MacBook\n")
    inputDirName  <<- "~/IB/securities/securities/src/test/resources"
    outputDirName <<- "~/tmp"

  } else if ( grepl("mopar", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's Mac Mini\n")
    inputDirName  <<- "~/IB/securities/securities/src/test/resources"
    outputDirName <<- "~/tmp"

  } else if ( grepl("hackintosh", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's Mac Mini\n")
    inputDirName  <<- "~/IB/securities/src/test/resources"
    outputDirName <<- "~/IB/securities/src/test/resources"
    inputDirName  <<- "~/runTime/longTermArchive"
    outputDirName <<- "~/runTime/longTermArchive"

  } else if (  grepl("garage4", actualMachine, ignore.case=TRUE ) ) {
    cat("Initialize variables specific to Sterg's PC in the Garage\n")
    inputDirName  <<- ""
    outputDirName <<- ""
    stop("garage4 does not have machine variables initialized")

  } else if (  grepl("Joes-iMac.local", actualMachine, ignore.case=TRUE ) ||
               grepl("Joes-MacBook-Pro.local", actualMachine, ignore.case=TRUE ) ) {
    cat("Initialize variables specific to Brian's iMac\n")
    inputDirName  <<- "~/workspace/securities/src/test/resources"
    outputDirName <<- "~/workspace/securities/src/test/resources"

  } else {
    cat("WHAT MACHINE IS THIS PROGRAM RUNNING ON?\n\tPlease tell Stergios the following information:\n\tSys.info(nodename)==", actualMachine, "\n\n")
    stop("Cannot proceed until I know which machine is trying to run this program.")

  }
  flush.console()

}

machineInfo()


fileNameES_csv    <- paste(inputDirName, '/', fileNameES_csv, sep='') 
fileNameYM_csv    <- paste(inputDirName, '/', fileNameYM_csv, sep='') 


# Read the 1-minute data from the ES csv file
cat("Reading raw csv 1-minute file ", fileNameES_csv, "\n")
flush.console()
# Make sure index is of class "POSIXct" by using tz="" !!!   This is an obscure fact.
allESData <- read.zoo(fileNameES_csv, sep=",", header=TRUE, tz="")
allESData <- as.xts(allESData) # Convert the zoo object to an xts object
# Make sure all seconds are set to "00"
minIndex          <- index(allESData)
second(minIndex)  <- 0
.index(allESData) <- minIndex


# Read the 1-minute data from the YM csv file
cat("Reading raw csv 1-minute file ", fileNameYM_csv, "\n")
flush.console()
# Make sure index is of class "POSIXct" by using tz="" !!!  This is an obscure fact.
allYMData <- read.zoo(fileNameYM_csv, sep=",", header=TRUE, tz="")
allYMData <- as.xts(allYMData)   # Convert the zoo object to an xts object
# Make sure all seconds are set to "00"
minIndex          <- index(allYMData)
second(minIndex)  <- 0
.index(allYMData) <- minIndex

# Rename all columns by prepending the ticker symbol, which follows the
# quantmod getSymbols() convention.
names(allESData) <- paste("ES.", names(allESData), sep="")
names(allYMData) <- paste("YM.", names(allYMData), sep="")

allData <- merge(allESData, allYMData, join='outer')

# Replace ‘NA’ with most recent non-‘NA’
# Each of the ES and YM data sets have missing minutes occasionally, and after
# the outer join those missing rows get created and filled in with NA's.  So
# perform a Last Observation Carried Forward replacement operation to fill in
# the NAs.  In the event that both ES & YM are missing the same minute/row,
# then that same minute will also be absent in the merge result, and
# hence is not addressed in this operation.

allData <- na.locf(allData)

# Extract the first and last day from the merged data to use in creating the file name for the merged xts object
startDate <- as.Date(index(first(allData)))
endDate   <- as.Date(index(last(allData)))


# The new file name
csvFileName <- paste(outputDirName, "/vwap-ESYM-1min-", 
                      format(startDate, "%Y%m%d"), "-", 
                      format(endDate, "%Y%m%d"), 
                      ".csv", sep="")

# save the merged data as a csv file
write.zoo(allData, file=csvFileName, sep=",")
