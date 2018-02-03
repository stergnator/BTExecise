# Convert a 1-Second stock price OHLC data CSV file into a 5-Second CSV file.
#
# Usage: Modify variables "fileNameIn", and "fileNameOut" 
#        or run this script from the unix command line:
#           R CMD BATCH --no-save --no-restore '--args fileNameIn="ES20110916B.csv" fileNameOut="ES-20110916-5Sec.csv" ' convert1Sec-2-5Sec.R output1.txt 
#
# Note: Make sure the input does not have duplicate records.
#
# Stergios Marinopoulos
# Created: 3/12/2013

# Clear the work space.
rm(list=ls(all=TRUE))
library(quantmod);

############################################################
# The set of global variables that must be initialized:
#
#    fileNameIn  <- "ES20110916B.csv"
#    fileNameOut <- "ES-20110916-5Sec.csv" 
#
############################################################

############################################################
# Read the arguments listed on the command line & Initialize global variables here
############################################################

args <- commandArgs(TRUE)

if ( length(args)==0 ) {
  # No command line arguments supplied.  Initialize to defaults here.

  # TODO This needs to change to work on other people's computers
  setwd("/Users/stergios/IB/trunk/R/prog018") ;

  # fileNameIn  <- "ES-20120921-09-1sec.csv" ;
  # fileNameOut <- "ES-20120921-09-5Sec.csv" ;
    fileNameIn  <- "ES20110916B.csv"
    fileNameOut <- "ES-20110916-5Sec.csv" 

} else {

  # Command line arguments supplied.  Set defaults based on command line args here.

  for(i in 1:length(args)){
    # Cycle through each element of the list and evaluate the expressions.
    ## DEBUG    cat("arg ", i, text=args[[i]])
    eval(parse(text=args[[i]]))
  }

}


############################################################
# Read in the csv file and convert to an xts object.
cat("Reading 1 Second Data: ", fileNameIn, "..." )
flush.console()
d <- as.xts(read.zoo(fileNameIn, header=TRUE, sep=",", tz=""))
cat("Done\n") 
flush.console()

## d5sec <- to.period(d, period='seconds', k=5, indexAt="startOf") 
##     "startOf" produces seconds at 04, 09, 14, 19, 24, 29, ...
##     "firstOf" produces seconds at 04, 09, 14, 19, 24, 29, ...
##     "endOf" produces seconds at 04, 09, 14, 19, 24, 29, ...
##
## Seems like there is a bug here with the choice of "indexAt"
## I have confirmed this bug again on 12/9/2013.
##
## From the documentation:
## To adjust the final indexing style, it is possible to set indexAt to one of the following: 
##     "yearmon", "yearqtr", The final index will then be yearmon, yearqtr, 
##     "firstof", "lastof", The final index will then be the first time of the period, the last time of the period, 
##     "startof", or "endof".  The final index will then be the starting time in the data for that period, or the ending time in the data for that period, respectively.
## However, the bug is that the results are always the same for any of the values of "indexAt"

cat("Converting to 5 seconds ...")
flush.console()
d5sec <- to.period(d, period='seconds', k=5, indexAt="firstof") 
cat("Done\n") 
flush.console()

# Correct for the indexAt bug.  Any five second interval must begin on a 0, 5, 10, 15, ..., 55 second.
names(d5sec) <- c("Open", "High", "Low", "Close", "Volume")
.index(d5sec) <- .index(d5sec) - 4

# Extract the seconds since the unix epoch from the time index, and add it to d5sec$start
d5sec$start <- as.numeric(index(d5sec))

cat("Writing 5 second data to ", fileNameOut, "...") 
flush.console()
write.zoo(d5sec, file=fileNameOut, index.name="tsStart", sep=",")
cat("Done\n") 
flush.console()
