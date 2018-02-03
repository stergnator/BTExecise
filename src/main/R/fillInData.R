#vim:ts=2:sts=2:sw=2:ht=2:expandtab:number:ruler:showmode:smartindent:ff=unix:foldmethod=marker 
############################################################

# Stergios Marinopoulos, 10/29/2013
#  
#  A program to fix NA's in the stock price OHLC data and to fill in missing
#  minutes.  The results are not yet saved to any file.  This was merely a
#  place to test the approaches.
#  
#  There are missing minutes (rows) in the ES (sp500) and YM (dow) data sets.
#  Often the missing rows are at different points in time between ES and YM.  One
#  problem is when the outer join is performed on the two ( via merge() ) the
#  resulting data set will then have 1/2 of a row with NA's where there was a
#  missing row of ES or YM.  The NA's can be filled in with na.locf() , i.e.
#  'Last Observation Carried Forward' replacement, where a ‘NA’ s replaced with
#  the most recent non-‘NA’
#  
#  In the event that both ES & YM are missing the same minute/row,
#  then that same minute will also be absent in the merge result, and
#  the na.locf() operation does not address this problem.
#  
#  This program finds the missing rows and creates a version of each using the
#  previous row's value.  This will only work on sequence of missing rows of 10 or
#  less.  This is done because there are gaps in time where we do not expect there
#  to be rows.  For example, all minutes on Saturday should be missing, as will
#  all minutes up to 3pm on Sunday, and on M-F 1:16pm to 1:30pm are never traded,
#  and the same for  minutes on M-F from 2:30pm to 3pm
#  


# Clear the work space.
rm(list=ls(all=TRUE))

library(chron)
library(xts)
library(lubridate)
library(quantmod)


# The full directory path to the CSV files containing 1-minute data.  This value
# will be overwritten in the machineInfo() function.
inputDirName  <- "~/runTime/longTermArchive"

# The full directory path to where the merged CSV file will be written.  This
# value will be overwritten in the machineInfo() function.
outputDirName <- "~/runTime/longTermArchive"

machineInfo <- function() {

  # Set machine specific values, usually these are file system locations.
  # This code runs on different people's computers, and this is an attempt
  # to accommodate the various environments.
  # For a new computer, run the code below in your R interpreter, capture the
  # output, and create a new section in the if-then-else block below specific
  # to your computer.
  #
  #               Sys.info()[["nodename"]]
  #
  # TODO: Put this routine in a .R file and have all other scripts
  # load it, but there is a problem with finding the path to load it!
  # Turning this into a real R package solves the path problem.

  actualMachine  <- Sys.info()[["nodename"]]

  if ( grepl("coupe", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's MacBook\n")
    inputDirName  <<- "~/IB/securities/securities/src/test/resources"
    outputDirName <<- "~/tmp"

  } else if ( grepl("hackintosh", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's Hackintosh\n")
    inputDirName  <<- "~/IB/securities//src/test/resources"
    outputDirName <<- "~/tmp"

  } else if ( grepl("mopar", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's Mac Mini\n")
    inputDirName  <<- "~/IB/securities/securities/src/test/resources"
    outputDirName <<- "~/tmp"

  } else if (  grepl("garage4", actualMachine, ignore.case=TRUE ) ) {
    cat("Initialize variables specific to Sterg's PC in the Garage\n")
    inputDirName  <<- ""
    outputDirName <<- ""
    stop("garage4 does not have machine variables initialized")

  } else if (  grepl("Joes-Rogoffs-iMac.local", actualMachine, ignore.case=TRUE ) ||
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


# construct the full path name to the CSV (& dput/dget) file containing 1-minute data
fileName.csv    <- "vwap-ESYM-1min-20130617-20130913.csv"
fileName.csv    <- paste(inputDirName, '/', fileName.csv, sep='') 


# Read the 1-minute data from the csv file
cat("Reading raw csv 1-minute file ", fileName.csv, "\n")
flush.console()
#  Definitions of the data columns
# 1    Open.ES    The price at the beginning of the minute
# 2    High.ES    The highest price observed during the minute 
# 3    Low.ES     The lowest price observed during the minute 
# 4    Close.ES   The price at the end of the minute
# 5    Volume.ES  The total number of shares traded during the minute
# 6    VWAP.ES    The Volume Weighted Average Price at the end of the minute
# 7    STDEV.ES   The standard deviation of vwap
# 8    VWP1STD.ES The closing price + 1 * stdev
# 9    VWP2STD.ES The closing price + 2 * stdev
# 10   VWM1STD.ES The closing price - 1 * stdev
# 11   VWM2STD.ES The closing price - 2 * stdev
# 12   BW05.ES    The 2 pole buterworth filter w/ parameter=5
# 13   EMA05.ES   The 5 period exponential moving average 
# 14   KFHI20.ES  The kalman filter of the high price with parameter=20
# 15   KFLO20.ES  The kalman filter of the low price with parameter=20
# 16   KFMP20.ES  The middle of the extreme kalman filters (KFHI20 + KFLO20)/2  
# 17   Signal.ES  The trade entry point.  Where the BW05 crosses over the VWAP.  +1 for long trades, -1 for short trades
# 18   MFE.ES     The Maximum Favorable Excursion is the peak profit that a trade might earns should the trade be closed/exited at the optimal point.
#
#  Columns above are for ES (SP500).  
#  All columns are then repeated for YM (Dow Industrials) to create columns 19 - 36
#

# Make sure index is of class "POSIXct" by using tz="" !!!
allData <- read.zoo(fileName.csv, sep=",", header=TRUE, tz="")  # nrow()=87657
allData <- as.xts(allData)                                      # nrow()=87657

minutesArray <- index(allData)

##    > head(minutesArray)
##    [1] "2013-06-17 00:00:00 PDT" "2013-06-17 00:01:00 PDT" "2013-06-17 00:02:00 PDT" "2013-06-17 00:03:00 PDT" "2013-06-17 00:04:00 PDT"
##    [6] "2013-06-17 00:05:00 PDT"
##
##    > head(as.numeric(minutesArray))
##    [1] 1371452400 1371452460 1371452520 1371452580 1371452640 1371452700
##
##    > diff(head(as.numeric(minutesArray)))
##    [1] 60 60 60 60 60

# Create an array containing the length in seconds from the current period start to the next period start
theDiffs <- diff(as.numeric(minutesArray))

##    > str(theDiffs)
##     num [1:87656] 60 60 60 60 60 60 60 60 60 60 ...

# Any period length > 60 reveals at least one missing minute

##    > theDiffs[theDiffs!=60]
##      [1]    960   2760    120    120    120    120    120    120    180    180    120    120    120    120    120    120    120    120
##     [19]    960   2760    120    120    120    120    120    960   2760    960   2760    960 175560    120    960   2760    960   2760
##     [37]    120    120    960   2760    120    120    120    960   2760    180    120    120    120    960 175560    180    120    120
##     [55]    120    960   2760    120    120    960   2760    120    120    120  17160    120    120    120    120    120    120    120
##     [73]    120    120    120    120    120    120    120    120    120    120    240    120    120    120    120    120    120    120
##     [91]    120    120    120    120    120    120    120    120    120    120  23460    120    120    120    120    120    120    120
##    [109]    120    960 175560    960   2760    120    180    120    120    180    120    120    120    960   2760    120    120    120
##    [127]    120    120    960   2760    960   2760    120    120    120    120    120    120    120    120    120    120    960 175560
##    [145]    120    120    120    120    120    120    180    120    120    120    120    120    120    120    120    960   2760    120
##    [163]    120    120    120    120    120    120    120    120    120    120    120    120    120    120    120    120    120    120
##    [181]    960    120   2760    120    120    180    120    120    120    180    120    960   2760    120    120    120    120    120
##    [199]    120    120    120    120    120    120    960   2760    120    120    120    120    120    960 175560    120    120    120
##    [217]    120    120    120    120    120    960   2760    120    120    120    120    120    120    120    120    120    120    960
##    [235]   2760    180    120    120    120    120    120    120    120    120    180    120    120    120    120    120    120    960
##    [253]   2760    120    120    120    240    120    120    120    120    120    120    120    180    120    120    120    120    120
##    


# Only fix missing spans of minutes that are less than 10 minutes long.
missingStarts <- minutesArray[theDiffs!=60 & theDiffs<600]

##    msArray <- c( missingStarts[1]-60, missingStarts[1], missingStarts[1]+60 )
##    > msArray
##    [1] "2013-06-17 15:26:00 PDT" "2013-06-17 15:27:00 PDT" "2013-06-17 15:28:00 PDT"

##    > allData[msArray,]
##                        Open.ES High.ES  Low.ES Close.ES Volume.ES  VWAP.ES STDEV.ES VWP1STD.ES VWP2STD.ES VWM1STD.ES VWM2STD.ES  BW05.ES EMA05.ES KFHI20.ES KFLO20.ES KFMP20.ES Signal.ES MFE.ES
##    2013-06-17 15:26:00 1634.25 1634.25 1634.25  1634.25         2 1633.699 2.566364   1636.265   1638.832   1631.133   1628.566 1634.273 1634.323  1635.020  1634.923  1634.971         0      0
##    2013-06-17 15:27:00 1634.25 1634.25 1634.25  1634.25         3 1633.699 2.564885   1636.264   1638.829   1631.134   1628.569 1634.248 1634.299  1634.969  1634.883  1634.926         0      0

for ( j in 1:length(missingStarts) ) {

  cat("Working on missing item: ", j, "\n")
  flush.console()
  # The row which has the following row(s) missing
  row <- allData[missingStarts[j] , ]
  nextTime <- missingStarts[j]
  
  repeat {

    nextTime <- nextTime + 60
    if ( nrow(allData[nextTime, ]) > 0 ) break

    index(row) <- nextTime ;

    if ( ! exists("newData") ) {
      # Collect the new rows in a small xts object, and then do the merge with the big xts object just once
      newData <- row
    } else {
      newData <- rbind(newData, row)
    }
    cat("added row for", format(nextTime, "%Y-%m-%d %H:%M:%S") , "\n") 
    flush.console()

  }


}

# Merge the new rows with the big xts object here.
allData <- merge(allData, newData)
