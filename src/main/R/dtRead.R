# vim: ts=2 sts=2 sw=2 ht=2 expandtab number ruler showmode smartindent ff=unix foldmethod=marker :
############################################################
# dtRead.R - A program to read in OHLC CSV files using Google's "data.table"
# library.  "data.table" is a replacement for "data.frame" that is more
# flexible. More importanly for my purposes it is faster, especially when it
# comes to reading CSV files (I have tested at 1 to 2 orders of magnitude
# faster.) data.table is also worth learning because it is implemented as a
# derived class of data.frame. So any library that takes a data.frame object
# (which is all financilay libraries practically), can take a data.table
# object.  This will be espically helpful when running large ML models time and
# again where data loading can take a signifcant portion of the time.  Finally,
# data.table brings a new paradigm to R for shaping and filtering data which is
# very powerful.
#
# Stergios Marinopoulos, 1/27/2014
#


# Clear the work space.
rm(list=ls(all=TRUE))

library(chron)
library(xts)
library(lubridate)
library(quantmod)
library(quantstrat)
require(gbm)
library(data.table)
library(fasttime)

Sys.setenv(TZ="UTC")

############################################################
# The set of global variables that must be initalized:
#
#    fileName.csv <- "vwap-ESYM-1min-20121217-20130308.csv"
#
############################################################
# How to run this script from the unix command line:
#
# R CMD BATCH --no-save --no-restore '--args initDate="2013-09-09" endDate="2013-09-13" unit1Target=1.0 unit2Target=2.0 fileName.csv="vwap-ESYM-1min-20130617-20130913.csv" ' ~/IB/securities/src/main/R/qsLearn05.R  week013.out
#
############################################################

# Initialize global variables here

# Read the arguments listed on the command line
args <- commandArgs(TRUE)

if ( length(args)==0 ) {
  # No command line argument supplied.  Initialize to defaults here.

  ## DEBUG
  ## print("No arguments supplied.")

  # Supply default values

  fileName.csv <- "vwap-YM-20131220-1min-20130916-20131213.csv"

  # Running in interactive mode so display pdf files on screen
  openPdfs <- TRUE

} else {
  # Process command line arguments
  # Command line arguments supplied.  Set defaults bassed on command line args here.

  for(i in 1:length(args)){
    # Cycle through each element of the list and evaluate the expressions.
    cat("arg ", i, text=args[[i]])
    eval(parse(text=args[[i]]))
  }

  # Since we are running in batch mode, prevent pdf files from being opened.
  openPdfs <- FALSE

}

## DEBUG    stop("All Done reading command line args") ;

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
  # This code runs on different people's compters, and this is an attempt
  # to accomodate the various environments.
  # For a new computer, run the code below in your R interpreter, capture the
  # output, and create a new section in the if-then-else block below specific
  # to your computer.
  #
  #               Sys.info()[["nodename"]]
  #
  # TODO: Put this routine in a .R file and have all other scripts
  # load it, but there is a problem with finding the path to load it!
  # Turning this into a real R package solvea the path problem.

  actualMachine  <- Sys.info()[["nodename"]]

  if ( grepl("coupe", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's MacBook\n")
    inputDirName  <<- "~/IB/securities/securities/src/test/resources"
    outputDirName <<- "~/tmp"

  } else if ( grepl("hackintosh", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's Hackintosh\n")
    inputDirName  <<- "~/IB/securities/src/test/resources"
    outputDirName <<- "~/tmp"

    #    if ( nchar(subDirName) > 0 ) {
    #      # For automated runs, place the files a little lower in the hierarchy
    #      # according to the command line argument named subDirName.
    #      outputDirName <<- paste(outputDirName, "/", subDirName, "/T", unit2Target, sep="") 
    #      if ( ! file.exists(outputDirName) ) {
    #        dir.create(outputDirName, recursive=TRUE)
    #      }
    #    }

  } else if ( grepl("mopar", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's Mac Mini\n")
    inputDirName  <<- "~/IB/securities/securities/src/test/resources"
    outputDirName <<- "~/tmp"

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

############################################################
# Read in the stock price data from a file and convert it to an xts object

# construct the full path name to the CSV (& dput/dget) file containing 1-minute data
fileName.csv    <- paste(inputDirName, '/', fileName.csv, sep='') 

# Read the 1-minute data from the csv file
cat("Reading raw csv 1-minute file ", fileName.csv, "\n")
flush.console()
#  Definitions of the data columns
# 1    ES.Open    The price at the beginning of the minute
# 2    ES.High    The highest price observed during the minute 
# 3    ES.Low     The lowest price observed during the minute 
# 4    ES.Close   The price at the end of the minute
# 5    ES.Volume  The total number of shares traded during the minute
# 6    ES.VWAP    The Volume Weighted Average Price at the end of the minute
# 7    ES.STDEV   The standard deviation of vwap
# 8    ES.VWP1STD The closing price + 1 * stdev
# 9    ES.VWP2STD The closing price + 2 * stdev
# 10   ES.VWM1STD The closing price - 1 * stdev
# 11   ES.VWM2STD The closing price - 2 * stdev
# 12   ES.BW05    The 2 pole buterworth filter w/ parameter=5
# 13   ES.EMA05   The 5 period exponentail moving average 
# 14   ES.KFHI20  The kalman filter of the high price with parameter=20
# 15   ES.KFLO20  The kalman filter of the low price with parameter=20
# 16   ES.KFMP20  The middle of the extreme kalman filters (KFHI20 + KFLO20)/2  
# 17   ES.Signal  The trade entry point.  Where the BW05 crosses over the VWAP.  +1 for long trades, -1 for short trades
# 18   ES.MFE     The Maximum Favoriable Excursion is the peak profit that a trade might earns should the trade be closed/exited at the optimal point.
#
#  Columns above are for ES (SP500).  
#  All columns are then repeated for YM (Dow Industrials) to create columns 19 - 36
#

# Make sure index is of class "POSIXct" by using tz="" !

# The slow way to read a csv file we are replacing:
#    allData1 <- read.zoo(fileName.csv, sep=",", header=TRUE, tz="")   # nrow()=87657
#    allData1 <- as.xts(allData1)                                      # nrow()=87657

allData <- fread(input=fileName.csv, sep=",", header=TRUE)
b       <- fastPOSIXct(allData$Index, "PDT")
x       <- xts( as.data.frame(allData)[,-1], order.by=b)
