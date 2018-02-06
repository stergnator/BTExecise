#vim:ts=2:sts=2:sw=2:ht=2:expandtab:number:ruler:showmode:smartindent:ff=unix:foldmethod=marker 
############################################################
#
# Use stochastic gradient boosting to create a decision tree based trading system.
#
# Create a simple model of the 1-minute VWAP x BW05 (Buterworth) crossover
# system.  Ignore data holdout rules for now and instead use a 10 fold cross
# validation.  Solve the model with the "gbm" package.  No training or testing
# data has been separated.  No truth tables created.
# Each trade is plotted on a candlestick chart.

# Stergios Marinopoulos
# Created: 4/23/2013


# Clear the work space.
rm(list=ls(all=TRUE))

library(chron)
library(xts)
library(lubridate)
library(quantmod)
library(quantstrat)
require(gbm)


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
  # I would love to put this routine in a .R file and have all other scripts
  # load it, but there is a problem with finding the path to load it!  If this
  # was turned into a real R package, that would solve the path problem.

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

# This is only temporary... STM
# Drop the YM (Dow) data for now, and keep just the ES (SP500) data columns
allData <- allData[, 1:18]

# Limit the data set to just the hours of 5am to 9am.
d529    <- allData["T05:00/T09:00",]                            # nrow()=156022


# Write an ASCII text representation of the R object "d529" to a file
cat("Done Reading, Converting, and Filtering to 5am to 9am\n") ;
flush.console()


# Extract the signals for the reduced data set... Can I delete this?
signalsAllD529   <- d529[d529$Signal.ES!=0, ]
signalsLongD529  <- d529[d529$Signal.ES==1, ]
signalsShortD529 <- d529[d529$Signal.ES==-1, ]

# Extract the signals for the entire data set
signalsAll   <- allData[allData$Signal.ES!=0, ]
signalsLong  <- allData[allData$Signal.ES==1, ]
signalsShort <- allData[allData$Signal.ES==-1, ]


# Set up the data structure for BUGS for the long singals
successOfLongs <- as.vector(signalsLongD529$MFE.ES >= 0.75) + 0 

# 
## DEBUG    stop("Signals have been extracted.")


############################################################
# Plot one days worth of prices and filters.  This code is here so we can
# discuss the underlying chart that motivates this model.  Eventually, we'll
# want to verify our systems on a chart anyway, so we might as well make sure
# we can plot from the beginning.
############################################################
if(FALSE) {

  # chartProps    <- genTAForCharts(ticker, oneMinData)
  genTAForCharts <- function(ticker, d) {

    strategyName <- "BW05xVWAP" 
    plotVolume   <- 'addVo();'
    # plotVolume <- ''

    # Before the signals can be plotted they must first be checked for existence.
    # If there are any, add them to the list of Chart TA's.  This check must be
    # done otherwise chartSeries() produces an error.
    signalsShortTA <- "" 
    if (length(signalsShortOneDay) > 0 ) {
      signalsShortTA <- "addTA(Cl( signalsShortOneDay ), type='p', col='black', pch=25, bg='red3', cex=1.5, on=1 );"
    }
    signalsLongTA <- "" 
    if (length(signalsLongOneDay) > 0 ) {
    signalsLongTA <- "addTA(Cl( signalsLongOneDay ), type='p', col='black', pch=24, bg='green3', cex=1.5, on=1 );"
    }

    # color reference:      http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
    # line type reference:  http://students.washington.edu/mclarkso/documents/line%20styles%20Ver2.pdf
    # point type reference: http://rgraphics.limnology.wisc.edu/pch.php

    ta <- paste(
       # EMA05 "addTA(oneMinData[, 'EMA05'], on=1, col='red', lwd=2, lty=1);",
       # SMA48 "addTA(oneMinData[, 'SMA48'], on=1, col='blue', lwd=2, lty=1);",

       "addTA(oneMinData[, 'VWAP'], on=1, col='darkgreen', lwd=3);",

       "addTA(oneMinData[, 'VWP1STD'], on=1, col='antiquewhite3', lwd=1, lty=1);",
       "addTA(oneMinData[, 'VWP2STD'], on=1, col='antiquewhite3', lwd=1, lty=1);",
       "addTA(oneMinData[, 'VWM1STD'], on=1, col='antiquewhite3', lwd=1, lty=1);",
       "addTA(oneMinData[, 'VWM2STD'], on=1, col='antiquewhite3', lwd=1, lty=1);",

       "addTA(oneMinData[,'BW05'], on=1, col='purple', lwd=2, lty=1);",
       "addTA(oneMinData[,'KFHI20'], on=1, col='black', lwd=1, lty=2);",
       "addTA(oneMinData[,'KFLO20'], on=1, col='black', lwd=1, lty=2);",

       signalsShortTA,
       signalsLongTA,

       plotVolume,  #   "addVo();" ,
       sep=""
    )

    lastBarTime    <- index(last(d))
    lastBarTimeStr <- format(lastBarTime, "%Y%m%d %H:%M:%S")
    rightNowStr    <- format( Sys.time(), "%H:%M:%S")
    messageStr     <- paste(ticker, ", ", strategyName, ", ", lastBarTimeStr, " @ ", rightNowStr, sep="")   

    # Subset of the day to plot
    ss <- NULL                    # Plot the entire day
    # ss <- "last 8 hours"
    # ss <- "last 3 hours"
    # ss <- "T03:00/T08:00"
    # ss <- "T06:00/T13:00"
    # ss <- "T06:00/T09:00"
      ss <- "T05:00/T09:30"
    # ss <- "T23:00/T03:45"       # for debugging use very narrow & odd windows
    # ss <- paste("T04:55/T", sprintf("%02d", hour(now())), ":59", sep="")
    # if ( hour(now()) > 9 ) ss <- paste("last", 4, "hours")

    return(list(messageStr=messageStr, ta=ta, ss=ss))
  } # End Of genTAForCharts()

  # Pull out one day's worth of data, and then plot the day
  #
  # Extract the list of days present in "allData"
  theDays    <- unique(as.Date(index(allData)))

  # Choose the day in the middle
  theDay     <- theDays[floor(length(theDays)/2) ]  # Choose the day in the middle of the data set
  theDay     <- paste(theDay, '/', theDay, sep='')  # Build an ISO8601 day pattern, ex:  "2012-12-17/2012-12-17"

  oneMinData <- allData[theDay,]                    # Extract just 1 days worth of data

  # Strip the ".ES" off the column names
  names(oneMinData) = gsub(pattern="\\.ES$", replacement="", x=names(d529), perl=TRUE)


  # Extract the signals for the entire data set
  signalsAllOneDay   <- oneMinData[oneMinData$Signal!=0, ]
  signalsLongOneDay  <- oneMinData[oneMinData$Signal==1, ]
  signalsShortOneDay <- oneMinData[oneMinData$Signal==-1, ]

  chartProps <- genTAForCharts("ES", oneMinData)

  chartSeries(oneMinData, subset=chartProps$ss,
              name=chartProps$messageStr,
              type="candlesticks",
              theme=chartTheme(theme='white', up.col='green', dn.col='red'),
              TA=chartProps$ta
  )

} # End of plotting code


# Create a new xts object which contains just the model data
# "Allocate" the xts object in a ridiculous manner.
# (stupid/hacky R stuff 'SHRS'... trying to prevent coercion into a  list)

model        <- Cl(d529) 
names(model) <- "f1"  # rename the column to "f1", short for "feature 1"  (SHRS)


# Build some "features" for the ML model.  I have not given a lot of thought to
# selecting the features... They are simply easy to compute and I have to give
# the model some features...
model$f1 <- d529$BW05 / d529$VWAP   # feature one is BW05/VWAP

model$f2 <- rep(0, nrow(model))     # (SHRS)
model$f2 <- d529$KFHI20 / d529$BW05

model$f3 <- rep(0, nrow(model))     # (SHRS)
model$f3 <- d529$KFLO20 / d529$BW05

model$f4 <- rep(0, nrow(model))     # (SHRS)
model$f4 <- d529$STDEV / d529$BW05

# The data as read from the csv file contains long trades, short trades, and
# time periods where no action is taken.  Reduce this to just the minutes where
# a long trade is taken.
longTrades <- d529[ d529[, 'Signal.ES'] == 1, which.i=TRUE]

# Limit the features object to just the long trade rows
model <- model[longTrades, ]

# Compute the label.  Divide MFE by it's median.  All >=1 are considered
# desirable; <1 undesirable.  We want to predict those >= 1.
mfeMedian   <- median(as.vector(d529[longTrades, 'MFE.ES']))
model$Label <- rep(0, nrow(model))   # (SHRS)
model$Label <- d529[longTrades, 'MFE.ES'] / mfeMedian

model$Label[model$Label >= 1] <- 1  # Scale label to meet library/solver requirements.
model$Label[model$Label < 1]  <- 0


cat("Feature Data & Label have been created\n") ; flush.console()
## stop("DEBUGGING STOP")


############################################################
######### GMB Related Parameters START #####################

GBM.distribution = "adaboost"    # bernouli (logistic regression for 0-1 outocmes)
                                 # adaboost (exp loss for 0-1 outcomes)
                                 # poisson (count outcomes)
GBM.n.trees=400          # The total number of trees to fit. This is equivalent to 
                         # the number of iterations and the number of basis functions 
                         # in the additive expansion.

GBM.shrinkage=0.01       # Learning rate (0.001 to 0.1), aka shrinkage, step-size reduction
GBM.bag.fraction=0.5     # For stochastic gradient boosting this needs to be 0.5
GBM.cv.folds=10          # Typical Cross Validation factor

GBM.interaction.depth=4  # "Tree Depth" The maximum depth of variable interactions. 
                         # 1 implies an additive model, 2 implies a model 
                         # with up to 2-way interactions, etc.
GBM.n.minobsinnode=5     # Minimum number of observations in the trees terminal nodes.

GBM.train.fraction=1.0   # The first train.fraction * nrows(data)
                         # observations are used to fit the gbm and
                         # the remainder are used for computing out-of-sample 
                         # estimates of the loss function.




gbmTrain <- gbm( Label ~ . , data=model, 
  distribution=GBM.distribution,
  n.trees=GBM.n.trees,             
  shrinkage=GBM.shrinkage,
  bag.fraction=GBM.bag.fraction,
  cv.folds=GBM.cv.folds,
  interaction.depth=GBM.interaction.depth,
  n.minobsinnode=GBM.n.minobsinnode,
  train.fraction=GBM.train.fraction
)


plotFileName <- paste( outputDirName, "/gbmOutput.pdf", sep="")
pdf(file=plotFileName, width=13, height=7.5)

bestIter <- gbm.perf(gbmTrain, method="cv")
# Optionally plots various performance measures
cat("\nGBM estimates the optimal number of boosting iterations = ", bestIter, "\n", sep="")
print(summary(gbmTrain, n.trees=bestIter, plotit=TRUE, order=TRUE, cBars=10)) # based on the estimated best number of trees

# compactly print the first and best trees for curiosity
print(pretty.gbm.tree(gbmTrain,1))
print(pretty.gbm.tree(gbmTrain,bestIter))


#create marginal plots
par(mfrow=c(1,4))
plot.gbm(gbmTrain, i.var=1, n.tree=bestIter)
plot.gbm(gbmTrain, i.var=2, n.tree=bestIter)
plot.gbm(gbmTrain, i.var=3, n.tree=bestIter)
plot.gbm(gbmTrain, i.var=4, n.tree=bestIter)
par(mfrow=c(1,1))

# contour plot of variables 1 and 2 after "best" iterations
plot.gbm(gbmTrain, i.var=1:2, n.tree=bestIter)

# 3-way plots
plot.gbm(gbmTrain, i.var=c(1,2,3), n.tree=bestIter, cont=20)

dev.off()
system(paste("open ", plotFileName, sep=""))
