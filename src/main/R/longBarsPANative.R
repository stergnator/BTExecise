# vim: ts=2 sts=2 sw=2 ht=2 expandtab number ruler showmode smartindent ff=unix foldmethod=marker :
############################################################
#
# longBarsPANative.R - A program that formulates a simple crossover system,
# plots the data in weekly candlestick charts, performs quantstrat analysis,
# and then a PerformanceAnalytics analysis. This program was derived from
# "longBarsPA.R".  The new emphasis in this program is to use the quantstrat
# native modeling technique.  We need to do this to take advantage of speedups,
# parameter optimization features, and separation of signal generation code
# from the blotter accounting calls.  Finally, generate the pdf performance
# report using RMarkDown.
#
# The default time frame of 30 minute bars is changeable via the
# "barSizeInMins" variable
#
# The primary system is the Simple Moving Average(48) with a 2
# Period Away Filter (sma48-2Away).
#
# For securities that trade 24 hours (FX and Futures) trading time can be
# limited to regular hours via the "regularTradingHoursOnly" variable.
#
# This program will always work on just one security in just one start/stop
# time period.  This is in contrast to the previous versions that allowed many
# securities on many time ranges.
#
# USAGE: to run this program from the command line:
#     R CMD BATCH --no-save --no-restore longBarsPANative.R
#
# Created: 7/2/2014
#

# TODO: (8/4/2014)
# Where does the transaction price come from?
# For monthly, quarterly and yearly data, quantstrat will use the current bar
# to get the price.   For anything with a higher frequency (eg.  weekly, daily,
# intraday), quantstrat will use the next bar to get the price."
# https://stat.ethz.ch/pipermail/r-sig-finance/2012q3/010568.html
#
# A quantstrat author suggests:
#    I suggest that you use prefer='Open' [... in add.rule()/ruleSignal()] to
#    use the opening rate of the next bar instead. Be aware that your results
#    will be unrealistic though, because in practice you will usually not get
#    the opening price of the next bar.
#
# To which STM says "well, the next bar's open price is a lot closer to the
# current bar's close price which should only be a few seconds apart." The
# close price of the next bar is 30 mins away(!) and that price is not nearly
# as realistic as compared to the open.



# Since we are now using rm.strat() below, we can no longer clear the work space using this technique.
# rm(list=ls(all=TRUE))

library(chron)
library(RSQLite)
library(quantstrat)
library(quantmod)
library(lubridate)
library(PerformanceAnalytics)
library(xtable)
library(rmarkdown)


############################################################
# Set the TimeZone.  This is critical.
#
# Here's the quoted help from Joshua:
#
#    You need to explicitly set the timezone of your R session using:
#    Sys.setenv(TZ="UTC").
#
#    If you do not do this, then the blotter portfolio xts object will be
#    initialized with your current system's timezone, but the timezone on the GE
#    data is "UTC" because it has a "Date" class index.  That causes the chain
#    rule's parent order price lookup to fail (because the timestamps do not
#    match).
#
# Wow! That is subtle.  Even though all the examples clearly show setting the
# timezone, I somehow managed to miss it.
Sys.setenv(TZ="GMT")
############################################################


# ticker <- "AAPL"
# ticker <- "AMZN"
# ticker <- "CL-20140520"
# ticker <- "ES-20140620"
# ticker <- "EUR-20140616"
# ticker <- "EURUSD"
# ticker <- "GBPUSD-20140616"
# ticker <- "GLD"
# ticker <- "GOOG"
# ticker <- "NQ-20140620"
# ticker <- "ORCL"
  ticker <- "SPY"
# ticker <- "USDCAD"
# ticker <- "USDCAD-20140617"
# ticker <- "USO"
# ticker <- "YM-20140620"

  barSizeInMins   <- 30

  # How many weeks of data should we load from the sqlite db.
  numWeeks  <- 25
  startDate <- as.POSIXct(Sys.Date() - days(numWeeks*7))
  endDate   <- now()

# startDate <- as.Date("2012-09-30")             # a time period
# endDate   <- as.POSIXct("2012-10-05 23:59:59")
# 
# startDate <- as.Date("2014-05-01")             # a time period
#
# startDate <- as.POSIXct("2014-05-01 00:00:02")
# endDate   <- as.POSIXct("2014-05-31 23:59:59")

# Limit data to regular market hours: 'T06:30/T13:00'
regularTradingHoursOnly = TRUE

startDateStr <- as.character( startDate )
endDateStr   <- as.character( endDate )

# Create an ISO:8601 time interval between the start and end date
# Update this below after the data is loaded from the DB
subsetStr    <- paste(startDate, '/', endDate, sep='') # '2013-07-08/2013-07-12'


# The strategy name we will pass into performance analytics
strategyStr  <- 'SMA48W2Away'
portfolioStr <- 'SMA48W2Away'

############################################################
# The location of the sqlite db files.
dbPath <- "~/runTime"

# The directory path to where the output files will be written.  This
# value will be overwritten in the machineInfo() function.
outputDirName <- "~/tmp"

# The directory path to where the markdown files can be found.  This
# value will be overwritten in the machineInfo() function.
reportInputDir    = "~/IB/securities/securities/src/main/R"

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
    dbPath         <<- "~/runTime"
    outputDirName  <<- "~/tmp"
    reportInputDir <<- "~/IB/securities/securities/src/main/R"

  } else if ( grepl("hackintosh", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's Hackintosh\n")
    dbPath         <<- "~/runTime"
    outputDirName  <<- "~/tmp"
    reportInputDir <<- "~/IB/securities/src/main/R"

  } else if ( grepl("RoadRunner", actualMachine, ignore.case=TRUE  ) ) {
    cat("Initialize variables specific to Sterg's Macbook Pro - RoadRunner\n")
    dbPath         <<- "~/runTime"
    outputDirName  <<- "~/tmp"
    reportInputDir <<- "~/IB/securities/src/main/R"

  } else if (  grepl("garage4", actualMachine, ignore.case=TRUE ) ) {
    cat("Initialize variables specific to Sterg's PC in the Garage\n")
    dbPath         <<- "~/runTime"
    outputDirName  <<- ""
    reportInputDir <<- ""
    stop("garage4 does not have machine variables initialized")

  } else if (  grepl("Joes-iMac.local", actualMachine, ignore.case=TRUE ) ||
               grepl("Joes-MacBook-Pro.local", actualMachine, ignore.case=TRUE ) ) {
    cat("Initialize variables specific to Joes's iMac\n")
    dbPath         <<- "~/runTime"
    outputDirName  <<- "~/workspace/securities/src/test/resources"
    reportInputDir <<- "~/workspacw/securities/src/main/R"

  } else {
    cat("WHAT MACHINE IS THIS PROGRAM RUNNING ON?",
        "Please send Stergios the output from running this command:\n",
        "\n\tSys.info()[['nodename']]",
        "\n\n") 
    stop("Cannot proceed until the program knows who is running this program.")

  }
  flush.console()

}
machineInfo()


############################################################
createPlotFileName <- function(startDate, endDate, ticker, barSizeInMins)
{
  startDateStr2 <- format(startDate, "%Y%m%d")
  endDateStr2   <- format(endDate, "%Y%m%d")
  if ( length(ticker) == 1 ) 
  {
    plotFileName <- paste(outputDirName, "/", ticker, "-", barSizeInMins, "min-", 
                          startDateStr2, "-", endDateStr2,
                          ".pdf", sep="")
  } else {
    stop(paste( "createPlotFileName() ERROR. ticker has length greater than one:  '", ticker, "'", sep="") )
  }
}

############################################################
# data <- get5SecData("YM", "2012-01-07 15:00:00", "2012-10-09 11:10:00")
#
get5SecData <- function(ticker, from, to) 
{
  ## DEBUG    ticker <- "YM"
  ## DEBUG    from   <- "2012-10-07 15:00:00"
  ## DEBUG    to     <- "2012-10-09 11:00:00"

  from.secs <- as.numeric(as.POSIXct(from, tz = "America/Los_Angeles"))
  to.secs   <- as.numeric(as.POSIXct(to,   tz = "America/Los_Angeles"))

  fn <- paste(dbPath, "/fiveSecond-", ticker, ".sqlite", sep="")
  # DEBUG print(fn)
  db <- dbConnect( dbDriver("SQLite") , dbname=fn, flags=SQLITE_RO, cache_size=64000, synchronous=0 )
  dbGetQuery(db, "pragma cache_size=64000") 
  dbGetQuery(db, "pragma synchronous=0") 

  sql       <- sprintf("SELECT start, tsStart, open, high, low, close, volume FROM Bars5Seconds WHERE start >= %d  AND start < %d ORDER BY start", from.secs, to.secs )
 
  ## Wrap the call to dbGetQuery() with error handling code such that if it is
  ## simply a BUSY error, we will wait 1/2 second and try again.  If it is
  ## another error, well, that does not really matter at this point since the
  ## program will fail in an yet unknown way...  Below is the error message we
  ## want to avoid: 
  ##     Error in sqliteFetch(rs, n = -1, ...) : 
  ##       RSQLite driver: (RS_SQLite_fetch: failed first step: database is locked)
  ##
  ## This happened again after the error handling was put in place.
  ##     Error in sqliteFetch(rs, n = -1, ...) : 
  ##       RSQLite driver: (RS_SQLite_fetch: failed first step: database is locked)
  ##
  ##  It happened AGAIN, after I switched to a lower level function approach!
  ##     Error in sqliteFetch(rs, n = -1, ...) : 
  ##       RSQLite driver: (RS_SQLite_fetch: failed first step: database is locked)
  ##

  attempt <- 0
  gotResultsSet <- FALSE
  while(!gotResultsSet) {
    attempt <- attempt + 1
    resultSetObj <- tryCatch(dbSendQuery(db, sql), 
                              error=function(e) 
                              { 
                                print("Caught RSQLite Error: dbSendQuery() Failed")
                                print(e)
                                return(e)
                              }
                            )
    error <- dbGetException(db) 

    if (error$errorMsg != "OK") {
      cat("SQLite returned an error on dbSendQuery().  Wait 0.1 seconds and try again\n") 
      print(error)
      ##  OBSERVED:    $errorNum
      ##  OBSERVED:    [1] 5
      ##  OBSERVED:    
      ##  OBSERVED:    $errorMsg
      ##  OBSERVED:    [1] "error in statement: database is locked"
      
      ## Do not call dbClearResult(resultSetObj) here or else the following error msg is produced
      ##     Error in function (classes, fdef, mtable)  : 
      ##       unable to find an inherited method for function "dbClearResult", for signature "simpleError"

      Sys.sleep(attempt * 0.1)  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
      next
    } 
    gotResultsSet <- TRUE 
  }

  attempt <- 0
  gotData <- FALSE
  while(!gotData) {
    attempt <- attempt + 1
    trades <- tryCatch(fetch(resultSetObj, n=-1), 
                         error=function(e)
                         {
                           print("Caught RSQLite Error: fetch() Failed")
                           print(e)
                           return(e)
                         }
                      )
    error  <- dbGetException(db) 

    ##  DEBUG    print(class(trades))
    ##  OBSERVED    [1] "Caught RSQLite Error: fetch() Failed"
    ##  OBSERVED    Error in trades[, -c(1, 2)] : incorrect number of dimensions
    
    if (inherits(trades, "error") ) {
      cat("SQLite returned an error on fetch().  Using inherits().... Wait 0.1 seconds and try again\n") 
      print(error)
      Sys.sleep(attempt * 0.1) # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
      next 
    }

    if (error$errorMsg != "OK" ) {
      cat("SQLite returned an error on fetch().  Using error$errorMsg.... Wait 0.1 seconds and try again\n") 
      print(error)
      Sys.sleep(attempt * 0.1)  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
      dbClearResult(resultSetObj) # we probably need to call this here.  Untested so far.
      next 
    }

    gotData <- TRUE

    dbClearResult(resultSetObj)
  }

  dbDisconnect(db)

  # FX will return "-1" or negative volumes.  So just set it to +1
  trades$volume <- ifelse(trades$volume == -1, 1, trades$volume)
  # Filter out "0" volume trades
  # trades <- trades[trades$volume > 0, ]

  trades.xts  <- as.xts(trades[, -c(1,2)], order.by=as.POSIXct(trades[, "tsStart"], format="%Y%m%d %H:%M:%S" ) )
  colnames(trades.xts) <- c("Open", "High", "Low", "Close", "Volume")
  return(trades.xts)
}

############################################################
# dataInHigherTimeframe <- higherTimeFrame(data.5second, period='minutes', size=15)
higherTimeFrame <- function(ticker, d, period='minutes', size=1) 
{
  data.htf <- to.period(d, period=period, k=size, indexAt="startof")
  colnames(data.htf) <- c("Open", "High", "Low", "Close", "Volume")
  if (ticker == 'EURUSD' ) {
    # Force FX to always have volume = 1 
    data.htf[, 'Volume'] <- 1
  }
  return(data.htf)
}

############################################################
# dataInHigherTimeframe <- filterByTimes(data.htf, 'T06:30/T13:00')
filterByTimes <- function(x, filterStr)
{
  return(x[filterStr,])
}

# CUD is the n-period sum of (up periods = 1) and (down periods = -1)
CUD = function(cl, n)
{   temp = runSum(ifelse(ROC(cl, 1, type="discrete") > 0, 1, -1), n)
    colnames(temp) = "CUD"
    temp
}
############################################################
# getXOverPeriodsAway() - a function to filter crosses via a "days-away" criteria a'la MacGee & WHC Bassetti's Basing Point Procedure
#   PARAMETERS - 
#     x1                   - Primary Price Data.  Must be a xts/OHLC object.
#     filterColName        - The mollifier originally used to generate the crosses that appear in x1[, crossColName]
#     crossesColName       - The crosses of x1 & x2 - in a particular direction (only longs, or only shorts).
#     cancelCrossesColName - The immediately following crosses in the opposite direction found in 'crossesColName'.
#     periodsAway          - How many 'days away' (in the Magee/Bassetti sense) are required for the filter.
#     direction            - Are we looking for 'LONGS' or 'SHORTS'?
#
#     S$goLong  = getXOverPeriodsAway(S, "SMA48", "SMA48XUp", "SMA48XDown", periodsAway, 'LONG') 
#     S$goShort = getXOverPeriodsAway(S, "SMA48", "SMA48XDown", "SMA48XUp", periodsAway, 'SHORT') 
getXOverPeriodsAway <- function( x1, filterColName, crossesColName, cancelCrossesColName, periodsAway, direction) 
{

  ## DEBUG  BLOCK
  ##    x1                   = S
  ##    filterColName        = "SMA48" 
  ##    direction            = "LONG"
  ##    crossesColName       = "SMA48XUp"
  ##    cancelCrossesColName = "SMA48XDown"
  ##    periodsAway          = 2
  ## DEBUG  BLOCK

  if ( ! is.xts(x1) ) {
    stop("getXOverPeriodsAway() - Only works on OHLC/xts objects.\n")
  }

  # Prepare the return object as an xts 1 column object with the same index as in x1
  filteredCrosses        = Cl(x1)
  filteredCrosses[, 1]   = rep(0, nrow(filteredCrosses) )
  names(filteredCrosses) = paste(filterColName, "FC", direction, sep="")

  # Reduce the crosses to just those rows where a cross occurs
  crosses = na.omit(x1[, crossesColName])

  # Reduce the cross cancellations to just those rows where a cancel occurs
  cancelCrosses = na.omit(x1[, cancelCrossesColName])

  # Extract the row numbers of the cross cancellations into a simple vector
  cancelCrossesRN = x1[index(cancelCrosses), which.i=TRUE]

  if( nrow(crosses) < 1 ) {
    stop("getXOverPeriodsAway() - No crosses to work on.\n")
  }

  for (j in 1:nrow(crosses)) 
  {
    ## DEBUG j = 1
    xOver = x1[index(crosses[j]), which.i=TRUE] # xOver is a scalar, i.e. a "row number"
    ## DEBUG cat(direction, " xOver(", as.character( index(crosses[j])), ")\n\t\t", sep="")
    foundEntry = FALSE
    periodsAwayArray = rep(0, periodsAway)
    periodsAwayCounter = 0 
    cancel = FALSE
    for(i in c(1:periodsAway) ) {
      ## DEBUG i=1
      offSet = xOver + i
      if ( offSet > nrow(x1) ) # Don't search past the end of the price data
      {
        next
      }
      if ( any ( offSet == cancelCrossesRN ) && cancel == FALSE ) {
        ## DEBUG cat( "\n\t\tCANCELED on offSet = ", i, " (", as.character(index(x1[offSet,])), ')', sep="")
        cancel = TRUE
      }
      if ( cancel == TRUE ) {
        next
      }

      if ( direction == 'LONG' ) {
        # --------------LONG SIDE START --------------------------
        lowPrice = as.numeric(Lo(x1[offSet] ) )
        if ( lowPrice > as.numeric(x1[offSet, filterColName]) ) {
          ## DEBUG cat("above ")
          periodsAwayArray[i] = 1
          periodsAwayCounter = periodsAwayCounter + 1
          if (periodsAwayCounter >= periodsAway && foundEntry == FALSE) {
            filteredCrosses[index(x1[offSet])] = 1 # = offSet
            foundEntry = TRUE
          }
        } else {
          ## DEBUG cat("below ")
          periodsAwayArray[i] = 0
        }

        # --------------LONG SIDE END --------------------------

      } else if ( direction == 'SHORT' ) {

        # --------------SHORT SIDE START --------------------------

        highPrice = as.numeric(Hi(x1[offSet,] ) )
        if ( highPrice < as.numeric(x1[offSet, filterColName]) ) {
          ## DEBUG cat("below ")
          periodsAwayArray[i] = 1
          periodsAwayCounter = periodsAwayCounter + 1
          if (periodsAwayCounter >= periodsAway && foundEntry == FALSE) {
            filteredCrosses[index(x1[offSet])] = -1 # = offSet
            foundEntry = TRUE
          }
        } else {
          ## DEBUG cat("above ")
          periodsAwayArray[i] = 0
        }

        # --------------SHORT SIDE END --------------------------

      } else {

        stop(paste( "ERROR. What direction is this: '", direction, "'", sep="") )

      }

    }
    ## DEBUG cat("\n\t\t")
    ## DEBUG print(periodsAwayArray)
    ## DEBUG cat("\n")
  }
  return(filteredCrosses)
}

############################################################
############################################################
# The guts
#
############################################################
############################################################

############################################################
# Open a pdf file for charts to be printed to.  This is not
# for the markdown reports.  This is for intermediate reporting.
# This may go away in the future.
############################################################

plotFileName = createPlotFileName(startDate, endDate, ticker, barSizeInMins)
pdf(file=plotFileName, width=13.5, height=8.0)

cat("Working on ", ticker, "\n", sep="") 

# Load the 5 seconds data from a sqlite db file.
S = get5SecData(ticker, startDateStr, endDateStr)

# convert the 5-Second data into 30-minute data
S = higherTimeFrame(ticker, S, period='minutes', size=barSizeInMins)

if ( regularTradingHoursOnly ) {
  # Limit data to regular market hours:
  S = filterByTimes(S, 'T06:30/T13:00')
}

# The Main Filter Line is the SMA(48)
S$SMA48   = SMA(Cl(S), n=48)
S$VWAP48  = VWAP(price=Cl(S), volume=Vo(S), n=48) 

# Compute the cross overs of the SMA48 and the S closing price.
# Find the cross overs and store them in the XTS object.

############################################################
# Return the xts index values from a column in an xts object matching a
# particular value.  Think of this as a more flexible version of na.omit()..
#
#  interestingIndices = findIndexValues(S, "goShort", -1)
#  S$SMA48W2Away[ findIndexValues(S, "goShort", -1) ] = -1
#
findIndexValues <-  function(x, colName, findValue)
{
  ## DEBUG VALUES
  ##  x         = S
  ##  colName   = "goShort"
  ##  findValue = -1
  ##  index(x[x[, colName] == findValue])
  ## DEBUG VALUES
  return ( index( x[  x[, colName] == findValue   ] ) )
}


# Extract the simple cross overs and add them to the main XTS "S" object.
# Be careful with the results returned by sigCrossover.  It returns TRUE/NA in
# an xts object of equal length.  As I found out a column full of mostly NA's
# is not good input for sigThreshold()
S$SMA48XUp   = sigCrossover(label="longCross", data=S, columns=c("Close","SMA48"), relationship=c("gt") )
S$SMA48XDown = sigCrossover(label="shortCross", data=S, columns=c("Close","SMA48"), relationship=c("lt") )

# Now filter the just found cross overs by enusring the next "two bars" are
# still "away" (ala basing point proceedure)
periodsAway = 2

# Find the 2 bars away filtered entires for the LONG side.
# DEBUG
cat("LONG SIDE: ") ;cat(rep('#', 40), sep="") ; cat("\n") 
S$goLong = getXOverPeriodsAway(S, "SMA48", "SMA48XUp", "SMA48XDown", periodsAway, 'LONG') 
# DEBUG
cat("S$goLong\n")
# DEBUG
print(S$goLong[S$goLong==1])

# Find the 2 bars away filtered entires for the SHORT side.
# DEBUG
cat("SHORT SIDE: ") ;cat(rep('#', 40), sep="") ; cat("\n") 
S$goShort = getXOverPeriodsAway(S, "SMA48", "SMA48XDown", "SMA48XUp", periodsAway, 'SHORT') 
# DEBUG
cat("S$goShort\n")
# DEBUG
print(S$goShort[S$goShort==-1])

# Combine the long and short filtered signals into one indicator column.  This
# will be useful for adding indicators, signals, and rules for quantstart.
S$SMA48W2Away = S$goLong

S$SMA48W2Away[ findIndexValues(S, "goShort", -1) ] = -1
              
# The indicator function that will be referenced by add.indicator()
# Note: It is reasonable to include the previous 30 lines (starting at #541) as
# part of this function.  For now keep it external for easy debugging.
SMAAWAY <- function(n=2) 
{
  return( S[, "SMA48W2Away"] )
}

# Finished filtering the entries for sma48 xOver with price and
# an entry of "two bars away" (ala basing point proceedure)

############################################################
# Start Quantstrat Work Here
############################################################

# Start with quantstrat setup

# All currency instruments must be defined before instruments of other types
# may be defined.  We only work with US Dollar, so define it and move on.
currency("USD")

# For a simple stock or etf:
tickerSymbol = "S"
stock(tickerSymbol, currency="USD", multiplier=1)

############################################################
# Facilitate re-running this code from within an active R environment.  If
# you use rm.strat() then you cannot clear the work space using
# "rm(list=ls(all=TRUE))"
rm.strat(strategyStr)
initDate = index(S[1]) - 3600
initPortf(name=strategyStr, symbols=tickerSymbol, initDate=initDate, currency="USD")

# The initAcct function constructs the data container used to store
# calculated account values such as aggregated P&L, equity, etc.  NOTE: that
# initDate is prior to the start of the data - double check that it actually
# is.
initAcct(name=strategyStr, portfolios=portfolioStr, initDate=initDate, initEq=1e4, currency="USD")

# The function initOrders sets up an order container for the portfolio.
initOrders(portfolio=portfolioStr, initDate=initDate)

# The function strategy creates a strategy object.
strategy(strategyStr, store=TRUE)

# ls(.blotter)
# .blotter holds the portfolio and account object 
# [1] "account.SMA48W2Away"   "portfolio.SMA48W2Away"

# ls(.strategy)
# .strategy holds the orderbook and strategy object
# [1] "order_book.SMA48W2Away" "SMA48W2Away"           

summary(getStrategy(strategyStr))

##    Indicators are typically standard technical or statistical analysis
##    outputs, such as moving averages, bands, or pricing models
##    
##    Indicators are applied before signals and rules, and the output of
##    indicators may be used as inputs to construct signals or fire rules
##    
##    My indicator function called SMAAWAY() returns the combined results
##    of calling getXOverPeriodsAway() for both LONG and SHORT scenarios.
##    QQQQ Perhaps I should remove the "48" part of the name....
##

# This will add a column to mktdata named "SMA48W2Away.SALab" because the
# SMAAWAY() function returns an xts object which has a column named
# "SMA48W2Away" which then has ".SALab" concatenated on to it.
add.indicator(strategy=strategyStr, name="SMAAWAY", 
              arguments=list(n=2), label="SALab")

## Add signal for 2 days away on the long side.  (adds column named "sigGoLong" to mktdata)
add.signal(strategy=strategyStr, name="sigThreshold", 
           arguments=list(column="SMA48W2Away.SALab", relationship="eq", threshold=1, cross=TRUE),
           label="sigGoLong" )

## Add signal for 2 days away on the short side.  (adds column named "sigGoShort" to mktdata)
add.signal(strategy=strategyStr, name="sigThreshold", 
           arguments=list(column="SMA48W2Away.SALab", relationship="eq", threshold=-1, cross=TRUE),
           label="sigGoShort" )

## In mktdata the signal columns are named sigGoLong and sigGoShort

############################################################
###############  TRADING RULES - START #####################
############################################################

## We will use the notion of "Bracket Orders".
## 
## Bracket orders are designed to help limit your loss and lock in a profit by
## "bracketing" an order with two opposite-side orders. A BUY order is bracketed
## by a high-side sell limit order and a low-side sell stop order. A SELL order is
## bracketed by a high-side buy stop order and a low side buy limit order.
## 
## Bracket orders are an effective way to manage your risk and lock in a profit on
## an order that has yet to execute. 
## 
## The order quantity for the high and low side bracket orders matches the
## original order quantity in our current use.  This will change as we use
## multiple profit target orders, and replace the initial stop-loss order with a
## trailing-stop order after N-bars.
## 
## Here's more on the idea behind a Bracket Order:
## http://www.online-stock-trading-guide.com/bracket-order.html


# Add Exit Rules First

# Exit all longs when we see price crossing down below SMA48.  Think of this as the initial stop-loss rule when we first go long.
add.rule(strategy=strategyStr, name='ruleSignal',
  arguments = list(
    sigcol      = "SMA48XDown",   # Col name to check for signal
    sigval      = TRUE,           # signal value to match
    replace     = FALSE, # When this rule fires, the Profit Taker will get canceled by the orderset "ocolong", so we do not need to replace any other open orders using this flag.  That's the theory anyway, let's see if it works!
    orderside   = 'long',
    ordertype   = 'market',
    orderqty    = 'all',
    prefer      = 'Open'

  ),
  type   = 'exit',
  label  = 'ExitLongSMADown'
)

# Price Target Exit Rule
myTarget = 1.25
# Add a long target, i.e. a profit taking target for UNIT 1
add.rule(strategyStr, name = 'ruleSignal',
  arguments=list(sigcol='sigGoLong' , sigval=TRUE,
    replace   = FALSE,
    orderside = 'long',
    ordertype = 'limit',
    tmult     = FALSE,
    threshold = myTarget,
    orderqty  = -1,
    prefer    = 'Open'
  ),
  type   = 'chain',
  parent = 'EnterLong',
  label  = 'ExitLongPT'
)

# Add a Trailing Stop for the long side after/when the profit taker executes.
trailingStopThreshold = 1
add.rule(strategyStr, name='ruleSignal',
  arguments = list(
    sigcol    = "sigGoLong", sigval=TRUE, 
    replace   = TRUE,  # There is 1 other open order when this rule is executed, namely "ExitLongSMADown".  But it will be canceled by the orderset "ocolong", so I do not need this rule to replace it.  That's the theory anyway.  Let's see if it works!
#   orderqty  = -1,    # QQQQ    This will generate an error from line 282 in order.R.  Somehow the -1 gets converted to a string and the is.numeric() test fails.  We can use the 'all' value here.  So use it and move on.  UGH. 
    orderqty  = 'all',
    ordertype = 'stoptrailing',
    orderside = 'long',
    tmult     = FALSE,
    threshold = trailingStopThreshold,
    ruletype  = 'exit',
    prefer    = 'Open'
  ),
  type   = 'chain',
  parent = 'ExitLongPT',
  label  = 'TrailingExitLong'
)

# Go Long when we have long signal.
add.rule(strategyStr, name = 'ruleSignal',
  arguments=list(
    sigcol='sigGoLong' , sigval=TRUE,
    replace   = FALSE,
    orderside = 'long' ,
    ordertype = 'market',
    orderqty  = 2,
    prefer    = 'Open'
  ),
  type  = 'enter',
  label = 'EnterLong'
)

## SHORT COMMENT    # Exit shorts when we see price crossing up on SMA48 - Original exit rule
## SHORT COMMENT    add.rule(strategy=strategyStr, name='ruleSignal',
## SHORT COMMENT      arguments = list(sigcol="SMA48XUp", sigval=TRUE,
## SHORT COMMENT        replace     = FALSE,
## SHORT COMMENT        orderside   = 'short',
## SHORT COMMENT        ordertype   = 'market',
## SHORT COMMENT        orderqty    = 'all',
## SHORT COMMENT        orderset    = 'ocoshort',
## SHORT COMMENT        pricemethod = 'market'),
## SHORT COMMENT      type  = 'exit',
## SHORT COMMENT      label = 'ExitCBShort'
## SHORT COMMENT    )


## SHORT COMMENT    # Stop-Loss for Short side
## SHORT COMMENT    add.rule(strategyStr, name = 'ruleSignal',
## SHORT COMMENT     arguments=list(sigcol='sigGoShort' , sigval=TRUE,
## SHORT COMMENT      replace   = FALSE,
## SHORT COMMENT      orderside = 'short',
## SHORT COMMENT      ordertype = 'stoplimit', tmult=FALSE, threshold = 1.0,
## SHORT COMMENT      orderqty  = 'all',
## SHORT COMMENT      orderset  = 'ocoshort',
## SHORT COMMENT      prefer      = 'Open'
## SHORT COMMENT     ),
## SHORT COMMENT     type='chain', parent='EnterSHORT',
## SHORT COMMENT     label='StopLossSHORT'
## SHORT COMMENT    )

## COMMENT FOR TRAILING STOP EXP    # Stop-Loss for Long side
## COMMENT FOR TRAILING STOP EXP    add.rule(strategyStr, name = 'ruleSignal',
## COMMENT FOR TRAILING STOP EXP     arguments=list(sigcol='sigGoLong' , sigval=TRUE,
## COMMENT FOR TRAILING STOP EXP      replace   = FALSE,
## COMMENT FOR TRAILING STOP EXP      orderside = 'long',
## COMMENT FOR TRAILING STOP EXP      ordertype = 'stoplimit', tmult=FALSE, threshold = -1.0,
## COMMENT FOR TRAILING STOP EXP      orderqty  = 'all',
## COMMENT FOR TRAILING STOP EXP      orderset  = 'ocolong',
## COMMENT FOR TRAILING STOP EXP      prefer    = 'Open'
## COMMENT FOR TRAILING STOP EXP     ),
## COMMENT FOR TRAILING STOP EXP     type='chain', parent='EnterLONG',
## COMMENT FOR TRAILING STOP EXP     label='StopLossLONG'
## COMMENT FOR TRAILING STOP EXP    )


## HOLD OUT    # Add a Trailing Stop for the long side
## HOLD OUT    add.rule(strategyStr, name = 'ruleSignal',
## HOLD OUT      arguments=list(sigcol='sigGoLong' , sigval=TRUE,
## HOLD OUT        replace   = FALSE,
## HOLD OUT        orderside = 'long',
## HOLD OUT        ordertype = 'stoptrailing', tmult=TRUE, threshold=0.004,
## HOLD OUT        orderqty  = 'all',
## HOLD OUT        orderset  = 'ocolong',
## HOLD OUT        prefer    = 'Open'
## HOLD OUT      ),
## HOLD OUT      type='chain', parent='EnterLONG',
## HOLD OUT      label='StopTrailingLONG'
## HOLD OUT    )




## SHORT COMMENT    # Add a short target
## SHORT COMMENT    add.rule(strategyStr, name = 'ruleSignal',
## SHORT COMMENT     arguments=list(sigcol='sigGoShort' , sigval=TRUE,
## SHORT COMMENT      replace   = FALSE,
## SHORT COMMENT      orderside = 'short',
## SHORT COMMENT      ordertype = 'limit', tmult=FALSE , threshold=-myTarget,
## SHORT COMMENT      orderqty  = 'all',
## SHORT COMMENT      orderset  = 'ocoshort',
## SHORT COMMENT      prefer    = 'Open'
## SHORT COMMENT     ),
## SHORT COMMENT     type='chain', parent='EnterSHORT',
## SHORT COMMENT     label='TakeProfitSHORT'
## SHORT COMMENT    )

## SHORT COMMENT    ## DEBUG    # Add a Trailing Stop for the short side
## SHORT COMMENT    ## DEBUG    add.rule(strategyStr, name = 'ruleSignal',
## SHORT COMMENT    ## DEBUG      arguments=list(sigcol='short' , sigval=TRUE,
## SHORT COMMENT    ## DEBUG        replace   = FALSE,
## SHORT COMMENT    ## DEBUG        orderside = 'short',
## SHORT COMMENT    ## DEBUG        ordertype = 'stoptrailing', tmult=TRUE, threshold=0.004,
## SHORT COMMENT    ## DEBUG        orderqty  = 'all',
## SHORT COMMENT    ## DEBUG        orderset  = 'ocoshort',
## SHORT COMMENT    ## DEBUG        prefer    = 'Open'
## SHORT COMMENT    ## DEBUG      ),
## SHORT COMMENT    ## DEBUG      type='chain', parent='EnterSHORT',
## SHORT COMMENT    ## DEBUG      label='StopTrailingSHORT'
## SHORT COMMENT    ## DEBUG    )

# Normal entry rules


## SHORT COMMENT    # Go Short when we have short signal - original entry rule
## SHORT COMMENT    add.rule(strategyStr, name = 'ruleSignal',
## SHORT COMMENT     arguments=list(sigcol='sigGoShort', sigval=TRUE,
## SHORT COMMENT      replace   = FALSE,
## SHORT COMMENT      orderside = 'short',
## SHORT COMMENT      ordertype = 'market',
## SHORT COMMENT      orderqty  = -1,
## SHORT COMMENT      orderset  = 'ocoshort',
## SHORT COMMENT      prefer    = 'Open'
## SHORT COMMENT     ),
## SHORT COMMENT     type  = 'enter',
## SHORT COMMENT     label = 'EnterSHORT'
## SHORT COMMENT    )



############################################################
###############  TRADING RULES - END #######################
############################################################


out = applyStrategy(strategy=strategyStr, portfolios=portfolioStr,
                    verbose=TRUE
                    )

dateRange=paste(
           as.character(index(first(S))-3600),
           '::',
           as.character(index(last(S))+3600),
           sep='')


# See if we have an open position at the end of the data, and if so close it out.
lastDT          = index(last(S))
closingPosition = getPosQty(Portfolio=strategyStr, Symbol=tickerSymbol, Date=lastDT)
if ( closingPosition > 0 ) {
  # We are still long, exit these positions
  cat(paste("We are still long ", closingPosition, " units, exit these positions", sep="") )
  exitPrice = as.numeric(Cl(last(S)))
  addTxn(Portfolio=strategyStr, Symbol=tickerSymbol, TxnDate=lastDT, TxnPrice=exitPrice, TxnQty = -closingPosition , TxnFees=0)

  # Now force this transaction into the orderbook.
  obook    = getOrderBook(portfolio=portfolioStr)
  obookXTS = obook[[portfolioStr]][[tickerSymbol]]
  r        = obookXTS[nrow(obookXTS),]
  lastS    = last(S)       # The last row ( last time ) of the market data
  index(r) = index(lastS)  # Force this new/last row to have the last time of the market data.

  # Fill out this last row accordingly. 
    r[,"Order.Qty"]        = -closingPosition
    r[,"Order.Price"]      = Cl(last(S))
    r[,"Order.Type"]       = "market"
  # r[,"Order.Side"]       = 
  # r[,"Order.Threshold"]  = 
    r[,"Order.Status"]     = "closed"
    r[,"Order.StatusTime"] = format(index(lastS), "%Y-%m-%d %H:%M:%S")
    r[,"Prefer"]           = "close"
  # r[,"Order.Set"]        = 
  # r[,"Txn.Fees"]         = 
    r[,"Rule"]             = "SimClose"
  # r[,"Time.In.Force"]    = 

  obookXTS=rbind(obookXTS, r)

  # Now update the orderbook in the strategy.
  # Taken from lines 432-434 in ruleOrderProc.R
  # now put the orders back in
  # assign order book back into place (do we need a non-exported "put" function?)
  obook[[portfolioStr]][[tickerSymbol]] = obookXTS
  assign(paste("order_book", portfolioStr, sep='.'), obook, envir=.strategy)

} else if ( closingPosition < 0 ) {
  # We are still short, exit these positions
  cat(paste("We are still short ", closingPosition, " units, exit these positions", sep="") )
  exitPrice = as.numeric(Cl(last(S)))
  addTxn(Portfolio=strategyStr, Symbol=tickerSymbol, TxnDate=lastDT, TxnPrice=exitPrice, TxnQty = -closingPosition , TxnFees=0)

  # Now force this transaction into the orderbook.
  obook    = getOrderBook(portfolio=portfolioStr)
  obookXTS = obook[[portfolioStr]][[tickerSymbol]]
  r        = obookXTS[nrow(obookXTS),]
  lastS    = last(S)       # The last row ( last time ) of the market data
  index(r) = index(lastS)  # Force this new/last row to have the last time of the market data.

  # Fill out this last row accordingly. 
    r[,"Order.Qty"]        = -closingPosition
    r[,"Order.Price"]      = Cl(last(S))
    r[,"Order.Type"]       = "market"
  # r[,"Order.Side"]       = 
  # r[,"Order.Threshold"]  = 
    r[,"Order.Status"]     = "closed"
    r[,"Order.StatusTime"] = format(index(lastS), "%Y-%m-%d %H:%M:%S")
    r[,"Prefer"]           = "close"
  # r[,"Order.Set"]        = 
  # r[,"Txn.Fees"]         = 
    r[,"Rule"]             = "SimClose"
  # r[,"Time.In.Force"]    = 

  obookXTS=rbind(obookXTS, r)

  # Now update the orderbook in the strategy.  (This is a hack!)
  # Taken from lines 432-434 in ruleOrderProc.R
  # now put the orders back in
  # assign order book back into place (do we need a non-exported "put" function?)
  obook[[portfolioStr]][[tickerSymbol]] = obookXTS
  assign(paste("order_book", portfolioStr, sep='.'), obook, envir=.strategy)

}


# Calculate P&L and resulting equity with blotter

dateRange=paste(
           as.character(index(first(S))-1),
           '::',
           as.character(index(last(S))+1),
           sep='')

updatePortf(strategyStr, Dates = dateRange)
updateAcct(strategyStr,  Dates = dateRange)
updateEndEq(strategyStr, Dates = dateRange)

#look at the order book
# ADD THE ORDERBOOK TO A NICE REPORT FOR STUDY.
obook=getOrderBook(portfolio=portfolioStr)

# RMarkdown fails when trying to print the xts object obook$S$S.  The reason is
# the order book xts object has unequal spacing in time, which causes something
# deep inside RMarkdown to barf.
#
# Solution: Convert it into a data.frame which avoids the special effort
# RMarkdown puts into formatting an xts object.  Plus is has the added benefit
# of printing the data.frame row.names which are the time indices in the xts
# object.

obookXTS = obook[[portfolioStr]][[tickerSymbol]]  # A more general way to access the order book.  You might have > 1 portfolio and > 1 security in the orderbook.
# obookXTS = obook[["SMA48W2Away"]][["S"]]        # alternative way to access order book xts object.
# obookXTS = obook[[1]][[1]]                      # alternative way to access order book xts object.

obookDF  = as.data.frame(obookXTS)

getTxns(Portfolio=strategyStr, Symbol=tickerSymbol)
transactions = getTxns(Portfolio=strategyStr, Symbol=tickerSymbol)

############################################################
# Create the chart enhancements.
############################################################

# > obookXTS
#                     Order.Qty Order.Price Order.Type     Order.Side Order.Threshold Order.Status Order.StatusTime      Prefer Order.Set Txn.Fees Rule               Time.In.Force
# 2014-06-24 08:00:00 "2"       "196.39"    "market"       "long"     NA              "closed"     "2014-06-24 08:30:00" "Open" NA        "0"      "EnterLong"        ""
# 2014-06-24 08:30:00 "-1"      "197.87"    "limit"        "long"     "1.5"           "replaced"   "2014-07-01 08:30:00" "Open" NA        "0"      "ExitLongPT"       ""
# 2014-06-24 10:00:00 "all"     "196.14"    "market"       "long"     NA              "closed"     "2014-06-24 10:30:00" "Open" NA        "0"      "ExitLongSMADown"  ""
#------------------------------------------------------------------------------------------
# 2014-06-27 13:00:00 "2"       "195.79"    "market"       "long"     NA              "closed"     "2014-06-30 06:30:00" "Open" NA        "0"      "EnterLong"        ""
# 2014-06-30 06:30:00 "-1"      "197.21"    "limit"        "long"     "1.5"           "closed"     "2014-07-01 08:30:00" "Open" NA        "0"      "ExitLongPT"       ""
# 2014-07-01 08:30:00 "all"     "196.21"    "stoptrailing" "long"     "-1"            "replaced"   "2014-07-01 09:30:00" "Open" NA        "0"      "TrailingExitLONG" ""
# 2014-07-01 09:30:00 "all"     "196.35"    "stoptrailing" "long"     "-1"            "replaced"   "2014-07-01 10:00:00" ""     NA        "0"      "TrailingExitLONG" ""
# 2014-07-01 10:00:00 "all"     "196.63"    "stoptrailing" "long"     "-1"            "replaced"   "2014-07-03 06:30:00" ""     NA        "0"      "TrailingExitLONG" ""
# 2014-07-03 06:30:00 "all"     "196.99"    "stoptrailing" "long"     "-1"            "replaced"   "2014-07-03 08:00:00" ""     NA        "0"      "TrailingExitLONG" ""
# 2014-07-03 08:00:00 "all"     "197.05"    "stoptrailing" "long"     "-1"            "replaced"   "2014-07-03 08:30:00" ""     NA        "0"      "TrailingExitLONG" ""
# 2014-07-03 08:30:00 "all"     "197.15"    "stoptrailing" "long"     "-1"            "replaced"   "2014-07-03 09:00:00" ""     NA        "0"      "TrailingExitLONG" ""
# 2014-07-03 09:00:00 "all"     "197.29"    "stoptrailing" "long"     "-1"            "closed"     "2014-07-07 08:30:00" ""     NA        "0"      "TrailingExitLONG" ""
#------------------------------------------------------------------------------------------
# 2014-07-22 07:30:00 "2"       "198.31"    "market"       "long"     NA              "closed"     "2014-07-22 08:00:00" "Open" NA        "0"      "EnterLong"        ""
# 2014-07-22 08:00:00 "-1"      "199.86"    "limit"        "long"     "1.5"           "replaced"   "2014-08-11 08:00:00" "Open" NA        "0"      "ExitLongPT"       ""
# 2014-07-25 06:30:00 "all"     "198.11"    "market"       "long"     NA              "closed"     "2014-07-25 07:00:00" "Open" NA        "0"      "ExitLongSMADown"  ""
#------------------------------------------------------------------------------------------
# 2014-08-08 11:00:00 "2"       "192.82"    "market"       "long"     NA              "closed"     "2014-08-08 11:30:00" "Open" NA        "0"      "EnterLong"        ""
# 2014-08-08 11:30:00 "-1"      "194.44"    "limit"        "long"     "1.5"           "closed"     "2014-08-11 08:00:00" "Open" NA        "0"      "ExitLongPT"       ""
# 2014-08-11 08:00:00 "all"     "193.44"    "stoptrailing" "long"     "-1"            "closed"     "2014-08-12 08:00:00" "Open" NA        "0"      "TrailingExitLONG" ""
#------------------------------------------------------------------------------------------
#
# > transactions
#                     Txn.Qty Txn.Price Txn.Fees Txn.Value Txn.Avg.Cost Net.Txn.Realized.PL
# 2014-05-16 05:30:00       0      0.00        0      0.00         0.00                0.00
#------------------------------------------------------------------------------------------
# 2014-06-24 08:30:00       2    196.37        0    392.74       196.37                0.00
# 2014-06-24 10:30:00      -2    195.81        0   -391.62       195.81               -1.12
#------------------------------------------------------------------------------------------
# 2014-06-30 06:30:00       2    195.71        0    391.42       195.71                0.00
# 2014-07-01 08:30:00      -1    197.21        0   -197.21       197.21                1.50
# 2014-07-07 08:30:00      -1    197.29        0   -197.29       197.29                1.58
#------------------------------------------------------------------------------------------
# 2014-07-22 08:00:00       2    198.36        0    396.72       198.36                0.00
# 2014-07-25 07:00:00      -2    198.13        0   -396.26       198.13               -0.46
#------------------------------------------------------------------------------------------
# 2014-08-08 11:30:00       2    192.94        0    385.88       192.94                0.00
# 2014-08-11 08:00:00      -1    194.44        0   -194.44       194.44                1.50
# 2014-08-12 08:00:00      -1    193.44        0   -193.44       193.44                0.50
#------------------------------------------------------------------------------------------


# Create the Entry Price Line.
ep = Cl(mktdata)
names(ep) = 'entryPrice'
ep[,1] = NA

# Create the Extreme (High or Low) Price Line
expl = Cl(mktdata)
names(expl) = 'extreemPrice'
expl[,1] = NA

# Create the Trailing Price Line
tp = Cl(mktdata)
names(tp) = 'trailingPrice'
tp[,1] = NA

longEntries = subset(obookXTS, Order.Qty=="2" & Order.Side=="long" & Order.Status=="closed")
shortEntries = subset(obookXTS, Order.Qty=="-2" & Order.Side=="short" & Order.Status=="closed")

# Construct vectors of the entry price (ep), the target price (expl) and the
# stop price (tp) for each trade.  The target price and the stop price for a
# trade may have two phases if the Profit Taker trade is executed. In this case
# the first phase conists of the "profit taking target" in the "expl", and the
# sma48 values in the "tp".  After the Profit Taker is executed the "expl" is
# populated with the "High Price Seen To Date" and the "tp" is ( expl -
# trailingStopThreshold )
for(i in 1:nrow(longEntries) ) {

  ## DEBUG i = 2
  enterDate = as.character(longEntries[i, "Order.StatusTime"])    # Strip PDT/PST & convert POSIXct to a string.
  whichTxn = transactions[enterDate, which.i=TRUE]
  exit1 = transactions[whichTxn+1,]

  if ( as.numeric(exit1[1, "Txn.Qty"]) == -1 ) {
    # exit 1 is the Profit Taker; thus there will also be a trailing stop exit
    # as well.  This branch creates both "phases" as described above.

    # This is the trailing stop exit.
    exit2     = transactions[whichTxn+2,]
    exit1Date = as.character(index(exit1))   # Strip PDT/PST & convert POSIXct to a string.
    exit2Date = as.character(index(exit2))   # Strip PDT/PST & convert POSIXct to a string.

    ssFullRange = paste(enterDate, "/", exit2Date, sep="")
    ss1stRange  = paste(enterDate, "/", exit1Date, sep="")
    ss2ndRange  = paste(exit1Date, "/", exit2Date, sep="")
    
    ep[ssFullRange,1]   = as.numeric( transactions[whichTxn, "Txn.Price"] )

    expl[ss1stRange, 1] = as.numeric( transactions[whichTxn, "Txn.Price"] ) + myTarget 
    tp[ss1stRange, 1]   = mktdata[ss1stRange, "SMA48"]


    bookSlice   = obookXTS[ss2ndRange,]
    # Guard against exit1 and exit2 happening at the same time (or in the same 30min bar really)
    if (nrow(bookSlice) > 0) {
      for( j in 1:nrow(bookSlice) ) {

        # Construct the subsets where new highs have been established and are in control.
        ## DEBUG j=1 
        startSS = as.character(index(bookSlice[j])) # Strip PDT/PST & convert POSIXct to a string.
        endSS   = as.vector(bookSlice[j,'Order.StatusTime'])
        allSS   = paste(startSS, "/", endSS, sep="")
      
        hiSoFar        = as.numeric(Hi(mktdata[startSS])) 
        expl[allSS, 1] = hiSoFar
        tp[allSS, 1]   = hiSoFar - trailingStopThreshold

      }
    }

  } else if ( as.numeric(exit1[1, "Txn.Qty"]) == -2 ){
    # No Profit Taker.  There is only 1 exit for this trade, and it will be
    # SMA48 driving the exit of both units.  exit1 is the SMA48XDown for both
    # units.  This branch will create the single "phase" of ep, expl, and tp

    exit1Date            = as.character(index(exit1))   # Strip PDT/PST & convert POSIXct to a string.
    ssFullRange          = paste(enterDate, "/", exit1Date, sep="")
    ep[ssFullRange,1]    = as.numeric( transactions[whichTxn, "Txn.Price"] )
    expl[ssFullRange, 1] = as.numeric( transactions[whichTxn, "Txn.Price"] ) + myTarget 
    tp[ssFullRange, 1]   = mktdata[ssFullRange, "SMA48"]

  } else {
    stop("Error in parsing transactions... 'There are no other viable options'.")
  }


}

# OLD    for(i in seq(from=2, to=nrow(transactions), by=2)  ) {
# OLD    
# OLD      ## DEBUG i = 2
# OLD      enterDate   = as.character(index(transactions[i]))   # Strip PDT/PST & convert POSIXct to a string.
# OLD      exitDate    = as.character(index(transactions[i+1]))
# OLD      ssTxn       = paste(enterDate, "/", exitDate, sep="")
# OLD      bookSlice   = obookXTS[ssTxn,]
# OLD      ep[ssTxn,1] = as.numeric(transactions[i, "Txn.Price"])
# OLD      long        = as.numeric(transactions[i, "Txn.Qty"]) > 0
# OLD    
# OLD      for( j in 1:nrow(bookSlice) ) {
# OLD    
# OLD        # Construct the subsets where new highs have been established and are in control.
# OLD        ## DEBUG j=1 
# OLD        startSS = as.character(index(bookSlice[j])) # Strip PDT/PST & convert POSIXct to a string.
# OLD        endSS   = as.vector(bookSlice[j,'Order.StatusTime'])
# OLD        allSS   = paste(startSS, "/", endSS, sep="")
# OLD    
# OLD        if ( long ) {
# OLD          hiSoFar = as.numeric(Hi(mktdata[startSS])) 
# OLD          expl[allSS, 1] = hiSoFar
# OLD          tp[allSS, 1] = hiSoFar - 1
# OLD        } else {
# OLD          lowSoFar = as.numeric(Lo(mktdata[startSS])) 
# OLD          expl[allSS, 1] = lowSoFar
# OLD          tp[allSS, 1] = lowSoFar + 1
# OLD        }
# OLD      }
# OLD    }
############################################################

## Performance Plot
chart.Posn(strategyStr, Symbol=tickerSymbol)
#
## Plot the Max Favorable Excursion (MFE) (i.e. if you exited the position at
## peak profit you would realize MFE)
chart.ME(strategyStr, Symbol=tickerSymbol, type="MFE")
#
## Plot the Max Adverse Excursion (MAE) (i.e. if you exited the position at peak
## loss you would realize MAE)
chart.ME(strategyStr, Symbol=tickerSymbol, type="MAE")


############################################################
# Gross Trade Statistics
# tradeStats() calculates statistics commonly seen in strategy reports from
# books and execution platforms.  'trades' are calculated flat to flat for this
# report, representing a full round trip
(tstats = tradeStats(Portfolio=strategyStr, Symbol=tickerSymbol))

## TODO ADD TO REPORT
PerformanceAnalytics:::textplot(t(tstats))

############################################################
# Per Trade Statistics

# The table returned from perTradeStats() is very wide.  Let's define 3
# different groups of columns, and then we will print the three groups in the
# RMarkdown report.
perTradeStats1 = c("Start", "End", "Init.Pos", "Max.Pos", "Num.Txns", "Max.Notional.Cost", "Net.Trading.PL" )
perTradeStats2 = c("Start", "End", "Init.Pos", "MAE", "MFE", "Pct.Net.Trading.PL", "Pct.MAE", "Pct.MFE" )
perTradeStats3 = c("Start", "End", "Init.Pos", "tick.Net.Trading.PL", "tick.MAE", "tick.MFE" )

(ptstats = perTradeStats(Portfolio=strategyStr, Symbol=tickerSymbol))

# Convert the POSIXct columns to strings so xtable can print them out properly.
ptstatsStr = ptstats
ptstatsStr[,"Start"] = as.character(ptstats[,c("Start")])
ptstatsStr[,"End"]   = as.character(ptstats[,c("End")])

# plot(ptstats$Start, cumsum(ptstats$Net.Trading.PL), type='b', xlab='TakeProfit', ylab='Net.Trading.PL', main='SMA48w2Away')

# Note: chart.Histogram has a bad habit of failing if there is only one trade
# in ptstats.
if(nrow(ptstats)>1) {
  chart.Histogram(ptstats$Pct.Net.Trading.PL, methods=c('add.normal'), main='SMA48w2Away returns')
}

tradeQuantiles(Portfolio=strategyStr, Symbol=tickerSymbol, scale='percent', probs=c(.5,.9,.99))

############################################################
# Compute trade statistics

# Trade related
tableTrades = cbind(
  c("Trades","Win Percent","Loss Percent","W/L Ratio"),
  c(tstats[,"Num.Trades"],tstats[,c("Percent.Positive","Percent.Negative")],
  tstats[,"Percent.Positive"]/tstats[,"Percent.Negative"]))

# Profit related
tableProfit = cbind(
  c("Net Profit","Gross Profits","Gross Losses","Profit Factor"),
  c(tstats[,c("Net.Trading.PL","Gross.Profits","Gross.Losses",
    "Profit.Factor")]))

# Averages
tableWins = cbind(
  c("Avg Trade","Avg Win","Avg Loss","Avg W/L Ratio"),
  c(tstats[,c("Avg.Trade.PL","Avg.Win.Trade","Avg.Losing.Trade",
    "Avg.WinLoss.Ratio")]))

tableTradeStats = data.frame(tableTrades,tableProfit,tableWins)
print(tableTradeStats)

############################################################
# Compute advanced performance statistics

# Plot cumulative return and drawdown

# The portfolio analysis does not work for just one day.  If you try anyway
# you get the following error message:
#
#   Error in periodicity(y) : can not calculate periodicity of 1 observation
#
# If you have just one day, and you want to see the portfolio analysis on a
# minute by minute basis, which might be a good idea for debugging, then try
# this code: 
#
# rets <- PortfReturns(Account=strategyStr, period="none")

rets <- PortfReturns(Account=strategyStr)

# Clear rownames to avoid timeBased/xtsible error when using certain functions
# with table.Arbitrary (e.g. Return.annualized)
rownames(rets) <- NULL

charts.PerformanceSummary(rets)

# Compute performance statistics
tablePerf <- table.Arbitrary(rets,
  metrics=c(
    "Return.cumulative",
    "Return.annualized",
    "SharpeRatio.annualized",
    "CalmarRatio"),
  metricsNames=c(
    "Cumulative Return",
    "Annualized Return",
    "Annualized Sharpe Ratio",
    "Calmar Ratio")
) 


# Compute Risk Statistics
tableRisk <- table.Arbitrary(rets,
  metrics=c(
    "StdDev.annualized",
    "maxDrawdown",
    "VaR",
    "ES"),
  metricsNames=c(
    "Annualized StdDev",
    "Max DrawDown",
    "Value-at-Risk",
    "Conditional VaR")
)


# Performance and Risk Statistics
tablePerformanceStats <- data.frame(
  rownames(tablePerf),tablePerf[,1],
  rownames(tableRisk),tableRisk[,1]
)
print(tablePerformanceStats)

############################################################
# Construct the TA argument to chartSeries in pieces and assemble at the end.

SOD <- index(S["T06:30/T06:31",])  # Start Of Days...

# Useful references for R graphic plotting
#
#    color reference:  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#    line type reference:  http://students.washington.edu/mclarkso/documents/line%20styles%20Ver2.pdf
#    point type reference: http://rgraphics.limnology.wisc.edu/pch.php
#
#    EXAMPLES: 
#
#    lty=0 : "blank"       lty=4 : "dotdash"
#    lty=1 : "solid"       lty=5 : "longdash"
#    lty=2 : "dashed"      lty=6 : "twodash"
#    lty=3 : "dotted"
#      
#    The "pch" characters are actual font symbols (i.e. "Plot CHaracter), so you
#    are limited to what is available in your fonts.
#
#    pch=19 : solid circle               pch=23 : diamond
#    pch=20 : bullet (small circle)      pch=24 : triangle point up
#    pch=21 : circle                     pch=25 : triangle point down
#    pch=22 : square
#

## DEBUG    names(S) = c( "Open", "High", "Low", "Close", "Volume", "SMA48", "VWAP48", "SMA48XUp", "SMA48XDown", "goLong", "goShort")

ta01 = ""

longExits = s5 = as.character(subset(obookXTS, grepl("ExitLong", Rule) & grepl("closed", Order.Status) )[, "Order.StatusTime"])

if ( any(S$goLong==1) ) {
  ## DDDD    s1=index(na.omit(S$SMA48XUp))
  ## DDDD    s2=index(na.omit(S$goLong))
  s1=findIndexValues(S, "SMA48XUp", 1)
  s2=findIndexValues(S, "goLong", 1)
  ta01 <- paste ( ta01, 
                  "addTA(Cl(S[s1]), type='p', col='black', bg='darkgreen',  pch=24, cex=0.75, on=1 );",
                  "addTA(Cl(S[s2]), type='p', col='black', bg='lightgreen', pch=23, cex=1.25, on=1 );",
                  "addTA(Cl(S[s5]), type='p', col='black', bg='orange',     pch=23, cex=1.25, on=1 );",
                  sep="  "
                )
} 
if ( any(S$goShort==-1) ) {
  ## DDDD    s3=index(na.omit(S$SMA48XDown))
  ## DDDD    s4=index(na.omit(S$goShort))
  s3=findIndexValues(S, "SMA48XDown", 1)
  s4=findIndexValues(S, "goShort", -1)
  ta01 <- paste ( ta01, 
                  "addTA(Cl(S[s3]), type='p', col='black', bg='red',       pch=25, cex=0.75, on=1 );",
                  "addTA(Cl(S[s4]), type='p', col='black', bg='violetred', pch=23, cex=1.25, on=1 );",
                  sep="  "
                )
} 

# Combine the partial TA args into one final TA arg

ta = paste(
         ta01,
#        "addTA(S$VWAP48, on=1, col='darkgreen',  lwd=3);",
#        "addTA(CUD(cl=Cl(S), n=7), col='red',  lwd=1);",           # this will be on panel #2
#        "addTA(CUD(cl=Cl(S), n=15), col='black',  lwd=1, on=2);",  # overlay the other CUD
         "addTA(S$SMA48,  on=1, col='steelblue3', lwd=0.5, lty=1);",
         "addTA(Op(S[SOD,]), type='p', col='deepskyblue', pch=22, bg='slateblue3', cex=1.0, on=1 );",
        #"addVo();" ,
         sep="  "
)

############################################################
# Finished constructing the TA argument to chartSeries()


# Chart the data one week at a time.  All weeks will be in one PDF file.
# In order to do so, I must find the list of weeks found in the data,
# and then create a list of firstDay/lastDay subsets for each of the weeks found.

theDays    = unique(as.Date(index(S))) # "2014-05-12" "2014-05-13" "2014-05-14" "2014-05-15" ... 
dow        = wday(theDays)  # DayOfWeek # 2 3 4 5 6 2 3 4 5 6 3 4 5 6 2 3 4 5 6 2 3 4 5 6
numDays    = length(theDays)
allWeeksSS = c()
oneWeek    = c()

if ( regularTradingHoursOnly == TRUE ) {
  endTimeTS = " 13:00:00"
} else {
  endTimeTS = " 23:59:50"
}

for (i in 1:numDays) {
  # Something tells me that I should be able to do all this in just one branch,
  # but it works for now...
  oneWeek[length(oneWeek)+1] = i
  if ( dow[i] == 6 || i == numDays ) {
    # We are at a Friday, or at the data's last day
    firstDay = oneWeek[1]
    lastDay  = oneWeek[length(oneWeek)]
    theWeek  = paste(theDays[firstDay], "/", theDays[lastDay], endTimeTS, sep="")
    allWeeksSS[length(allWeeksSS)+1] = theWeek
    oneWeek    = c()
  } else if (dow[i+1] < dow[i]) {
    # Peek ahead into next week
    # There was no Friday, and the next day is in the next week.
    firstDay = oneWeek[1]
    lastDay  = oneWeek[length(oneWeek)]
    theWeek  = paste(theDays[firstDay], "/", theDays[lastDay], endTimeTS, sep="")
    allWeeksSS[length(allWeeksSS)+1] = theWeek
    oneWeek    = c()
  }
}

# Loop over all weekly subsets and plot each week

taEnhanced = paste(
         ta,
         "addTA(epSS,   on=1, col='blue',  lwd=1);",
         "addTA(explSS, on=1, col='green', lwd=1);",
         "addTA(tpSS,   on=1, col='red',   lwd=1);",
         sep="  "
)
for(i in 1:length(allWeeksSS) ) {
  messageStr = paste( ticker, "-", barSizeInMins, "min, ", i, "/", length(allWeeksSS),  sep="")

  ## DEBUG i = 2
  epSS   = ep[allWeeksSS[i]]
  explSS = expl[allWeeksSS[i]]
  tpSS   = tp[allWeeksSS[i]]

  chartSeries(OHLCV(S), 
              subset=allWeeksSS[i],
              name=messageStr,
              type="candlesticks",
              theme=chartTheme(theme='white', up.col='green', dn.col='red'),
              TA=taEnhanced
  )
}

############################################################
# Close the pdf file
############################################################
dev.off()
system(paste("open ", plotFileName, sep="") )

## KEEP ???? PerformanceAnalytics:::textplot(obookDF, show.rownames=TRUE)

## EXPERIMENTAL    ############################################################
## EXPERIMENTAL    # Create a Baseline Buy & Hold Account
## EXPERIMENTAL    ############################################################
## EXPERIMENTAL    
## EXPERIMENTAL    # initialize portfolio and account
## EXPERIMENTAL    rm.strat("buyHold")
## EXPERIMENTAL    initPortf(name="buyHold", symbols=tickerSymbol, initDate=startDateStr, currency="USD")
## EXPERIMENTAL    initAcct(name="buyHold", portfolios="buyHold", initDate=startDateStr, initEq=1e4, currency="USD")
## EXPERIMENTAL    
## EXPERIMENTAL    # place a single transaction
## EXPERIMENTAL    txns        = getTxns(Portfolio=strategyStr, Symbol = tickerSymbol)
## EXPERIMENTAL    bhStartDate = time(txns)[2]
## EXPERIMENTAL    bhBuyPrice  = as.numeric(txns[bhStartDate, "Txn.Price"])
## EXPERIMENTAL    bhEndDate   = time(txns)[nrow(txns)]
## EXPERIMENTAL    bhSellPrice = as.numeric(txns[bhEndDate, "Txn.Price"])
## EXPERIMENTAL    addTxn("buyHold", Symbol=tickerSymbol, TxnDate=bhStartDate, TxnPrice=bhBuyPrice, TxnQty=1 , TxnFees=0)
## EXPERIMENTAL    # update portfolio and account
## EXPERIMENTAL    Dates <- paste(bhStartDate,bhEndDate,sep="::")
## EXPERIMENTAL    updatePortf(Portfolio="buyHold",Dates=Dates)
## EXPERIMENTAL    updateAcct(name="buyHold",Dates=Dates)
## EXPERIMENTAL    updateEndEq(Account="buyHold",Dates=Dates)
## EXPERIMENTAL    
## EXPERIMENTAL    rets.bh = Return.calculate(getAccount("buyHold")$summary$End.Eq)


############################################################
# Practice with Report Generation.
############################################################

obookCSVFileName  = paste(outputDirName, "/", "orderBook.csv", sep="")
write.zoo( obookXTS, file = obookCSVFileName, sep="," )

transactionsCSVFileName  = paste(outputDirName, "/", "transactions.csv", sep="")
write.zoo( transactions, file = transactionsCSVFileName, sep="," )


reportInFileName  = paste(reportInputDir, "/", "reportNative.Rmd", sep="")
## DEBUG reportInFileName  = paste(reportInputDir, "/", "reportTest.Rmd", sep="")
reportOutFileName = paste(outputDirName, "/", "reportNative-", ticker, ".pdf", sep="")

## Using RMarkdown, latex, pandoc to generate a PDF report.
## For debugging report generation use "clean=FALSE" which will not remove
## intermediate files created during rendering.  See "reportNative.knit.md" in
## the current directory and the many files in
## "outpurDirName/reportNative-SPY_files"

# knitr::opts_chunk$set(cache=TRUE)

# TODO - Add the high lines and trailing price lines to the charts for the trailingstop orders.
# Get the row numbers of the transaction events to prepare for enhanced trailingstop graphics.
# FIX the closing rowing number by decrementing the even entries by 1.  Ugh.
# obookXTS[obookXTS$Order.Status=="closed"|obXTS$Order.Status=="open", which.i=TRUE]


# Original column names in the order book.
colNames = c("Order.Qty", "Order.Price", "Order.Type", "Order.Side",
      "Order.Threshold", "Order.Status", "Order.StatusTime", "Prefer", "Order.Set",
      "Txn.Fees", "Rule", "Time.In.Force")

# Shortend column names in the order book.
colNames = c("Qty", "Price", "Type", "Side",
      "Thr", "Status", "StatusTime", "Prefer", "OSet",
      "Fees", "Rule", "TIF")
names(obookDF)   = colNames   # Make columns names shorter for better printing in the pdf report

# Remove a few (empty for now) columns in the order books.
obookDF[,"Fees"] = NULL       # Drop the transaction fees column
obookDF[,"TIF"]  = NULL       # Drop the time-in-force column

rmarkdown::render(input=reportInFileName, output_file=reportOutFileName, quiet=FALSE, clean=TRUE)
system(paste("open ", reportOutFileName, sep="") )
