#vim:ts=2:sts=2:sw=2:ht=2:expandtab:number:ruler:showmode:smartindent:ff=unix:foldmethod=marker 
############################################################

# A program that runs the vwap(allDay) vs BW05 crossover system.  It dumps the
# data to a csv file, and plots the charts with trade signals to a pdf file.
#
# Usage: Modify variables "ticker", "startDate", and "endDate"
# Or run this script from the unix command line:
#   R CMD BATCH --no-save --no-restore '--args ticker="ES-20110916" startDate="2012-06-10" endDate="2012-09-14" ' convert5Sec2Mins.R output1.txt
#
#   R CMD BATCH --no-save --no-restore '--args ticker="AAPL" startDate="20140501" endDate="20140531" ' ~/securities/src/main/R/convert5Sec2Mins.R AAPL.log

# Clear the work space.
rm(list=ls(all=TRUE))

library(RSQLite)
library(quantmod)
library(quantstrat)
library(lubridate)

############################################################
# The set of global variables that must be initialized:
#
#    ticker     <- "ES-20120921"
#    startDate  <- as.Date("2012-06-10")
#    endDate    <- as.Date("2012-09-14")
#
############################################################

############################################################
# Read the arguments listed on the command line & Initialize global variables here
############################################################

args <- commandArgs(TRUE)

if ( length(args)==0 ) {
  # No command line arguments supplied.  Initialize to defaults here.

  ##    fiveSecond-ES-20120921.sqlite
  ##    1340991200  20120629 10:33:20  1350.25     1350.5      1350.25     1350.25     8           1350.38     6  
  ##    1347914450  20120917 13:40:50  1460.5      1460.5      1460.5      1460.5      0           1460.5      0   
  ##    From: 2012-06-10 15:00:00 (Sunday)
  ##    To:   2012-09-14 13:15:00 (Friday)
  ##    
  ##    fiveSecond-ES-20121221.sqlite
  ##    1347863390  20120916 23:29:50  1455.0      1455.0      1455.0      1455.0      0           1455.0      0 
  ##    1356124495  20121221 13:14:55  1424.75     1424.75     1424.75     1424.75     0           1424.75     0 
  ##    From: 2012-09-16 15:00:00 (Sunday)
  ##    To:   2012-12-14 13:15:00 (Friday)
  ##
  ##    fiveSecond-ES-20130315.sqlite
  ##    1355463700  20121213 21:41:40  1415.0      1415.0      1415.0      1415.0      0           1415.0      0 
  ##    1363382095  20130315 14:14:55  1561.25     1561.25     1561.25     1561.25     0           1561.25     0  
  ##    From: 2012-12-16 15:00:00  (Sunday)
  ##    To:   2013-03-08 13:15:00  (Friday)
  
  ticker           = "AAPL"
  startDate        = as.Date("2014-05-01")
  endDate          = as.Date("2014-05-02")

  ## Other Examlpes:

  ##    ticker     = "ES-20110916" 
  ##    startDate  = as.Date("2011-06-13")
  ##    endDate    = as.Date("2011-09-09")
  
  ##    # fiveSecond-ES-20120921.sqlite
  ##    ticker     <- "ES-20120921"
  ##    startDate  <- as.Date("2012-06-11")
  ##    endDate    <- as.Date("2012-09-14")
  
  ##    # fiveSecond-YM-20120921.sqlite
  ##    ticker     <- "YM-20120921"
  ##    startDate  <- as.Date("2012-06-11")
  ##    # endDate    <- as.Date("2012-09-14")
  ##    endDate    <- as.Date("2012-06-15")
  
  ##    # fiveSecond-ES-20121221.sqlite
  ##    ticker     <- "ES-20121221"
  ##    startDate  <- as.Date("2012-09-17")
  ##    endDate    <- as.Date("2012-12-14")
  
  ##    # fiveSecond-YM-20121221.sqlite
  ##    ticker     <- "YM-20121221"
  ##    startDate  <- as.Date("2012-09-17")
  ##    endDate    <- as.Date("2012-12-14")
  
  
} else {

  # Command line arguments supplied.  Set defaults based on command line args here.

  for(i in 1:length(args)){
    # Cycle through each element of the list and evaluate the expressions.
    ## DEBUG    cat("arg ", i, text=args[[i]])
    eval(parse(text=args[[i]]))
  }

  startDate  <- as.Date(startDate)
  endDate    <- as.Date(endDate)

}



# ss

# strategy <- "SMA48"
  strategy <- "BW05 x VWAP"
# strategy <- "BW05 x KFChannel"
# strategy <- "EMA05 x KFChannel"
# strategy <- "EMA05 x BW05"

# Prevent volume from being plotted on the following tickers:
plotNoVolume <- c("EURUSD", "EUR-20130318", "EUR-20130617") 


## MANUALLY    extraHour <- 0  # For PST - Pacific Standard Time

## DEBUG    dbPath <- "~/NetBeansProjects/TWS/DataLogger"
dbPath <- "~/runTime"

############################################################
#
getMinData <- function(ticker="AAPL", startDate="2014-01-01", endDate="2014-01-01", minsInBar=1) 
{

  # Recall that the 5-second data stored in our sqlite db files is in Time Zone
  # "America/Los_Angeles".  Nominally you can think of this as PST/PDT, but we
  # have to account for this when computing the unix epoch.

  # Create a time stamp version string of start and stop dates
  # TODO: Check if already in TS format for future flexibility.
  startDateTS = paste(startDate, " 00:00:00", sep="")
  endDateTS   = paste(endDate, " 13:15:00", sep="")

  # Convert the time stamp versions into epoch seconds
  startSecs   = as.numeric( as.POSIXct(startDateTS, tz = "America/Los_Angeles") ) 
  endSecs     = as.numeric( as.POSIXct(endDateTS, tz = "America/Los_Angeles") ) 

  ## DEBUG    Run the following lines to build sql statements that will help you debug the date to seconds conversion.
  ## DEBUG    sqlTestStart1 = sprintf("select 'startDate', start, tsStart, %d, '%s' from Bars5Seconds where start>=%d order by start limit 10;", startSecs, startDateTS, startSecs)
  ## DEBUG    sqlTestStart2 = sprintf("select 'endDate',   start, tsStart, %d, '%s' from Bars5Seconds where start<=%d order by start DESC limit 10;", endSecs,   endDateTS,   endSecs)

  ## MANUALLY    # The time in seconds at 00:00:00 (HH:MM:SS) on startDate
  ## MANUALLY    startSecs   <- as.numeric(as.POSIXct(startDate) + (7+extraHour) * 3600)
  ## MANUALLY    # The time in seconds at 13:15:00 (HH:MM:SS) on startDate
  ## MANUALLY    endSecs     <- as.numeric(as.POSIXct(endDate)   + (7+extraHour) * 3600) + 13.25 * 3600

  ## DEBUG   day    <- Sys.Date()
  ## DEBUG   ticker <- "YM"

  ## DEBUG    cat("startSecs: ", format(as.POSIXct(startSecs, origin="1970-01-01"), "%Y%m%d %H:%M:%S"), "\n" )
  ## DEBUG    cat("endSecs  : ", format(as.POSIXct(endSecs, origin="1970-01-01"), "%Y%m%d %H:%M:%S"), "\n" )
      
  ## DEBUG    browser()

  fn <- paste(dbPath, "/fiveSecond-", ticker, ".sqlite", sep="")
  ## DEBUG cat("Using DB FILE: ", fn, "\n")
  db <- dbConnect( dbDriver("SQLite") , dbname=fn, flags=SQLITE_RO, cache_size=64000, synchronous=0 )

  ## sql <- sprintf("SELECT start, tsStart, open, high, low, close, volume FROM Bars5Seconds WHERE start >= %d  AND start < %d and Volume > 0 ORDER BY start", startSecs, endSecs )
  sql <- sprintf("SELECT start, tsStart, open, high, low, close, volume FROM Bars5Seconds WHERE start BETWEEN %d AND %d ORDER BY start", startSecs, endSecs )
  ## DEBUG print(sql)


  ## Wrap the call to dbGetQuery() with error handling code such that if it is
  ## simply a BUSY error, we will wait 1/2 second and try again.  If it is
  ## another error, well, that does not really matter at this point since the
  ## program will fail in an yet unknown way...  Below is the error message we
  ## want to avoid: 
  ##     Error in sqliteFetch(rs, n = -1, ...) : 
  ##       RSQLite driver: (RS_SQLite_fetch: failed first step: database is locked)
  ##

  attempt <- 0
  gotResultsSet <- FALSE
  while(!gotResultsSet) {
    attempt <- attempt + 1
    sleepLength <- attempt * 0.1  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
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

      Sys.sleep(sleepLength)  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
      next
    } 
    gotResultsSet <- TRUE 
  }

  attempt <- 0
  gotData <- FALSE
  skipDbFile <<- FALSE
  while(!gotData) {
    ## DEBUG cat("attempt: ", attempt, "\n") 
    attempt <- attempt + 1
    sleepLength <- attempt * 0.1  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
    trades <- tryCatch(fetch(resultSetObj, n=-1), 
                         error=function(e)
                         {
                           print("Caught RSQLite Error: fetch() Failed")
                           print(e)
                           ## <simpleError in sqliteFetch(res, n = n, ...): RSQLite driver: (RS_SQLite_fetch: failed: database disk image is malformed)>
                           e<-"<simpleError in sqliteFetch(res, n = n, ...): RSQLite driver: (RS_SQLite_fetch: failed: database disk image is malformed)>"
                           badDbFile <- grep("database disk image is malformed", e, fixed=TRUE)
                           if ( length(badDbFile) > 0 ) 
                             skipDbFile <<- TRUE
                           return(e)
                         }
                      )
    error  <- dbGetException(db) 

    if ( skipDbFile == TRUE ) {
      return(FALSE) 
    }

    ##  DEBUG    print(class(trades))
    ##  OBSERVED    [1] "Caught RSQLite Error: fetch() Failed"
    ##  OBSERVED    Error in trades[, -c(1, 2)] : incorrect number of dimensions
    
    if (inherits(trades, "error") ) {
      cat("SQLite returned an error on fetch().  Using inherits().... Wait 0.1 seconds and try again\n") 
      print(error)
      Sys.sleep(sleepLength)  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
      next 
    }

    if (error$errorMsg != "OK" ) {
      cat("SQLite returned an error on fetch().  Using error$errorMsg.... Wait 0.1 seconds and try again\n") 
      print(error)
      Sys.sleep(sleepLength)  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
      dbClearResult(resultSetObj) # we probably need to call this here.  Untested so far.
      next 
    }

    gotData <- TRUE

    dbClearResult(resultSetObj)
  }

  dbDisconnect(db)

  # FX will return "-1" or negative volumes.  So just set it to +1
  trades$volume <- ifelse(trades$volume == -1, 1, trades$volume)
  # Filter out "0" volume trades.  Why? This is the point where rows go missing. I might want to revisit this decision.
  trades <- trades[trades$volume > 0, ]

  # Detect the format of the DATETIME object in the DB, and adjust the format string accordingly.
  formatStr <- "%Y%m%d %H:%M:%S"
  if ( grepl("\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d", trades[1, "tsStart"], perl=TRUE) ) 
  {
    formatStr <- "%Y-%m-%d %H:%M:%S"
  }


  trades.xts <- as.xts(trades[, -c(1,2)], order.by=as.POSIXct(trades[, "tsStart"], format=formatStr ) )
  trades.min <- to.period(trades.xts, period='minutes', k=minsInBar, indexAt="startof")
  colnames(trades.min) <- c("Open", "High", "Low", "Close", "Volume")

  # Make sure all seconds are set to "00"
  minIndex           <- index(trades.min)
  second(minIndex)   <- 0
  .index(trades.min) <- minIndex

  return(trades.min)
}

############################################################
# VWAP as used by ZeroHedge
#
# d2 <- VWAP.sm(oneMinData)
#
VWAP.sm <- function(x)
{
  # TODO: There is still a bug in this code when it is called for a stock for many days.

  ## DEBUG x        <- oneMinData 
  ## BUGGY  dataDays.old  <- unique(as.Date(index(x)))  # converts time like '2013-01-18 16:02:25" into 2013-01-19!
  dataDays      <- unique(as.Date(strptime(index(x), "%Y-%m-%d")))
  vwap          <- Cl(x)  # initialize storage & rename the column
  names(vwap)   <- c('VWAP')
  stdev         <- Cl(x)  # initialize storage & rename the column
  names(stdev)  <- c('STDEV')
  vwp1sd        <- Cl(x)  # initialize storage & rename the column
  names(vwp1sd) <- c('VWP1STD')
  vwp2sd        <- Cl(x)  # initialize storage & rename the column
  names(vwp2sd) <- c('VWP2STD')
  vwm1sd        <- Cl(x)  # initialize storage & rename the column
  names(vwm1sd) <- c('VWM1STD')
  vwm2sd        <- Cl(x)  # initialize storage & rename the column
  names(vwm2sd) <- c('VWM2STD')

  ## DEBUG    browser()
  for (day in dataDays) 
  {
    if ( is.na(day) ) next ;
    ## DEBUG    cat("in VWAP.sm: day Before as.Date=()", day, "\n")

    day      <- as.Date(day) 
    ## DEBUG    cat("in VWAP.sm: day After as.Date()=", day, "\n")
    ## DEBUG day <- as.Date( dataDays[3] )

    sss      <- paste(day, ' 00:00:00', '/', day, ' 23:59:59', sep='')
    ## DEBUG    cat("in VWAP.sm: sss=", sss, "\n")
    dayData  <- x[sss, ]

    ## DEBUG    
    ## DEBUG    cat("in VWAP.sm: nrows for day=", nrow(dayData), "\n" )

    pv       <- Cl(dayData) * Vo(dayData)
    sumVol   <- cumsum(Vo(dayData))
    sumVol[sumVol$Volume==0, 'Volume'] <- 1  ## set any (early) 0 entries to 1 to avoid NaN's
    sumPV    <- cumsum(pv)
    day.vwap <- sumPV / sumVol 
    names(day.vwap) <- c('VWAP')

    vwap[index(day.vwap), 'VWAP'] <- day.vwap[, 'VWAP'] # accumulate in local storage

    # Compute the StdDev, and +- 1 and 2 StdDev Columns
    VWAP.day.diff <- as.numeric( Cl(dayData) - day.vwap[, 'VWAP'] )

    day.sd        <- Cl(dayData)  # initialize
    names(day.sd) <- c("sd") 
    sd.tmp        <- rollapply(data=VWAP.day.diff, width=1:length(VWAP.day.diff), FUN=sd, partial=TRUE, align="right")
    day.sd$sd     <- sd.tmp
    #     rollapply manually
    #     for (i in 1:length(VWAP.day.diff) )
    #     {
    #       day.sd[i, "sd"] <- sd(VWAP.day.diff[1:i])      
    #     }

    day.p1sd <- vwap +     day.sd
    day.p2sd <- vwap + 2 * day.sd
    day.m1sd <- vwap -     day.sd
    day.m2sd <- vwap - 2 * day.sd

    stdev[index(day.sd), 'STDEV']      <- day.sd[,   1] # accumulate in local storage
    vwp1sd[index(day.p1sd), 'VWP1STD'] <- day.p1sd[, 1] # accumulate in local storage
    vwp2sd[index(day.p2sd), 'VWP2STD'] <- day.p2sd[, 1] # accumulate in local storage
    vwm1sd[index(day.m1sd), 'VWM1STD'] <- day.m1sd[, 1] # accumulate in local storage
    vwm2sd[index(day.m2sd), 'VWM2STD'] <- day.m2sd[, 1] # accumulate in local storage

  }

  # add the vwap and other computed columns
  x <- cbind(x, VWAP=vwap,
                STDEV=stdev,
                VWP1STD=vwp1sd,
                VWP2STD=vwp2sd,
                VWM1STD=vwm1sd,
                VWM2STD=vwm2sd
             )  

  # return the original data plus all computed data.
  return(x)
}

############################################################
# butterworth.R - 2 Polo Butterworth Filter
#    The Butterworth filter is a type of signal processing filter designed to
#    have as flat a frequency response as possible in the passband. It is also
#    referred to as a maximally flat magnitude filter.
#
#    REFERENCES:
#    2 Pole Butterworth Filter - //// From 'Cybernetic Analysis for Stocks and Futures' by John Ehlers
#    from https://sites.google.com/site/prospectus/welcome/released-thinkscript-studies
#    Original code from Dave Newberg at
#    http://www.davenewberg.com/Trading/EhlersCodes.html
#    Ported to Thinkscript by Prospectus at
#    http://readtheprospectus.wordpress.com
#    http://www.davenewberg.com/Trading/TS_Code/Ehlers_Indicators/2_pole_Butterworth.html
############################################################
# price is an xts object.
# period is an integer.
# returns a 1 column xts object with column name of "BW"
# bw05.stm  <- butterworth(price=Cl(S), period=10)
# 
butterworth <- function(price, period)
{

  # DEBUG
  # DEBUG    library(quantmod)
  # DEBUG    startDate <- as.Date("2011-01-01")
  # DEBUG    endDate   <- as.Date("2012-10-02")
  # DEBUG    ticker <- "SPY"
  # DEBUG    S <- getSymbols(ticker, from=startDate, to=endDate, auto.assign=FALSE)
  # DEBUG    S <- adjustOHLC(S, use.Adjusted=TRUE)
  # DEBUG    period <- 10
  # DEBUG    price <- Cl(S) 

  a1    <- exp(-1.414*pi/period)
  # b1  <- 2*a1*cos(1.414*180/period)   # When cosine function is in degrees
  b1    <- 2*a1*cos(1.414*pi/period)    # When cosine function is in radians
  coef2 <- b1
  coef3 <- -a1*a1
  coef1 <- (1-b1+a1*a1)/4

  # Butter = coef1*(price+2*price[1]+price[2])+ coef2*Butter[1] + coef3*Butter[2];
  # If CurrentBar<3 then Butter=price;

  b.xts        <- price
  colName      <- paste("BW", period, sep="")
  names(b.xts) <- c(colName)
  n            <- nrow(price)
  p <- as.numeric(price)
  for (i in 3:n) 
  {
    # DEBUG i <- 3
    b.xts[i,1] <- coef1 * ( p[i] + 2*p[i-1] + p[i-2] ) +
      coef2*as.numeric(b.xts[i-1,1]) +
      coef3*as.numeric(b.xts[i-2,1])
  }

  return(b.xts)
}

############################################################
############################################################
# k.50 <- kalmanF( S, gain=50 )
kalmanF <- function(S, gain = 500 ) {

  price         <- as.vector(S)      # Convert the closing prices to a simple vector
  kalman        <- rep(0.0, nrow(S) )    # working array
  Kalman        <- S      # final, return value will be an xts object
  names(Kalman) <- "kalman"
  # prediction    <- 0          # Current Predicition
  prediction    <- price[1]          # Current Predicition
  velocity      <- 0          # Velocity

  for( k in 1:nrow(S) ) {

    if ( k == 1 ) {
        ## Initialize first prediction as the acutal first value
        prediction <- price[k]
    }

    DeltaK     <- price[k] - prediction    ## Difference between last KalmanFilter value and current price.
    smooth     <- prediction + DeltaK * sqrt(( gain / 10000) * 2)
    velocity   <- velocity + ((gain / 10000) * DeltaK)
    prediction <- smooth + velocity        ## Final value for Kalman Filter.
    kalman[k]  <- prediction               ## Save predicition for this bar.

  }

  # Fill the xts object with the kalman calculations
  Kalman[,1] <- kalman
  return(Kalman)
}

############################################################
############################################################
performTA <- function(omd) {

  # QQQQ There is still a bug in VWAP.sm when it is called for a stock for many days.
  omd    <- VWAP.sm(omd)

  # Butterworth Filter - STM Version
  bw05     <- butterworth(price=Cl(omd), period=5)   # Column Name: BW05
  ema05    <- EMA(Cl(omd), n=5)
  #sma48    <- SMA(Cl(omd), n=48)
  kfHi.020 <- kalmanF(Hi(omd), gain=20)
  kfLo.020 <- kalmanF(Lo(omd), gain=20)

  omd$BW05 <- rep(0, nrow(omd))
  omd$BW05 <- bw05[,1]

  omd$EMA05 <- rep(0, nrow(omd))
  omd$EMA05 <- ema05[,1]

  #omd$SMA48 <- rep(0, nrow(omd))
  #omd$SMA48 <- sma48[,1]

  omd$KFHI20 <- rep(0, nrow(omd))
  omd$KFHI20 <- kfHi.020[,1]

  omd$KFLO20 <- rep(0, nrow(omd))
  omd$KFLO20 <- kfLo.020[,1]

  # KF MidPoint
  omd$KFMP20 <- rep(0, nrow(omd))
  omd$KFMP20 <- ( kfLo.020[,1] + kfHi.020[,1] ) / 2

  return(omd)
}

############################################################
############################################################
findSignals <- function(d, columns, relationship) {
  #   signals.short <- sigCrossover("goShort", data=oneMinData, columns=c("BW05","VWAP"), relationship=c("lt") )
  #   signals.short <- signals.short[signals.short[, 'goShort'] == TRUE, which.i=TRUE]
  sigs <- sigCrossover("sigName", data=d, columns=columns, relationship=relationship )
  sigs <- sigs[sigs[, 'sigName'] == TRUE, which.i=TRUE]
  return(sigs)
}

############################################################
############################################################
genSignals <- function(d, whichSignal) {

  if ( whichSignal == "BW05 x VWAP" ) {
    # Crossovers of VWAP with BW05
    signals.short <- findSignals(d=oneMinData, columns=c("BW05", "VWAP"), relationship=c("lt") )
    signals.long  <- findSignals(d=oneMinData, columns=c("BW05", "VWAP"), relationship=c("gt") )
  } else if ( whichSignal == "BW05 x KFChannel" ) {
    # Crossovers of BW05 to the KF High/Low Channel
    signals.short <- findSignals(d=d, columns=c("BW05", "KFLO20"), relationship=c("lt") )
    signals.long  <- findSignals(d=d, columns=c("BW05", "KFHI20"), relationship=c("gt") )
  } else if ( whichSignal == "EMA05 x KFChannel" ) {
    # Crossovers of BW05 to the KF High/Low Channel
    signals.short <- findSignals(d=d, columns=c("EMA05", "KFLO20"), relationship=c("lt") )
    signals.long  <- findSignals(d=d, columns=c("EMA05", "KFHI20"), relationship=c("gt") )
  } else if ( whichSignal == "EMA05 x BW05" ) {
    # Crossovers of BW05 to the KF High/Low Channel
    signals.short <- findSignals(d=d, columns=c("EMA05", "BW05"), relationship=c("lt") )
    signals.long  <- findSignals(d=d, columns=c("EMA05", "BW05"), relationship=c("gt") )
  }
  return(list(signals.short=signals.short, signals.long=signals.long))
}

############################################################
############################################################
genTAForCharts <- function(ticker, d) {

  plotVolume <- 'addVo();'
  if ( any ( ticker == plotNoVolume ) ) plotVolume <- ''

  # Before the signals can be plotted they must first be checked for existence.
  # If there are any, add them to the list of Chart TA's.  This check must be
  # done otherwise chartSeries() produces an error.
  signalsShortTA <- "" 
  if (length(signals.short) > 0 ) {
    signalsShortTA <- "addTA(Cl( oneMinData[ signals.short ,] ), type='p', col='black', pch=25, bg='red3', cex=1.5, on=1 );"
  }
  signalsLongTA <- "" 
  if (length(signals.long) > 0 ) {
    signalsLongTA <- "addTA(Cl( oneMinData[ signals.long ,] ), type='p', col='black', pch=24, bg='green3', cex=1.5, on=1 );"
  }

  maxPeaksWhereTA <- ""
  if (length(maxPeaksWhere) > 0 ) {
    maxPeaksWhereTA <- "addTA(Hi( oneMinData[ maxPeaksWhere,] ), type='p', col='black', pch=23, bg='green3', cex=1.0, on=1 );"
  }

  minBottomsWhereTA <- ""
  if (length(minBottomsWhere) > 0 ) {
    minBottomsWhereTA <- "addTA(Lo( oneMinData[ minBottomsWhere,] ), type='p', col='black', pch=23, bg='red', cex=1.0, on=1 );"
  }


  # color reference:  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  # line type reference:  http://students.washington.edu/mclarkso/documents/line%20styles%20Ver2.pdf
  # point type reference: http://rgraphics.limnology.wisc.edu/pch.php

  ## COLOR VWAP LINE ATTEMP    vwap.diff <- diff(d$VWAP)
  ## COLOR VWAP LINE ATTEMP    vwap.diff[1,1] <- 0
  ## COLOR VWAP LINE ATTEMP    vwapColors <<- ifelse(as.vector(vwap.diff[,1])<0, "red", "darkgreen")
  ## COLOR VWAP LINE ATTEMP    "addTA(oneMinData[, 'VWAP'], on=1, col=vwapColors, lwd=3);",

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
     maxPeaksWhereTA,
     minBottomsWhereTA,

     plotVolume,  #   "addVo();" ,
     sep=""
  )

  lastBarTime    <- index(last(d))
  lastBarTimeStr <- format(lastBarTime, "%Y%m%d %H:%M:%S")
  rightNow       <- Sys.time()
  rightNowStr    <- format( Sys.time(), "%H:%M:%S")
  messageStr     <- paste(ticker, ", ", strategy, ", ", lastBarTimeStr, " @ ", rightNowStr, sep="")   

  ss <- NULL
  ## DEBUG  rangeSecs <- 10 * 60
  ## DEBUG  ss <- paste(format(lastBarTime-rangeSecs, "%Y%m%d %H:%M:%S"), "/", format(lastBarTime+rangeSecs, "%Y%m%d %H:%M:%S"), sep="")
  # ss <- "last 8 hours"
  # ss <- "last 3 hours"
  # ss <- "T03:00/T08:00"
  # ss <- "T06:00/T09:00"
    ss <- "T05:00/T10:00"
  # ss <- paste("T05:00/T", sprintf("%02d", hour(now())), ":59", sep="")
  # if ( hour(now()) > 9 ) ss <- paste("last", 4, "hours")

  return(list(messageStr=messageStr, ta=ta, ss=ss))
}


############################################################
############################################################
## MAIN PROGRAM MODES START HERE ###########################
############################################################
############################################################




############################################################
# Plot multiple days of VWAP's for one security
############################################################

theDays   <- as.Date(startDate:endDate)

# MANUALLY    startSecsMN   <- as.numeric(as.POSIXct(startDate) + (7+extraHour) * 3600)
# MANUALLY    endSecs       <- as.numeric(as.POSIXct(endDate)   + (7+extraHour) * 3600) + 13.25 * 3600
# MANUALLY    oneMinData    <- getMinData(ticker, startSecsMN, endSecs, 1) 

oneMinData    <- getMinData(ticker, startDate, endDate, 1) 
oneMinData    <- performTA(oneMinData)
signals       <- genSignals(oneMinData, strategy)
signals.short <- signals$signals.short
signals.long  <- signals$signals.long

oneMinData$Signal <- rep(0, nrow(oneMinData))
oneMinData[signals.short, "Signal"] <- -1
oneMinData[signals.long, "Signal"]  <-  1

# Find the maximum price in between a long signal and the next short signal
# Go Long..........................    Max At.....................
# 2012-12-17 06:35:00 1414.00 (456)
# 2012-12-17 07:26:00 1414.50 (507)    2012-12-17 09:26:00 1422.50
# 2012-12-17 11:25:00 1417.50 (746)

oneMinData$MFE <- rep(0, nrow(oneMinData))
maxPeaksWhere  <- c()
for( longStart in signals.long )
{
# longStart        <- 507
  longEnd          <- first(signals.short[signals.short > longStart])
  if ( is.na(longEnd) ) break ;
  maxPrice         <- max(oneMinData[longStart:longEnd, "High"])
  maxWhere.local   <- which.max(oneMinData[longStart:longEnd, "High"])

  maxWhereDateTime <- index(oneMinData[longStart:longEnd, "High"][maxWhere.local,])
  maxWhere.global  <- oneMinData[maxWhereDateTime, which.i=TRUE]
  maxPeaksWhere    <- c(maxPeaksWhere, maxWhere.global)
  amount           <- maxPrice - as.vector(oneMinData[longStart, "Close"])

  oneMinData[longStart, "MFE"] <- amount 

#  cat(longStart, longEnd, maxPrice, maxWhere.global, amount, "\n")
}

minBottomsWhere  <- c()
for( shortStart in signals.short )
{
# shortStart        <- 507
  shortEnd          <- first(signals.long[signals.long > shortStart])
  if ( is.na(shortEnd) ) break ;
  minPrice         <- min(oneMinData[shortStart:shortEnd, "Low"])
  minWhere.local   <- which.min(oneMinData[shortStart:shortEnd, "Low"])

  minWhereDateTime <- index(oneMinData[shortStart:shortEnd, "Low"][minWhere.local,])
  minWhere.global  <- oneMinData[minWhereDateTime, which.i=TRUE]
  minBottomsWhere  <- c(minBottomsWhere, minWhere.global)
  amount           <- as.vector(oneMinData[shortStart, "Close"]) - minPrice

  oneMinData[shortStart, "MFE"] <- amount 

#  cat(longStart, longEnd, maxPrice, maxWhere.global, amount, "\n")
}



chartProps <- genTAForCharts(ticker, oneMinData)

archivePath <- paste(dbPath, "/longTermArchive", sep="") ;

csvFileName <- paste(archivePath, "/vwap-", ticker, "-1min-", 
                      format(startDate, "%Y%m%d"), "-", 
                      format(endDate, "%Y%m%d"), 
                      ".csv", sep="")
write.zoo(oneMinData, file=csvFileName, sep=",")

## a <- oneMinData["T05:00/T09:00",]
## write.zoo(a, file=csvFileName, sep=",")

## To read the data back in use the following:
## install.packages("chron")   # if necessary - one time only...
# library(chron)
# fmt <- "%Y-%m-%d %H:%M:%S"
# d <- read.zoo("vwap-ES-20130315-1min-20130301-20130309.csv", sep=",", FUN=as.chron, format=fmt, header=TRUE)
# d <- as.xts(d)


# Always plotting to PDF in this mode 
plotFileName <- paste(archivePath, "/vwap-", ticker, "-1min-", 
                      format(startDate, "%Y%m%d"), "-", 
                      format(endDate, "%Y%m%d"), 
                      ".pdf", sep="")
pdf(file=plotFileName, width=13, height=7.5)
# Plot one day at a time
for ( day in theDays ) {
  day <- as.Date(day)
  # if ( wday(day)==1 || wday(day)==7 ) next   # SKIP Saturday & Sunday
    ss <- paste(day, ' 05:00:00/', day, ' 09:00:00', sep='')  # Limit to RTH
  # ss <- paste(day, ' 00:00:00/', day, ' 13:00:00', sep='')  # Limit to RTH
  if ( wday(day)==1 ) {
    # Sunday needs a different Subset to work properly.... We normally
    # subset to 5:00-13:00 on M-F, but for Sundays this causes the error message:
    # Error in periodicity(x) : can not calculate periodicity of 1 observation
    ss    <- paste(day, ' 15:00:00/', day, ' 23:59:00', sep='')  # Limit to RTH
  }
  if( nrow(oneMinData[ss,]) < 1 ) {
    # Skip Sundays for Stocks, but for Futures we want to Plot Sundays.
    # We always want to skip Saturdays.
    # So if the Subset returns no rows, then we want to skip that day.
    next
  }
  messageStr <- paste(day, "  ", wday(day, label=TRUE), sep='')
  print(messageStr)
  chartSeries(oneMinData, subset=ss,
              width=20, height=12,
              name=paste(ticker, " ",  messageStr ),
              type="candlesticks",
              theme=chartTheme(theme='white', up.col='green', dn.col='red'),
              TA=chartProps$ta
  )
}

# Always plotting to PDF in this mode 
dev.off()
system(paste("open ", plotFileName, sep="") )
