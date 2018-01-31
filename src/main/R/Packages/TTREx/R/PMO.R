#'TTREx
#'
#'Extensions to TTR by Stergios Marinopoulos
#'
# Text to appear under the "Details" section go here.
#
#'@aliases TTREx PMO MesaStochastic SuperSmoother GetOHLCSqlite KalmanFilter Kama
#'
#'@param x  a xts data object of prices
#'@param lookBackLen  length of lookback period to be used in the first EMA.
#'@param smoothLen  number of periods to smooth the result of the first EMA.
#'@return For \code{PMO()} a xts object that contains the following columns:
#'\enumerate{
#'  \item \code{roc1} The discrete day rate-of-change
#'  \item \code{ema35} The \code{ema(35, roc1) * 10}
#'  \item \code{ema20} The \code{ema(20, ema35)}
#'}
#'For \code{MesaStochastic()} a xts object that contains the following columns:
#'\enumerate{
#'  \item \code{HP}  The High Pass Filter
#'  \item \code{SuperSmoother} Ehler's Super Smoother on the High Pass Filter
#'  \item \code{Stoch}         Regular Stochastics
#'  \item \code{MesaStoch}     Ehler's Stochastics
#'}
#'For \code{SuperSmoother()} a xts object that contains the following columns:
#'\enumerate{
#'  \item \code{SS}      Ehler's SuperSmoother
#'  \item \code{SSEMA}   An EMA of the SuperSmoother
#'  \item \code{SSDiff}  The percent difference of SS and SSEMA (\code{(SS-SSEMA)/SS)}
#'  \item \code{SSSig}   An EMA of SSDiff
#'}
#'For \code{GetOHLCSqlite()} Load OHLC data from a 5 second sqlite DB file.
#'\enumerate{
#'  \item \code{fn}         The file name and path of the 5 second sqlite database file
#'  \item \code{ticker}     The ticker name ("ES")
#'  \item \code{startSecs}  Start point of data to load
#'  \item \code{endSecs}    End point of data to load
#'  \item \code{numMinutes} Bar size to return
#'}
#'For \code{KalmanFilter()} Perform simple Kalman Filter
#'\enumerate{
#'  \item \code{S}       A single column xts object to operate on, e.g. Cl(OHLC)
#'  \item \code{gain}    Desired gain.
#'}
#'For \code{Kama()}  Kaufman Adaptive Moving Average
#'\enumerate{
#'  \item \code{s}       A single column xts object to operate on, e.g. Cl(OHLC)
#'  \item \code{fast}    The fast move average length
#'  \item \code{slow}    The slow moving average length
#'  \item \code{period}  The lookback period
#'  \item \code{debug}   boolean flag for debug print outs
#'}
#'@references #'\enumerate{
#'  \item DecisionPoint PMO is documented at \url{http://stockcharts.com/school/doku.php?st=pmo&id=chart_school:technical_indicators:dppmo}
#'  \item TASC January 2014 Magazine article 'Predictive Indicators for Effective Trading Strategies' - The Ehler's \code{C#} source code for the article is found in \url{http://www.ninjatrader.com/SC/January2014SC.zip}
#'}
#'@note
#' \code{PMO} calculates the DecisionPoint Price Momentum Oscillator.  It is
#' derived by taking a one day rate of change and smoothing it with two
#' Exponential Moving Averages. The first EMA defaults to 35 periods, and it is
#' then further smoothed by a second EMA which defaults to 20 periods. 
#'
#' A note on how the \code{ROC()} indicator from the \code{TTR} package is used
#' by PMO.  \code{ROC()} provides the percentage difference of a series over
#' two observations, while the momentum indicator simply provides the
#' difference.  It offers two different type of computations:
#' \enumerate{
#'   \item type="continuous"  \code{ROC=diff(log(price), 1)}          (default)
#'   \item type="discrete"    \code{ROC=(price/lag.xts(price)) - 1}
#' }
#' The EXCEL code for the 1 DAY ROC (Column B) found in the example spreadsheat
#' uses the "discrete" type and is as follows:\cr\cr
#' \code{=SUM(((B13/B12)*100)-100)}\cr
#'
#'
#' \code{MesaStochastic} calculates a John F. Eller's HighPass, Filter, and
#' Stochastics.  For the \code{MesaStochastic()} function, the reference can be
#' found is the TASC January 2014 Magazine article 'Predictive Indicators for
#' effective trading strategies' - The \code{C#} source code for the article is
#' found in \code{http://www.ninjatrader.com/SC/January2014SC.zip}
#'
#'
#' \code{SuperSmoother} calculates a John F. Eller's \code{SuperSmoother()} function.
#' For the \code{SuperSMoother()} function, the reference can be
#' found is the TASC January 2014 Magazine article 'Predictive Indicators for
#' effective trading strategies' - The \code{C#} source code for the article is
#' found in \code{http://www.ninjatrader.com/SC/January2014SC.zip}
#'
#'
#'
#'
#'@rdname TTREx
#'@export PMO
"PMO" <-
function(x, lookBackLen=35, smoothLen=20)
{
  # The DecisionPoint Price Momentum Oscillator is derived by taking a one day
  # rate of change and smoothing it with two Exponential Moving Averages. The
  # first EMA defaults to 35 periods and is multiplied by 10. It is then further
  # smoothed by a second EMA which defaults to 20 periods. 
  #
  # PMO = 20 EMA (10 * 35 EMA(((Today's Price/Yesterday's Price)*100)-100))
  #
  # roc   = ((Today's Price/Yesterday's Price)*100)-100
  # ema35 = EMA(35, roc) * 10
  # ema20 = EMA(20, ema35)
  #
  #
  roc1 = ROC(x=x, n=1, type="discrete")*100
  names(roc1) = c("roc.1")

  ema35 = EMA(x=roc1[,"roc.1"], n=35) * 10
  names(ema35) = c("ema.35")

  ema20 = EMA(x=ema35[,"ema.35"], n=20)
  names(ema20) = c("ema.20")

  ## DDDD pmo=x
  ## DDDD names(pmo)=c("price")

  ## DDDD pmo$roc1=roc1[,"roc.1"]
  roc1$ema35=ema35[,"ema.35"]
  roc1$ema20=ema20[,"ema.20"]

  return(roc1)
}

#-------------------------------------------------------------------------#

#'@rdname TTREx
#'@export MesaStochastic
"MesaStochastic" <- 
function(x, n=20)
{

  theLength = n 
  alpha1    = 0
  a1        = 0
  b1        = 0
  c1        = 0
  c2        = 0
  c3        = 0
  HighestC  = 0
  LowestC   = 0
  count     = 0
  Stoc      = 0
          
  # private DataSeries HP;
  # private DataSeries Filt;
  # private DataSeries Stoc1;
  # public  DataSeries MyStochastics

  # Highpass filter cyclic components whose periods are shorter than 48 bars
  HP            = x ; names(HP)            = c("HP")
  # Smooth HP with a Super Smoother Filter
  # Two Pole High Pass Filter.  The effects of spectral dilation are removed.  This gives
  # the oscillator a zero mean to accurately assess turning points and generally redices indicator lag.
  Filt          = x ; names(Filt)          = c("Filt")
  Stoc1         = x ; names(Stoc1)         = c("Stoc1")
  MyStochastics = x ; names(MyStochastics) = c("MyStochastics")

  # In the R programming language angles are in radians, not degrees 
  # i.e., a right angle is pi/2 ==>  pi/2 (radians) = 90 (degrees) ==> 1 radian = 90degress/(pi/2) = 57.29578 degrees

  # alpha1 = ((Math.Cos(((.707 * 360 / 48) * Math.PI) / 180)) + (Math.Sin(((.707 * 360 / 48) * Math.PI) / 180)) - 1) / (Math.Cos(((.707 * 360 / 48) * Math.PI) / 180));
  alpha1 = ((cos(((.707 * 360 / 48) * pi) / 180)) + (sin(((.707 * 360 / 48) * pi) / 180)) - 1) / (cos(((.707 * 360 / 48) * pi) / 180))

  # a1 = Math.Exp(-1.414 * 3.14159 / 10);
  a1 = exp( -1.414 * 3.14159 / 10 )
   
  # b1 = 2 * a1 * (Math.Cos(((1.414 * 180 / 10) * Math.PI) / 180));
  b1 = 2 * a1 * (cos(((1.414 * 180 / 10) * pi) / 180))

  # c2 = b1;
  c2 = b1

  # c3 = (a1 * -1) * a1;
  c3 = (a1 * -1) * a1

  # c1 = 1 - c2 - c3;
  c1 = 1 - c2 - c3


  # Direct copy of the C# "for" loops.  (Not a very R way of doing things)
  numPeriods = nrow(x)
  for ( rCount in 1:numPeriods ) {
    cat("row: ", rCount, "\n", sep="")

    if(rCount <= theLength)
    {
      MyStochastics[rCount,1] = 0
      HP[rCount,1]            = 0
      Filt[rCount,1]          = 0
      next
    } 

    # HP.Set((1 - alpha1 / 2) * (1 - alpha1 / 2) * (Close[0] - 2 * Close[1] + Close[2]) + 2 * (1 - alpha1) * HP[1] - (1 - alpha1) * (1 - alpha1) * HP[2]);

    HP[rCount, 1] = ((1 - alpha1 / 2) * (1 - alpha1 / 2) * (as.numeric(x[rCount,1]) - 2 * as.numeric(x[rCount-1,1]) + as.numeric(x[rCount-2,1]) ) + 2 * (1 - alpha1) * as.numeric(HP[rCount-1, 1]) - (1 - alpha1) * (1 - alpha1) * as.numeric(HP[rCount-2, 1])  )

    # This is also computed in the function SuperSmoother() below in an R way.
    # Filt.Set(c1 * (HP[0] + HP[1]) / 2 + c2 * Filt[1] + c3 * Filt[2]);
    Filt[rCount, 1] = (c1 * (as.numeric(HP[rCount, 1]) + as.numeric(HP[rCount-1, 1])) / 2 + c2 * as.numeric(Filt[rCount-1, 1]) + c3 * as.numeric(Filt[rCount-2, 1]) )
    
    HighestC = as.numeric(Filt[rCount, 1])
    LowestC = as.numeric(Filt[rCount, 1])
    
    for(i in 1:(theLength) )
    {
        testValue = as.numeric(Filt[rCount-i, 1])

        if(testValue > HighestC)
        {
            HighestC = testValue
        }
        
        if(testValue < LowestC)
        {
            LowestC = testValue
        }
    }
    
    # Stoc1.Set((Filt[0] - LowestC) / (HighestC - LowestC));
    Stoc1[rCount, 1] = ((as.numeric(Filt[rCount, 1]) - LowestC) / (HighestC - LowestC))
    
    # MyStochastics.Set(c1 * (Stoc1[0] + Stoc1[1]) / 2 + c2 * MyStochastics[1] + c3 * MyStochastics[2]);
    MyStochastics[rCount, 1] = (c1 * (as.numeric(Stoc1[rCount, 1]) + as.numeric(Stoc1[rCount-1, 1]) ) / 2 + c2 * as.numeric(MyStochastics[rCount-1, 1]) + c3 * as.numeric(MyStochastics[rCount-2, 1]) )

  }

  ############################################################
  # Computation Finished
  #
  # stop("COMPUTATION FINISHED")  # for debugging

  # Assemble the return xts object, using "HP" as the base
  HP$SuperSmoother = Filt[, 1]
  HP$Stoch         = Stoc1[, 1] 
  HP$MesaStoch     = MyStochastics[, 1]

  return(HP)
}

#-------------------------------------------------------------------------#

#'@rdname TTREx
#'@export SuperSmoother
"SuperSmoother" <- 
function(x, emaLength=5) 
{

# Vars: a1(0), b1(0), c1(0), c2(0), c3(0), Filt(0);
  a1 = 0 
  b1 = 0 
  c1 = 0 
  c2 = 0 
  c3 = 0 

  Filt          = x ; names(Filt)          = c("Filt")

  # In the R programming language the trigonometrics arguments are in radians,
  # not degrees.  i.e., a right angle is pi/2 ==>  pi/2 (radians) = 90
  # (degrees) ==> 1 radian = 90degress/(pi/2) = 57.29578 degrees

  # If the reader is translating to another computer language, please note that
  # the trigonometric arguments are in degrees whereas they are in radians in
  # most other languages.

  # I wonder which one the article is actually using?
  # sqrt(2)     = 1.414214
  # cos(pi/4)*2 = 1.414214

# a1 = expvalue(-1.414*3.14159 / 10)
  a1 = exp( -sqrt(2) * pi / 10 )


# b1 = 2*a1*Cosine(1.414*180 / 10)
  b1 = 2 * a1 * cos( ( sqrt(2) * 180 / 10) * (pi/180) )

  c2 = b1
  c3 = -a1*a1
  c1 = 1 - c2 - c3

  lags = Lag(x, k=1:2)

# Filt = c1*(Close + Close[1]) / 2 + c2*Filt[1] + c3*Filt[2];
  Filt = c1*(x + lags[,1]) / 2 + c2*lags[,1] + c3*lags[,2];

  FiltEma     = EMA(Filt, emaLength)
  FiltDiff    = (Filt[,1] - FiltEma[,1])/Filt[,1]
  FiltSig     = EMA(FiltDiff, emaLength)

  names(Filt) = c("SS")
  Filt$SSEMA  = FiltEma[,1]
  Filt$SSDiff = FiltDiff[,1]
  Filt$SSSig  = FiltSig[,1]

  return(Filt)
}

#-------------------------------------------------------------------------#

#'@rdname TTREx
#'@export GetOHLCSqlite
"GetOHLCSqlite" <- 
function(fn, ticker, startSecs, endSecs, numMinutes=1) 
{
  ############################################################
  # Load the 5 second OHLC data from a sqlite DB and convert it into 1 minute
  # OHLC data.  This is a complex routine due to the fact that while this program
  # is reading from the SQLite db file, another program may be writing to it. So
  # I have to handle the conflict here.  MySql, Postgres, or any other server
  # based RDBMS would handle this for me, but that's the trade off for using
  # SQLite
  ##    
  ##    FIX THIS ERROR:  added on 10/28/13 .  The file was locked
  ##    
  ##    Working on ES-20131220 for 2013-10-28
  ##    [1] "Caught RSQLite Error: fetch() Failed"
  ##    <simpleError in sqliteFetch(res, n = n, ...): RSQLite driver: (RS_SQLite_fetch: failed first step: database is locked)>
  ##    
  ##    

  if('package:DBI' %in% search() || require('DBI',quietly=TRUE)) {
    if('package:RSQLite' %in% search() || require('RSQLite',quietly=TRUE)) {
    } else { warning(paste("package:",dQuote("RSQLite"),"cannot be loaded" )) }
  } else {
    stop(paste("package:",dQuote('DBI'),"cannot be loaded."))
  }

  if('package:lubridate' %in% search() || require('lubridate',quietly=TRUE)) {
  } else { 
    stop(paste("package:",dQuote("lubridate"),"cannot be loaded" )) 
  }


  if('package:xts' %in% search() || require('xts',quietly=TRUE)) {
  } else { 
    stop(paste("xts:",dQuote("xts"),"cannot be loaded" )) 
  }

  ## DEBUG
  ## DEBUG    ticker     = "ES"
  ## DEBUG    fn         = '~/runTime/fiveSecond-ES-20150320.sqlite'
  ## DEBUG    numMinutes = 1
  ## DEBUG    Sys.setenv(TZ="UTC")
  ## DEBUG    endSecs    = as.integer( now( tz = "America/Los_Angeles") )
  ## DEBUG    startSecs  = endSecs - 24*5
          

  if ( is.double(startSecs) ) {
    startSecs = as.integer(startSecs)
  }
  if ( is.double(endSecs) ) {
    endSecs = as.integer(endSecs)
  }

  if ( startSecs > endSecs ) {
    stop("GetOHLCSqlite() ERROR:  startSecs > endSecs.   The start time must be before the end time!")
  }

  # DEBUG print(fn)
  # OLD    db <- dbConnect( dbDriver("SQLite") , dbname=fn, flags=SQLITE_RO, cache_size=32000, synchronous=0 )

  drv = DBI::dbDriver("SQLite")

  db = tryCatch(DBI::dbConnect(drv, dbname=fn, flags=SQLITE_RO, cache_size=32000, synchronous="off" ),
                error=function(e) 
                { 
                  print(paste("Caught RSQLite Error: dbConnect() Failed", fn) )
                  print(e)
                  return(e)
                }
  )
  error = DBI::dbGetException(db) 
  if (error$errorMsg != "OK") {
    stop(paste("ERROR: Could not open database", fn) )
  }

  dbSymbols = DBI::dbListTables(db)

  tableName = "Bars5Seconds"
  if( "Bars5Seconds" %in% dbSymbols ) {
    tableName = "Bars5Seconds"
  } else if( "Bars" %in% dbSymbols ) {
    tableName = "Bars"
  } else {
    DBI::dbDisconnect(db)
    stop(paste("ERROR: Could not find 'Bars' or 'Bars5Seconds' in data base", fn) )
  }


  ## SELECT start, tsStart, open, high, low, close, volume FROM Bars WHERE start >= 1419010351  AND start < 1419010471 ORDER BY start 
  sql = sprintf("SELECT start, tsStart, open, high, low, close, volume FROM %s WHERE start >= %d  AND start < %d ORDER BY start ;", tableName, startSecs, endSecs )

  ## DEBUG print(sql)

  ## Wrap the call to dbGetQuery() with error handling code such that if it is
  ## simply a BUSY error, we will wait  a short time and try again.  If it is
  ## another error, well, that does not really matter at this point since the
  ## program will fail in an yet unknown way...  Below is the error message we
  ## are trying to handle properly: 
  ##
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

  # Send the query.  Enter a loop, and exit when the query has been sent.
  attempt       = 0
  gotResultsSet = FALSE
  while(!gotResultsSet) {
    attempt      = attempt + 1
    sleepLength  = attempt * 0.1  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
    resultSetObj = tryCatch(dbSendQuery(db, sql), 
                              error=function(e) 
                              { 
                                print("Caught RSQLite Error: DBI::dbSendQuery() Failed")
                                print(e)
                                return(e)
                              }
                            )
    error = DBI::dbGetException(db) 

    if (error$errorMsg != "OK") {
      cat("RSQLite returned an error on dbSendQuery().  Wait 0.1 seconds and try again\n") 
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
    gotResultsSet = TRUE 
  }

  # Fetch the data.  Enter a loop, and exit when the data has been fetched.
  attempt = 0
  gotData = FALSE
  skipDbFile <<- FALSE      # variable that an error closure will use to communicate back to "us"
  while(!gotData) {
    attempt     = attempt + 1
    sleepLength = attempt * 0.1  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
    trades = tryCatch(DBI::dbFetch(resultSetObj, n=-1), 
                         error=function(e)
                         {
                           print("Caught RSQLite Error: DBI::dbFetch() Failed")
                           print(e)
                           ## <simpleError in sqliteFetch(res, n = n, ...): RSQLite driver: (RS_SQLite_fetch: failed: database disk image is malformed)>
                           e="<simpleError in sqliteFetch(res, n = n, ...): RSQLite driver: (RS_SQLite_fetch: failed: database disk image is malformed)>"
                           badDbFile = grep("database disk image is malformed", e, fixed=TRUE)
                           if ( length(badDbFile) > 0 ) 
                             skipDbFile <<- TRUE
                           return(e)
                         }
                      )
    error = DBI::dbGetException(db) 

    if ( skipDbFile == TRUE ) {
      DBI::dbClearResult(resultSetObj)
      DBI::dbDisconnect(db)
      stop(paste("ERROR: Error with db file: ", fn))
      return(FALSE) 
    }
    rm(skipDbFile, envir=.GlobalEnv) # Clean up after ourselves.

    ##  DEBUG    print(class(trades))
    ##  OBSERVED    [1] "Caught RSQLite Error: fetch() Failed"
    ##  OBSERVED    Error in trades[, -c(1, 2)] : incorrect number of dimensions
    
    if (inherits(trades, "error") ) {
      cat("RSQLite returned an error on fetch().  Using inherits().... Wait 0.1 seconds and try again\n") 
      print(error)
      Sys.sleep(sleepLength)  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
      next 
    }

    if (error$errorMsg != "OK" ) {
      cat("RSQLite returned an error on DBI::fetch().  Using error$errorMsg.... Wait 0.1 seconds and try again\n") 
      print(error)
      Sys.sleep(sleepLength)  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
      dbClearResult(resultSetObj) # we probably need to call this here.  Untested so far.
      next 
    }

    gotData = TRUE

    DBI::dbClearResult(resultSetObj)
  }

  DBI::dbDisconnect(db)

  # All 5 second data has been loaded.


  # FX will return "-1" or negative volumes.  So just set it to +1
  trades$volume = ifelse(trades$volume == -1, 1, trades$volume)

  ## BAD IDEA    Changed this behavior on 12/19/2014
  ## BAD IDEA    # Filter out "0" volume trades
  ## BAD IDEA    trades <- trades[trades$volume > 0, ]

  # Convert the 5 second data into an xts object
  trades.xts  = as.xts(trades[, -c(1,2)], order.by=as.POSIXct(trades[, "tsStart"], format="%Y%m%d %H:%M:%S" ) )

  # Convert the 5 second xts object into a 1 minute xts object
  trades.min = to.period(trades.xts, period='minutes', k=numMinutes, indexAt="startof")
  colnames(trades.min) = c("Open", "High", "Low", "Close", "Volume")

  # QQQQ  Force FX to always have volume = 1 
  # QQQQ  list of FX is hard coded here.  UGH.
  if ( any( ticker == c( "GBPUSD", "USDCAD", "EURUSD") ) ) {
    trades.min[, 'Volume'] = 1
  }
  
  return(trades.min)
}

## DEBUG    --------------------------------------------------------------------------------
## DEBUG    Sys.setenv(TZ="UTC")
## DEBUG    mins          = 10
## DEBUG    ticker        = "ES-20150320"
## DEBUG    dbFileName    = '~/runTime/fiveSecond-ES-20150320.sqlite'
## DEBUG    
## DEBUG    # startDate   = as.POSIXct("2014-12-07 00:00:00", tz = "America/Los_Angeles")
## DEBUG    # endDate     = as.POSIXct("2014-12-11 13:00:00", tz = "America/Los_Angeles")
## DEBUG    
## DEBUG    # startSecs   = as.numeric(startDate)
## DEBUG    # endSecs     = as.numeric(endDate)
## DEBUG    
## DEBUG    endSecs       = as.integer( now( tz = "America/Los_Angeles") )
## DEBUG    startSecs     = endSecs - 3600 * 10
## DEBUG    
## DEBUG    S = GetOHLCSqlite(fn=dbFileName, ticker="ES", startSecs=startSecs, endSecs=endSecs, numMinutes=mins)
## DEBUG
## DEBUG    --------------------------------------------------------------------------------
## DEBUG
## DEBUG    Sys.setenv(TZ="UTC")
## DEBUG    mins         = 10
## DEBUG    ticker       = "CL-20150120"
## DEBUG    tickerSymbol = "CL"
## DEBUG    dbFileName   = paste('~/runTime/fiveSecond-', ticker,  '.sqlite', sep='')
## DEBUG    
## DEBUG    
## DEBUG    startDate   = as.POSIXct("2014-12-15 00:00:00", tz = "America/Los_Angeles")
## DEBUG    endDate     = as.POSIXct("2014-12-22 13:00:00", tz = "America/Los_Angeles")
## DEBUG    
## DEBUG    startSecs   = as.integer(startDate)
## DEBUG    endSecs     = as.integer(endDate)
## DEBUG    
## DEBUG    S = GetOHLCSqlite(fn=dbFileName, ticker=tickerSymbol, startSecs=startSecs, endSecs=endSecs, numMinutes=mins)
## DEBUG
## DEBUG    --------------------------------------------------------------------------------

#-------------------------------------------------------------------------#

#'@rdname TTREx
#'@export KalmanFilter
"KalmanFilter" <- 
function(S, gain = 500 ) {
  price      <- as.vector(S)       # Convert the closing prices to a simple vector
  kalman     <- rep(0.0, nrow(S) ) # Working array
  prediction <- price[1]           # Current Predicition
  velocity   <- 0                  # Velocity

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
  Kalman        <- S      # final, return value will be an xts object
  names(Kalman) <- "kalman"
  Kalman[,1] <- kalman
  return(Kalman)
}

#-------------------------------------------------------------------------#

#'@rdname TTREx
#'@export Kama
"Kama" <- 
function(s, fast=2, slow=30, period=10, withBW=FALSE, debug=FALSE) {

  if('package:PerformanceAnalytics' %in% search() || require('PerformanceAnalytics',quietly=TRUE)) {
  } else { 
    stop(paste("PerformanceAnalytics:",dQuote("PerformanceAnalytics"),"cannot be loaded" )) 
  }

  ## DEBUG    s   = Cl(S)
  # Initialize answer xts object
  ans = s
  names(ans) = c("price")

  ## ans = xts(x=rep(x=0, times=length(s)), order.by=index(s))
  ## names(ans) = c("kama")
  ## ans$kama = s[,1]  - I could do this, but by using zero as on the line below, bugs will be easier to identify
  ans$kama = xts(x=rep(x=0, times=length(s)), order.by=index(s))

  ## DEBUG
  ## DEBUG    fast=2
  ## DEBUG    slow=30
  ## DEBUG    period=10

  ## CSharp    if (CurrentBar < Period)
  ## CSharp    {
  ## CSharp      Value.Set(Input[0]);
  ## CSharp      return;
  ## CSharp    }
  ans[1:period, "kama"] = s[1:period, 1]

  ## CSharp    if (CurrentBar > 0)
  ## CSharp    {
  ## CSharp      diffSeries.Set(Math.Abs(Input[0] - Input[1]));
  ## CSharp    }
  diffSeries = abs(s - Lag(x=s))
  ans$diffSeries = diffSeries

  ## CSharp    double fastCF = 2.0 / (double)(fast + 1);
  ## CSharp    double slowCF = 2.0 / (double)(slow + 1);

  fastCF = 2.0 / (fast + 1)
  slowCF = 2.0 / (slow + 1)

  ## CSharp    double signal = Math.Abs(Input[0] - Input[Period]);
  signal = abs(s - Lag(x=s, k=period))
  ans$signal = signal

  ## CSharp    double noise  = SUM(diffSeries, Period)[0];
  if (debug==TRUE) { t1=Sys.time() }
  noise = apply.rolling(diffSeries, width=period, FUN=sum)
  if (debug==TRUE) { t2=Sys.time(); cat(paste("PROFILE: apply.rolling() on ", dQuote("noise"), " at TTREX line 655 took: ", (t2-t1), "seconds \n"), sep="") }
  ans$noise = noise

  ## R check for noise == 0     any(noise==0, na.rm=TRUE) ... do this below
  ## Do this below by check for Inf after using noise as denomiator.

  ## CSharp    // Prevent div by zero
  ## CSharp    if (noise == 0)
  ## CSharp    {
  ## CSharp      Value.Set(Value[1]);
  ## CSharp      return;
  ## CSharp    }

  ## CSharp    double smooth = Math.Pow((signal / noise) * (fastCF - slowCF) + slowCF, 2);

  smooth = ( ( ans$signal / ans$noise ) * (fastCF - slowCF) + slowCF ) ^ 2

  # For any "noise" == 0, smooth could be one of Inf, -Inf, or NaN.  Set smooth
  # to zero for these cases so that the current kama will simply be the
  # previous value.
  if ( any(is.infinite(smooth)) ) {
    smooth[which(is.infinite(smooth)), 1] = 0
  }
  if ( any(is.nan(smooth)) ) {
    smooth[which(is.nan(smooth)), 1] = 0
  }

  ## CSharp    Value.Set(Value[1] + smooth * (Input[0] - Value[1]));

  startIndex = last(which(is.na(smooth))) + 1 


  if (debug==TRUE) { t1=Sys.time() }
  for(i in startIndex:nrow(s)) {
    ans[i, "kama"] = as.numeric(ans[i-1, "kama"]) + as.numeric(smooth[i, 1]) * (   as.numeric(ans[i,"price"]) - as.numeric(ans[i-1, "kama"])   )
  }
  if (debug==TRUE) { t2=Sys.time(); cat(paste("PROFILE: loop for ", dQuote("kama"), " at TTREX line 689 took: ", (t2-t1), "seconds \n"), sep="") }

  
  ## DEBUG OUTPUT START
  ## Dump calculations to a file for debugging same computations being made in java code.
  if (debug==TRUE) {
    kamaDebug = ans
    i = as.integer(index(kamaDebug)) + 8 * 3600
    kamaDebug$epoc = i
    ## QQQQ - A hard coded path here could be trouble....
    write.zoo(kamaDebug, file = "~/runTime/log/kamaR.csv", index.name = "Index", row.names = TRUE, sep=',')
  }
  ## DEBUG OUTPUT END


  # Compute the BINARY WAVE
  ## Raw[period]= ama[period] - ama[period-1]
  kamaDiff = diff(ans[, "kama"] )

  ## FilterPercent: = Input ("Filter Percentage", 0,100,15) / 100;
  filterPercent = 0.15

  ## local filter = FilterPercent/100 * mathex.stdev( Raw, period-Period+1, period);
  if (debug==TRUE) { t1=Sys.time() }
  ans$kamaStd    = apply.rolling(kamaDiff, FUN="sd", width=period)
  if (debug==TRUE) { t2=Sys.time(); cat(paste("PROFILE: apply.rolling for ", dQuote("kamaStd"), " at TTREX line 715 took: ", (t2-t1), "seconds \n"), sep="") }
  ans$kamaFilter = filterPercent * ans[, 'kamaStd']

  firstIndex = 1
  if( anyNA(ans[, 'kamaFilter'] ) ) 
  {
    ## firstIndex = kamaFilter[!is.na(head(kamaFilter, 20)), which.i=TRUE][1]
    firstIndex = last(which(is.na( ans[, 'kamaFilter'] ))) + 1 
  }

  if(withBW==TRUE) {
  ans$kamaLow  = xts(x=rep(x=0, times=length(s)), order.by=index(s))
  ans$kamaHigh = xts(x=rep(x=0, times=length(s)), order.by=index(s))
  ans$kamaBW   = xts(x=rep(x=0, times=length(s)), order.by=index(s))


  if (debug==TRUE) { t1=Sys.time() }
  for(i in (firstIndex+1):nrow(ans)) {

    kamaNow    = as.numeric(ans[i, "kama"])
    kamaPrev   = as.numeric(ans[i-1, "kama"])
    kamaFilter = as.numeric(ans[i, "kamaFilter"])

    if (kamaNow < kamaPrev ) {
      ans[i, "kamaLow"] = ans[i, "kama"]
    } else {
      ans[i, "kamaLow"] = ans[i-1, "kamaLow"]
    }

    if (kamaNow > kamaPrev ) {
      ans[i, "kamaHigh"] = ans[i, "kama"]
    } else {
      ans[i, "kamaHigh"] = ans[i-1, "kamaHigh"]
    }

    if ( kamaNow - as.numeric(ans[i, "kamaLow"]) > kamaFilter ) {
       ans[i, "kamaBW"] = 1
    
    } else if ( as.numeric(ans[i, "kamaHigh"]) - kamaNow > kamaFilter ) {
       ans[i, "kamaBW"] = -1

    } else {
       ans[i, "kamaBW"] = 0
    }

  }
  if (debug==TRUE) { t2=Sys.time(); cat(paste("PROFILE: for loop on ", dQuote("kamaBW"), " at TTREX line 715 took: ", (t2-t1), "seconds \n"), sep="") }
  return(ans[, c("kama", "kamaBW") ])
  }

  return(ans[, c("kama") ])

}


#-------------------------------------------------------------------------#




# vim: ts=2 sts=2 sw=2 ht=2 expandtab number ruler showmode smartindent ff=unix foldmethod=marker :
