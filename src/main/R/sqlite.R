# vim: ts=2 sts=2 sw=2 ht=2 expandtab number ruler showmode smartindent ff=unix foldmethod=marker :


# Load the 5 second OHLC data from a sqlite DB and convert it into 1 minute OHLC
# xts object  This is a complex routine due to lock contention when sqlite has
# concurrent reads and writes.  Since I want to support multiple processes
# accessing the sqlite DB I have to handle the conflict here.  Of course, MySql,
# Postgres, or any other server based RDBMS would handle this automatically, but
# that's the trade off for using SQLite
#
# Stergios Marinopoulos
# Created: 1/09/2011


if('package:DBI' %in% search() || require('DBI',quietly=TRUE)) {
  if('package:RSQLite' %in% search() || require('RSQLite',quietly=TRUE)) {
  } else { warning(paste("package:",dQuote("RSQLite"),"cannot be loaded" )) }
} else {
  stop(paste("package:",dQuote('DBI'),"cannot be loaded."))
}

library(lubridate)
library(xts)


getOHLCData <- function(fn, ticker, startSecs, endSecs, numMinutes=1) 
{
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
    stop("getOHLCData() ERROR:  startSecs > endSecs.   The start time must be before the end time!")
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
  ## another error, the program will fail in an yet unknown way...  Below is the
  ## error message we are trying to handle properly: 
  ##
  ## Error in sqliteFetch(rs, n = -1, ...) : 
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
  skipDbFile <<- FALSE
  while(!gotData) {
    attempt     = attempt + 1
    sleepLength = attempt * 0.1  # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
    trades = tryCatch(DBI::dbFetch(resultSetObj, n=-1), 
                         error=function(e)
                         {
                           print("Caught RSQLite Error: DBI::dbFetch() Failed")
                           print(e)
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
      Sys.sleep(sleepLength)      # sleep NumberAttempts * 0.1 seconds - Linearly back off algo
      dbClearResult(resultSetObj)
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

  # Force FX to always have volume = 1 
  # list of FX is hard coded here.  UGH.
  if ( any( ticker == c( "GBPUSD", "USDCAD", "EURUSD") ) ) {
    trades.min[, 'Volume'] = 1
  }
  
  return(trades.min)
}

## --------------------------------------------------------------------------------

Sys.setenv(TZ="UTC")
mins         = 1                                                             # This will set the higher time frame size in mins.
ticker       = "SPY"
tickerSymbol = "SPY"
dbFileName   = paste('~/runTime/fiveSecond-', ticker,  '.sqlite', sep='')    # Edit to reflect your file system path.

startDate   = as.POSIXct("2014-10-01 00:00:00", tz = "America/Los_Angeles")  # Change these start and stop times as desired.
endDate     = as.POSIXct("2014-12-31 13:00:00", tz = "America/Los_Angeles")

startSecs   = as.integer(startDate)
endSecs     = as.integer(endDate)

S = getOHLCData(fn=dbFileName, ticker=tickerSymbol, startSecs=startSecs, endSecs=endSecs, numMinutes=mins)

# Remove data from outside regular trading hours.
# xts makes extensive use of ISO 8601 sub-setting:
S = S['T06:30/T13:00']


##     --------------------------------------------------------------------------------
