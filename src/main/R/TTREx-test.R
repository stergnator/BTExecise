# vim: ts=2 sts=2 sw=2 ht=2 expandtab number ruler showmode smartindent ff=unix foldmethod=marker :
############################################################
#
# Stergios Marinopoulos, 4/7/2014
#
# Examples / Testing of the TTREx package
#
# John F. Ehlers : 'Predictive Indicators for effective trading strategies' -
# TASC January 2014" This code was ported from the CSharp code listed for
# NinjaTrader as found in the following archive:
# http://www.ninjatrader.com/SC/January2014SC.zip    In places below you will
# see the origional CSharp code commented out.

# Clear the work space.
rm(list=ls(all=TRUE))

library(quantmod)
library(TTREx)
Sys.setenv(TZ="UTC")

OHLC=getSymbols('SPY',   from='2012-01-01', to='2013-12-31', adjust=TRUE, auto.assign=FALSE)

# Create some xts objects for drawing horizontal lines in chartSeries()
# This is a work around as chartSeries() does not support the addition of 
# grahpics primitives by the end user.  We are creating new timeseries objects,
# but will fill them with constant values where we desire the horizontal lines
# to be.
l0  = .xts(rep( 0,nrow(OHLC)), .index(OHLC))
l1  = .xts(rep( 1,nrow(OHLC)), .index(OHLC))
lp2 = .xts(rep( 2,nrow(OHLC)), .index(OHLC))
lm2 = .xts(rep(-2,nrow(OHLC)), .index(OHLC))

l10 = .xts(rep( 0.10,nrow(OHLC)), .index(OHLC))
l20 = .xts(rep( 0.20,nrow(OHLC)), .index(OHLC))
l30 = .xts(rep( 0.30,nrow(OHLC)), .index(OHLC))
l40 = .xts(rep( 0.40,nrow(OHLC)), .index(OHLC))
l50 = .xts(rep( 0.50,nrow(OHLC)), .index(OHLC))
l60 = .xts(rep( 0.60,nrow(OHLC)), .index(OHLC))
l70 = .xts(rep( 0.70,nrow(OHLC)), .index(OHLC))
l80 = .xts(rep( 0.80,nrow(OHLC)), .index(OHLC))
l90 = .xts(rep( 0.90,nrow(OHLC)), .index(OHLC))

############################################################
# Now plot some examples

############################################################
# SuperSmoother Example
if(FALSE) # TRUE to run
{
  x=(Op(OHLC)+ Hi(OHLC)+ Lo(OHLC)+ Cl(OHLC))/4

  smoothed  = SuperSmoother(x, emaLength=3)
# Convert Ehlers SuperSmoother in to a list so it can be attached so that chartSeries() 
# can find them, when it comes time to resample or redraw.
  smoothed2 = as.list(smoothed)
  attach(smoothed2, name="ssexample")

# Create the long string to the chartSeries() TA argument
  ta = paste(
              "addTA(SSDiff[,1],       col='#FF0000')",   # Plot 2
#           "addTA(SSSig[,1],  on=2, col='#0000FF')",   # On Plot 2
              "addTA(l0,         on=2, col='#000000')",   # On Plot 2

              "addTA(SS[,1],     on=1, col='#FF0000')",   # On Plot 1
              "addTA(SSEMA[,1],  on=1, col='#0000FF')",   # On Plot 1

              sep=";"
            )

  ss="2013-01-01/2013-12-31"

  chartSeries(OHLC, subset=ss, theme=chartTheme(theme='white'), TA=ta)

  detach(name="ssexample", character.only = TRUE)
  stop("SuperSmoother Example FINISHED")  # for debugging
}

############################################################
# MesaSthocastic Example
if(TRUE)  # FALSE to skip
{
  eFilter=MesaStochastic(Cl(OHLC))

  # Convert Ehlers filter into a list so it can be attached so that chartSeries() can find them.
  eFilter2=as.list(eFilter)
  attach(eFilter2, name="msexample")

  # Create the long string to the chartSeries() TA argument
  ta = paste(
              "addTA(SuperSmoother[,1])",        # Plot 2
              "addTA(MesaStoch[,1])",        # Plot 3

              # Horizontal lines for plot 2
              "addTA(l0,  on=2, col='#FF0000')", # On Plot 2
              "addTA(lp2, on=2, col='#444444')", # On Plot 2
              "addTA(lm2, on=2, col='#444444')", # On Plot 2

              # Horizontal lines for plot 3
              "addTA(l0,  on=3, col='#AAAAAA')", # On Plot 3
              "addTA(l20, on=3, col='#AAAAAA')", # On Plot 3
              "addTA(l50, on=3, col='#AAAAAA')", # On Plot 3
              "addTA(l80, on=3, col='#AAAAAA')", # On Plot 3
              "addTA(l1,  on=3, col='#AAAAAA')", # On Plot 3
              sep=";"
              )


  ss="2012-09-24/2013-11-06"

  chartSeries(OHLC, subset=ss, theme=chartTheme(theme='white'), TA=ta)
  detach(name="msexample", character.only = TRUE)
}
# This looks a lot like the basing point procedure.
# min55 <- runMin(Lo(OHLC),55)
# chartSeries(OHLC, TA="addTA(min55)" )
# addTA(min55,on=2) 




############################################################
# KalmanFitler, Kama, and GetOHLCSqlite Examples

library(TTREx)
library(quantmod)

Sys.setenv(TZ="UTC")
mins          = 10
ticker        = "ES-20150320"
dbFileName    = '~/runTime/fiveSecond-ES-20150320.sqlite'

startDate   = as.POSIXct("2014-12-07 00:00:00", tz = "America/Los_Angeles")
endDate     = as.POSIXct("2014-12-11 13:00:00", tz = "America/Los_Angeles")

startSecs   = as.numeric(startDate)
endSecs     = as.numeric(endDate)

# endSecs       = as.integer( now( tz = "America/Los_Angeles") )
# startSecs     = endSecs - 3600 * 10

S = GetOHLCSqlite(fn=dbFileName, ticker="ES", startSecs=startSecs, endSecs=endSecs, numMinutes=mins)
kf  = KalmanFilter(S=Cl(S), gain=50)

amaDF          = Kama(s=Cl(S), debug=FALSE) # returns a DF with column names c("kama", "kamaBW")
ama            = amaDF[, "kama"]
amaBW          = amaDF[, "kamaBW"]
