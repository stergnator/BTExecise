# Program to plot 1 second data at varying timeframes with VWAP plotted on it.
#
# Stergios Marinopoulos
# Created: 3/12/2013

# Example Usage:  (use runPlot1Sec.sh)
#
#    R --quiet \
#      --slave \
#      --no-readline \
#      --file=plot1Sec.R \
#      --args \
#      dataFileName $DATAFILENAME \
#      minuteBarSize $MINUTEBARSIZE

suppressPackageStartupMessages(library(quantmod)) ;
suppressPackageStartupMessages(library(batch)) ;

dataFileName <- c("ES-20111101-064000-01sec.csv") ;

minuteBarSize <- c(5) ;  # Ex: 5

minuteBarSizeName <- paste("0", minuteBarSize, sep="") ;  # Ex: 05


## Overwrite the values of 'dataFileName' if it was passed in on the command line args
sink("/dev/null"); ## prevent the next line from printing to stdout.
parseCommandArgs(evaluate=TRUE) ;
sink();
print(dataFileName, quote=FALSE) ;

# Format the minute component of the pdf file name
if ( minuteBarSize >= 10 )
{
  # Perfect as is.  No alterations necessary.
  minuteBarSizeName <- paste(minuteBarSize, "min", sep="") ;
} else {
  # pad the single digit number if a leading zero
  minuteBarSizeName <- paste("0", minuteBarSize, "min", sep="") ;
}

pdfFileName = gsub(".csv", ".pdf", dataFileName) ;

dataTs    <- read.zoo(dataFileName, header=TRUE, sep=",", tz="") ; # read file
dataXts   <- as.xts(dataTs) ; # convert file

dataDate <- gsub(".*(\\d\\d\\d\\d\\d\\d\\d\\d).*", "\\1", dataFileName, perl=TRUE) ;  # returns YYYYMMDD
dataDate <- gsub("(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)", "\\1-\\2-\\3", dataDate, perl=TRUE ); # returns YYYY-MM-DD

oneSecPV     <- c(0) ;
oneSecSumVol <- c(0) ;
oneSecSumPV  <- c(0) ;
oneSecVWAP   <- c(0) ;

## If we have a 1-second file, convert it to 1 minute data
if ( grep(".*01sec.csv$", dataFileName ) == 1 )
{
  ## But first calculate the VWAP using the one second data
  oneSecPV       <- Cl(dataXts) * Vo(dataXts) ;
  oneSecSumVol   <- cumsum(Vo(dataXts)) ;
  oneSecSumPV    <- cumsum(oneSecPV) ;
  oneSecVWAP     <- oneSecSumPV / oneSecSumVol ;
  oneMinuteVWAP  <- oneSecVWAP[endpoints(oneSecVWAP, on="minutes", k=minuteBarSize)] ;

  ## DEBUG print("Converting Data to 1-Minute Data", quote=FALSE) ;
  dataXts <- to.period(dataXts, period = 'minutes', k=minuteBarSize)
  colnames(dataXts) <- c("Open", "High", "Low", "Close", "Volume");
  pdfFileName = gsub("01sec", minuteBarSizeName,  pdfFileName) ;
}


if ( as.numeric(strftime(end(dataXts), "%H")) >= 7 )
{
  channelTimeRange <- c('T06:30/T07:00') ;
  ## addLines(h=c(1195, 1188.50));
  ## addVLine(as.POSIXct("2011-03-14 06:34:59")) ;
  maxChannelPrice <- max(dataXts[channelTimeRange][,2]) ;
  minChannelPrice <- min(dataXts[channelTimeRange][,3]) ;
  TASTR <- c("addVo();addVWAPSM(on=-1);addLines(h=c(maxChannelPrice, minChannelPrice));") ;
} else
{
  TASTR <- c("addVo();addVWAPSM(on=-1);") ;
}

vline1<-as.POSIXct(paste(dataDate, " ", "06:30", sep=""))
vline2<-as.POSIXct(paste(dataDate, " ", "07:00", sep=""))

VWAPSM <-
function(x)
{
  ## Functions by newTA() are only called once.  
  ## This function is only called once by newTA, so make sure all computations are vector in nature
  ## and that the return value is a vector containing the answers for the entire input series

  ## I am going to plot 1-minute data, but I want to plot VWAP from 1-second data.
  ## So I created a 1-second based vwap data, and then converted it to 1-minute data above,
  ## and in this routine I will simple reference the precomputed answer.

  return(oneMinuteVWAP);

  ## If I wanted to compute vwap from the data passed into this routine, presumably 1-minute data
  ## I would perform the following computations:
  ## pv     <- Cl(x) * Vo(x) ;
  ## sumvol <- cumsum(Vo(x)) ;  #colnames(sumvol) <- c("SumVolume") ;
  ## sumpv  <- cumsum(pv) ;     #colnames(sumpv)  <- c("SumPriceVolume") ;
  ## vwap   <- sumpv/sumvol ;   #colnames(vwap)   <- c("VWAP") ;
  ## return(vwap) ;

}
addVLine = function(dtlist) 
{ 
 plot(addTA(xts( rep(TRUE, NROW(dtlist)), dtlist), on=1, col="#333333")) 
} 
addVWAPSM <- newTA(FUN=VWAPSM, col="green", legend="VWAP" ) ;

pdf(pdfFileName, width=10.0, height=8.0) ;
candleChart(
            dataXts
            , name=pdfFileName
            , theme="white"
#           , TA="addVWAPSM(on=-1);addLines(h=c(maxChannelPrice, minChannelPrice));"
            , TA=TASTR
            ) ;
dev.off() ;
print(paste("open ", pdfFileName), quote=FALSE) ;
system(paste("open ", pdfFileName)) ;
