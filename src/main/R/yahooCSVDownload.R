# vim: ts=2 sts=2 sw=2 ht=2 expandtab number ruler showmode smartindent ff=unix

#  A program to download daily stock prices from yahoo or google.
#
#
# Created: 2012-03-16
# Stergios Marinopoulos
#
#
# 0) Steps to use this program follow.
# 
# 1) Create a string variable that contains the list of tickers you want to
# download.  See 'gspc', 'sp100', 'sp500', 'etfs', 'etfsDebug' below for examples.
#
# 2) Make sure to use the variable in the "run.params" data.frame, near line 100.
#
# 3) Run this program
#
# 4) Examine the CSV files
#
# 5) If a CSV file already exists, then only the most recent list of days where
# data missing will be downloaded and then appended to the CSV file.
#
# 6) The function fetchStockData() controls the date range of the data to be downloaded.
#
#
## Clear the work space!
rm(list = ls(all = TRUE))

library(quantmod) ;
# Notes on changes to tickers/companies:
# Removing PSX as of 5/30/12 - for some reason the history now only goes back two months...
# Removing EP as of 6/12/12 - it has disappeared.
# MMI removed as of 6/12/12 - bought by google, and no longer trading.
# NVLS removed as of 6/12/12 - Lam Research Corp's (NASDAQ:LRCX) merger with Novellus Systems Inc. (NASDAQ:NVLS)
# PSX removed as of 5/30/12 - the stock no longer goes back very far????

gColNames        <- c("Date","Open","High","Low","Close","Volume", "Adjusted") ;
dataSourceYahoo  <- "yahoo" ;   # use yahoo to get data from a long time ago.  About 20 years.
dataSourceGoogle <- "google" ;  # use google to get closing prices as soon as possible after the daily close.

# Handle Yahoo's differences of some stock symbology.
# BFB 404 error at yahoo  should be BF-B @yahoo,  it is "BF.B" @Google
setSymbolLookup(BFB=list(name="BF-B",src="yahoo")) ;
setSymbolLookup(GSPC=list(name="^GSPC",src="yahoo")) ;

# Various groups of stocks to download together.
gspc <- " GSPC " ;

sp100 <- "
AA    AAPL  ABT   AEP   ALL   AMGN  AMZN  AVP   AXP   BA    BAC   BAX   BHI  
BK    BMY   BRK.B CAT   C     CL    CMCSA COF   COP   COST  CPB   CSCO  CVS  
CVX   DD    DELL  DIS   DOW   DVN   EMC   ETR   EXC   F     FCX   FDX   GD   
GE    GILD  GOOG  GS    HAL   HD    HNZ   HON   HPQ   IBM   INTC  JNJ   JPM  
KFT   KO    LMT   LOW   MA    MCD   MDT   MET   MMM   MO    MON   MRK   MS   
MSFT  NKE   NOV   NSC   NWSA  NYX   ORCL  OXY   PEP   PFE   PG    PM    QCOM 
RF    RTN   S     SLB   SLE   SO    T     TGT   TWX   TXN   UNH   UPS   USB  
UTX   VZ    WAG   WFC   WMB   WMT   WY    XOM   XRX   SPY   DIA   IWM   QQQ  
EMR   NEM   INTU
" ; 

sp500 <- "
MMM   ACE   ABT   ANF   ACN   ADBE  AMD   AES   AET   AFL   A     GAS   APD  
ARG   AKAM  AA    ATI   AGN   ALL   ALTR  MO    AMZN  AEE   AEP   AXP   AIG  
AMT   AMP   ABC   AMGN  APH   APC   ADI   AON   APA   AIV   APOL  AAPL  AMAT 
ADM   AIZ   T     ADSK  ADP   AN    AZO   AVB   AVY   AVP   BHI   BLL   BAC  
BK    BCR   BAX   BBT   BEAM  BDX   BBBY  BMS   BRK.B BBY   BIG   BIIB  BLK  
HRB   BMC   BA    BWA   BXP   BSX   BMY   BRCM        CHRW  CA    CVC   COG  
CAM   CPB   COF   CAH   CFN   KMX   CCL   CAT   CBG   CBS   CELG  CNP   CTL  
CERN  CF    SCHW  CHK   CVX   CB    CI    CINF  CTAS  CSCO  C     CTXS  CLF  
CLX   CME   CMS   COH   KO    CCE   CTSH  CL    CMCSA CMA   CSC   CAG   COP  
CNX   ED    STZ         GLW   COST  CVH   COV   CSX   CMI   CVS   DHI   DHR  
DRI   DVA   DF    DE    DELL  DNR   XRAY  DVN   DV    DO    DTV   DFS   DISCA
DLTR  D     RRD   DOV   DOW   DPS   DTE   DD    DUK   DNB   ETFC  EMN   ETN  
EBAY  ECL   EIX   EW          EA    EMC   EMR   ETR   EOG   EQT   EFX   EQR  
EL    EXC   EXPE  EXPD  ESRX  XOM   FFIV  FDO   FAST  FII   FDX   FIS   FITB 
FHN   FSLR  FE    FISV  FLIR  FLS   FLR   FMC   FTI   F     FRX   BEN   FCX  
FTR   GME   GCI   GPS   GD    GE    GIS   GPC   GNW   GILD  GS    GR    GT   
GOOG  GWW   HAL   HOG   HAR   HRS   HIG   HAS   HCP   HCN   HNZ   HP    HES  
HPQ   HD    HON   HRL   HSP   HST   HCBK  HUM   HBAN  ITW   TEG   INTC  ICE  
IBM   IFF   IGT   IP    IPG   INTU  ISRG  IVZ   IRM   XYL   JBL   JEC   CBE  
JDSU  JNJ   JCI   JOY   JPM   JNPR  K     KEY   KMB   KIM   KLAC  KSS   KFT  
KR    LLL   LH    LM    LEG   LEN   LUK   LXK   LIFE  LLY   LTD   LNC   LLTC 
LMT   L     LO    LOW   LSI   MTB   M     MRO   MPC   MAR   MMC   MAS   ANR  
MA    MAT   MKC   MCD   MHP   MCK   MJN   MWV         MDT   MRK   MET   PCS  
MCHP  MU    MSFT  MOLX  TAP   MON   MCO   MS    MOS         MSI   MUR   MYL  
NBR   NDAQ  NOV   NTAP  NFLX  NWL   NFX   NEM   NWSA  NEE   NKE   NI    NE   
NBL   JWN   NSC   NTRS  NOC   NU    CMG         NRG   NUE   NVDA  NYX   ORLY 
OXY   OMC   OKE   ORCL  OI    PCAR  IR    PLL   PH    PDCO  PAYX  BTU   JCP  
PBCT  POM   PEP   PKI   PRGO  PFE   PCG   PM    PNW   PXD   PBI   PCL   PNC  
RL    PPG   PPL   PX    PCP   PCLN  PFG   PG    PGN   PGR   PLD   PRU   PEG  
PSA   PHM   QEP   PWR   QCOM  DGX   RRC   RTN   RHT   RF    RSG   RAI   RHI  
ROK   COL   ROP   ROST  RDC   R     SWY   SAI   CRM   SNDK  SLE   SCG   SLB  
SNI   SEE   SHLD  SRE   SHW   SIAL  SPG   SLM   SJM   SNA   SO    LUV   SWN  
SE    S     STJ   SWK   SPLS  SBUX  HOT   STT   SRCL  SYK   SUN   STI   SVU  
SYMC  SYY   TROW  TGT   TEL   TE    THC   TDC   TER   TSO   TXN   TXT   HSY  
TRV   TMO   TIF   TWX   TWC   TIE   TJX   TMK   TSS   TRIP  TSN   TYC   USB  
UNP   UNH   UPS   X     UTX   UNM   URBN  VFC   VLO   VAR   VTR   VRSN  VZ   
VIAB  V     VNO   VMC   WMT   WAG   DIS   WPO   WM    WAT   WPI   WLP   WFC  
WDC   WU    WY    WHR   WFM   WMB   WIN   WEC   WPX   WYN   WYNN  XEL   XRX  
XLNX  XL    YHOO  YUM   ZMH   ZION
SPY   DIA   IWM   QQQ 
      RIG
" ;

etfs <- "
SPY  GLD  VWO   EEM   EFA  QQQ  IVV  TIP  VTI  LQD  IWF  IWM  BND  AGG  HYG  IWD  DIA  JNK  VNQ   VIG  
IJH  SHY  DVY   SLV   MDY  XLK  IAU  CSJ  SDY  EWZ  PFF  VEA  IJR  BSV  XLE  VUG  XLF  IVW  IWB   IWR  
VEU  GDX  FXI   XLU   MOO  DBC  VTV  EWJ  XLP  MBB  EWC  CIU  IVE  IEF  EMB  VB   XLV  AMJ  IWN   IWO   TBT  
VO   OEF  IYR   DEM   IWP  IWV  VOO  EPP  VV   EWY  XLI  VYM  IWS  RSP  XLY  IJK  BIV  EWG  AMLP  VCSH  TLT  
MUB  ICF  ACWI  AAXJ  RWX  EWA  VGK  EWT  VGT  SHV  DJP  IJS  IEI  IJJ  VBK  RSX  XLB 
" ;

etfsDebug <- "
SPY  GLD  VWO   EEM   EFA  QQQ
" ;


# Uncomment the group of stocks you wish to download below.
run.params <- data.frame(

  # StockList = gspc, 
  # StockList = sp500, 
  # StockList = sp500[!sp500 %in% sp100],   # This is the difference between sp500 and sp100
  # StockList = sp100, 
  # StockList = etfs, 
    StockList = etfsDebug, 
  # StockList = "CVX",
  # StockList = "A AA AAPL ABC" ,
  # StockList = paste(sp500, " ", etfs),
 
  PauseBetween = 5,   # Pause N seconds between each request, trying not to hammer Yahoo too much.

  When=Sys.time(),
  WhenPP=format( Sys.time(), "%Y%m%d_%H%M%S"),
  stringsAsFactors=FALSE         # Do not edit
) ;





############################################################
# The file system location where we store our daily stock data previously
# fetched from yahoo.  By default assume we are running on a Windows PC
stockDBDir     = "C:/storage/stockdb" ;
actualMachine  = Sys.info()[["nodename"]]
if ( actualMachine == "localhost"                           ||
     grepl("hackintosh", actualMachine, ignore.case=TRUE  ) ||
     grepl("RoadRunner", actualMachine, ignore.case=TRUE  )
   ) {
  cat("Initialize variables specific to one of Sterg's OSX computers.\n") ;
  stockDBDir <- "~/CSV";

} else if ( actualMachine == "mopar" ) {
  cat("Initialize variables specific to Sterg's Mac Mini") ;
  stockDBDir <- "~/CSV";

} else if ( actualMachine == "GARAGE4" ) {
  cat("Initialize variables specific to Sterg's Garage Computer\n") ;
  stockDBDir <- "C:/storage/stockdb" ;
  ## DEBUG stockDBDir <- "C:/storage/stockdbDebug" ;

} else if ( actualMachine == "US145200" ) {
  cat("Initialize variables specific to Rob's Home Computer\n") ;
  stockDBDir <- "C:/users/CSV" ;

} else if (  grepl("Joes-Rogoffs-iMac.local", actualMachine, ignore.case=TRUE ) ||
             grepl("Joes-MacBook-Pro.local", actualMachine, ignore.case=TRUE ) ) {
  cat("Initialize variables specific to Brian's iMac\n")
  stockDBDir     <- "~/CSV";
} else {
  cat("WHAT MACHINE IS THIS PROGRAM RUNNING ON?",
      "Please send Stergios the output from running this command:\n",
      "\n\tSys.info()[['nodename']]",
      "\n\n")
  stop("Cannot proceed until the program knows who is running this program.")

}
flush.console() ;

############################################################
# Allow command line arguments to override any default setting in run.params.

overRideCommandArgs <- function(p) {
  args <- commandArgs(trailingOnly = TRUE) ;

  if ( length(args) > 0 ) {
    ## DEBUG    
    print(paste("command line args:", args)) ;
    for(a in args) {
      commandLineArgs <- unlist(strsplit(a, "=" )) ;
      p[1, commandLineArgs[1]] <- commandLineArgs[2] ;
    }

    for(n in names(p)) {
      cat(sprintf("AFTER CL OVERRIDE USING: %25s: %30s\n", n, p[1,n])) ;
    }

  }
  return(p) ;
}

run.params <- overRideCommandArgs(run.params) ;

############################################################
# Function to create the output directory if it does not exist.
checkDBDir <- function(toDir) {

  dirParts <- unlist(strsplit(toDir, "/")) ;
  leafDir  <- dirParts[length(dirParts)] ;
  parentDir  <- paste(dirParts[-length(dirParts)], collapse="/") ;

  if ( ! file.exists(toDir) ) {
      dir.create(file.path(parentDir, leafDir)) ;
      cat("Creating directory for CSV data storage:", toDir, "\n") ;  
  } else {
      ## DEBUG   cat(toDir, "Already Exists\n") ;  
  }
}

# Make sure the output directory exists.
checkDBDir(stockDBDir) ;

############################################################
############################################################
# Function to convert a string into a vector.  Think split() in perl
str2Vector <- function(s) {
  s <- gsub("\\n", " ", s, perl=TRUE) ;   # replace a newline with a space
  s <- sub("^\\s", "", s, perl=TRUE) ;    # drop leading space
  s <- sub("\\s$", "", s, perl=TRUE) ;    # drop trailing space
  s <- unlist(strsplit(s, "\\s+", perl=TRUE))  # split on white space
  s <- s[nzchar(s)] ;                     # only return non-null strings
} 


############################################################
############################################################
# Load the old data if it exists, fecth the most up to data,
# merge it all, and return it.
#
# d <- fetchStockData("CSCO", "C:/storage/stockdb/CSCO.csv" ) ;
# d <- fetchStockData("DD", "C:/storage/stockdb/DD.csv" ) ;
# d <- fetchStockData("SPY", "C:/storage/stockdb/SPY.csv" ) ;
fetchStockData <- function(ticker, fileName, forceNewData=FALSE) {
  # Date,Open,High,Low,Close,Volume
  # 2009/01/02,16.4100,17.0000,16.2500,16.9600,40980600
  # 2009/01/05,16.8500,17.3000,16.7500,17.1100,45480200
  # ....  
  # 2010/09/02,20.4800,20.7000,20.3100,20.5200,59407400
  # 2010/09/03,20.9700,21.1300,20.7600,21.0400,54434400

  lastDayWeHave <- "1990-01-01" ;

  today         <- Sys.Date() ;
  nr            <- 0 ;  # assume we do not have any old data for this stock.

  # assume incremental data update is needed
  # dataSource <- dataSourceGoogle ;
  dataSource <- dataSourceYahoo ;
  cat(paste("Processing ", ticker , "\n", sep="") ) ;

  if ( file.exists( fileName ) & forceNewData==FALSE) {
    # good, old file exists
    stockData <-read.csv(fileName, header=TRUE, 
      col.names = gColNames,
      stringsAsFactors=FALSE
    ) ;

    nr <- nrow(stockData) ;
    if ( nr > 0 ) {
      # old file exists, and it has data too!
      lastDayWeHave <- gsub("/", "-", stockData[nr, "Date"], perl=TRUE) ;
      cat(paste("\tLast day we have data for ", ticker, " = ", lastDayWeHave, "\n", sep="") ) ;
    } else {
      # old file exists, but it has no data
      dataSource <- dataSourceYahoo ;
      cat(paste("\tThe file (", fileName, ") exists, but is empty.  No data for ", ticker, "\n", sep="") ) ;
    }
  } else {
    # New ticker symbol
    dataSource <- dataSourceYahoo ;
    cat(paste("\tFetch All New Data for ", ticker , "\n", sep="") ) ;
  }

  fromDate <- as.POSIXct( lastDayWeHave ) ;
  toDate   <- today ; 
  cat(paste("\tGetting new data for ", ticker, " from=", fromDate, ", to=", toDate, "\n", sep="") ) ;

  ## QQQQ WARNING - We cannot use Google for getting the long look back history due to a bug they have in their data from the period Dec 28,29,30  2003:
  ## QQQQ WARNING     Warning messages:
  ## QQQQ WARNING   1: In getSymbols.google(Symbols = c("AIG", "AMT", "AMP",  ... :
  ## QQQQ WARNING     google duplicate bug - missing Dec 28,29,30 of 2003
  ## QQQQ WARNING   2: In getSymbols.google(Symbols = c("AIG", "AMT", "AMP",  ... :
  ## QQQQ WARNING     google duplicate bug - missing Dec 28,29,30 of 2003

  gotData <- FALSE ;
 
  while ( !gotData ) {
    newData <- NULL ;
    
    try ( newData <- getSymbols(ticker, from=fromDate, to=toDate, auto.assign=FALSE, src=dataSource ) )

    if(!is.null(newData)) { 
      gotData <- TRUE 
    } else {
      cat(paste("\t", dataSource, " failed to process our request - sleep 5 secs and try again\n", sep="") )
      Sys.sleep(5) 
    } 

  }

  ## DEBUG newData  <- newData[, c(1,2,3,4,5)] ; # Drop the "Adjusted" column...
  names(newData) <- gColNames[2:7] ;      # remove the ticker prefix from each column name

  if( nr > 0 ) {
    # convert old stock data to xts 
    stockDataXts <- xts(stockData[, -1], as.Date(stockData[, 1]), src = "csv", updated = Sys.time())
    # merge old and new data
    allData <- rbind( stockDataXts[1:(nrow(stockDataXts)-1),], newData ) ;
  } else {
    # there was only new data.
    allData <- newData ;
  }

  cat(paste("\tGot new data!  Last Day for new data: ", index(allData)[nrow(allData)], "\n", sep="") ) ;
  return(allData) ;
}

############################################################
############################################################

stockList <- str2Vector( as.vector(run.params[1, 'StockList']) )
                        
############################################################
############################################################
# OLD  flist <- list.files(path=stockDBDir, pattern = ".csv$", full.names=TRUE) ;
# OLD  for(f in flist) {
# OLD    ticker <- sub("(.*)+/([A-Z]+).csv", "\\2", f, perl=TRUE)

for(f in sort(stockList)) {
  ## DEBUG    if ( f < "XEL" ) next ;  # Skip paste stocks we already downloaded today...

  ticker   <- f
  fileName <- paste(stockDBDir, "/", ticker, ".csv", sep="") ;
  oldFile  <- paste(stockDBDir, "/", ticker, ".old", sep="") ;

  d        <- fetchStockData(ticker, fileName, forceNewData=TRUE) ;

  if ( is.na( file.info(oldFile)[1,1] ) != TRUE ) {
    try(file.rename(fileName, oldFile), silent=TRUE) ;
  }
  write.zoo(d, file=fileName, index.name="Date", quote=FALSE, sep="," )
  ## If we get blocked by YAHOO, we might need to pause inbetween stocks.
  Sys.sleep(run.params[1, "PauseBetween"])
}
