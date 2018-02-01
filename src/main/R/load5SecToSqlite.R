# Load a 5-Second OHLCV CSV file into a 5-Second SQLite Database file.
#
#
# Usage: Modify variables "fileNameIn", and "fileNameOut" 
#        Make sure the input does not have duplicate records.
#        If the SQLite database file does not exist it will be created, and the schema will be added to it.

# Created: 3/12/2013

# Clear the work space.  (Useful for when running interactively)
rm(list=ls(all=TRUE))

library(RSQLite)
library(quantmod)
setwd("~/Your/Output/Path/Goes/Here")

fileNameIn <-  "ES-20110916-5Sec.csv" 
fileNameDB <- "fiveSecond-ES-20120921.sqlite"


createTables <- FALSE
if ( ! file.exists(fileNameDB) ) {
  # If the sqlite DB file does not exist, we need to create an empty file first.
  createTables <- TRUE
  zz <- file(fileNameDB, "w")  # open an output file connection
  close(zz)
}

# Read the CSV file into an xts object.  This takes a few minutes, for 1.2M lines.
cat("Read file ", fileNameIn, "... ")
flush.console()
d <- as.xts(read.zoo(fileNameIn, header=TRUE, sep=",", tz=""))
cat("DONE.", fileNameIn, "\n")
flush.console()

# Open a connection to the SQLite DB file.
db <- tryCatch(dbConnect( dbDriver("SQLite") , dbname=fileNameDB, flags=SQLITE_RW, cache_size=32000, synchronous=0 ),
                            error=function(e) 
                            { 
                              print(paste("Caught RSQLite Error: dbConnect() Failed", fileNameDB) )
                              print(e)
                              return(e)
                            }
                          )
error <- dbGetException(db) 
if (error$errorMsg != "OK") {
  # Fail Early - For some reason the database would not allow a connection.
  stop(paste("Could not open database", fileNameDB) )
}

# Check if the schema needs to be created.
tableList <- dbListTables(db)
if ( length(tableList) < 1 ) 
{
  # If the SQLite DB File does not have any tables in it, we need to create the Bars5Seconds table and add an index as well.
  ddl1 <- "CREATE TABLE Bars5Seconds ( start INTEGER primary key, tsStart TIMESTAMP, open REAL, high REAL, low REAL, close REAL, volume INTEGER, wap REAL, count INTEGER );"
  ddl2 <- "CREATE INDEX tsStartIndex ON Bars5Seconds(tsStart );"
  res1 <- dbSendQuery(db, ddl1)
  res2 <- dbSendQuery(db, ddl2)
}

# sql <- "INSERT OR REPLACE INTO Bars5Seconds ( start, tsStart,  open, high, low, close, volume, wap, count ) VALUES (1344292835, '20120806 15:40:35', 1389.5, 1389.5, 1389.5, 1389.5, 0, 1389.5, 0) ;"
# sql <- "INSERT OR REPLACE INTO Bars5Seconds ( start, tsStart,  open, high, low, close, volume, wap, count ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) " ;

# Start a transaction
dbBeginTransaction(db)
numRows <- nrow(d)
i       <- 1

while( i <= numRows )
{
  if ( trunc(i/100000)*100000 == i ) {
    # After 100000 rows, commit the transaction
    dbCommit(db)
    # Start a new transaction
    dbBeginTransaction(db)
  }

  sql <- sprintf ( "INSERT INTO Bars5Seconds ( start, tsStart,  open, high, low, close, volume, wap, count ) VALUES (%d, '%s', %f, %f, %f, %f, %d, 0, 0)",
                  as.numeric(d[i, "start"]),
                  format(index(d[i])),
                  as.numeric(d[i, "Open"]),
                  as.numeric(d[i, "High"]),
                  as.numeric(d[i, "Low"]),
                  as.numeric(d[i, "Close"]),
                  as.numeric(d[i, "Volume"])
                 )

  res <- dbSendQuery(db, sql)
  dbClearResult(res)

  i <- i + 1
}

# Commit the final transaction.
dbCommit(db)

# Close the connection to the db.
dbDisconnect(db)
