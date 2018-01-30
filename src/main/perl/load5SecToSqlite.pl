#!/usr/bin/perl

# A program to read 5 second OHLC data from a CSV file and insert the records into 
# SQLite DB file with a table named Bars5Seconds
#
# This program is based on a very similar R program.  But that program suffers
# from R being slow slow slow for some very simple operations.
#
# Created 12/10/2013
#
use strict;
use DBI;

my $csvFileName = "ES-20110916-5Sec.csv" ;
my $dbFileName = "fiveSecond-ES-20120921B-tada.sqlite" ;

############################################################
# Parse Command line argumentso
# Proper arguments are no args at all, or the following:
#
#     load5SecToSqlite.pl csvFileName=something.csv dbFileName=something.sqlite
#
if ( scalar(@ARGV) == 2 ) {
  foreach my $arg (@ARGV) {
    ## DEBUG    print($arg, "\n") ;
    my ( $key, $value ) = split( '=', $arg ) ;
    if ( $key eq '--csvFileName' ) {
      $csvFileName = $value ;
    } elsif ( $key eq '--dbFileName' ) {
      $dbFileName = $value ;
    } else {
      print("Unrecogonized argument: $arg\n") ;
      exit() ;
    }
  }
}

my $dbh = DBI->connect("dbi:SQLite:dbname=$dbFileName", "", "") ;

############################################################
# See if the Database file has the tables.  Unlike the RSQLite, perl's
# DBD::SQLite will create a new db file if it does not exist.  In this case, we
# have to create the schema, so check for the table now.
my $createTable = 1 ;
my $meta_info = $dbh->table_info('%', '%', '%', 'TABLE') ; # parameters are Catalog, Schema, Table, Type
while(my($qualifier, $object_owner, $object_name, $object_type, $comments) = $meta_info->fetchrow_array())
{
  if ( $object_name eq "Bars5Seconds" ) {
    $createTable = 0 ;
  }
  # DEBUG print $object_owner,"\t",$object_name,"\t",$object_type,"\n";
}

if ( $createTable == 1 ) {
  my $ddl1 = "CREATE TABLE Bars5Seconds ( start INTEGER primary key, tsStart TIMESTAMP, open REAL, high REAL, low REAL, close REAL, volume INTEGER, wap REAL, count INTEGER )" ;
  my $ddl2 = "CREATE INDEX tsStartIndex ON Bars5Seconds(tsStart )" ;
  my $res1 = $dbh->do( $ddl1 ) ;
  my $res2 = $dbh->do( $ddl2 ) ;
}

############################################################
# Open the CSV File
open my $input, '<', $csvFileName or die "can't open $csvFileName: $!";
my $lineCount = 1 ;
my $line = <$input> ; # Throw away first line

############################################################
# Create prepared SQL statements.
my $sqlPrepared = "INSERT OR REPLACE INTO Bars5Seconds ( start, tsStart,  open, high, low, close, volume, wap, count ) VALUES (?, ?, ?, ?, ?, ?, ?, 0, 0)" ;
my $sth = $dbh->prepare($sqlPrepared) ;


############################################################
# Iterate over all CSV records, using transactions to improve speed.
$dbh->do('BEGIN TRANSACTION');

while ($line = <$input>) {
  chomp $line ;
  my ( $tsStart, $open, $high, $low, $close, $volume, $start) = split(',', $line) ;
  $tsStart =~ s/-//g ;

  ##   Simple Approach:  No prepared statement
  ##   my $sql = sprintf "INSERT OR REPLACE INTO Bars5Seconds ( start, tsStart,  open, high, low, close, volume, wap, count ) VALUES (%d, '%s', %f, %f, %f, %f, %d, 0, 0);", ##                   $start, $tsStart,
  ##                   $open, $high, $low, $close, $volume
  ##             ;
  ##   $dbh->do($sql) ;

  # Every 1000 CSV records finish the transactions, and start a new one.
  if ( int($lineCount/1000)*1000 == $lineCount ) {
    $dbh->do('COMMIT');
    $dbh->do('BEGIN TRANSACTION');
  }
  $sth->execute( $start, $tsStart, $open, $high, $low, $close, $volume ) ;

  $lineCount = $lineCount + 1 ;
}

# Finish the last outstanding transaction.
$dbh->do('COMMIT');

# Clean up and finsih
close $input or die "can't close $csvFileName: $!";
$dbh->disconnect ;
