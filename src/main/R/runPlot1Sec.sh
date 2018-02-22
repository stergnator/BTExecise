#!/bin/sh

# Call program "plot1Sec.R"
# plot1Sec.R is a program to plot 1 second data at varying timeframes with VWAP plotted on it.
#
# Stergios Marinopoulos
# Created: 11/03/2011

DATAFILENAME=`ls -t *01sec.csv | head -1`
echo $DATAFILENAME
MINUTEBARSIZE=1

if [ -n "$1" ]   # Test for non-empty string
then
  if [ -e "$1" ]
  then
    DATAFILENAME="$1"
  else
    echo "$1 does not exist"
    exit 
  fi
fi 

if [ -n "$2" ]   # Test for non-empty string
then
  ###if echo $2 | grep "^[0-9]*$" > /dev/null
  if [[ $2 = *[[:digit:]]* ]];
  then
    MINUTEBARSIZE="$2"
  else
    echo "$2 is not a number"
    exit
  fi
fi 

R --quiet \
  --slave \
  --no-readline \
  --file=plot1Sec.R \
  --args \
  dataFileName $DATAFILENAME \
  minuteBarSize $MINUTEBARSIZE
