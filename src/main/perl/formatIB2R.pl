#!/usr/bin/perl 
#===============================================================================
#
#         FILE:  formatIB2R.pl
#
#        USAGE:  ./formatIB2R.pl | sort | uniq > R_ready_file
#
#  DESCRIPTION: 
#               Convert IB historical 1 second data into a zoo compatiable csv
#               file for R.  TO USE: edit the subroutine initFileList() to
#               return the list of IB 1 second files to convert. OR, use the
#               command line arguments to specify a directory containing *bz2
#               files as follows:
#
#               formatIB2R.pl  path=/Users/stergios/IB/trunk/Examples/rawData/YM-20110916 | sort | uniq > R_read_file
#
#      OPTIONS:  ---
# REQUIREMENTS:  ---
#      VERSION:  1.0
#      CREATED:  03/28/2011 12:33:13
#===============================================================================

use strict;
use warnings;
use POSIX 'strftime';



my $bunzip2 = q|/usr/bin/bunzip2| ;
my $path = q|/Users/stergios/IB/trunk/Examples/rawData/YM-20110916| ;


############################################################
# Parse Command line arguments
# Proper arguments are no args at all, or the following:
#
#     formatIB2R.pl --path=~/IB/trunk/Examples/rawData/YM-20110916
#
if ( scalar(@ARGV) == 1 ) {
  foreach my $arg (@ARGV) {
    ## DEBUG  print($arg, "\n") ;
    my ( $key, $value ) = split( '=', $arg ) ;
    if ( $key eq '--path' ) {
      $path = $value ;
    } else {
      print("Unrecogonized argument: $arg\n") ;
      exit() ;
    }
  }
}

# does directory exist
if (-d $path) { 
  ## DEBUG  print "$path exists\n" ;
} else { 
  print "Directory $path does not exist.\n" ;
  exit(2) ;
}

# Hardcoded files are placed in the initFileList() subroutine.
# my @theFiles = initFileList() ;  

# read all the *.bz2 files in the path
my @theFiles = readFileList($path) ;

## DEBUG    print map {"$_\n"} @theFiles ;
## DEBUG    exit() ;

## processOneFile(qq|$path/1sec-ES-20110617-20110317-20110317.csv.bz2|) ;

print qq("Date","Open","High","Low","Close","Volume"\n) ;
foreach my $fileName ( @theFiles ) {
  processOneFile($path . '/' . $fileName) ;
}

############################################################
############################################################

sub processOneFile {
    my $fileName = shift ;

    open(my $fh, "$bunzip2 -c $fileName|") ;

    while(my $line = <$fh>) {
      chomp($line) ;
      chomp($line) ;

      ## reqId,startTime,endTime,endTime,open,high,low,close,volume,wap,count,hasGaps,segmentStart,segmentEnd
      next if $line =~ /reqId/i ; 

      if ( $line !~ /^\d+/ )
      {
        print STDERR "UNKNOWN line type ($.): $line / $fileName\n" ;
        next ;
      }

      ## Input Format
      ## 1,1300086000,1300086001,2011-03-14 00:00:01,1296.0,1296.0,1296.0,1296.0,64,1296.0,20,false,20110314 00:00:00,20110314 00:33:20
      ## 1,1300086001,1300086002,2011-03-14 00:00:02,1296.0,1296.0,1296.0,1296.0,3,1296.0,1,false,20110314 00:00:00,20110314 00:33:20
      ## 1,1300086002,1300086003,2011-03-14 00:00:03,1296.0,1296.0,1296.0,1296.0,0,1296.0,0,false,20110314 00:00:00,20110314 00:33:20
      ## 1,1300086003,1300086004,2011-03-14 00:00:04,1296.0,1296.0,1296.0,1296.0,0,1296.0,0,false,20110314 00:00:00,20110314 00:33:20
      ## 1,1300086004,1300086005,2011-03-14 00:00:05,1296.0,1296.0,1296.0,1296.0,12,1296.0,9,false,20110314 00:00:00,20110314 00:33:20
      ## 1,1300086005,1300086006,2011-03-14 00:00:06,1295.75,1295.75,1295.75,1295.75,21,1295.75,4,false,20110314 00:00:00,20110314 00:33:20
      ## 1,1300086006,1300086007,2011-03-14 00:00:07,1296.0,1296.0,1295.75,1296.0,22,1296.0,13,false,20110314 00:00:00,20110314 00:33:20
      ## 1,1300086007,1300086008,2011-03-14 00:00:08,1296.0,1296.0,1295.75,1295.75,118,1295.75,40,false,20110314 00:00:00,20110314 00:33:20

      my( $reqId,$startTimeEpoch,$endTimeEpoch,$endTimeTS,$open,$high,$low,$close,$volume,$wap,$count,$hasGaps,$segmentStart,$segmentEnd) ;
      ($reqId,$startTimeEpoch,$endTimeEpoch,$endTimeTS,$open,$high,$low,$close,$volume,$wap,$count,$hasGaps,$segmentStart,$segmentEnd) 
        = split(/,/, $line) ;

      my $startTimeTS = strftime "%Y-%m-%d %H:%M:%S", localtime($startTimeEpoch) ;

      ## Output format:
      ##  "Date","Open","High","Low","Close","Volume"
      ##  2009-06-01 00:00:00,923.5,923.75,923,923.25,5
      ##  2009-06-01 00:10:00,926.75,928,926.75,928,5
      ##  2009-06-01 00:15:00,928,929.25,927.75,928.5,8

      print qq($startTimeTS,$open,$high,$low,$close,$volume\n) ;

    }

}


############################################################
############################################################

sub initFileList {
    return qw(
1sec-YM-20110916-20110601-20110601.csv.bz2
1sec-YM-20110916-20110602-20110602.csv.bz2
1sec-YM-20110916-20110603-20110603.csv.bz2
1sec-YM-20110916-20110605-20110605.csv.bz2
1sec-YM-20110916-20110606-20110606.csv.bz2
1sec-YM-20110916-20110607-20110607.csv.bz2
1sec-YM-20110916-20110608-20110608.csv.bz2
1sec-YM-20110916-20110609-20110609.csv.bz2
1sec-YM-20110916-20110610-20110610.csv.bz2
1sec-YM-20110916-20110612-20110612.csv.bz2
1sec-YM-20110916-20110613-20110613.csv.bz2
1sec-YM-20110916-20110614-20110614.csv.bz2
1sec-YM-20110916-20110615-20110615.csv.bz2
1sec-YM-20110916-20110616-20110616.csv.bz2
1sec-YM-20110916-20110617-20110617.csv.bz2
1sec-YM-20110916-20110619-20110619.csv.bz2
1sec-YM-20110916-20110620-20110620.csv.bz2
1sec-YM-20110916-20110621-20110621.csv.bz2
1sec-YM-20110916-20110622-20110622.csv.bz2
1sec-YM-20110916-20110623-20110623.csv.bz2
1sec-YM-20110916-20110624-20110624.csv.bz2
1sec-YM-20110916-20110626-20110626.csv.bz2
1sec-YM-20110916-20110627-20110627.csv.bz2
1sec-YM-20110916-20110628-20110628.csv.bz2
1sec-YM-20110916-20110629-20110629.csv.bz2
1sec-YM-20110916-20110630-20110630.csv.bz2
1sec-YM-20110916-20110701-20110701.csv.bz2
1sec-YM-20110916-20110703-20110703.csv.bz2
1sec-YM-20110916-20110704-20110704.csv.bz2
1sec-YM-20110916-20110705-20110705.csv.bz2
1sec-YM-20110916-20110706-20110706.csv.bz2
1sec-YM-20110916-20110707-20110707.csv.bz2
1sec-YM-20110916-20110708-20110708.csv.bz2
1sec-YM-20110916-20110710-20110710.csv.bz2
1sec-YM-20110916-20110711-20110711.csv.bz2
1sec-YM-20110916-20110712-20110712.csv.bz2
1sec-YM-20110916-20110713-20110713.csv.bz2
1sec-YM-20110916-20110714-20110714.csv.bz2
1sec-YM-20110916-20110715-20110715.csv.bz2
1sec-YM-20110916-20110717-20110717.csv.bz2
1sec-YM-20110916-20110718-20110718.csv.bz2
1sec-YM-20110916-20110719-20110719.csv.bz2
1sec-YM-20110916-20110720-20110720.csv.bz2
1sec-YM-20110916-20110721-20110721.csv.bz2
1sec-YM-20110916-20110722-20110722.csv.bz2
1sec-YM-20110916-20110724-20110724.csv.bz2
1sec-YM-20110916-20110725-20110725.csv.bz2
1sec-YM-20110916-20110726-20110726.csv.bz2
1sec-YM-20110916-20110727-20110727.csv.bz2
1sec-YM-20110916-20110728-20110728.csv.bz2
1sec-YM-20110916-20110729-20110729.csv.bz2
1sec-YM-20110916-20110731-20110731.csv.bz2
1sec-YM-20110916-20110801-20110801.csv.bz2
1sec-YM-20110916-20110802-20110802.csv.bz2
1sec-YM-20110916-20110803-20110803.csv.bz2
1sec-YM-20110916-20110804-20110804.csv.bz2
1sec-YM-20110916-20110805-20110805.csv.bz2
1sec-YM-20110916-20110807-20110807.csv.bz2
1sec-YM-20110916-20110808-20110808.csv.bz2
1sec-YM-20110916-20110809-20110809.csv.bz2
1sec-YM-20110916-20110810-20110810.csv.bz2
1sec-YM-20110916-20110811-20110811.csv.bz2
1sec-YM-20110916-20110812-20110812.csv.bz2
1sec-YM-20110916-20110814-20110814.csv.bz2
1sec-YM-20110916-20110815-20110815.csv.bz2
1sec-YM-20110916-20110816-20110816.csv.bz2
1sec-YM-20110916-20110817-20110817.csv.bz2
1sec-YM-20110916-20110818-20110818.csv.bz2
1sec-YM-20110916-20110819-20110819.csv.bz2
1sec-YM-20110916-20110821-20110821.csv.bz2
1sec-YM-20110916-20110822-20110822.csv.bz2
1sec-YM-20110916-20110823-20110823.csv.bz2
1sec-YM-20110916-20110824-20110824.csv.bz2
1sec-YM-20110916-20110825-20110825.csv.bz2
1sec-YM-20110916-20110826-20110826.csv.bz2
1sec-YM-20110916-20110828-20110828.csv.bz2
1sec-YM-20110916-20110829-20110829.csv.bz2
1sec-YM-20110916-20110830-20110830.csv.bz2
1sec-YM-20110916-20110831-20110831.csv.bz2
1sec-YM-20110916-20110901-20110901.csv.bz2
1sec-YM-20110916-20110902-20110902.csv.bz2
1sec-YM-20110916-20110904-20110904.csv.bz2
1sec-YM-20110916-20110905-20110905.csv.bz2
1sec-YM-20110916-20110906-20110906.csv.bz2
1sec-YM-20110916-20110907-20110907.csv.bz2
1sec-YM-20110916-20110908-20110908.csv.bz2
1sec-YM-20110916-20110909-20110909.csv.bz2
1sec-YM-20110916-20110911-20110911.csv.bz2
1sec-YM-20110916-20110912-20110912.csv.bz2
1sec-YM-20110916-20110913-20110913.csv.bz2
1sec-YM-20110916-20110914-20110914.csv.bz2
1sec-YM-20110916-20110915-20110915.csv.bz2
1sec-YM-20110916-20110916-20110916.csv.bz2
    ) ;
}

############################################################
sub readFileList {
  my $dirName = shift ;

  opendir DIR, $dirName ;
  my @files = grep { $_ ne '.' && $_ ne '..' && $_ =~ /bz2$/} readdir DIR;
  closedir DIR;
  return @files ;
}
