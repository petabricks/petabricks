#!/usr/bin/perl

#determines number of iterations needed for error < threshold
#writes the iterations needed for different sizes to outfile
#run "./converTest -h" for usage
#threshold set to 0.5

use Getopt::Std;
getopts("ha:bc:sv");
if (defined($opt_h)) {
	print "Usage: converSOR.pl <min> <max> <threshold> <outfile>  \n";;
        print "min              starting size of matrix\n";
        print "max              finishing size\n";
        print "threshold        error threshold\n";
        print "outfile          output file for iterations needed\n";
	exit 0;
}

die "Need four arguments: $!" unless @ARGV==4;

$fn="SOR2D"; $para="one";

$EXACT = "Xexact";
$XTMP = "Xtemp";
$Xerror ="Xerror";


for ($count=$ARGV[0];$count<=$ARGV[1]; $count++){

  system("~/hecura/scripts/genmatrixZeroBound.py $count > Xin");
  system("~/hecura/scripts/genmatrixZeroBound.py $count > RHS");
  system("./BS2D Xin RHS $EXACT");
  system("./$fn Xin RHS $para $XTMP");
  system("./error $EXACT $XTMP $Xerror");

open (FH,$Xerror) or die ("Error opening file $Xerror\n");
$err=<FH>;
$err=<FH>;
close FH;
$iterations=1;

while ($err >= $ARGV[2] && $iterations < 2000) {
     system("./$fn $XTMP RHS $para $XTMP");
     system("./error $EXACT $XTMP $Xerror");
     open (FH,$Xerror) or die ("Error opening file $Xerror\n");
     $err=<FH>;  
     $err=<FH>;
     close FH;
     $iterations++;
}

open (FH,">>",$ARGV[3]) or die ("Error opening file $ARGV[3]\n");
print FH "$count\t$iterations\n";
close FH;

}
