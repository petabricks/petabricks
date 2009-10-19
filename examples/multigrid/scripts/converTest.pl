#!/usr/bin/perl

#determines number of iterations needed for error < threshold
#writes the error and iterations needed to outfile
#run "./converTest -h" for usage
#threshold set to 0.05

use Getopt::Std;
getopts("ha:bc:sv");
if (defined($opt_h)) {
	print "Usage: converTest.pl <function> <threshold> <outfile> <X> <B> \n";
	print "function:\n";
        print "j                (Jacobi)\n";
        print "s                (SOR)\n";
        print "m                (Multigrid V-cycle)\n";
        print "f                (Full multigrd v-cycle)\n";
	exit 0;
}

die "Need five arguments: $!" unless @ARGV==5;

$EXACT = "Xexact";
system("./BS2D $ARGV[3] $ARGV[4] $EXACT");

$TMP = "temp";
if ($ARGV[0] eq "j") {
    $fn="Jacobi2D"; $para="one";
}
elsif  ($ARGV[0] eq "s")   { 
    $fn="SOR2D"; $para="w one";
}   
elsif  ($ARGV[0] eq "m")   { 
    $fn="mgv2D"; $para="w one one itb";
}   
else   { 
    $fn="fmgv2D"; $para="w one one itb ksize";
}   

#print "./$fn $ARGV[3] $ARGV[4] $para $TMP\n";
system("./$fn $ARGV[3] $ARGV[4] $para $TMP");
system("./error $EXACT $TMP $ARGV[2]");

open (FH,$ARGV[2]) or die ("Error opening file $ARGV[2]\n");
$err=<FH>;
$err=<FH>;
close FH;
$iterations=1;

while ($err >= $ARGV[1] && $iterations < 1000) {
     system("./$fn $TMP $ARGV[4] $para $TMP");
     system("./error $EXACT $TMP $ARGV[2]");
     open (FH,$ARGV[2]) or die ("Error opening file $ARGV[2]\n");
     $err=<FH>;  
     $err=<FH>;
     close FH;
     $iterations++;
}

#open (FH,">",$ARGV[2]) or die ("Error opening file $ARGV[2]\n");
#print FH "$fn\nError: $err";
#print FH "Iterations: $iterations\n";
#close FH;

print "$fn\nError: $err";
print "Iterations: $iterations\n";
