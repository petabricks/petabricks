#!/usr/bin/perl

use Getopt::Std;
getopts("ha:bc:sv");
if (defined($opt_h)) {
	print "Usage: generate.pl <infile> <transformname> \n";
	exit 0;
}

die "Need infile and outfile: $!" unless @ARGV==2;

$pbfile = "$ARGV[1].pbcc";

open (FILE, $ARGV[0])  or die ("Error opening file $ARGV[0]\n");
open (OUTFILE, ">", $pbfile) or die ("Error opening file $pbfile \n");
print (OUTFILE "#include \"SOR2D.pbcc\"\n\n");
print (OUTFILE "transform $ARGV[1]\n");
print (OUTFILE "from IN[n,n], B[n,n]\n");
print (OUTFILE "to OUT[n,n]\n");
print (OUTFILE "{\n");
print (OUTFILE "   to (OUT o) from(IN in, B b)\n");
print (OUTFILE "  {\n");
print (OUTFILE "    ElementT it=1;\n");
print (OUTFILE "    switch (n)\n");
print (OUTFILE "    {\n");

while (<FILE>) {
  chomp;
  my @values = split("\t",$_);
  print (OUTFILE "	case $values[0]:\n");
  print (OUTFILE "	  it=$values[1];\n");
  print (OUTFILE "	  break;\n");
}

print (OUTFILE "	default:\n");
print (OUTFILE "	  it = 1;\n");
print (OUTFILE "      }\n");
print (OUTFILE "      SOR2D(o, in, b, it);\n");
print (OUTFILE " }\n");
print (OUTFILE "}\n");
close(FILE);
close(OUTFILE);
