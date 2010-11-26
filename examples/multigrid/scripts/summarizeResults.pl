#!/usr/bin/perl

my $result;
while ($line = <>) {
  chomp($line);
  if ($line =~ /^([A-Za-z]+)\d+,(\d*),compute,([-+]?\d*\.?\d+([eE][-+]?\d+)?)$/) {
    $result->{$1}->{$2}->[$#{$result->{$1}->{$2}} + 1] = $3;
  }
}

foreach $function (keys %{$result}) {

  print "$function\n";
  %curResults = %{$result->{$function}};

  foreach $size (sort {$a <=> $b} (keys %curResults)) {
#    print "Size = $size\n";
    my @temp;
    @temp = sort {$a <=> $b} @{$curResults{$size}};
#    foreach $i (0 .. $#temp) {
#      print "  $temp[$i]\n";
#    }
    $average = 0;
    foreach $i (2 .. $#temp - 2) {
      $average += $temp[$i];
    }
    $numSamples = $#temp + 1;
    $average /= $numSamples - 4;
#    print "  Average: $average\n";
    print "$size\t$average\t$numSamples\n";
  }

}
