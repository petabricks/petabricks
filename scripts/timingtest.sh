#!/bin/bash

Q='>/dev/null 2>/dev/null'

function p(){
  echo $@ >&2
}

function die(){
  p $@
  exit 1
}

function ec(){
  $@ || die "FAILED: $@"
}

NCPU=`grep -c -E '^(processor|CPU)' /proc/cpuinfo`

#make sure we are in the right dir and compiled
test -d ./examples || cd ..
ec test -d ./examples
ec test -d ./src


ODIR=./timingdata/`hostname`

function runTest(){
  test "$1" = "#" && return 0
  
  if ! test -f ./examples/$1 
  then
    p FAILED to compile $1
    return 1
  fi

  TESTNAME=`echo "$1" | tr -s '[ /.]' _`

  DATFILE="$ODIR/$TESTNAME"_"$2.dat"
  CFGFILE="$ODIR/$TESTNAME"_"$2.cfg"

  p -n "Running timing test for $1..."
  ./scripts/timingwrapper.py ./examples/$1 $2 --config=$CFGFILE >> $DATFILE
  p done
}

function firstArg(){
  echo "$1"
}

function map(){
  while read X
  do
    $@ $X
  done
}

function mapToTests(){
map $@ << EOF
  add  100
  multiply 100
  transpose 100
  sort/Sort 1000
EOF
}

if test "$1" != "-f"; then
  ec make -j$NCPU 
  (cd examples && ec make -j$NCPU `mapToTests firstArg`)
fi
mapToTests runTest

