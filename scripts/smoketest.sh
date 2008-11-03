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

#make sure we are in the right dir and compiled
test -d ./examples || cd ..
ec test -d ./examples
ec test -d ./src
ec make

function compilePbcc(){
  if ./src/pbc ./examples/$1.pbcc 2>/dev/null >/dev/null
  then
    printf "passed  | "
    return 0
  else
    printf "FAILED  | N/A                  "
    return 1
  fi
}

function testPbcc(){
  TST=./testdata/.output/`echo $1 $(basename "$2") $(basename "$3") $(basename "$4") $(basename "$5") | tr -s '[ /.]' _`
  if ./examples/$1 $2 $3 $4 $5 $6 $7 $8 $9 $TST >/dev/null 2>/dev/null
  then
    if git diff --exit-code $TST >/dev/null
    then
      printf "passed               "
    else
      printf "FAILED (wrong output)"
    fi
  else
      printf "FAILED (run error)   "
  fi
}

function runTest(){
  test "$1" = "#" && return 0
  printf "|%20s | " $1
  compilePbcc $@ && testPbcc $@
  printf "|\\n"
}

function runEachTest(){
  while read X
  do
    runTest $X
  done
}

printf '+---------------------+---------+----------------------+\n'
printf '|                NAME | COMPILE | TEST                 |\n'
printf '+---------------------+---------+----------------------+\n'

R2Da=./testdata/Rand2Da
R2Db=./testdata/Rand2Db
R2Dodd=./testdata/Rand2Dodd
R1D=./testdata/Rand1D
R1Dodd=./testdata/Rand1Dodd
R0D=./testdata/Rand0D
ONE=./testdata/One0D
ONEPOINTFIVE=./testdata/OnePointFive0D
TWO=./testdata/Two0D
TEN=./testdata/Ten0D

runEachTest << EOF
  add       $R2Da $R2Db
  multiply  $R2Da $R2Db
  transpose $R2Da 
  test1  $R2Da
  test2  $R2Da
  test3  $R2Da
  test4  $R2Da
  test5  $R2Da
  test6  $R2Da
  test7  $R2Da
  test8  $R2Da $R2Db $R0D
  test9  $R2Da $R0D
  test10 $R2Da
  test11 $R2Dodd
  test12 $R2Da
  poisson/Jacobi2D   $R2Da $R2Db $ONE
  poisson/Jacobi2D   $R2Da $R2Db $TEN
  poisson/SOR2D      $R2Da $R2Db $ONE
  poisson/SOR2D      $R2Da $R2Db $TEN
  poisson/BS2D       $R2Da $R2Db
  sort/Quicksort     $R1Dodd
  sort/Mergesort     $R1Dodd
  sort/Insertionsort $R1Dodd
  sort/Radixsort     $R1Dodd
  sort/Sort          $R1D
  sort/Sort          $R1Dodd
EOF

printf '+---------------------+---------+----------------------+\n'

