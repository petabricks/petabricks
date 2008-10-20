#!/bin/sh

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
  if ./examples/$1 $2 $3 $4 $5 ./testdata/.$1.out
  then
    if git-diff --exit-code ./testdata/.$1.out >/dev/null
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
  printf "|%10s | " $1
  compilePbcc $@ && testPbcc $@
  printf "|\\n"
}

function runEachTest(){
  while read X
  do
    runTest $X
  done
}

printf '+-----------+---------+----------------------+\n'
printf '|      NAME | COMPILE | TEST                 |\n'
printf '+-----------+---------+----------------------+\n'

R2Da=./testdata/Rand2Da
R2Db=./testdata/Rand2Db
R1D=./testdata/Rand1D
R0D=./testdata/Rand0D

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
EOF

printf '+-----------+---------+----------------------+\n'

