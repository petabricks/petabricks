#!/bin/bash

for ((i = 0; i <= 4; i += 1)); do

  echo "Testing accuracy bin $i"

  ./Poisson2DRefMGVA --graph --exp --step 2 --min 4 --max 1024 --offset 1 --trials-sec 1 --trials-max 100000 --transform="Poisson2DRefMGVA<$i>" --noisolation

done
