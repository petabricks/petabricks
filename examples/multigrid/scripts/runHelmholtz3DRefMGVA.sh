#!/bin/bash

for ((i = 0; i <= 4; i += 1)); do

  echo "Testing accuracy bin $i"

  ./Helmholtz3DRefMGVA --graph --exp --step 2 --min 1 --max 64 --trials-sec 1 --trials-max 100000 --transform="Helmholtz3DRefMGVA<$i>" --noisolation

done
