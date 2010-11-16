#!/bin/bash

for ((j = 2; j <= 32; j *= 2)); do

  n=$(($j+1))

  for ((i = 0; i <= 4; i += 1)); do

    echo "Testing input size ${n}, accuracy bin ${i} ..."

    ./Helmholtz3DMG --n ${n} --config configs/Helmholtz3DMG/nodirect/best/n${n}_acc${i}/config

  done

done
