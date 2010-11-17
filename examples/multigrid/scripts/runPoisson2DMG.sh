#!/bin/bash

for ((j = 4; j <= 512; j *= 2)); do

  n=$(($j+1))

  for ((i = 0; i <= 4; i += 1)); do

    echo "Testing input size ${n}, accuracy bin ${i} ..."

    ./Poisson2DMG --n ${n} --config configs/Poisson2DMG/nodirect/best/n${n}_acc${i}/config

  done

done
