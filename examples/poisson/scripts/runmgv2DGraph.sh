#!/bin/bash

for ((i = 1; i <= 9; i += 2)); do

  echo "Running prec_case = $i"

  cat mgv2DGraph.cfg | sed -r "s/_prec_case = [0-9]+/_prec_case = $i/" > mgv2DGraph.cfg.temp ; mv mgv2DGraph.cfg.temp mgv2DGraph.cfg
  #cat mgv2DGraph.cfg

  for ((j = 1; j <= 5; j += 1)); do
    ./mgv2DGraph --graph --multigrid --min 1 --max 1 --trials 100
    ./mgv2DGraph --graph --multigrid --min 2 --max 2 --trials 30
    ./mgv2DGraph --graph --multigrid --min 3 --max 4 --trials 10
    ./mgv2DGraph --graph --multigrid --min 5 --max 12 --trials 1
  done

done


