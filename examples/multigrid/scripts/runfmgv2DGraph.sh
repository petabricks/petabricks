#!/bin/bash

for ((i = 1; i <= 9; i += 2)); do

  echo "Running prec_case = $i"

  cat fmgv2DGraph.cfg | sed -r "s/_prec_case = [0-9]+/_prec_case = $i/" > fmgv2DGraph.cfg.temp ; mv fmgv2DGraph.cfg.temp fmgv2DGraph.cfg
  #cat fmgv2DGraph.cfg

  for ((j = 1; j <= 5; j += 1)); do
    ./fmgv2DGraph --graph --multigrid --min 1 --max 1 --trials 100
    ./fmgv2DGraph --graph --multigrid --min 2 --max 2 --trials 30
    ./fmgv2DGraph --graph --multigrid --min 3 --max 4 --trials 10
    ./fmgv2DGraph --graph --multigrid --min 5 --max 12 --trials 1
  done

done
