#!/bin/bash

for ((i = 1; i <= 5; i += 1)); do

  cat Poisson2D.cfg | sed -r "s/prec_case = [0-9]+/prec_case = $i/" > Poisson2D.cfg.temp ; mv Poisson2D.cfg.temp Poisson2D.cfg
  cat Poisson2D.cfg | grep prec_case

  for ((j = 1; j <= 5; j += 1)); do
    ./Poisson2D --graph --multigrid --min 1 --max 1 --trials 100
    ./Poisson2D --graph --multigrid --min 2 --max 2 --trials 30
    ./Poisson2D --graph --multigrid --min 3 --max 4 --trials 10
    ./Poisson2D --graph --multigrid --min 5 --max 12 --trials 1
  done

done
