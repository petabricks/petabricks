#!/bin/bash

for ((i = 1; i <= 5; i += 1)); do

  cat FullPoisson2D.cfg | sed -r "s/prec_case = [0-9]+/prec_case = $i/" > FullPoisson2D.cfg.temp ; mv FullPoisson2D.cfg.temp FullPoisson2D.cfg
  cat FullPoisson2D.cfg | grep prec_case

  for ((j = 1; j <= 5; j += 1)); do
    ./FullPoisson2D --graph --multigrid --min 1 --max 1 --trials 100
    ./FullPoisson2D --graph --multigrid --min 2 --max 2 --trials 30
    ./FullPoisson2D --graph --multigrid --min 3 --max 4 --trials 10
    ./FullPoisson2D --graph --multigrid --min 5 --max 12 --trials 1
  done

done
