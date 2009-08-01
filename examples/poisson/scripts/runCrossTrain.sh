#!/bin/bash

numWorkers=8

cp FullPoisson2D.harpertown.cfg FullPoisson2D.cfg
./runFullPoisson2D.sh > runFullPoisson2D.train-harpertown.run-peryn.out

cp FullPoisson2D.harpertown.cfg Poisson2D.cfg
./Poisson2D &> /dev/null
./runPoisson2D.sh > runPoisson2D.train-harpertown.run-peryn.out

cp FullPoisson2D.barcelona.cfg FullPoisson2D.cfg
./runFullPoisson2D.sh > runFullPoisson2D.train-barcelona.run-peryn.out

cp FullPoisson2D.barcelona.cfg Poisson2D.cfg
./Poisson2D &> /dev/null
./runPoisson2D.sh > runPoisson2D.train-barcelona.run-peryn.out

cat FullPoisson2D.niagra.cfg | sed -r "s/Workers = [0-9]+/Workers = $numWorkers/" > FullPoisson2D.cfg
./runFullPoisson2D.sh > runFullPoisson2D.train-niagra.run-peryn.out

cat FullPoisson2D.niagra.cfg | sed -r "s/Workers = [0-9]+/Workers = $numWorkers/" > Poisson2D.cfg
./Poisson2D &> /dev/null
./runPoisson2D.sh > runPoisson2D.train-niagra.run-peryn.out
