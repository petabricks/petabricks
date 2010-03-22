#!/bin/bash

./Poisson2DFMG --config=/scratch/cychan/Poisson2DFMG.cfg --autotune --min 2 --max 1024 --offset 1 --trials 3 --transform=Poisson2DMG
./Poisson2DFMG --config=/scratch/cychan/Poisson2DFMG.cfg --autotune --min 2 --max 1024 --offset 1 --trials 3 --transform=Poisson2DFMG
