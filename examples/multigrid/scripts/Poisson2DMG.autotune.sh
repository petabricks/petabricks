#!/bin/bash

nohup ./Poisson2DMG --config=/scratch/cychan/Poisson2DMG.cfg --autotune --min 2 --max 1024 --offset 1 --trials 3 &> /scratch/cychan/Poisson2DMG.autotune.out &
disown
