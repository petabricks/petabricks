#!/bin/bash

cp ./scripts/Poisson2DFMG.autotune.helper.sh /scratch/cychan/
nohup /scratch/cychan/Poisson2DFMG.autotune.helper.sh &> /scratch/cychan/Poisson2DFMG.autotune.out &
disown
