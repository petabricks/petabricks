#!/bin/bash

cp ./scripts/Helmholtz3DFMG.autotune.helper.sh /scratch/cychan/
nohup /scratch/cychan/Helmholtz3DFMG.autotune.helper.sh &> /scratch/cychan/Helmholtz3DFMG.autotune.out &
disown
