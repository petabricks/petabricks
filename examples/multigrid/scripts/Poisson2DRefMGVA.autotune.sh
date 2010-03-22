#!/bin/bash

nohup ./Poisson2DRefMGVA --config=/scratch/cychan/Poisson2DRefMGVA.cfg --autotune --autotune-site=-1 --min 2 --max 1024 --offset 1 &> /scratch/cychan/Poisson2DRefMGVA.autotune.out &
disown
