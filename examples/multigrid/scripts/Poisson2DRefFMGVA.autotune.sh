#!/bin/bash

nohup ./Poisson2DRefFMGVA --config=/scratch/cychan/Poisson2DRefFMGVA.cfg --autotune --autotune-site=-1 --min 2 --max 1024 --offset 1 &> /scratch/cychan/Poisson2DRefFMGVA.autotune.out &
disown
