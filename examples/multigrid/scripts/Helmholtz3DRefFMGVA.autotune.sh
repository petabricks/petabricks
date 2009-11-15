#!/bin/bash

nohup ./Helmholtz3DRefFMGVA --config=/scratch/cychan/Helmholtz3DRefFMGVA.cfg --autotune --autotune-site=-1 --min 1 --max 64 &> /scratch/cychan/Helmholtz3DRefFMGVA.autotune.out &
disown
