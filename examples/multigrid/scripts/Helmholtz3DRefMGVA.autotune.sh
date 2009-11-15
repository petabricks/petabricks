#!/bin/bash

nohup ./Helmholtz3DRefMGVA --config=/scratch/cychan/Helmholtz3DRefMGVA.cfg --autotune --autotune-site=-1 --min 1 --max 64 &> /scratch/cychan/Helmholtz3DRefMGVA.autotune.out &
disown
