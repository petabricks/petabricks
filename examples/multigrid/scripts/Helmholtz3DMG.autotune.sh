#!/bin/bash

nohup ./Helmholtz3DMG --config=/scratch/cychan/Helmholtz3DMG.cfg --autotune --min 1 --max 64 &> /scratch/cychan/Helmholtz3DMG.autotune.out &
disown
