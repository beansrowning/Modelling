#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 13
#$ -R y
# 1 + number of slaves
# Run in Spawn mode
mpirun -np 5 R --vanilla --no-save -f mpitest.R
