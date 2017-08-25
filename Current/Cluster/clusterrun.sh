#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 17
#$ -R y
# 1 + number of slaves
# Run in non-spawn mode
mpirun -np 17 R --vanilla --no-save -q -f clusterrun.R
