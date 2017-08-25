#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 17
#$ -R y
# 1 + number of slaves
# Run in Spawn mode
mpirun -np 1 R --vanilla --no-save -q -f clusterrun3_1.R
