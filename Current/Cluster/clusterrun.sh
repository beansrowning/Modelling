#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 17
#$ -R y
# 1 + number of slaves
mpirun -np 17 R --no-save -q -f clusterrun.R
