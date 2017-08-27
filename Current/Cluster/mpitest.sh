#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 13
#$ -R y
# 1 + number of slaves
# Run in Spawn mode
#      Master
#  1   2   3     4
# 5 6 7 8 9 10 11 12
mpirun -np 1 R --vanilla --no-save -f mpitest.R 13
