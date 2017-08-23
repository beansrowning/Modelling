#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 2
#$ -R y
mpirun -np 2 R --no-save -q -f clusterrun.R
