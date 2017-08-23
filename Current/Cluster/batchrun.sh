#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 16
#$ -R y
mpirun -np 16 bash RMPISNOW --no-save -q -f clusterrun.R
