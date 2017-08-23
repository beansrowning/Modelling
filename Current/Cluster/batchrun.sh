#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 16
$ -R y
export PATH = "home/lsh1604217/R/x86_64-redhat-linux-gnu-library/3.4/snow/RMPISNOW;%PATH%"
mpirun -np 16 RMPISNOW --no-save -q -f clusterrun.R
