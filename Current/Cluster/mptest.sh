#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe smp 12
#$ -R y
export OMP_NUM_THREADS=12
R --vanilla -f openMP.R
