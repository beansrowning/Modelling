#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 20
#$ -R y
mpirun -np 20 R -f hello_world.R
