#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 32
#$ -R y
mpirun -np 32 R -f hello_world.R
