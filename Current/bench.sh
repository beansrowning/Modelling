#!/bin/bash
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 32
#$ -R y
mpirun -np 32 R --vanilla -f gsbench.R 
