#!/bin/bash
#$ -N Summary_stats2
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=1.5G,h_vmem=1.5G
#$ -q parallel.q
#$ -pe openmpi 128
#$ -R y
mpirun -np 128 R CMD BATCH fivenum2.R
