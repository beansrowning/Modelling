#!/bin/bash
#$ -N Sweden_worst_case
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 44
#$ -R y
mpirun -np 44 R CMD BATCH sweden_wc.R
mpirun -np 44 R CMD BATCH swedenpt1_wc.R
