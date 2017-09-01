#!/bin/bash
#$ -N Malta_worst_case
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=1.5G,h_vmem=1.5G
#$ -q parallel.q
#$ -pe openmpi 80
#$ -R y
mpirun -np 80 R CMD BATCH maltapt1_wc.R
mpirun -np 80 R CMD BATCH malta_wc.R
