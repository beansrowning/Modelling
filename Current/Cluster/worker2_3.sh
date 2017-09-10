#!/bin/bash
#$ -N Worker_2
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=2.5G,h_vmem=2.5G
#$ -q parallel.q
#$ -pe openmpi 140
#$ -R y
#mpirun -np 64 R CMD BATCH sweden_wc.R
#mpirun -np 64 R CMD BATCH latvia_wc.R
mpirun -np 140 R CMD BATCH malta_wc.R
