#!/bin/bash
#$ -N Latvia
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=2G,h_vmem=2G
#$ -q parallel.q
#$ -pe openmpi 128
#$ -R y
mpirun -np 128 R CMD BATCH latvia.R
mpirun -np 128 R CMD BATCH latviapt1.R
mpirun -np 128 R CMD BATCH latvia_wc.R
mpirun -np 128 R CMD BATCH latviapt1_wc.R
