#!/bin/bash
#$ -N Malta
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=2G,h_vmem=2G
#$ -q parallel.q
#$ -pe openmpi 21
#$ -R y
#mpirun -np 64 R CMD BATCH malta.R
#mpirun -np 64 R CMD BATCH maltapt1.R
mpirun -np 21 R CMD BATCH maltapt1_wc.R
# mpirun -np 21 R CMD BATCH malta_wc.R
